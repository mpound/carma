/**
 * @author Dave Mehringer
 *
 * @version 1.0
 * @usage monitorDataDeleter [conffile=[dbms/conf/dbms.conf]] [frames=[0]] 
 * @description
 * This program is responsible for deleting monitor data files after they
 * have been loaded into the database and archived
 *
 * @key conffile dbms/dbms.conf string file from which to get directory info on
 *                                     directories where files to be deleted
 *                                     are located; the conffile location
 *                                     interpreted by Program::getConfFile()
 *
 * @key dosdp true bool
 *      Require sdp links
 *
 * @key dotransfer true bool
 *      Require transfer links
 *
 * @key frames 0 i number of frames to run for, 0=>run forever
 *
 * @key testing false bool
 *	If true, don't really remove files
 *
 * @key maxFiles 60000 i maximum number of files
 *
 * @key frameDelay 3600 i number of frames to delay before deleting files (3600 = half hour)
 * 
 * @logger DEFAULT_FACILITY carma.dbms.monitorDataDeleter
 */

#include <cerrno>
#include <cstring>
#include <dirent.h>
#include <unistd.h>
#include <list>
#include "carma/dbms/DBConfigurator.h"
#include "carma/monitor/DataflowSubsystem.h"
#include "carma/util/FileUtils.h"
#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"
#include "carma/util/Time.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/StopWatch.h"

using namespace std;
using namespace carma::dbms;
using namespace carma;
using namespace carma::monitor;
using carma::util::StopWatch;

static util::frameType maxFrames = 0;
static bool dosdp = false;
static bool dotransfer = false;
static bool testing = false;
static bool backlog = false;
static int maxFiles = 0;
static int peakFiles = 0;
static string peakDir = "";
static int frameDelay = 0;

//#define TESTING

// Used to remove leading path so trace messages are a bit shorter.
static const char *top[] = {"dbload", "monitorData", "sdp", "transfer"};
static const int NTOPS= sizeof(top)/sizeof(*top);

static void makeShortName(const string &longName, string &shortName)
{ shortName = longName;

    for(int i=0; i < NTOPS; i++)
    {  string::size_type n = shortName.rfind(top[i]);
	if(n != string::npos)
	{  shortName.erase(0, n);
	    break;
	}
    }
}

static int myremove(const string &rmname)
{string name=rmname;
 
    makeShortName(rmname, name); 
    
    string msg = "Not really removing: \"" + name + "\"";
    
#if defined(TESTING)
    cout << msg << endl;
#else
    
    CPTRACE(carma::util::Trace::TRACE5, msg);
#endif
    
    return 0;
}


// Remove a file. If removal fails, free up rest of namelist and return
// error message.
// If removal worked, add name of file that was removed to msg.
static int myremove(const string &mpFile, stringstream &msg)
{int status;

    if(testing)
	status = myremove(mpFile);
    else if(mpFile == "") // Ignore empty names.
	return 0;
    else
	status = remove(mpFile.c_str());
    
    if(status != 0)
    {   
	const int savedErrno = errno;
	msg << "error deleting " << mpFile << ", ::remove failed ("
	    << strerror( savedErrno ) << ").";
	carma::util::programLogErrorIfPossible( msg.str( ).c_str( ) );
	
	// CPTRACE isn't here because it grabs the line number.
	return EXIT_FAILURE;
    }
    
    { string sn;
	makeShortName(mpFile, sn);
	msg << sn << " ";
    }
    return 0;
}

class watchedData{
public:
    watchedData()
	{ reset(); resetPasses();}
    
    ~watchedData(){}
    void incPasses(const int npasses=1) {npasses_ += npasses;}
    void incFilesSeen(int nfiles)
	{ if((minFilesSeen_ > nfiles)||(minFilesSeen_<0))
	    {minFilesSeen_ = nfiles; minFilesSeenPass_= npasses_;}
	    
	    if(maxFilesSeen_ < nfiles)
	    {maxFilesSeen_ = nfiles; maxFilesSeenPass_=npasses_;}
	    nFilesSeen_ += nfiles;
	}
    void incDoneFiles(int nfiles)
	{ if((minDoneFiles_ > nfiles)||(minDoneFiles_ < 0))
	    {minDoneFiles_ = nfiles; minDoneFilesPass_= npasses_;}
	    
	    if(maxDoneFiles_ < nfiles)
	    {maxDoneFiles_ = nfiles; maxDoneFilesPass_= npasses_;}
	    
	    nDoneFiles_ += nfiles;
	}
    void incNotReadyFiles(int nfiles)
	{ if((minNotReadyFiles_ > nfiles)||(minNotReadyFiles_ < 0))
	    {minNotReadyFiles_ = nfiles; minNotReadyFilesPass_= npasses_;}
	    
	    if(maxNotReadyFiles_ < nfiles)
	    {maxNotReadyFiles_ = nfiles; maxNotReadyFilesPass_= npasses_;}
	    
	    nNotReadyFiles_ += nfiles;
	}
    void resetPasses(){npasses_ = 0;}
    void reset()
	{ npasses_ = 0;
	    nFilesSeen_ = nDoneFiles_ = nNotReadyFiles_ = 0;
	    minFilesSeen_ = -1; minFilesSeenPass_ = 0;
	    maxFilesSeen_ = -1; maxFilesSeenPass_ = 0;
	    minDoneFiles_ = -1; minDoneFilesPass_ = 0;
	    maxDoneFiles_ = -1; maxDoneFilesPass_ = 0;
	    minNotReadyFiles_ = -1; minNotReadyFilesPass_ = 0;
	    maxNotReadyFiles_ = -1; maxNotReadyFilesPass_ = 0;
	}
    
    void printInfo(const char *str=0)const;
    int npasses_;
    int nFilesSeen_;
    int minFilesSeen_;
    int minFilesSeenPass_;
    int maxFilesSeen_;
    int maxFilesSeenPass_;
    int nDoneFiles_;
    int minDoneFiles_;
    int minDoneFilesPass_;
    int maxDoneFiles_;
    int maxDoneFilesPass_;
    int nNotReadyFiles_;
    int minNotReadyFiles_;
    int minNotReadyFilesPass_;
    int maxNotReadyFiles_;
    int maxNotReadyFilesPass_;
};

void watchedData::printInfo(const char *msg)const
{ stringstream m;
    //	   float afpp = static_cast<float>(nFilesSeen)/nPasses;
    if(msg)
	m << msg;
    m << " After " << npasses_ << " passes: "
      << " nFiles=" << nFilesSeen_ << " nDone=" << nDoneFiles_
      << " nNotReady=" << nNotReadyFiles_;
    if(npasses_ > 0)
    { m << ". Avg per pass: "
	<< " nFiles=" << nFilesSeen_/npasses_
	<< " nDone=" << nDoneFiles_/npasses_
	<< " nNotReady=" << nNotReadyFiles_/npasses_;
    }
    m  << ".";
    CPTRACE(carma::util::Trace::TRACE5, m.str());
    carma::util::Program::getLogger() 
	<< ::log4cpp::Priority::INFO << m.str();
}

// Given the name of a symbolic link, return the name of the file
// it points to. Returns false if there was a problem.
static bool readLink(const string &linkName, string &fileName)
{char namebuf[4097];
    int nchars = readlink(linkName.c_str(), namebuf, sizeof(namebuf)-1);
    if(nchars >= 0)
    {    namebuf[nchars] = '\0';
	fileName = namebuf;
    } 
    else 
    {
	const int savedErrno = errno;
	ostringstream os;
	os << "::readlink failed (" << strerror( savedErrno ) << ").";
	carma::util::programLogErrorIfPossible( os.str( ).c_str( ) );
#if defined(TESTING)
        cout << "Problem with readlink(" << linkName << ")\n";
        perror(linkName.c_str());
#endif
    }
    return (nchars >= 0);
}

// get all files in the given directory, runs scandir and then returns the
// files in a list
bool getFiles(list<string> &files, string &directory,
	      const MonitorAverageType & avgType,const bool mpdir = false){
    int n;
    int length;
    string simpleName;
    struct dirent **namelist = 0;
    n = scandir(directory.c_str(), &namelist, 0, alphasort);
    if (n < 0) {
	const int savedErrno = errno;
	ostringstream errmsg;
	errmsg << "::scandir " << directory << " failed ( " 
	       << ::strerror( savedErrno ) << " ).";
	carma::util::programLogErrorIfPossible( errmsg.str( ).c_str( ) );
	
	return false;
    }
    if(n > peakFiles){
	peakFiles = n;
	peakDir = directory;
    }
    // if we have too many files the system is probably lagging
    //  thus get rid of non-critial files to speed things up
    if(n > ::maxFiles && !::backlog){
	::backlog = true;
	return false;
    }
    for(int i = 0; i < n; i++) {
	simpleName = string(namelist[i]->d_name);
	free(namelist[i]);
	length = simpleName.length();
	// Ignore names that are too short or don't end in ".done".
	// unless a backlog is detected then the listing will be:
	// all frame data files
	// all dbload files
	// all sdp and transfer files marked .done
	// all monitor files
	if(length >= 6 && (simpleName.substr(length - 5) == ".done" || mpdir || ::backlog)) {
	  if(mpdir || (::backlog && (simpleName.substr(length - 5) == ".done"))){
		files.push_back(simpleName);
	    }
	    else if(!::backlog){
		files.push_back(simpleName.substr(0,length-5));
	    }
	}
    }
    free(namelist);
    return true;
}

// rests the counts of the deleted files
void resetCounts(int write[], int trans[], int sdp[]){
    for(int i = 0; i < 4; i++){
	write[i] = 0;
	trans[i] = 0;
	sdp[i] = 0;
    }
}

int deleteData(const map<MonitorDataIndex,string>& dataDirs,
	       const carma::monitor::DataflowSubsystem &dataflow_) {
    map<MonitorDataIndex,string>::const_iterator iter = dataDirs.begin();
    map<MonitorDataIndex,string>::const_iterator iter1 = dataDirs.begin();
    map<MonitorDataIndex,string> mpWriteDirs;
    map<MonitorDataIndex,string> mpArchiveDirs;
    map<MonitorDataIndex,string> mpSdpDirs;
    for(iter=dataDirs.begin(); iter != dataDirs.end(); iter++) {
        MonitorDataIndex mdi = iter->first;
        MonitorDataAreaType areaType = mdi.getAreaType();
        if(areaType == MP_WRITE_AREA) {
            mpWriteDirs[mdi] = iter->second;
        } else if(areaType == MP_TRANSFER_AREA) {
            mpArchiveDirs[mdi] = iter->second;
        } else if(areaType == MP_SDP_AREA) {
            mpSdpDirs[mdi] = iter->second;
        }
    }
    // memory for namelist is allocated in scandir
    string dir;
    string simpleName;
    string mpDataFile;
    string mpArchiveFile;
    string mpSdpFile;
    int writeCount[4];
    int transCount[4];
    int sdpCount[4];
    bool haveDataFile, haveSdpFile;
    MonitorAverageType avgType;
    MonitorAggregateType aggType;
    bool done = false;
    int beginFrame = carma::util::Time::computeCurrentFrame();
    stringstream msg;
    StopWatch totalTimer(StopWatch::WALL_CLOCK,"Total Loop Time");
    StopWatch msgTimer;
    StopWatch scandirTimer( StopWatch::WALL_CLOCK, "Scandir stopwatch" );
    StopWatch readlinkTimer( StopWatch::WALL_CLOCK, "Readlink stopwatch" );
    StopWatch existsTimer( StopWatch::WALL_CLOCK, "Exists stopwatch" );
    StopWatch removeTimer( StopWatch::WALL_CLOCK, "Remove stopwatch" );
    unsigned long nFilesSeen=0, nFilesNotReady=0, nDoneFiles=0, nPasses=0;
    watchedData wdst;
    const double msgInterval=5*60.0; // # of seconds between msgs.
    DataflowSubsystem::MonitorDataDeleter& ms = dataflow_.monitorDataDeleter();
    dataflow_.startAutoWriter(0.100);
    /* For each directory listed in the (db)load area:
       Scan the directory for a list of file names. (w/o path).
       
       Create absolute file names for load, write, transfer(archive) and sdp.
       
       Check to see if the write file and transfer and sdp links exist. (If the
       load link didn't exist, it wouldn't be in the directory list).
       
       If all of the required symbolic links (load, transfer and sdp) exist,
       delete the data file. Otherwise, go to next file in the dir list. If the
       data file doesn't exist, the program died after removing it, but
       before the links could be removed, so it's OK to remove them.
       
       Once the data file is removed, remove transfer & sdp links.
       
       When the transfer and sdp links and the data file have been removed,
       it's safe to delete the load link.
       
    */
    // initialize the MPs
    ms.lastRun().runTime().setValue(0.0);
    ms.lastRun().backlog().setValue(false);
    ms.lastRun().farthestBehind().setValue("");
    ms.lastRun().peak().setValue(0);
    for(int i = 0; i < 4; i++){
	DataflowSubsystem::DeletedFiles& inf = ms.deletedFiles(i);
	inf.ahw().setValue(0);
	inf.transfer().setValue(0);
	inf.monitor().setValue(0);
	inf.total().setValue(0);
    }
    msgTimer.start();
    int count = 0;
    while(!done) {
	if(totalTimer.isRunning())
	    totalTimer.stop();
	totalTimer.start();
	resetCounts(writeCount,transCount,sdpCount);
	peakFiles = 0;
	peakDir = "";
	bool restart = false;
	if(::backlog){
	    count++;
	    if(count > 2){
		::backlog = false;
		count = 0;
	    }
	}
	for(iter=mpArchiveDirs.begin(); iter != mpArchiveDirs.end(); iter++) {
	    if(restart){
		ms.lastRun().backlog().setValue(::backlog);
		break;
	    }
	    MonitorDataIndex mdi= iter->first;
	    aggType = mdi.getAggregateType();
	    avgType = mdi.getAverageType();
	    dir = iter->second;
	    MonitorDataIndex mdi_write(aggType,avgType,MP_WRITE_AREA);
	    MonitorDataIndex mdi_sdp(aggType,avgType,MP_SDP_AREA);
	    MonitorDataIndex mdi_transfer(aggType,avgType,MP_TRANSFER_AREA);
	    int select = static_cast<int>(avgType);
	    list<string> writeFiles;
	    list<string> transFiles;
	    list<string> sdpFiles;
	    // Get list of files (actually links) in each load subdirectory.
	    bool mpSdp = (mpSdpDirs[mdi_sdp] != "");
	    scandirTimer.start();
	    if(!getFiles(transFiles,mpArchiveDirs[mdi_transfer],
				      avgType)){
		if(::backlog){
		    restart = true;
		    scandirTimer.stop();
		    break;
		}
		return EXIT_FAILURE;
	    }
	    if(!getFiles(writeFiles,mpWriteDirs[mdi_write],avgType,true)){
		if(::backlog){
		    restart = true;
		    scandirTimer.stop();
		    break;
		}
		return EXIT_FAILURE;
	    }
	    if(mpSdp && !getFiles(sdpFiles,mpSdpDirs[mdi_sdp],
				      avgType)){
		if(::backlog){
		    restart = true;
		    scandirTimer.stop();
		    break;
		}
		return EXIT_FAILURE;
	    }



	    scandirTimer.stop();
	    nPasses += 1;
	    nFilesSeen += ((int)mpArchiveDirs.size() - 2);  // Ignore "." & "..".
	    for(list<string>::iterator it = transFiles.begin(); it != transFiles.end(); it++) {
		nDoneFiles += 1;
		// Build filenames for the other links and the data file.
		mpArchiveFile = dir + "/" + *it + ".done";
		if(::backlog)
		    mpArchiveFile = dir + "/" + *it;
		
		{string linkVal;
		    readlinkTimer.start();
		    bool isOK = readLink(mpArchiveFile, linkVal);
		    readlinkTimer.stop();
		    
		    if(!isOK)
		    { carma::util::Program::getLogger() 
			    << ::log4cpp::Priority::ERROR
			    << "Error reading link for "
			    << mpArchiveFile;
		      if(!::backlog)
			return EXIT_FAILURE;
		    }
		    else
			mpDataFile = linkVal;
		}
		
		// Which one's exist?
		existsTimer.start();
		bool found = false;
		bool isDone = ((*it).substr((*it).length() - 5) == ".done");
		found = false;
		for(list<string>::iterator it2 = sdpFiles.begin(); it2 != sdpFiles.end(); it2++){
		    if(::backlog){
			if(isDone && *it == *it2){
			    sdpFiles.erase(it2);
			    found = true;
			    break;
			}
			else if(!isDone && *it == (*it2).substr(0,(*it2).length()-5)){
			    sdpFiles.erase(it2);
			    found = true;
			    break;
			}
		    }
		    else if(*it == *it2){
			found = true;
			sdpFiles.erase(it2);
			break;
		    }
		}

		haveSdpFile = found;
		found = false;
		for(list<string>::iterator it2 = writeFiles.begin(); it2 != writeFiles.end(); it2++){
		    if(::backlog){
			if(isDone && (*it).substr(0,(*it).length()-5) == *it2){
			    writeFiles.erase(it2);
			    found = true;
			    break;
			}
			else if(!isDone && *it == *it2){
			    writeFiles.erase(it2);
			    found = true;
			    break;
			}
		    }
		    else if(*it == *it2){
			found = true;
			writeFiles.erase(it2);
			break;
		    }
		}
		haveDataFile = found;
		bool haveCmdFile = 
		    util::FileUtils::exists("/tmp/monitorDataDeleter.cmd");
		
		existsTimer.stop();

		// Continue with deletions if all required links exist
		// or there is no data file (Which means deleting had
		// been interrupted during an earlier run), or a backlog
		// has been detected.
		bool canProceed = false;
		if(!haveDataFile){
		    canProceed = true;
		}
		else if(::backlog && (haveSdpFile || !dosdp)){
		    canProceed = true;
		}
		else if (avgType == MINUTE_AVG) {
		    canProceed = true;
		}
		else if ((avgType == WBCORREL_AVG) || 
			 (avgType == SLCORREL_AVG)) {
		    canProceed = haveSdpFile || !dosdp;
		};

		//------------------------------------------------------------
		// Test if this file is older than the specified
		// number of frames.  Only delete it if it is.
		//------------------------------------------------------------

		if(canProceed) {
		  int currentFrame = carma::util::Time::computeCurrentFrame();

		  std::string::size_type istart = (*it).find("_");
		  std::string::size_type istop  = (*it).find(".");
    
		  if(istart != std::string::npos && istop != std::string::npos) {
		    std::string frameStr = (*it).substr(istart+1, istop-istart-1);
		    int fileFrame = atoi(frameStr.c_str());
		    if(currentFrame - fileFrame < ::frameDelay) {
		      canProceed = false;
		    }
		  }
		}

		if(!canProceed)
		{ nFilesNotReady += 1;
		    continue;
		}
		removeTimer.start();
		// delete the files
		if(haveDataFile){
		    string filename = mpWriteDirs[mdi_write] + "/" + *it;
		    if(isDone)
			filename = mpWriteDirs[mdi_write] + "/" + (*it).substr(0,(*it).length()-5);
		    if(myremove(filename, msg) != 0) {
			CPTRACE (carma::util::Trace::TRACE1, msg.str());
			carma::util::Program::getLogger() 
			    << ::log4cpp::Priority::ERROR
			    << msg.str();
			return EXIT_FAILURE;
		    }
		    writeCount[select]++;
		}
		if(haveSdpFile){
		    string filename = mpSdpDirs[mdi_sdp] + "/" + *it + ".done";
		    if((*it).substr((*it).length()-5) == ".done")
			filename = mpSdpDirs[mdi_sdp] + "/" + *it;
		    if(::backlog && (*it).substr((*it).length() - 5) == ".done"){
			filename = mpSdpDirs[mdi_sdp] + "/" + *it;
		    }
		    if(myremove(filename, msg) != 0) {
			CPTRACE (carma::util::Trace::TRACE1, msg.str());
			carma::util::Program::getLogger() 
			    << ::log4cpp::Priority::ERROR
			    << msg.str();
			return EXIT_FAILURE;
		    }
		    sdpCount[select]++;
		}
		string filename = mpArchiveDirs[mdi_transfer] + "/" + *it + ".done";
		if((*it).substr((*it).length()-5) == ".done")
		  filename = mpArchiveDirs[mdi_transfer] + "/" + *it;
		if(::backlog && (*it).substr((*it).length() - 5) == ".done"){
		  filename = mpArchiveDirs[mdi_transfer] + "/" + *it;
		}
		if(myremove(filename, msg) != 0) {
		  CPTRACE (carma::util::Trace::TRACE1, msg.str());
		  carma::util::Program::getLogger() 
		    << ::log4cpp::Priority::ERROR
		    << msg.str();
		  return EXIT_FAILURE;
		}
		transCount[select]++;

		removeTimer.stop();
		
		CPTRACE (carma::util::Trace::TRACE5, "Deleted: " 
			 << msg.str())
		    if(haveCmdFile)
			carma::util::Program::getLogger() 
			    << ::log4cpp::Priority::INFO
			    << "Deleted: "
			    << msg.str().c_str();
		
		msg.str("");
	    }
	}
	
	// Note: One pass through can take less than 1 frame time so
	// even maxFrames=1 will result in a second pass.
	done = ((::maxFrames > 0) && ((carma::util::Time::computeCurrentFrame()
				       - beginFrame) >= ::maxFrames));
	
	wdst.incPasses(nPasses);
	wdst.incFilesSeen(nFilesSeen);
	wdst.incDoneFiles(nDoneFiles);
	wdst.incNotReadyFiles(nFilesNotReady);
	nFilesSeen = nDoneFiles = nFilesNotReady = nPasses = 0;
	
	msgTimer.stop();
	if(((msgTimer.getCumulativeElapsedTime() >= msgInterval) &&
	    (wdst.nFilesSeen_ > 0)) || done){
	    wdst.printInfo("File stats: ");
	    wdst.reset();
	    msgTimer.getCumulativeElapsedTime(true); // Reset counter.
	    // Log file IO stats as well while resetting timers.
	    
	    const double scandirSecs = scandirTimer.getCumulativeElapsedTime( true );
	    const double readlinkSecs = 
		readlinkTimer.getCumulativeElapsedTime( true );
	    const double existsSecs = 
		existsTimer.getCumulativeElapsedTime( true );
	    const double removeSecs =
		removeTimer.getCumulativeElapsedTime( true );
	    ostringstream msg; 
	    msg << "File stats I/O timing info (all wall clock in seconds): ";
	    msg << scandirSecs << "s in scandir, ";
	    msg << readlinkSecs << "s reading links, ";
	    msg << existsSecs << "s checking for file existence, ";
	    msg << removeSecs << "s removing files.";
	    carma::util::programLogInfoIfPossible( msg.str() );
	}
	totalTimer.stop();
	// write statistics to the monitor stream
	const double runTime = totalTimer.getCumulativeElapsedTime( true );
	ms.lastRun().runTime().setValue(static_cast<float>(runTime));
	ms.lastRun().backlog().setValue(::backlog);
	std::string::size_type loc = peakDir.rfind("/");
	if(loc != std::string::npos)
	    loc = peakDir.rfind("/",loc-1);
	if(loc != std::string::npos)
	    loc = peakDir.rfind("/",loc-1);
	ms.lastRun().farthestBehind().setValue(peakDir.substr(loc+1));
	ms.lastRun().peak().setValue(peakFiles);
	for(int i = 0; i < 4; i++){
	    DataflowSubsystem::DeletedFiles& inf = ms.deletedFiles(i);
	    if(i == static_cast<int>(MINUTE_AVG)){
		inf.dataLength().setValue(carma::dbms::toString(MINUTE_AVG));
	    }
	    else if(i == static_cast<int>(WBCORREL_AVG)){
		inf.dataLength().setValue(carma::dbms::toString(WBCORREL_AVG));
	    }
	    else if(i == static_cast<int>(SLCORREL_AVG)){
		inf.dataLength().setValue(carma::dbms::toString(SLCORREL_AVG));
	    }
	    inf.ahw().setValue(sdpCount[i]);
	    inf.transfer().setValue(transCount[i]);
	    inf.monitor().setValue(writeCount[i]);
	    inf.total().setValue(sdpCount[i]+transCount[i]+writeCount[i]);
	}
	if(!done)
	    msgTimer.start();
	
	// don't hog the cpu
	if(!done ) {
	    // If maxFrames is 1, don't sleep so long. (Probably debugging).
	    int sleeptime = (maxFrames == 1) ? 1 : 300;
	    // however if there are too many files then we need to wait less
	    if(::backlog)
		sleeptime = 30;
	    sleep(sleeptime);
	}
    }   
    return EXIT_SUCCESS;
}


int carma::util::Program::main() {
    string conffile = getConfFile(getStringParameter("conffile"));
    ::maxFrames = Program::getIntParameter("frames");
    map<MonitorDataIndex,string> dataDirs;
    carma::dbms::DBConfigurator *dbconf = 0;
    int exitStatus;
    ::dosdp = getBoolParameter("dosdp");
    ::dotransfer = getBoolParameter("dotransfer");
    ::testing = getBoolParameter("testing");
    ::maxFiles = getIntParameter("maxFiles");
    ::frameDelay = getIntParameter("frameDelay");
    carma::monitor::DataflowSubsystem dataflow_;
    
#if defined TESTING
    ::testing = true;
#endif
    
    {std::ostringstream msg;
	log4cpp::Category& logger = carma::util::Program::getLogger();
	
	msg << "Starting with: conffile=" << conffile
	    << " maxFrames=" << maxFrames
	    << " dosdp=" << boolalpha << dosdp
	    << " dotransfer=" << dotransfer
	    << " testing=" << testing
	    << " frameDelay=" << frameDelay;
	
        CPTRACE(carma::util::Trace::TRACE2, msg.str());
        std::ostringstream os;
        logger << log4cpp::Priority::NOTICE << msg.str();
    }
    
    try {
        dbconf = new DBConfigurator(conffile);
        dataDirs = dbconf->getMonitorDataAreas();
        exitStatus = deleteData(dataDirs,dataflow_);
    } catch (const NotFoundException & exc) {
        // FIXME change to FATAL in production mode
        carma::util::Program::getLogger() << ::log4cpp::Priority::ERROR
					  << "Unable to read configation file " << conffile;
        exc.report();
        exitStatus = EXIT_FAILURE;
    } catch(...) {
        log4cpp::Category& logger = carma::util::Program::getLogger();
        std::ostringstream os;
        os << "monitorDataDeleter: Program::main - caught " 
           << "unknown exception."<< std::endl;
        logger << log4cpp::Priority::ERROR << os.str();
        exitStatus = EXIT_FAILURE;
    }
    
    delete dbconf;
    return exitStatus;
}
