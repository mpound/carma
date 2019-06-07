/**
 * @author Dave Mehringer
 *
 * @version $Id: monitorDataLoader.cc,v 1.89 2014/04/02 23:11:05 iws Exp $
 *
 * @usage monitorDataLoader [conffile=[dbms/dbms.conf]] [frames=[0]] [locallevel=[8]]
 *
 * @description
 * This program is responsible for loading monitor data into the database
 *
 * @key conffile dbms/dbms.conf string file from which to get database
 *                                     configuration info as well as
 *                                     directories where data files are
 *                                     located; the conffile location is
 *                                     interpreted by Program::getConfFile()
 *
 * @key localLevel 8 int level of output for this file
 *
 * @key frames 0 int number of frames to run for, 0 means run forever
 *
 * @key checkSig true bool check to make sure tagID/canonical name signature in
 *                         flat file matches that in the db before loading
 *                         (should *always* be true in production)
 * @key useWorkDir true bool Save binary to ASCII files to dbWorkDir.
 *
 * @key maxtime 5.0 double maximum time to spend in a directory
 *
 * @logger DEFAULT_FACILITY carma.dbms.monitorDataLoader
 */

#include <iostream>
#include <set>
#include <sstream>
#include <string>

#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>

#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/MonitorDataDatabase.h"
#include "carma/dbms/TableNames.h"
#include "carma/dbms/dbFFIO.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/dbms/MonitorDataTableFinisherThread.h"
#include "carma/dbms/TableManager.h"
#include "carma/monitor/DataflowSubsystem.h"
#include "carma/util/FileUtils.h"
#include "carma/util/StopWatch.h"
#include "carma/util/IllegalStateException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Program.h"
#include "carma/util/StopWatch.h"
#include "carma/util/Trace.h"
#include "carma/util/Time.h"

using namespace ::std;
using namespace carma::dbms;
using namespace carma::util;
using namespace carma::monitor;
using carma::util::StopWatch;


/** MDL can take a long time to process a directory (particularly
 frame). In order to get something processed from all directories in a
 more or less reasonable time, we limit the number of minutes spent in a
 given directory pass.
 (After the conversion to mysql 5, ~30 secs/per directory is typical).
 */
static double MAXTIMEPERDIR = 5.0*60.0;  // Time in seconds.

namespace {


bool gFinishedWithTablesIsRunning = false;
  bool useWorkDir = false;
  string workDir = "";

}  // namespace < anonymous >

/**
 * delete pointers to completed threads
 */
void deleteFinishedThreadPointers
    (set<const MonitorDataTableFinisherThread * >& finisherThreads) {
    set<const MonitorDataTableFinisherThread * >::const_iterator iter 
        = finisherThreads.begin();
    set<const MonitorDataTableFinisherThread *> threadsToErase;
    for(; iter != finisherThreads.end(); iter++) {
        if( (*iter)->isFinished()) {
            threadsToErase.insert(*iter);
        }
    }

#if 0
    for(iter = threadsToErase.begin(); iter != threadsToErase.end(); iter++) {
        CARMA_CPTRACE(Trace::TRACE6,"deleting finished thread " 
                << *iter);
        delete *iter;
        CARMA_CPTRACE(Trace::TRACE6, "successfully deleted finished "
                << "thread ");
        CARMA_CPTRACE(Trace::TRACE6,"removing finished thread " 
                << *iter << " from set");
        finisherThreads.erase(*iter);
        CARMA_CPTRACE(Trace::TRACE6, "successfullly removed finished "
                << "thread " << *iter << " from set");
        CARMA_CPTRACE(Trace::TRACE6, "Thread " << *iter 
                << " deleted and removed from set");
    }
#else
    for(iter = threadsToErase.begin(); iter != threadsToErase.end(); iter++) {

        CARMA_CPTRACE(Trace::TRACE6,"removing finished thread " 
                << *iter << " from set");
        finisherThreads.erase(*iter);
        CARMA_CPTRACE(Trace::TRACE6, "successfullly removed finished "
                << "thread " << *iter << " from set");
        CARMA_CPTRACE(Trace::TRACE6,"deleting finished thread " 
                << *iter);
        delete *iter;
        CARMA_CPTRACE(Trace::TRACE6, "successfully deleted finished "
                << "thread ");
        CARMA_CPTRACE(Trace::TRACE6, "Thread " << *iter 
                << " deleted and removed from set");
    }
#endif

}

void finishWithTable
    (const string& tableName, 
     set<const MonitorDataTableFinisherThread * >& finisherThreads,
     const DBConnection *dbc, 
     const MonitorAverageType avgType) {
    MonitorDataTableFinisherThread *mdtft = new MonitorDataTableFinisherThread
        (tableName, dbc,tableName, avgType);
    finisherThreads.insert(mdtft);
    ::deleteFinishedThreadPointers(finisherThreads);
}    

/*
string getSignature(const string& filename, const unsigned& sigLen) {
    int fd = open(filename.c_str(),O_RDONLY);
    auto_ptr<char>buf(new char[sigLen]);
    read(fd, buf.get(), sigLen);
    close(fd);
    return string(buf.get(),sigLen);
}
*/


/**
 * get the blessed signature valid at the specified frame count
 */
string
getBlessedSignature( frameType            frameCount,
                     const DBConnection * dbc, 
                     log4cpp::Category &  logger ) {
    MonitorConfigurationDatabase mcdb(dbc);
    
    auto_ptr<string> bst (mcdb.getTagIDNameSignature(frameCount));
    
    if ( bst.get() == 0 ) {
        ostringstream emsg;
        emsg << "The database contains no blessed tagID->canonical name "
             << "signatures for frameCount " << frameCount << ", which means "
             << "I cannot check for potential db "
             << "integrity problems, I refuse to run under such "
             << "conditions. Run " << Program::getRootDir()
             << "bin/canonicalNamesToTagIDsConfFileWriter to insert a "
             << "blessed signature into the db, then try me again.";
        logger << log4cpp::Priority::ERROR << emsg.str();
        throw CARMA_EXCEPTION(IllegalStateException,emsg.str());
    }
    
    return *bst;
}

static const string binSuffix(".bin");

static void removeSuffix(string &f, const string &suffix)
{  string::size_type n = f.rfind(suffix);
    if(n != string::npos)
      f.erase(n, string::npos);
}

// Given a file name like:
// <...>/monitor_data/dbload/frame/complex
// Return the "frame" (frame, minute, slcorrel, wbcorrel).
static void getDirType(const string &f, string &type)
{  string tmp = f; 
   string::size_type n = tmp.rfind("/");
   if(n != string::npos)
   { tmp.erase(n, string::npos);	// Erase trailing "/numeric", etc.
     if((n = tmp.rfind("/")) != string::npos) // Erase leading path.
     { tmp.erase(0, n+1);
       type = tmp;
       //       cout << "Dir type is " << tmp << " N(" << n << ")" << endl;
     }
   }
}

// Given a file name like:
// <...>/monitor_data/dbload/frame/complex
// Return the "complex" (numeric, short, string, complex).
static void getAggType(const string &f, string &type)
{  string tmp = f; 
  string::size_type n = tmp.rfind("/");
  if(n != string::npos)
    tmp.erase(0, n+1);	// Erase leading path.
}

// Given the name of a symbolic link, return the name of the file
// the link points to. Returns false if there was a problem. (See errno).
static bool readLink(const string &linkName, string &fileName)
{char namebuf[4097];
 int nchars = readlink(linkName.c_str(), namebuf, sizeof(namebuf)-1);
 if(nchars >= 0)
 {  namebuf[nchars] = '\0';
    fileName = namebuf;
 }
 return (nchars >= 0);
}

// If the fullName is a link pointing to a file with a name ending in ".bin":
//  1) Create an ASCII version of the file with the same name, but no ".bin".
//     If useWorkDir is true, the file is written to it.
//  2) Create a link to it with the same name as fullName w/o the .bin.
//  3) Add ".done" suffix to link name.
//  4) If useWorkDir is true, fullName is set to the ASCII file.
/*
  With work dir.
  Make asciiName point to workDir/simpleName.
  Copy binary file to it.
  linkName = fullName w/o ".bin"

  simpleName - name of symbolic link to data file w/o path.
  fullName   - name of symbolic link to data file with path.
*/

int handleBinaryFiles(string &simpleName, string &fullName,
		      const string &dirType)
{ string binFile;	// Name of binary file.
  string asciiFile;  // Name of ASCII data file.
  string linkName;   // Name of link to ASCII file.
  ostringstream emsg;

  //  cout << "simpleName(in) = " << simpleName << ". fullName = " << fullName << endl;
  if(fullName.rfind(binSuffix) == string::npos)
    { emsg << "fullName does not end in .bin!\n";
      log4cpp::Category & logger = Program::getLogger();
      logger << log4cpp::Priority::ERROR << emsg.str();
      return -1;
    }

  // Name of the real binary file.
  if(!readLink(fullName, binFile))
  {  //cout << "Could not readlink fullname.\n";
         //perror(fullName.c_str());
    emsg << "Could not readlink " << fullName << ".\n";
    log4cpp::Category & logger = Program::getLogger();
    logger << log4cpp::Priority::ERROR << emsg.str();
    return -2;
  }

  if(useWorkDir)
  { asciiFile = workDir + "/" + dirType + "_" + simpleName;
  }
  else
  {  asciiFile = binFile;
  }
  removeSuffix(asciiFile, binSuffix);

  // If the ASCII file already exists, don't recreate it.
  // (Or should we recreate it in case MDL died here??)
  if(!carma::util::FileUtils::exists(asciiFile))
    dbFFIOb::copyToASCII(binFile, asciiFile, true);
  //  else
  //    cout << asciiFile << " already exists.\n";

  // Now link to the file.
  linkName = fullName;
  removeSuffix(linkName, binSuffix);

  // If the link to the ASCII file exists, don't try to recreate it.
  if(!carma::util::FileUtils::exists(linkName))
    { if(symlink(asciiFile.c_str(), linkName.c_str()))
      {	emsg << "Error creating link: " << asciiFile
	     << "<-" << linkName;
	log4cpp::Category & logger = Program::getLogger();
	logger << log4cpp::Priority::ERROR << emsg.str();
	return -3;
      }
    }
  //  else
  //    cout << linkName << " already exists.\n";

  // If we're here, there is an ASCII datafile and a link to it.
  // So, rename the link to the binary to .done
  string doneFile = fullName + ".done";
  /*  int res = */ rename(fullName.c_str(), doneFile.c_str());

  // Remove ".bin" from the args.
  removeSuffix(simpleName, binSuffix);
  fullName = asciiFile;

  return 0;
}

static double getCurrentTime()
{struct timespec s;
 /*long err =*/ clock_gettime(CLOCK_REALTIME, &s);
 double rtn = s.tv_sec + s.tv_nsec*1.0e-9;
 return rtn;
}

/**
 * Track the number of files seen (in the load directories), loaded into
 * the database,  converted from ASCII and times.
 */

class RunTimeStats {
public:
  RunTimeStats() { myserial= rtsserial++; init(); }
  ~RunTimeStats(){}
  void printTimeStats(bool done, const string &dir);
  // Start/stop runtime clock.
  void startClock(){if(!runTime.isRunning()) runTime.start();}
  void stopClock(){if(runTime.isRunning()) runTime.stop();}

  void resetWatch(StopWatch &sw)
  { if(sw.isRunning()) sw.stop();
    sw.getCumulativeElapsedTime(true);  // Reset
  }
  void stopWatch(StopWatch &sw)
  { if(sw.isRunning()) sw.stop();
  }
  double getElapsedRunTime(){
    stopClock();
    return runTime.getElapsedTime();
  }
  void init();	// Reset things.
  ////////////////////////////////////////////////////////////////
  // Values that are accumulated until runtime gets to printInterval.
  StopWatch runTime, loadTime, binaryConvertTime;
  StopWatch scandir, newtable, finishtable;
  unsigned numFilesSeen;	// # of files seen.
  unsigned numFilesLoaded, numBinaryConverted, numDoneFilesSeen;
  unsigned numPasses;	// # of times directory was processed this interval.
  ////////////////////////////////////////////////////////////////
  unsigned myserial;
  static unsigned rtsserial;		// Which object is this (debuging).
  static unsigned printInterval;		// How often printing should happen.
};

unsigned RunTimeStats::rtsserial = 0;
unsigned RunTimeStats::printInterval=60;	// Min # of seconds between printing.
// (The program has to spend this much time in a directory before printing.
//  The total elapsed time is likely much more).

void RunTimeStats::init()
{ numFilesSeen = numFilesLoaded= numBinaryConverted= numDoneFilesSeen = 0;
  numPasses = 0; 
  resetWatch(runTime);
  resetWatch(loadTime);
  resetWatch(binaryConvertTime);
  resetWatch(scandir);
  resetWatch(newtable);
  resetWatch(finishtable);
  // cout << "RTS (" << myserial << ") inited\n";
}

void RunTimeStats::printTimeStats(bool done, const string &datadir)
{bool wasRunning = runTime.isRunning();

 if(wasRunning)  // Need to stop before reading.
    runTime.stop();
  // Total time spent running the program.
  double rt = runTime.getCumulativeElapsedTime();
  // Ignore if we didn't spend enough time or do enough.
  if( ((rt < printInterval) || (numFilesLoaded == 0)) && !done)
    {
#if 0
      cout << datadir << "(" << myserial << ")(numPasses=" << numPasses
	   << ") not enough time/files (" << rt
	   << ", " << numFilesLoaded << ")\n";
#endif
      return;
    }
  { if(loadTime.isRunning())
      loadTime.stop();
    // Time spent loading data.
   double lt = loadTime.getCumulativeElapsedTime(true);
   if(binaryConvertTime.isRunning())
     binaryConvertTime.stop();
   // Time spent converting from binary to ASCII.
   double bct = binaryConvertTime.getCumulativeElapsedTime(true);

   if(scandir.isRunning())
     scandir.stop();
   //   double sdt = scandir.getCumulativeElapsedTime(true);

   if(newtable.isRunning())
     newtable.stop();
   double newt = newtable.getCumulativeElapsedTime(true);

   if(finishtable.isRunning())
     finishtable.stop();
   double finisht = finishtable.getCumulativeElapsedTime(true);

    /* Line format: (times are in seconds).
       datadir="frame/numeric" nfiles="5" elapsedtime="1.2" filesloaded="5"
       fileloadtime="0.67" binaryfilesconverted="5" binaryconverttime="0.53"
       numpasses="20"
       Note that nfiles, etc. are totals for numpasses.


       Col	variable
       1-3	<Date/Time>
       4	datadir  (eg. frame/numeric)
       5	elapsedtime
       6	filesloaded
       7	fileloadtime
       8	binaryfilesconverted
       9	binaryconverttime
#removed       10	scandirtime
       11	newtabletime
       12	numdonefilesseen
       13	numpasses
       14	finishtabletime
    */
    ostringstream msg;
    msg.precision(3);
    msg << "Times: " << "datadir=\"" << datadir << "\""
	<< " nfiles=\"" << numFilesSeen << "\""
	<< " elapsedtime=\"" << rt << "\" "
	<< " filesloaded=\"" << numFilesLoaded << "\""
	<< " fileloadtime=\"" << lt << "\""
	<< " binaryfilesconverted=\"" << numBinaryConverted << "\""
	<< " binaryconverttime=\"" << bct << "\""
	<< " newtabletime=\"" << newt << "\""
	<< " numdonefilesseen=\"" << numDoneFilesSeen << "\""
	<< " numpasses=\"" << numPasses << "\""
	<< " finishtime=\"" << finisht << "\"";

    CARMA_CPTRACE(Trace::TRACE3, msg.str());
    log4cpp::Category & logger = Program::getLogger();
    logger << log4cpp::Priority::INFO << msg.str();
  }
  init();
}

// Keep track of how much time MDL sleeps and print out the stats
// when the total reaches some arbitrary value. (Don't want to
// swamp the logs, but would like print frequently enough to be useful).
// Max amount of time to sleep. (More or less arbitrary).
static const unsigned MAXSLEEPTIME = 30;

static unsigned totalSleep=0, nSleepPasses=0;
static void printSleepStats(unsigned usleepTime, double elapsedTime)
{ static double totalElapsedTime=0.0;

  totalSleep += usleepTime;
  nSleepPasses ++;
  totalElapsedTime += elapsedTime;
  //  if(totalSleep >= 300)  // Print every 5 minutes, more or less.
  if(totalElapsedTime > 300) // Print every 5 minutes, more or less.
  { ostringstream msg;
    msg << "Times: data=\"timing/sleep\" "
	<< "sleepTime=\"" << totalSleep
	<< " nPasses=\"" << nSleepPasses << "\"";

    CARMA_CPTRACE(Trace::TRACE3, msg.str());
    log4cpp::Category & logger = Program::getLogger();
    logger << log4cpp::Priority::INFO << msg.str();

    totalSleep = 0;
    nSleepPasses = 0;
    totalElapsedTime = 0.0;
  }
}


// Remove pathname. and leave, for example, "frame/numeric".
static void removePath(const string &dir, string &name)
{ string tmp = dir, b;
  string::size_type n = tmp.rfind("/");
  if(n != string::npos)
  { b = tmp.substr(n, tmp.size()-n);
    tmp.erase(n, string::npos);
    if((n = tmp.rfind("/")) != string::npos) // Erase leading path.
    { tmp.erase(0, n+1);
      name = tmp + b;
    }
  }
}

/**
   dbload/frame/{complex,numeric,short,string}/file.mpdat.bin->
   	monitorData/frame/{complex,numeric,short,string}/file.mpdat.bin

   Other directories are similar but binary files are currently only used
   in the frame subdirectories.

   Once a file has been loaded, the link has a ".done" added to it.

   For binary files, the binary file is converted to ASCII and a link
   to it is created. The original link is renamed to ".done".

 */

int
loadData( const DBConnection *              dbc,
          TableManager &                    tm, 
          map< MonitorDataIndex, string > & dataDirs,
          const int                         maxFrames,
          const bool                        checkSig ,
	  const carma::monitor::DataflowSubsystem &dataflow_) {
    log4cpp::Category & logger = Program::getLogger();
    
    if ( checkSig == false ) {
        CARMA_CPTRACE(Trace::TRACE6, "checkSig=false, not "
                      << "checking signature (if in production, this is NOT "
                      << "the right thing to do");
    }
    
    string tableName;
    int partitionID;
    struct dirent **namelist = 0;
    string filename;
    string simpleName;
    string outfile;
    int res;
    map<MonitorDataIndex,string>::iterator iter = dataDirs.begin();
    map<MonitorDataIndex,string> mpLoadDirs;
    map<MonitorDataIndex,int> dir2fileCount;
    map<MonitorDataIndex,int> dir2lastCreationCount;
    map<MonitorDataIndex,string> activeTableName;
    map<string,RunTimeStats> timeStats;
    map<MonitorAverageType, map<MonitorAggregateType,int> > numberLoaded;
    map<MonitorAverageType, map<MonitorAggregateType,int> > numberToLoad;
    map<MonitorAverageType, map<MonitorAggregateType,int> > oldestTableFrame;
    map<MonitorAverageType, map<MonitorAggregateType,int> > newestTableFrame;
    DataflowSubsystem::MonitorDataLoader& ms = dataflow_.monitorDataLoader();
    DataflowSubsystem::MonitorPointDatabase& mpd = dataflow_.monitorPointDatabase();
    dataflow_.startAutoWriter(0.100);

    for(int i = 0; i < 4; i++){
      DataflowSubsystem::LoadedFiles& inf = ms.loadedFiles(i);
      for(int j = 0; j < 4; j++){
	numberLoaded[static_cast<MonitorAverageType>(i)][static_cast<MonitorAggregateType>(j)] = 0;
	numberToLoad[static_cast<MonitorAverageType>(i)][static_cast<MonitorAggregateType>(j)] = 0;
	DataflowSubsystem::MDLSubTypes& minf = inf.mDLSubTypes(j);
	minf.numberLoaded().setValue(0);
	minf.numberToLoad().setValue(0);
	minf.lastRunTime().setValue(0.0);
      }
    }

    for(int i = 0; i < 4; i++){
      DataflowSubsystem::DatabaseFiles& inf = mpd.databaseFiles(i);
      for(int j = 0; j < 4; j++){
	newestTableFrame[static_cast<MonitorAverageType>(i)][static_cast<MonitorAggregateType>(j)] = 0;
	oldestTableFrame[static_cast<MonitorAverageType>(i)][static_cast<MonitorAggregateType>(j)] = 999999999;
	DataflowSubsystem::MPSubTypes& minf = inf.mPSubTypes(j);
	minf.oldestTableDate().setValue("");
	minf.newestTableDate().setValue("");
	minf.oldestTableFrame().setValue(0);
	minf.newestTableFrame().setValue(0);
      }
    }

    for(iter=dataDirs.begin(); iter != dataDirs.end(); iter++) {
        MonitorDataIndex mdi = iter->first;
        if(mdi.getAreaType() == MP_LOAD_AREA) {
            mpLoadDirs[mdi] = iter->second;
            dir2fileCount[mdi] = 0;
            dir2lastCreationCount[mdi] = -1;
            activeTableName[mdi] = "";
	    { string dirName;
	      removePath(iter->second, dirName);
	      timeStats[dirName].init();
	    }
        }
    }

    int fileCount;
    int lastCreationCount;
    string dir;
    string donefile;
    //bool useProdDB = dbc->usingProductionDB();
    string doneSuffix = ".done";
    //    unsigned reqSpace = 20;
    // Rough estimate of the amount of space a table can take in MB assuming
    // no packing.
    // (Use a big estimate to reduce frequency of having to drop tables).
    unsigned reqSpace = 300;
    const frameType beginFrame = Time::computeCurrentFrame( );
    const frameType endFrame = beginFrame + maxFrames;

    string tag;
    // fix up NULLs in MONITOR_DATA_INDEX table which
    // may have been left from a prior run
    {
        MonitorDataDatabase mddb(dbc);
        mddb.updateMonitorIndexTable(0);
    }
    
    set<const MonitorDataTableFinisherThread * > finisherThreads;

    bool done = false;
    
    string dirType = "";
    string aggType = "";

    MonitorAverageType mavgType;
    MonitorAggregateType maggType = MAX_AGGREGATE_DATA_TYPE;

    double elapsedTime;		// Amount of time to traverse all dirs.
    while ( done == false ) {
      ostringstream stmt;
      stmt << "SELECT tableName,minIntegration,maxIntegration FROM " 
	   << getTableName(MONITOR_INDEX_TABLE) 
	   << " WHERE minIntegration is not NULL "
	   << "ORDER BY minIntegration ASC";         
      Table t = dbc->execSQLSelect(stmt.str(),
				    "Monitor Tables - Oldest Data to Newest");
      Column<string> tableList = t.getStringColumn("tableName");
      Column<int> minList = t.getIntColumn("minIntegration");
      Column<int> maxList = t.getIntColumn("maxIntegration");

      for(unsigned int i=0; i < tableList.size(); i++) {
	if(tableList[i].compare(0, 5, "Frame") == 0){
	  mavgType = FRAME_AVG;
	}
	else if(tableList[i].compare(0, 8, "SLCorrel") == 0){
	  mavgType = SLCORREL_AVG;
	}
	else if(tableList[i].compare(0, 8, "WBCorrel") == 0){
	  mavgType = WBCORREL_AVG;
	}
	else if(tableList[i].compare(0, 6, "Minute") == 0){
	  mavgType = MINUTE_AVG;
	}
	else{
	  continue;
	}
	if(tableList[i].find("ComplexMonitorData") != string::npos){
	  maggType = COMPLEX_TYPE;
	}
	else if(tableList[i].find("NumericMonitorData") != string::npos){
	  maggType = NUMERIC_TYPE;
	}
	else if(tableList[i].find("ShortMonitorData") != string::npos){
	  maggType = SHORT_TYPE;
	}
	else if(tableList[i].find("StringMonitorData") != string::npos){
	  maggType = STRING_TYPE;
	}
	else{
	  continue;
	}
	if(minList[i] < oldestTableFrame[mavgType][maggType]){
	  oldestTableFrame[mavgType][maggType] = minList[i];
	}
	else if(maxList[i] > newestTableFrame[mavgType][maggType]){
	  newestTableFrame[mavgType][maggType] = maxList[i];
	}
      }

      for(int i = 0; i < 4; i++){
	DataflowSubsystem::DatabaseFiles& inf = mpd.databaseFiles(i);
	for(int j = 0; j < 4; j++){
	  DataflowSubsystem::MPSubTypes& minf = inf.mPSubTypes(j);
	  string tempString = carma::util::Time::getFITSdateTimeString(
		  static_cast<frameType>(oldestTableFrame[static_cast<MonitorAverageType>(i)][static_cast<MonitorAggregateType>(j)]),0);
	  string::size_type pos = tempString.find("T");
	  tempString.replace(pos,1," ");
	  minf.oldestTableDate().setValue(tempString);
	  
	  tempString = carma::util::Time::getFITSdateTimeString(
                 static_cast<frameType>(newestTableFrame[static_cast<MonitorAverageType>(i)][static_cast<MonitorAggregateType>(j)]),0);
	  pos = tempString.find("T");
	  tempString.replace(pos,1," ");
	  minf.newestTableDate().setValue(tempString);
	  minf.oldestTableFrame().setValue(oldestTableFrame[static_cast<MonitorAverageType>(i)][static_cast<MonitorAggregateType>(j)]);
	  minf.newestTableFrame().setValue(newestTableFrame[static_cast<MonitorAverageType>(i)][static_cast<MonitorAggregateType>(j)]);
	}
      }

      // StopWatch dirTime; // Amount of time spent processing each directory.

        // loop over monitor data directories
        elapsedTime = 0.0;
        for(iter=mpLoadDirs.begin(); iter != mpLoadDirs.end(); iter++) {
            MonitorDataIndex mdi= iter->first;
	    string dirName; // Name of dir. eg. frame/numeric.
	    double dirStartTime = getCurrentTime(); // Time this dir started.
            dir = iter->second;
	    getDirType(dir, dirType);
	    if(dirType.find("frame") != string::npos){
	      mavgType = FRAME_AVG;
	    }
	    else if(dirType.find("minute") != string::npos){
	      mavgType = MINUTE_AVG;
	    }
	    else if(dirType.find("slcorrel") != string::npos){
	      mavgType = SLCORREL_AVG;
	    }
	    else if(dirType.find("wbcorrel") != string::npos){
	      mavgType = WBCORREL_AVG;
	    }
	    getAggType(dir, aggType);
	    if(dirType.find("numeric") != string::npos){
	      maggType = NUMERIC_TYPE;
	    }
	    else if(dirType.find("string") != string::npos){
	      maggType = STRING_TYPE;
	    }
	    else if(dirType.find("short") != string::npos){
	      maggType = SHORT_TYPE;
	    }
	    else if(dirType.find("complex") != string::npos){
	      maggType = COMPLEX_TYPE;
	    }
	    DataflowSubsystem::LoadedFiles& inf = ms.loadedFiles(static_cast<int>(mavgType));
	    DataflowSubsystem::MDLSubTypes& minf = inf.mDLSubTypes(static_cast<int>(maggType));

	    numberLoaded[mavgType][maggType] = 0;
	    numberToLoad[mavgType][maggType] = 0;
	    removePath(dir, dirName);
	    RunTimeStats &rts = timeStats[dirName];
	    rts.startClock();
	    rts.numPasses++;

	    rts.scandir.start();
	      const int n = scandir(dir.c_str(), &namelist, 0, alphasort);
	    rts.scandir.stop();

        if ( n < 0 ) {
            const int savedErrno = errno;
            
            ostringstream emsg;
            
            emsg << "monitorDataLoader: Scandir returned " 
                 << n << " when scanning " << "directory " << dir 
                 << ". Check that the " << "directory exists and "
                 << " is readable (errno=" << savedErrno << ", "
                 << strerror( savedErrno ) << ")";
            
            CARMA_CPTRACE( Trace::TRACE1, emsg.str() );
            
            logger << log4cpp::Priority::CRIT << emsg.str();
            
            return EXIT_FAILURE;
        }
        
        fileCount = dir2fileCount[mdi];
        lastCreationCount = dir2lastCreationCount[mdi];
	    rts.numFilesSeen += n - 2; // Don't count "." & "..".

#if 0
	    { CARMA_CPTRACE (Trace::TRACE6, "Processing " << n - 2
			     << " files in " << dir);
	      logger << log4cpp::Priority::INFO
		     << "Processing " << n
		     << " files in " << dir;
	    }
#endif

            // loop over files in a monitor data directory
            for(int i = 0; i < n; i++) {
	      simpleName = string(namelist[i]->d_name);
	      free(namelist[i]);
	      bool isDoneFile = (simpleName.rfind(doneSuffix) != string::npos);
	      if(!isDoneFile)
		numberToLoad[mavgType][maggType]++;
	      else
		{ 
		  rts.numDoneFilesSeen++;
		  continue;
		}

		/** Limit the amount of time MDL spends in any one
		    directory.
		    The check is after the above free(), but before anything
		    intensive happens.
		    (Misses the scandir. Oh well).
		 */
#if 1
		{ double et = getCurrentTime() - dirStartTime;
		  if(et > MAXTIMEPERDIR)
		     continue;  // Loop to cleanup namelist.
		}
#else
		if(dirTime.isRunning())
		  dirTime.stop();
		if(i == 0)
		{ dirTime.getCumulativeElapsedTime(true); // reset.
		}
		else
		{ double et = dirTime.getCumulativeElapsedTime(false);
		   if(et > MAXTIMEPERDIR)
		     continue;  // Loop to cleanup namelist.
		}
		dirTime.start();
#endif

		// Easy early tests to avoid files we don't handle.
                if(simpleName == "." || simpleName == ".." )
		  continue;

                auto_ptr<struct stat> buf(new struct stat);
                string fullName = dir + "/" + simpleName;
                const int mm = stat(fullName.c_str(),buf.get());
                if ( mm == -1 ) {
                    const int savedErrno = errno;
                    
                  ostringstream emsg;
                  if ( savedErrno == ENOENT )
                    {
                      // Ignore links ending in ".done". monitorDataDeleter
                      // deals with them.
		      //    if(simpleName.rfind(doneSuffix) == string::npos)
		      if(!isDoneFile)
                      {  emsg << "Deleting dangling symlink " << fullName;
                         logger << log4cpp::Priority::WARN << emsg.str();
			 unlink(fullName.c_str());
		      }
                    } else {
                        emsg << "Unhandled error stating symlink " << fullName;
                        logger << log4cpp::Priority::ERROR << emsg.str();
                        throw CARMA_EXCEPTION
                            (IllegalStateException, emsg.str());
                    }

                }
		else if(!isDoneFile){
                    // This file needs to be loaded into the db

		    // Check for binary files.
		    if(simpleName.rfind(binSuffix) != string::npos)
		    { int err;
		      rts.binaryConvertTime.start();
		      err = handleBinaryFiles(simpleName, fullName, dirType);
		      rts.binaryConvertTime.stop();
		      rts.numBinaryConverted += 1;
		      if(err != 0)
		      {  return EXIT_FAILURE;
		      }
		    }

		    // Start a new a table every 10 files.
                    if(fileCount % 10 == 0 && fileCount != lastCreationCount) {
		      rts.finishtable.start();
                        if(activeTableName[mdi] != "") {
                            // this table is full, finish up with it
                            ::finishWithTable
                                  (activeTableName[mdi], finisherThreads,
                                   dbc, 
                                   mdi.getAverageType());
                            assert(!finisherThreads.empty());
                        }
		      rts.finishtable.stop();

		      rts.newtable.start();
                        // create a new table
                        {
                            ostringstream msg;

                            msg << Time::computeCurrentFrame() << "_" << fileCount;

                            tag = msg.str();
                        }
                        //FIXME need a way to determine estmated table size 
                        // in this call
                        partitionID = tm.createMonitorDataTable
                            (mdi.getAverageType(), mdi.getAggregateType(), 
                             reqSpace, tableName,tag);
                        lastCreationCount = fileCount;
                        activeTableName[mdi] = tableName;
                        CARMA_CPTRACE(Trace::TRACE5, 
                                "partition ID on which the new table "
                                << "named " << tableName << " has been "
                                << "created is " << partitionID);
		      rts.newtable.stop();
                    }

                    // the "file" will be a symlink created in the correct 
                    // place by the writer after the file has been completely
                    // written
                    filename = dir + "/" + simpleName;
                    /*
                    string fileSig = getSignature
                                   (filename,blessedSignature.length());
                    */
                    if ( checkSig ) {
                        CARMA_CPTRACE(Trace::TRACE6, "Checking "
                                      << "tagID/canonical name signature for "
                                      << "file " << filename);
                        string fileSig;
                        frameType frameCount;
                        FileUtils::readMonitorDataFlatFileHeader
                            (filename,frameCount,fileSig);
                        CARMA_CPTRACE(Trace::TRACE6, "Read "
                                      << "header of file " << filename 
                                      << " found frameCount=" << frameCount 
                                      << " and fileSig=" << fileSig);
                        if(frameCount == 0) {
                            ostringstream emsg;
                            emsg << "Corrupt file (frameCount is 0) "
				 << filename << " has no "
                                 << "header! I will not load it into the "
                                 << "database";
                            logger << log4cpp::Priority::WARN << emsg.str();
                            CARMA_CPTRACE(Trace::TRACE3, emsg.str());
                        } else {
                            string blessedSignature;
                            try {
                                blessedSignature = getBlessedSignature
                                    (frameCount,dbc,logger);
                            } catch (const IllegalStateException& 
                                     exc) {
                                ostringstream emsg;
                                emsg << exc.what() << ": Error determining "
                                     << "blessed signature "
                                     << "for file " << filename << " which has "
                                     << "frameCount " << frameCount;
                                logger << log4cpp::Priority::ERROR 
                                        << emsg.str();
                                throw CARMA_EXCEPTION
                                    (IllegalStateException,
                                     emsg.str());
                            }
                            CARMA_CPTRACE(Trace::TRACE6, 
                                          "Blessed signature is " 
                                          << blessedSignature);
                            if(fileSig != blessedSignature) {
                                ostringstream emsg;
                                emsg << "Tag ID->name signature " << fileSig 
                                     << " in file " << filename << " differs "
                                     << "from the blessed signature " 
                                     << blessedSignature << " . This file "
                                     << "will therefore not be ingested into "
                                     << "the database as it will cause "
                                     << "integrity problems";
				// NOTE: file IS ingested!!
				// Remove after things settle down (4/06).
				static int notIngestedCount=10;
				if(notIngestedCount > 0)
				{ logger << log4cpp::Priority::WARN 
                                        << emsg.str();
                                  CARMA_CPTRACE(Trace::TRACE3, 
                                              emsg.str());
				  notIngestedCount -= 1;
				}
                            }
                        }
                    } else {
                        ostringstream msg;
                        msg << "Application started with checkSig=f, so I'm "
                            << "not checking the tagID/canonical name "
                            << "signature of file " << filename << ". I hope "
                            << "you know what you are doing. If we running in "
                            << "a production environment, you obviously do "
                            << "not.";
                        logger << log4cpp::Priority::WARN << msg.str();
                        CARMA_CPTRACE(Trace::TRACE3, msg.str());
                    }
                    CARMA_CPTRACE(Trace::TRACE6, "Loading data "
                                  << "from " << filename);
		    // Have the database bulk load the file.
		    rts.loadTime.start();
                      dbc->loadDataFromFile
                        (filename,activeTableName[mdi], false,1,"\t");
		      rts.numFilesLoaded += 1;
		    rts.loadTime.stop();
                    fileCount++;
		    numberLoaded[mavgType][maggType]++;
                    numberToLoad[mavgType][maggType]--;
		    CARMA_CPTRACE(Trace::TRACE6, 
                                  "Successfully loaded data from " 
                                  << filename);
                    // done loading file, so rename it to indicate that
                    donefile = filename + doneSuffix;
                    res = rename(filename.c_str(), donefile.c_str());
                    if(res != 0) {
		      ostringstream msg;
                        CARMA_CPTRACE(Trace::TRACE1, 
                                    "Error renaming " << filename << " to "
                                    << donefile);
                        msg << "Error renaming " << filename << " to "
			    << donefile;
			logger << log4cpp::Priority::WARN << msg.str();
                    }
                } // == string.npos
		minf.numberLoaded().setValue(numberLoaded[mavgType][maggType]);
		minf.numberToLoad().setValue(numberToLoad[mavgType][maggType]);
		minf.lastRunTime().setValue((getCurrentTime() - dirStartTime)/60.0);

	    } // for(int i=0; 
	    free(namelist);
	    dir2fileCount[mdi] = fileCount;
	    dir2lastCreationCount[mdi] = lastCreationCount;
	    rts.stopClock();
	    elapsedTime += rts.getElapsedRunTime();
	    rts.printTimeStats(false, dirName);
	} // for( iter=mpLoadDirs

        if ( maxFrames != 0 )
            done = (Time::computeCurrentFrame( ) >= endFrame);
#if 0
        if ( done == false )
            sleep( 1 );  // be nice and don't hog the cpu
#else
	if(done == false)
	{  unsigned usleepTime = 0;
	   // Be nice and don't hog the cpu.
	   // If the directories are mostly empty and MDL zips through them,
	   // wait a few seconds for MAW to catch up.
	   // 30 secs seems as good as anything. (60secs?)
	   if(elapsedTime < MAXSLEEPTIME)
	     usleepTime = static_cast<unsigned>(MAXSLEEPTIME - elapsedTime);
	   // Make sure an accident doesn't make us sleep for a long time.
	   if((usleepTime == 0) || (usleepTime > 100))
	     usleepTime = 1;
#if 0
	   { ostringstream msg;
	     msg << "Sleeping for " << usleepTime << " sec(s).";
	     CARMA_CPTRACE(Trace::TRACE6, msg.str());
	   }
#endif
	   sleep(usleepTime);
	   printSleepStats(usleepTime, elapsedTime);
	}
#endif
    } // (done == false)

    // finish up with open tables
    for(iter=mpLoadDirs.begin(); iter != mpLoadDirs.end(); iter++) {
        MonitorDataIndex mdi= iter->first;
        if(activeTableName[mdi] != "") {
            ::finishWithTable(activeTableName[mdi], finisherThreads,
                              dbc, mdi.getAverageType());
        }
    }
    
    while ( gFinishedWithTablesIsRunning ) {
        CARMA_CPTRACE(Trace::TRACE1, "finishedWithTables() is "
                << "running, sleeping");
        sleep(1);
    }
    
    CARMA_CPTRACE(Trace::TRACE1, "size of finisher thread set " 
            << finisherThreads.size())
    set<const MonitorDataTableFinisherThread * >::const_iterator titer =
        finisherThreads.begin();
    for( ; titer != finisherThreads.end(); titer++) {
        CARMA_CPTRACE(Trace::TRACE6, "waiting for threads to exit " 
                << finisherThreads.size());
        while( !(*titer)->isFinished()) {
            sleep(1);
        }
    }

    CARMA_CPTRACE(Trace::TRACE6, "all threads have finished, "
            << "deleting remaining pointers... ");
    ::deleteFinishedThreadPointers(finisherThreads);
    return EXIT_SUCCESS;
}

int
Program::main( ) {
    const int maxFrames = getIntParameter("frames");
    const bool checkSig = getBoolParameter("checkSig");
    const string conffile = getConfFile( getStringParameter( "conffile" ) );
    useWorkDir = getBoolParameter("useWorkDir");
    MAXTIMEPERDIR = getDoubleParameter("maxtime")*60.0;
    carma::monitor::DataflowSubsystem dataflow_;
    map<MonitorDataIndex,string> dataDirs;
    auto_ptr<DBConfigurator> dbconf;
    try {
        auto_ptr<DBConfigurator> dbconftmp 
            (new DBConfigurator(conffile));
        dbconf = dbconftmp;
        dataDirs = dbconf->getMonitorDataAreas();

    } catch (const NotFoundException & exc) {
        ostringstream msg;
        msg << "Unable to read configation file " << conffile;
        CARMA_CPTRACE (Trace::TRACE1, msg);
        getLogger() << log4cpp::Priority::CRIT << msg.str();
        exc.report();
        return EXIT_FAILURE;
    }

    if(useWorkDir)
    { try {
	workDir = dbconf->getWorkDir();
	//	cout << "workDir is: |" << workDir << "|\n";

    } catch (const NotFoundException & exc) {
        ostringstream msg;
        exc.report();
        msg << "useWorkDir is true, but dbWorkDir was not set in config file: "
	    << conffile;
	msg << " Setting useWorkDir to false.";
        CARMA_CPTRACE (Trace::TRACE3, msg);
        getLogger() << log4cpp::Priority::NOTICE << msg;
	useWorkDir = false;
    }
    }

    //Trace::TraceLevel localLevel =
    //    (Trace::TraceLevel)getIntParameter("localLevel");

    if(useWorkDir && !carma::util::FileUtils::exists(workDir))
      {
      useWorkDir = false;
      }

    int exitStatus;
    log4cpp::Category & logger = getLogger();
    ostringstream msg;
    try {
        auto_ptr<DBConnection> dbc
            (DBConnectionFactory::createConnection(dbconf.get()));
        CARMA_CPTRACE(Trace::TRACE5, "Connection successful");
        TableManager tm(false,dbconf.get());
        exitStatus = loadData( dbc.get(), tm, dataDirs, maxFrames, checkSig,
			       dataflow_ );
    } catch (const DBConnectionException & exc) {
        msg << "monitorDataLoader: Program::main - caught " 
           << "DBConnectionException: " << exc.what();
        CARMA_CPTRACE (Trace::TRACE1, msg.str());
        logger << log4cpp::Priority::CRIT << msg.str();
        exitStatus = EXIT_FAILURE;
    } catch (const SQLException & exc) {
        msg << "monitorDataLoader: Program::main - caught " 
           << "SQLException: " << exc.what();
        CARMA_CPTRACE (Trace::TRACE1, msg.str());
        logger << log4cpp::Priority::CRIT << msg.str();
        exitStatus = EXIT_FAILURE;
    } catch (const TableManagerException & exc) {
        msg << "monitorDataLoader: Program::main - caught " 
           << "TableManagerException: " << exc.what();
        CARMA_CPTRACE (Trace::TRACE1, msg.str());
        logger << log4cpp::Priority::CRIT << msg.str();
        exitStatus = EXIT_FAILURE;
    } catch (const NotFoundException & exc) {
        msg << "monitorDataLoader: Program::main - caught " 
           << "NotFoundException: " << exc.what();
        CARMA_CPTRACE (Trace::TRACE1, msg.str());
        logger << log4cpp::Priority::CRIT << msg.str();
        exitStatus = EXIT_FAILURE;
    }/* catch(...) {
        msg << "monitorDataLoader: Program::main - caught " 
           << "unknown exception.";
        CARMA_CPTRACE (Trace::TRACE1, msg.str());
        logger << log4cpp::Priority::CRIT << msg.str();
        exitStatus = EXIT_FAILURE;
        }*/
    if(exitStatus == EXIT_SUCCESS) {
        CARMA_CPTRACE (Trace::TRACE4, "exiting normally");
    } else {
        CARMA_CPTRACE (Trace::TRACE1, "a problem occurred, "
                       << "exiting abnormally");
    }
    return exitStatus;
}
