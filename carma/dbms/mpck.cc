/**
 * Scan the flat file ingest directories and look for problems such
 * as missing links. Optionally creates a command file to remove old
 * files that weren't removed because of these problems.
 *
 * @author: Harold Ravlin
 *
 *
 * @usage mpck [-b <start>] [-e <end>] [-c|-L] [-ofile outfile] [-match str]
 *
 * @description Scan flat file directories looking for problems.
 *
 *  Example output for long format:
 *
 * minute/complex/complex_431827800.mpdat 3:  OK|: [NOTDONE]
 *       complex_431827800.mpdat.done[(db:L:64:2006-11-03 16:05)()] 
 *       complex_431827800.mpdat[(md:F:885283:2006-11-03 16:05)()] 
 *       complex_431827800.mpdat[(xfr:L:64:2006-11-03 16:05)(NOTDONE)] 
 *
 *  The first line shows the base name, the number of files found for
 * the base name, that all expected files were found (OK) and that
 * the group flag is set to NOTDONE - meaning that one or more files haven't
 * been given the \".done\" suffix.
 *  The next three lines list the files found corresponding to the base name.
 * In brackets are which directory:
 *	db	- dbload
 *	md	- monitordata
 *	sdp	- sdp
 *	xfr	- transfer
 * After that is whether the name was a file (F) or link (L), the number
 * of bytes in the file and the file's timestamp. Finally, any flags
 * for the file are shown. In this example, the mpdat file under transfer
 * hasn't been marked \".done\" yet.
 *
 * In this example, there are a couple of errors:
 *
 * frame/complex/complex_429341841-429341880.mpdat 1: isOrphan  Req: dbload monitorData Found: monitorData|: []
 *       complex_429341841-429341880.mpdat.bin.write[(md:F:0:2006-10-20 09:48)()] 
 *
 * One file was found, it is an orphan. Files under dbload and monitordata
 * were expected, but only a file under monitordata was found.
 *  complex_408435563.mpdat(xfr:L:87:2006-06-21)
 *
 *  Date selection:
 *   Dates are in a form like \"2006-06-28\", \"2006-Jun-28\"
 *  or +/-<delta> (The '+' or '-' is required).
 *   For start:
 *  	+delta - Start printing delta days after the earliest timestamp.
 *  	-delta - start printing delta days before the last timestamp.
 *
 *   For end:
 *  	+delta - stop printing delta days after the value used for start
 *		(not the first timestamp)
 *  	-delta means stop printing delta days before the last timestamp.
 *
 *  The default values for start is the first timestamp.
 *  The default values for end is the the earliest of the last timestamp
 *  and 7 days before the current time.
 *
 * This program scans the database flat file directories looking for problems.
 *
 * @key start 0	string	Start time
 *
 * @key end  0	string	End time
 *
 * @key ofile	""	string	Output filename.
 *
 * @key summary false	bool	Only print summary info.
 *
 * @key orphans false	bool	Only print orphans info.
 * @key command "info"	string	Command [info|longinfo|command]
 * @key execute	false	bool	If true, mpck will execute the command script.
 * @key match	""	string	Only output files with the string in the name.
 *
 * @key conffile dbms/dbms.conf string file from which to get database
 *                                     configuration info as well as
 *                                     directories where data files are
 *                                     located; the conffile location is
 *                                     interpreted by Program::getConfFile()
 * @key workdir ""	string	Full path name of the work directory.
 *
 * @logger DEFAULT_FACILITY carma.dbms.mpck
 * $CarmaCopyright$
 */


#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <dirent.h>
#include <unistd.h>
#include <strings.h>
#include "string.h"
#include <errno.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <pwd.h>

#include <map>
#include <string>
#include <fstream>
#include <iostream>
#include <cstdlib>
#include <vector>
#include <sstream>

#if !defined(MPCK_STANDALONE)
#include <log4cpp/Priority.hh>
#include "carma/util/Program.h"
#include "carma/dbms/DBConfigurator.h"
#include "carma/util/IllegalStateException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Trace.h"
using namespace carma;
using namespace carma::util;
using namespace carma::dbms;
#endif

using namespace std;

/*
foreach dbload, monitorData, sdp transfer
   foreach frame minute slcorrel wbcorrel
    foreach numeric complex short string
     get list of files
     add file to map indexed by (eg.) frame/numeric/<basefilename>.
        Where basefilename is the filename upto ".mpdat".
     Set flag indicating which of dbload, etc. file is from.

 A Set is the list of files with the same base file name. ie. The
monitorData file and all links to it.

 A broken set is a set that is missing one or more entries.

 Orphans are broken sets that are missing the dbload entry.

When done, print list.

TODO:

2. It should report on common problems in /opt/dbIngest, including 
(in summary form if needed):
     a) Dangling symlinks
     b) Incomplete file sets (short,complex,string,numeric)
     c) Any files ending in a .error suffix.
     d) Zero frame count files ?

3. It should provide options to correct all problem cases older than 
a adjustable cutoff time (default: 7 days).
     This should have a "force" option to do this without interactive 
approval by the user for each case.
     a) Delete dangling symlinks
     b) Delete incomplete file sets where all existing files are 
.done but one or more are missing entirely.
     c) Rename all .error files to remove the .error suffix.
     d) Remove partial .write files left by abrupt exit.
     e) Delete zero frame count files ?

*/

static const string DONESUFFIX = ".done";
static const string ERRORSUFFIX = ".error";
static const string BINARYSUFFIX = ".bin";

static const string UNKNOWN = "UNKNOWN";
static const time_t ONEDAY = 60*60*24; // # of seconds in 1 day.
static const ulong KB = 1024;
static const ulong MB = KB*1024;
static const ulong GB = MB*1024;

/* Flags for the different dirs.*/
typedef unsigned FileFlag;

static const FileFlag DBLOADF =  0x1;
static const FileFlag MONITORDATAF =  0x2;
static const FileFlag SDPF = 0x4;
static const FileFlag XFRF = 0x8;
static const FileFlag WRKF = 0x10;// workDir is sort of a surrogate monitordata
static const FileFlag DIRALLFLAGS = DBLOADF|MONITORDATAF|SDPF|XFRF|WRKF;

// Flags indicating which entries must be present for non broken links.
// For example, under frame entries  only appear under dbLoad and monitorData
// while slcorrel entries appear under all directories.
static const FileFlag FRAMEREQ = DBLOADF | MONITORDATAF;
static const FileFlag MINUTEREQ = DBLOADF | MONITORDATAF | XFRF;
static const FileFlag SLCORRELREQ = DBLOADF | MONITORDATAF | SDPF | XFRF;
static const FileFlag WBCORRELREQ = SLCORRELREQ;

// Flags indicating things that affect how a file or group is handled.
// They also are numbered after the FileFlags.
// CHANGED	- lstat failed. This usually means the file name
//		  changed while	we were looking at it.
// TIMERANGE	- File is outside the time range.
// NOTDONE	- Filename doesn't end in ".done"
// MISSINGDATA	- No MONITORDATA file found (group flag).
// ERRORFILE	- File ends in ".error".
// BINARY	- File ends in ".bin".
static const FileFlag FILE_CHANGED=0x100;
static const FileFlag FILE_TIMERANGE=0x200;
static const FileFlag FILE_NOTDONE=0x400;
static const FileFlag FILE_MISSINGDATA=0x800;
static const FileFlag FILE_ERROR=0x1000;
static const FileFlag FILE_BINARY=0x2000;

// These are indexes into Directory so they must match its configuration.
static const unsigned short DBI =  0x0;
static const unsigned short MDI =  0x1;
static const unsigned short SDPI = 0x2;
static const unsigned short XFRI = 0x3;
static const unsigned short WRKI = 0x4;

// List of directories under dbIngest that are processed.
typedef struct {string name; string sname; FileFlag flag;} DIRECTORY;

static const DIRECTORY Directory[] = {
  {"dbload", "db", DBLOADF}, {"monitorData", "md", MONITORDATAF},
  {"sdp", "sdp", SDPF}, {"transfer", "xfr", XFRF},
  {"workDir", "wrk", WRKF}
};
static const int NDIRECTORIES = sizeof(Directory)/sizeof(*Directory);

// This will be filled in (or not) during parameter parsing. It contains
// the full pathname for workDir. If the string is empty, it is assumed
// that the work dir is under topdir.
static string workDir = "";

// List of subdirectories under Directory
typedef struct {string name; FileFlag reqflags;} DATAAVERAGETYPE;
static const DATAAVERAGETYPE DataAverageType[] = {
  {"frame", FRAMEREQ}, {"minute", MINUTEREQ},
  {"slcorrel", SLCORRELREQ}, {"wbcorrel", WBCORRELREQ}
  };
static const int NAVERAGETYPES =
	sizeof(DataAverageType)/sizeof(*DataAverageType);
static const unsigned short FRAMEI=0; // Index of the frame entry.


// List of directories/what kind of data are stored under each AverageType
static const string AggregateType[] =
	{"numeric", "complex", "short", "string"};
static const int NAGGREGATETYPES =
	sizeof(AggregateType)/sizeof(*AggregateType);

static const struct {FileFlag state; const char *name;} FileStates[] =
{	{FILE_CHANGED, "CHANGED"}, {FILE_TIMERANGE, "TIMERANGE"},
	{FILE_NOTDONE, "NOTDONE"}, {FILE_MISSINGDATA, "MISSINGDATA"},
	{FILE_ERROR, "ERROR"}, {FILE_BINARY, "BINARY"}
};

static unsigned short NFILESTATES = sizeof(FileStates)/sizeof(*FileStates);

// Ignore the file if any of these flags are set.
static const FileFlag IGNORE_DEFAULTS =
	FILE_CHANGED | FILE_TIMERANGE | FILE_NOTDONE;

static inline bool missingData(const FileFlag flags)
{return (!(flags&(MONITORDATAF|WRKF)));}
  //{return (!(flags&FILE_MISSINGDATA|FILE_));}

static const uid_t MINUID = 31;	// Minimum user id which is allowed to have
				// this program execute the removal script.



////////////////////////////////////////////////////////////////
// Functions to remove and rename files are defined in the shell script.
// Besides providing for counting of the number of files processed, it
// makes it easy to edit the script to change what is done. (Such as
// not really deleting files or changing the amount of time to sleep).
//
// sh shell function to remove a file or link
static const char *REMOVEFUNC =
"################################################################\n\
# Remove a file or link if it exists and increment the appropriate counter.\n\
# After every REMOVEDCOUNTER0 deletions:\n\
#  /bin/sync then sleep for REMOVESLEEP seconds.\n\
# If REMOVESLEEP = 0, never sync or sleep.\n\
declare -ri REMOVEDCOUNTER0=10000\n\
declare -ri REMOVESLEEP=1\n\
\n\
declare -i REMOVEREQUESTS=0\n\
declare -i REMOVEDFILES=0\n\
declare -i REMOVEDLINKS=0\n\
declare -i REMOVEDCOUNTER=${REMOVEDCOUNTER0}\n\
\n\
mpremove() {\n\
    let REMOVEREQUESTS=++REMOVEREQUESTS\n\
    if [ -f $1 ]; then\n\
	let REMOVEDFILES=++REMOVEDFILES\n\
	let REMOVEDCOUNTER=--REMOVEDCOUNTER\n\
	/bin/rm $1\n\
    elif [ -L $1 ]; then\n\
	let REMOVEDLINKS=++REMOVEDLINKS\n\
	let REMOVEDCOUNTER=--REMOVEDCOUNTER\n\
	/bin/rm $1\n\
    fi\n\
\n\
    if [ $REMOVEDCOUNTER -le 0 ]; then\n\
	if [ $REMOVESLEEP -gt 0 ]; then\n\
	    /bin/sync\n\
	    /bin/sleep $REMOVESLEEP\n\
	fi\n\
	REMOVEDCOUNTER=$REMOVEDCOUNTER0\n\
    fi\n\
}\n\
################################################################\n";

// Shell commands to print out file removal stats.
static const char *REMOVEPRINT =
"echo \"$REMOVEREQUESTS calls to remove\"\n\
echo \"Removed $REMOVEDFILES files and $REMOVEDLINKS links\"";

static const char *RENAMEFUNC =
"################################################################\n\
# Rename .error files and count the number of files renamed.\n\
declare -i RENAMEREQUESTS=0\n\
declare -i RENAMEDFILES=0\n\
\n\
mprename() {\n\
    let RENAMEREQUESTS=++RENAMEREQUESTS\n\
    if [ -L $1 ]; then\n\
	let RENAMEDFILES=++RENAMEDFILES\n\
	/usr/bin/rename mpdat.error mpdat $1\n\
    fi\n\
}\n\
################################################################\n";

// Shell commands to print out rename stats.
static const char *RENAMEPRINT =
"if [ $RENAMEDFILES -gt 0 ]; then\n\
	echo \"$RENAMEREQUESTS calls to rename\"\n\
	echo \"Renamed $RENAMEDFILES .error files\"\n\
fi\n";


////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
//			Various support routines for above.
////////////////////////////////////////////////////////////////

// Convert from a dir flag to a dir index.
static unsigned short dirF2I(FileFlag flag)
{
  switch(flag) {
  case DBLOADF:
    return DBI;
    break;
  case MONITORDATAF:
    return MDI;
    break;
  case SDPF:
    return SDPI;
    break;
  case XFRF:
    return XFRI;
    break;
  case WRKF:
    return WRKI;
    break;
  default:
    return 0xffff;
  }
}

// Given a dir flag, return its corresponding string.
static const string &dir2String(FileFlag flag, bool longname=true)
{unsigned short index = dirF2I(flag);

 if(index >= NDIRECTORIES)
   return UNKNOWN;

 return longname ? Directory[index].name : Directory[index].sname;
}

// Return a string containing all entries represented in flags.
static const string dirFlags2String(FileFlag flags, bool longname=true)
{unsigned short mask = 1;
 string s = "";

 for(int i=0; i < NDIRECTORIES; i++, mask += mask)
 { FileFlag m = mask & flags;
   if(m == 0) continue;
   unsigned short index = dirF2I(m);
   if(s != "")
     s += " ";
   if(index >= NDIRECTORIES)
     s += UNKNOWN;
   else
     s += longname ? Directory[index].name : Directory[index].sname;
 }
 return s;
}

// Return a string containing all file states represented in flags.
const string fileStates2String(const FileFlag flags, const char *sep=" ")
{string s = "";

 for(int i=0; i < NFILESTATES; i++)
 { if(flags & FileStates[i].state)
   {  if(s != "")
        s += sep;
      s += FileStates[i].name;
   }
 }
 return s;
}

// Replace all occurrences of pat in s with repl.
// Returns the number of replacements.
unsigned gsub(string &s, const string &pat, const string &repl)
{ std::string::size_type n;
  std::string::size_type len = pat.size();
  unsigned nreplacements=0;
  while( (n = s.find(pat)) != string::npos)
  {  s.replace(n, len, repl);
     nreplacements++;
  }
  return nreplacements;
}

////////////////////////////////////////////////////////////////
//			Routines to deal with time based values.
////////////////////////////////////////////////////////////////

// Convert a time_t to a string.
static const char *timeFormats[] = {
  "%F %R", "%F"
};
static const unsigned short NTIMEFORMATS = sizeof(timeFormats)/sizeof(*timeFormats);

static void time2String(const time_t &itime, string &otime, unsigned short fmtnum=0)
{struct tm tms, *tmp;
 char obuf[64];
 const char *format;

 if(fmtnum < NTIMEFORMATS)
   format = timeFormats[fmtnum];
 else
   format = timeFormats[0];

 tmp = localtime_r(&itime, &tms);
 /*size_t len =*/ strftime(obuf, sizeof(obuf), format, tmp);
 otime = obuf;
}

// Convert a time_t value to a string.
static string time2String(const time_t &itime)
{struct tm tms, *tmp;
 string otime;
 char obuf[64];
 const char *format = "%F";

 tmp = localtime_r(&itime, &tms);
 /*size_t len =*/ strftime(obuf, sizeof(obuf), format, tmp);
 otime = obuf;
 return otime;
}

/*
 * Time:
 *  "YYYY-MO-DD" or "YYY-DDD-DD" -> start or end time.
 *  +int -> Time is int days after start.
 *  -int -> Time is int days before end.

Parse time option(s) into time & offset.
After processing files, apply options.
 */

class TimeInfo {
public:
  TimeInfo(){time=0; offset=0.0;}
  time_t time;
  float offset;
};

static bool string2Time(const char *timestr, TimeInfo &timeinfo)
{const char *format1 = "%F";
 const char *format2 = "%Y-%b-%d";
 const char *format=format1;
 struct tm tms;  
 bzero(&tms, sizeof(tms));
 char buf[255];
 time_t time= 0;

 if((*timestr == '-') || (*timestr == '+'))
 { // Treat as offset.
   timeinfo.offset = atof(timestr) * ONEDAY;
   return true;
 }

 // 2006-Jun-21 or 2006-06-21?
 if(strlen(timestr) < 10)
   { cerr << "Bad time string: " << timestr << endl;
     exit(1);
   }
 if(isalpha(timestr[5]))
   format = format2;

 char *foo = strptime(timestr, format, &tms);
 strftime(buf, sizeof(buf), format, &tms);
 // cout << timestr << "->" << buf << endl;

 if(foo == 0)
   { cerr << "Error converting time " << timestr << endl;
     exit(1);
   }

 time = mktime(&tms);
 if(time == (time_t)(-1))
 {   cout << "Error calling mktime\n";
   return false;
 }

 timeinfo.time = time;
 return true;
}

////////////////////////////////////////////////////////////////
//			Something to hold program options
////////////////////////////////////////////////////////////////

// Various options that affect what gets printed.
class Options {
public:
  enum ACTIONS { INFO, ALLINFO, COMMANDS};
  Options()
  {onlysummary = false; starttime = endtime = 0; haveMatchString_=false;
   onlyorphans = false;
   //   onlybrokens = onlydonefiles = true;	// Only process broken and done groups.
   onlybrokens = true;	// Only process broken and done groups.
   outputformat = INFO;
   execute = false;			// Execute the script that's created.
   ignoreflags = IGNORE_DEFAULTS;
  }
  ~Options(){};
  // Is time within this Option's time range?
  bool inRange(time_t time)const;
  void setMatchString(const string &str);
  void setMatchString(const char *str);
  bool getMatchString(string &str)const;
  void print(ostream &)const;
  // Does the string appear inside matchString? Also returns true if there is
  // no match string.
  bool isMatch(const string &)const;
  ACTIONS outputformat;	// Print info format. Otherwise generate cmd script.
  bool onlysummary;
  bool onlyorphans;
  bool onlybrokens;
  //  bool onlydonefiles;
  bool execute;		// Execute command script when done.
  FileFlag ignoreflags;
  time_t starttime, endtime;	// Ignore time stamp outside this range.
  bool haveMatchString()const{return haveMatchString_;}
  const string &matchString()const{return matchString_;}
  const char *outputFormat2String(ACTIONS of)const;
  string ofilename;
  string header;			// String to go at beginnng of file.
private:
  bool haveMatchString_;
  string matchString_;
};

void Options::print(ostream &str)const
{ ostringstream os;
  string times, timee;

  time2String(starttime, times);
  time2String(endtime, timee);
  os << "# Time range: " << times << " - " << timee << endl;
  if(haveMatchString())
    os << "# Match string: <" << matchString_ << ">\n";
  os << "# Output format = " << outputFormat2String(outputformat);
  os << " Execute - " << (execute ? "Yes" : "No") << endl;
  if(onlysummary)
    os << "# Summary only." << endl;
  os << "# Ignore flags: [" << fileStates2String(ignoreflags) << "]\n";
  if(workDir.size() > 0)
    os << "# workDir is: " << workDir << endl;
  os << endl;  // An extra line.
  str << os.str();
}

void Options::setMatchString(const string &str)
{
  matchString_ = str;
  haveMatchString_ = (str != "");
}

void Options::setMatchString(const char *str)
{
  if(str != 0)
  {  matchString_ = str;
     haveMatchString_ = (strlen(str) > 0);  
  }
  else
  { matchString_ = "";
    haveMatchString_ = false;
  }
}

bool Options::getMatchString(string &str)const
{
  str = matchString_;
  return haveMatchString_;
}

// Returns true if time is within the option's time range.
bool Options::inRange(time_t time)const
{ bool inrange;
    
    if((time >= starttime) && (time <= endtime))
      inrange = true;
    else
      inrange = false;

  return inrange;
}

// See if the matchString appears in str. Returns true if there is no
// matchString;
bool Options::isMatch(const string &str)const
{bool rtn;
  if(!haveMatchString_)
    rtn = true;
  else if(str.find(matchString_) != string::npos)
    rtn = true;
  else
    rtn = false;
  return rtn;
}

const char *Options::outputFormat2String(ACTIONS of)const
{
 switch(of) {
 case INFO:
   return "INFO";
   break;
 case ALLINFO:
   return "ALLINFO";
   break;
 case COMMANDS:
   return "COMMANDS";
   break;
 default:
   return "UNKNOWN";
   break;
 }
}

////////////////////////////////////////////////////////////////
// Keeps track of various statistics over all files

class GlobalFileStats{
public:
  GlobalFileStats(){ end = 0; orphans=0; brokens = 0; numlinks=numfiles=0;
  		numbytes=0; numothers=0;
		start = time(0) + ONEDAY;
  }
  void timecheck(const time_t time); // Update min/max time.
  void update(const struct stat &buf);
  // Write info to stream. If printall is false, only output times
  // and type of file information.
  void print(ostringstream &os, bool printall=true);
  time_t start;  // Earliest & latest time stamps.
  time_t end;
  unsigned orphans, brokens, numlinks, numfiles, numothers;
  ulong numbytes;
};

void GlobalFileStats::timecheck(const time_t time)
{
  if(time < start)
    start = time;
  else if(time > end)
    end = time;
}

// Update tracked statistics.
void GlobalFileStats::update(const struct stat &buf)
{
  timecheck(buf.st_mtime);
  numbytes += buf.st_size;
  if(S_ISLNK(buf.st_mode))
    numlinks++;
  else if(S_ISREG(buf.st_mode))
    numfiles++;
  else
    numothers++;
}

// Write the statistics to the stream.
void GlobalFileStats::print(ostringstream &os, bool printall)
{  os << "# Start time = " << time2String(start);
   os << " End time = " << time2String(end) << endl;
   if(printall) {
     os << "# Total # orphans = " << orphans;
     os << " Total # of brokens = " << brokens << endl;
   }
   os << "#  # links = " << numlinks << "; # files = " << numfiles
      << " # others " << numothers << " ";
   float size = numbytes;
   os << " Total space: ";
   os.precision(3);
   if(numbytes >= GB)
     os << size/GB << "GB";
   else if(numbytes >= MB)
     os << size/MB << "MB";
   else if(numbytes >= KB)
     os << size/KB << "KB";
   else
     os <<  numbytes << " bytes";
     //   os << " [" << numbytes << " bytes]";
   os << endl;
}

// This is updated when the directories are first processed.
static GlobalFileStats globalFileStats;
//static GlobalFileStats allGlobalFileStats;

////////////////////////////////////////////////////////////////
// Information about a particular file.


class FileStat {
public:
  FileStat(const string &path, const string &filename, FileFlag flag);
  FileStat &operator=(const FileStat &rhs);

  FileStat();
  ~FileStat(){}
  void stat(const string &path, const string &filename, short dirflag);
  bool isLink()const {return S_ISLNK(buf.st_mode);}
  bool isFile()const {return S_ISREG(buf.st_mode);}
  ulong size()const { return buf.st_size;}
  time_t mtime()const { return buf.st_mtime;}
  //  void print(ostringstream &, bool infoFormat=true)const;
  void print(ostringstream &, const Options &options, FileFlag groupFlags)const;
  void addStats(GlobalFileStats &filestats)const;
  // Compare last modified time to time range. (Can't be called until
  // after all files have been initially scanned.
  void checkTime(const Options &options);
  const string &fullName()const{return fullname;}
  const string &fileName()const{return name;}
  const struct stat &statBuf()const{return buf;}
  // If ignore is true, this file shouldn't be printed.
  // Always ignore if file changed since we couldn't even stat it.
  bool ignore()const
  {return (filestate_ & (FILE_CHANGED|FILE_TIMERANGE)) != 0;}

  FileFlag dirFlag()const{return dirflag_;}
  FileFlag fileState()const{return filestate_;}
  bool isDone()const{return !((filestate_ & FILE_NOTDONE)
			      &&(dirflag_!=MONITORDATAF));}
  bool isError()const{return (filestate_ & FILE_ERROR);}
  string fileInfoString()const;
private:
  //  void printInfo(ostringstream &, const Options &options,
  void printInfo(ostream &, const Options &options,
		 FileFlag groupFlags)const;
  //  void printCmd(ostringstream &, const Options &options,
  void printCmd(ostream &, const Options &options, FileFlag groupFlags)const;

  string name, fullname;
  int errorno_;
  struct stat buf;
  FileFlag dirflag_;
  FileFlag filestate_;	// Flags describing file.
};

FileStat::FileStat(const string &path, const string &filename, FileFlag flag)
{ filestate_ = 0;
  errorno_ = 0; 
  stat(path, filename, flag);
}

FileStat::FileStat()
{
  name = "?";
  bzero(&buf, sizeof(buf));
  dirflag_ = 0xffff;
  filestate_ = 0;
  errorno_ = 0;
}

FileStat &FileStat::operator=(const FileStat &rhs)
{
  if(this != &rhs)
    {  name = rhs.name;
       fullname = rhs.fullname;
       bcopy(&(rhs.buf), &buf, sizeof(buf));
       dirflag_ = rhs.dirflag_;
       filestate_ = rhs.filestate_;
       errorno_ = rhs.errorno_;
    }
  return *this;
}

// Lstat a file and update file related parameters.
// dirflag indicates which of frame, minute, etc. the file is under.
void FileStat::stat(const string &path, const string &filename, short dirflag)
{ fullname = path + "/" + filename;
  name = filename;
  dirflag_ = dirflag;
  filestate_ = 0;  // ?
  errno = 0;  // Need to reset any previous value.
  int status = lstat(fullname.c_str(), &buf);
  if(status < 0)
  {  errorno_ = errno;
     perror(name.c_str());
     filestate_ |= FILE_CHANGED; // File probably changed while being look at.
  }
  else  // See if links end in magic strings.
  { if(S_ISLNK(buf.st_mode))
    { string::size_type n = filename.rfind(DONESUFFIX);
      if(n == string::npos)
       {  filestate_ |= FILE_NOTDONE;	// File doesn't end in ".done".
          // If it doesn't in in ".done", does it end in ".error"/
          if(filename.rfind(ERRORSUFFIX) != string::npos)
	    filestate_ |= FILE_ERROR;	// Filename does end in ".error".
       }
    }
    if(filename.find(BINARYSUFFIX) != string::npos)
      filestate_ |= FILE_BINARY;
  }

  if(status == 0)
    globalFileStats.update(buf);
}

// Add this file's stats to filestats.
void FileStat::addStats(GlobalFileStats &filestats)const
{
  filestats.update(buf);
}

// See if file is within time range.
void FileStat::checkTime(const Options &options)
{
  if(!options.inRange(mtime()))
    filestate_ |= FILE_TIMERANGE;
}

#if 0
// Used to be used in printCmd().
static void removeSuffix(string &f, const string &suffix)
{  string::size_type n = f.rfind(suffix);
    if(n != string::npos)
      f.erase(n, string::npos);
}
#endif

// Return a string containing information about this file.
string FileStat::fileInfoString()const
{ string timestr;
  ostringstream os; 

  os << "[(" << dir2String(dirflag_, false) << ":"; 
  if(isLink())
    os << "L";
  else if(isFile())
    os << "F";
  else
    os <<"?";

  os << ":" << size() << ":";
  time2String(buf.st_mtime, timestr);
  os << timestr << ")("
     << fileStates2String(filestate_) << ")] ";

  // Add any error message.
  if(errorno_ != 0)
    os << "[! " << strerror(errorno_) << "!]";

  return os.str();
}

// Write info for this file to the stringstream.
//void FileStat::printInfo(ostringstream &os, const Options &options,
void FileStat::printInfo(ostream &os, const Options &options,
			 FileFlag groupFlags)const
{
  os << name << fileInfoString();
}

void FileStat::printCmd(ostream &os, const Options &options,
			FileFlag groupFlags)const
{ string timestr;
  string cmdprefix = "";

  // Check to see if file should be treated specially.
  // If print was called even though file should have been ignored.

  if(ignore())
    cmdprefix = "#[ignored] ";
  else if(!missingData(groupFlags))// If data file is missing, delete links.
  { // Rename .error links unless there is no data file. In which case,
    // just delete it.
    // If it isn't a .error file, comment out the command.
    if(!isError())
      if(groupFlags&FILE_NOTDONE) // If there is a link that isn't done
       { if(isDone())
           cmdprefix = "#[ignored] ";
         else
	   cmdprefix = "#[NOT_DONE] ";
       }
  }

  // Info goes on one line and 'rm' command goes on another.
  os << "# " << fileInfoString() << endl;

  os << cmdprefix << " ";
    if(isError() && !missingData(groupFlags))
    { string noerror = fullName();
    //      removeSuffix(noerror, ERRORSUFFIX);
      os << "    mprename " << fullName();
    }
    else
      os << "    mpremove " << fullName();
}

void FileStat::print(ostringstream &os, const Options &options,
		     FileFlag groupFlags)const
{
  if(options.outputformat== Options::COMMANDS)
    printCmd(os, options, groupFlags);
  else
    printInfo(os, options, groupFlags);
}

////////////////////////////////////////////////////////////////
// Holds FileStats for each file in a group. ie. A monitorData entry
// and all links to it.
// eg. For files under frame/numeric, there should be at most two entries:
// One for the data file under monitorData and one for the link under
// dbLoad.
// For files that get archived &/or loaded into visbricks, there would be
// entries for those links also.
// Files are added if the "basename" matches. Where basename is the filename
// with anything after the ".mpdat" removed. {eg. .bin, .done, .write, etc.

class FileGroup{
public:
  FileGroup() {dirflags_ = reqflags_ = 0; ignoregroup_ = false;}
  ~FileGroup() {}
  // Add a file to this group.
  void addFile(const string &path, const string &filename, FileFlag fflag,
	       FileFlag dirflag);
  // Flags indicating which directories are represented.
  FileFlag dirFlags()const{return dirflags_;}
  // Flags indicating which directories should be represented.
  FileFlag requiredFlags()const{return reqflags_;}
  vector<FileStat> files;	// Entries from scandir.
  // Look for individual files that might result in the whole group
  // being ignored. This check has to be done after the initial scan
  // because the time range isn't known until then.
  //  void updateGroup(const Options &options);
  // Add info entries for this group.
  bool printGroup(ostringstream &os, const Options &options,
		  GlobalFileStats &fstats,
		  const string &filename);
  bool ignoreGroup()const{return ignoregroup_;}
private:
  unsigned short dirflags_;		// Which of dbload, etc. files were found in
  				// the list.
  unsigned short reqflags_;		// Which flags are required.
  bool ignoregroup_;		// Ignore this group.
};

// Add a file to the group.
// Update global stats.
void FileGroup::addFile(const string &path, const string &filename,
			FileFlag dirflag, FileFlag reqflags)
{ FileStat stat(path, filename, dirflag);
  files.push_back(stat);
  // If there is a problem with any files in a group, ignore whole group.
  ignoregroup_ |= stat.ignore();

  dirflags_ |= dirflag;
  if(reqflags_ == 0)
    reqflags_ = reqflags;
  else if(reqflags_ != reqflags)
    cerr << "Required flags changed with " << path << " / " << filename << endl;
  if(reqflags == 0)
    cerr << "reqflags = 0 for " << path << " / " << filename << endl;
  // All good files get counted in stats.
#if 0
  if(!stat.ignore())
    ::globalFileStats.update(stat.statBuf());	// Update global stats.
#endif
}

// Returns true if file info was written into the stream.
bool FileGroup::printGroup(ostringstream &os, const Options &options,
			   GlobalFileStats &fstats,
			   const string &filename)
{ unsigned nentries = 0;
  ostringstream infobuf, filebuf;

    bool isBroken = (reqflags_ != dirflags_);
    bool isOrphan = !(dirflags_&DBLOADF);
    //    FileFlag groupFlags = dirflags_ & ~(MONITORDATAF|WRKF);
    FileFlag groupFlags = dirflags_;
    string brokenStatus = "";

    // If isBroken is true, see if it's because a file was in workDir
    // instead of monitorData.
    if(isBroken && (dirflags_&WRKF))
    { FileFlag tmpflags = (dirflags_ & ~WRKF) | MONITORDATAF;
      if(tmpflags == reqflags_)
	isBroken = false;
    }

    // Do nothing if this group should be ignored.
    if(ignoreGroup() && (options.outputformat != Options::ALLINFO))
      return false;

    if(options.onlybrokens && !isBroken)
      return false;

    if(options.onlyorphans && !isOrphan)
      return false;

    // Make a pass through the files checking time ranges and collecting
    // flags.
    unsigned size = files.size();
    for(unsigned i=0; i< size; i++)
    {FileStat &fs = files[i];
     
    	fs.checkTime(options);
	groupFlags |= fs.fileState();
    }

    // Ignore entire group if any file is outside the time range.
    if(groupFlags & FILE_TIMERANGE)
      return false;

    // Do match test outside of loop to enforce all or none rule.
    if(!options.isMatch(filename))
      return false;

    if(isOrphan)
    {   brokenStatus += "isOrphan ";
    }

    if(isBroken)
    {  string req = dirFlags2String(reqflags_);
       string bad = dirFlags2String(dirflags_);
       brokenStatus += " Req: " + req + " Found: " + bad + "|: ";
    }
    else // This usually doesn't get printed.
      brokenStatus += " OK|: ";

    infobuf << "# " << filename;
    infobuf << " " << size << ": ";
    infobuf << brokenStatus;
    infobuf << "[" << fileStates2String(groupFlags) << "]" << endl;

    for(unsigned i=0; i< size; i++)
    {const FileStat &fs = files[i];
     
      // Print info for this file.
      if(options.outputformat != Options::COMMANDS)
	filebuf << "#\t";
      fs.print(filebuf, options, groupFlags);
      filebuf << endl;
      fs.addStats(fstats);	// Count file's size, etc.
      nentries++;
    }

    // Don't actually print anything if no file info was printed.
    bool printedFiles = (nentries > 0);
    if(printedFiles)
    {  if(isOrphan)
	 fstats.orphans++;	// Number of groups.
       if(isBroken)
	 fstats.brokens++;
       os << infobuf.str() << filebuf.str();
    }

    return printedFiles;
}

static double getCurrentTime(const double start=0.0)
{
  double rtn = static_cast<double>(time(0)) - start;
 return rtn;
}

typedef map<string,FileGroup> FileGroupMap;


// Build a string containing path to a directory and get list of
// the files in it. Add each file to the list of files.
static void processDir(const string &topdir, const DIRECTORY &dir,
		       const DATAAVERAGETYPE &avgtype,
		       const string &type, FileGroupMap &filelist)
{string shortpath = avgtype.name + "/" + type;	// eg. "frame/numeric".
 string dirpath = topdir + "/" + dir.name;
 bool isWorkDir = (dir.flag & WRKF); // workDir uses a different name format.

 if(!isWorkDir)
   dirpath += "/" + shortpath;
 else if(workDir.size() > 0)  // If workDir exists, use it.
   dirpath = workDir;

 struct dirent **namelist = 0;

 // List of files in the directory.
  int nentries = scandir(dirpath.c_str(), &namelist, 0, alphasort);
  if(nentries < 0) {
    if(isWorkDir) {
      return;	// A missing workdir is not problem.
    } else {
      cerr << "Error doing directory scan for " << dirpath << " ("
           << strerror(errno) << ")" << endl;
      exit(1);
    }
  }

 for(int i = 0; i < nentries; i++)
 {string filename = namelist[i]->d_name;
  free(namelist[i]);
  if((filename == ".") || (filename == ".."))
    continue;
     
  // Find base file name. (ie. w/o anything after the mpdat).
  string basefilename = filename;
  // Files ending in ".bin" are handled separately to distinguish them
  // from the ASCII versions.
  string::size_type n = basefilename.rfind(".bin");
  if(n != string::npos)
    basefilename.erase(n+4, string::npos);
  else
  { n = basefilename.rfind(".mpdat");
    if(n != string::npos)
      basefilename.erase(n+6, string::npos);
  }

  // If this is the work directory, it's necessary to generate basename
  // from the filename parts.
  // workDir name format example:   frame_numeric_426918204-426918240.mpdat
  // Remember the average type and type. Remove average type. Use
  // these to build longfilename.
  if(isWorkDir)
  { n = basefilename.find("_");
    if(n != string::npos)
    {  string avgt = basefilename.substr(0, n);
       basefilename.erase(0, n+1);
       n = basefilename.find("_");
       if(n != string::npos)
       {  string type = basefilename.substr(0, n);
         shortpath = avgt + "/" + type;
       }else
	 continue; // Error.
    }else
      continue; // Error.
  }

  string longfilename = shortpath + "/" + basefilename;
  FileGroup &info = filelist[longfilename];
  // If isWorkDir, then avtype should be adjusted.
  info.addFile(dirpath, filename, dir.flag, avgtype.reqflags);
 }
 free(namelist);
}

static void buildFileList(const string &topdir, FileGroupMap &filelist)
{
 for(int i = 0; i < NDIRECTORIES; i++)
   for(int j=0; j < NAVERAGETYPES; j++)
     for(int k=0; k < NAGGREGATETYPES; k++)
       if(!(Directory[i].flag & WRKF))
	 processDir(topdir, Directory[i], DataAverageType[j], AggregateType[k],
		    filelist);

 // Handle any workDir separately. AggregateType is ignored.
 processDir(topdir, Directory[WRKI], DataAverageType[FRAMEI], AggregateType[0],
	    filelist);
}

static void printFileList(ostream &outs, FileGroupMap &filelist,
			  const Options &options)
{ ostringstream os;
 GlobalFileStats processedStats;

  for(FileGroupMap::iterator iter=filelist.begin();
      iter != filelist.end(); iter++) {
    string filename = iter->first;
    FileGroup &group = iter->second;
    os.str("");
    if(!group.printGroup(os, options, processedStats, filename))
      continue;
    if(!options.onlysummary && (os.str() != ""))
      outs << os.str() << endl;
  }

 if(options.outputformat == Options::COMMANDS)
 {  outs << REMOVEPRINT << endl << endl;
    outs << RENAMEPRINT << endl;
 }

  // Summary information.
  //  ostringstream os;
#if 0
  os << "\n# allGlobalFileStats\n";
  allGlobalFileStats.print(os, false);
#endif

  os << "\n# Global file statistics.\n";
  globalFileStats.print(os, false);

  os << "\n# Selected file statistics.\n";
  processedStats.print(os);
  outs << os.str();
}

static void process(ostream &of, FileGroupMap &filelist,
		    double listBuildTime, Options &options)
{double printStartTime = getCurrentTime();
 double printTime;

#if 0
 if(options.outputformat == Options::COMMANDS)
   of << options.header << endl;
#else
 if(options.header.size() > 0)
   of << options.header << endl;
#endif
 options.print(of);
 if(options.outputformat == Options::COMMANDS)
 {  of << REMOVEFUNC << endl;
    of << RENAMEFUNC << endl;
 }
 printFileList(of, filelist, options);
 printTime = getCurrentTime(printStartTime);
 of.precision(2);
 of << "# Read time = " << listBuildTime/60.0 << " min"
    << " Print time = " << printTime/60.0 << " min\n";
}

// Execute the shell script. Returns 0 for success else non 0.
static int executeScript(const string &script)
{int err = 0;

 cout << "Executing script: " << script << endl;
 string command = "/bin/bash -r " + script;

 errno = 0;	// Reset.
 FILE *filep = popen(command.c_str(), "r");
 if(filep == 0)
 { perror("Error opening script");
   err = -1;
 }
 else
 {
#if 0
   int c;
   while( (c = fgetc(filep) != EOF))
     putchar(c);
#endif
   // Wait for script to finish then look for errors.
   err = pclose(filep);
   if(err == -1)
     perror("Script error");
   else if(err > 0)
     strerror(err);  // rtn seems to be meaningless, however.
 }
 return err;
}

/*
 * Options:
 *  only orphans
 *  just summary.
 *  start time		"start", "end", "YYYY-MO-DD", +/-n(days)
 *  end time

 * Time:
 *  "YYYY-MO-DD" or "YYY-DDD-DD" -> start or end time.
 *  +int -> Time is int days after start.
 *  -int -> Time is int days before end.

 */

static void help(const char *name)
{
  cout << "Usage: " << name << " [h o s b <date> e<date> m <str>]" << endl;
  cout << " Prints a list of files in /opt/dbIngest directory.\n\n";
  cout << " -h	- Prints this message and exits.\n";
  cout
    << " -O	- Only list 'orphans' - those files or links w/o an entry in dbLoad.\n";
  cout << " -L	- Generate long list (all files)." << endl;
  cout << " -c	- Generate shell script to remove old files." << endl;
  cout << " -X	- If -c and -X are set, " << name << " will execute the script\n";
  cout << " -s	- Only print summary information.\n";
  cout << " -b <date> - Begin printing at <date>.\n";
  cout << " -e <date> - End printing at <date>.\n";
  cout << " -o <file> - file will be used for the output filename." << endl;
  cout << " -w <file> - Pathname of work directory." << endl;
  cout << " -m <string> - Only print lines whose 'base filename' contains <string>\n";
  cout << endl;
  cout << " Example output\n";
  cout <<"minute/complex/complex_431827800.mpdat 3:  OK|: [NOTDONE]\n";
  cout << "       complex_431827800.mpdat.done[(db:L:64:2006-11-03 16:05)()]"
       << endl;
  cout << "       complex_431827800.mpdat[(md:F:885283:2006-11-03 16:05)()]"
       << endl;
  cout << "       complex_431827800.mpdat[(xfr:L:64:2006-11-03 16:05)(NOTDONE)] "
       << endl;

  cout << endl;
  cout << " minute/complex/complex_431827800.mpdat is the 'base filename'.\n";
  cout << " 3 gives the number of files in the following list. (Not all may be printed).\n";
  cout << " (db:L:64:2006-11-03 16:05)()]" << endl;
  cout << " dbload(db), monitorData(md), sdp(sdp), transfer(xfr) or work(wrk),\n";
  cout << " whether it's a Link or File, the number of bytes in the file\n";
  cout << " and the file's mtime.\n";
  cout << endl;
  cout << "Date selection:\n";
  cout << " Dates are in a form like \"2006-06-28\",";
  cout << "  \"2006-Jun-28\" or +/-<delta>\n (The '+' or '-' is required).\n";
  cout << " For -b:\n";
  cout << "	+delta - Start printing delta days after the earliest timestamp.\n";
  cout << "	-delta - start printing delta days before the last timestamp.\n";

  cout << " For -e:\n";
  cout << "	+delta - stop printing delta days after the value used for -b\n		(not the first timestamp)\n";
  cout << "	-delta means stop printing delta days before the last timestamp.\n";

  cout << "The default values for -b and -e are the first and last timestamps.\n";

}

static const char *optstring="hOLscXb:e:m:o:w:";

static void processNormalArgs(int argc, char *argv[], Options &options,
			      TimeInfo &starttime, TimeInfo &endtime)
{int rtn;

#if defined(MPCK_STANDALONE)
 if(strcmp(argv[1], "--") == 0)
   { cout << "-- can't be used in standalone mode.\n";
     exit(1);
   }
#endif

  while((rtn=getopt(argc, argv, optstring)) >= 0)
    if( rtn != '?')
    {// printf("%d |%c| %s\n", rtn, rtn, optarg);

      switch(rtn) {
      case 's':				// Only print summaries.
	options.onlysummary = true;
	break;
      case 'O':				// Only print orphan info.
	options.onlyorphans = true;
	break;
      case 'L':				// Only print info about anything.
	options.outputformat = Options::ALLINFO;
	options.onlyorphans = false;
	options.onlybrokens = false;
	break;
      case 'c':				// Generate removal commands.
	options.outputformat = Options::COMMANDS;
	options.header = "#!/bin/bash";
	break;
      case 'b':				// Begin/start time.
	string2Time(optarg, starttime);
	break;
      case 'o':				// output filename.
	options.ofilename = optarg;
	break;
      case 'e':				// End time.
	string2Time(optarg, endtime);
	break;
      case 'X':				// Execute script.
	options.execute = true;
	break;
      case 'w':				// workDir path.
	workDir = optarg;
	break;
      case 'h':
	help(argv[0]);
	exit(0);
	break;
      case 'm':
	options.setMatchString(optarg);
	break;
      default:
	break;
      }
    }
}


#if !defined(MPCK_STANDALONE)
int Program::main()
#else
int main(int argc, char *argv[])
#endif
{FileGroupMap filelist;
 string topdir;
 int err=0;
 Options options; 
 TimeInfo starttime, endtime;

////////////////////////////////////////////////////////////////
  // The default is to ignore the previous 7 days.
  // It's not a good idea to mess with files too close to the current time
  // since files can be changing or some program is running behind.
  string2Time("-7", endtime);
  options.outputformat = Options::INFO;
  options.onlybrokens = true;	// Default is to just list problems.

////////////////////////////////////////////////////////////////
#if defined(MPCK_STANDALONE)
#if 1
 topdir = "/scr1/spica/carma/dbms/dbIngest";
#else
 topdir = "/opt/dbIngest";
#endif
#else
    const string conffile = getConfFile( getStringParameter( "conffile" ));

    auto_ptr<DBConfigurator> dbconf;
    try {
        auto_ptr<DBConfigurator> dbconftmp 
            (new DBConfigurator(conffile));
        dbconf = dbconftmp;
	topdir = dbconf->getTopDir();
    } catch (const NotFoundException & exc) {
        ostringstream msg;
        msg << "Unable to read configation file " << conffile;
        CARMA_CPTRACE (Trace::TRACE1, msg);
	//        getLogger() << log4cpp::Priority::ERROR << msg.str();
        exc.report();
        return EXIT_FAILURE;
    }

    workDir = dbconf->getWorkDir();

{string s;
 bool b; 
  s = getStringParameter("ofile");
  if(s != "")
    options.ofilename = s;
  b = getBoolParameter("summary");
  options.onlysummary = b;
  b = getBoolParameter("execute");
  options.execute = b;
  s = getStringParameter("command");
  if(s == "info")
    options.outputformat = Options::INFO;
  else if(s.find("long") == 0)
    options.outputformat = Options::ALLINFO;
  else if(s == "commands")
    options.outputformat = Options::COMMANDS;
  else
    { cerr << "Unrecognized command: " << s << endl;
    exit(1);
    }

  s = getStringParameter("match");
  if(s != "")
    options.setMatchString(s);
  s = getStringParameter("workdir");
  if(s.size() != 0)
    workDir = s;
}

    int argc = getExtraArgc( );
    char **argv = getExtraArgv();
#endif

////////////////////////////////////////////////////////////////

  processNormalArgs(argc, argv, options, starttime, endtime);

  // mpck won't run the execute script if the user id is below a certain
  // value.
  uid_t uid = getuid();

  if( (options.outputformat == Options::COMMANDS) && options.execute
      && (uid < MINUID))
    {  struct passwd *pw = getpwuid(uid);
         if(pw == 0)
	   {  perror(argv[0]);
	      return 1;
	   }
	 cout << argv[0]
	      << " can't execute removal script when run as user \""
	      << pw->pw_name << "\"\n";
	 return 1;
    }

  // Make sure we don't try to remove good file sets.
  // (Should also restrict date)
  if(options.outputformat == Options::COMMANDS)
    options.onlybrokens = true;

  double listStartTime = getCurrentTime();
    buildFileList(topdir, filelist);
  double listBuildTime = getCurrentTime(listStartTime);

  // If the start time is not supplied:
  // +offset is # of days after date of earliest file.
  // -offset is # of days before date of last file.
  int offset = static_cast<int>(starttime.offset);
  if(starttime.time != 0)
    options.starttime = starttime.time + offset;
  else
  if(offset > 0)
    options.starttime = globalFileStats.start + offset;
  else if(offset < 0)
    options.starttime = globalFileStats.end + offset; // (offset is neg).
  else
    options.starttime = globalFileStats.start;

  // If the end time is not supplied:
  // + endtime offsets are relative to options' startime, not the timestamp
  // of the earliest file. - offsets are relative to the last file's time.
  offset = static_cast<int>(endtime.offset);

  if(endtime.time != 0)
    options.endtime = endtime.time + offset;
  else
  if(offset > 0)
    options.endtime = options.starttime + offset;
  else if(offset < 0)
    options.endtime = globalFileStats.end + offset; // (offset is neg).
  else
    options.endtime = globalFileStats.end;

  // Make sure they're in range.
  if(options.starttime < globalFileStats.start)
    options.starttime = globalFileStats.start;
  else if(options.starttime > globalFileStats.end)
    options.starttime = globalFileStats.end;

  if(options.endtime < globalFileStats.start)
    options.endtime = globalFileStats.start;
  else if(options.endtime > globalFileStats.end)
    options.endtime = globalFileStats.end;

  if(options.starttime > options.endtime)
  { time_t tmp = options.starttime;
    options.starttime = options.endtime;
    options.endtime = tmp;
  }

  // A filename can be created using the start and end times.
  if(options.ofilename == "")
  {  string s1, s2;
     time2String(options.starttime, s1, 1);
     time2String(options.endtime, s2, 1);
     options.ofilename = "mpck_" + s1 + "_" + s2;

     if(options.haveMatchString())
     { string mstr = options.matchString();
       gsub(mstr, "/", "_");  // Replace '/' chars with '_'.
       gsub(mstr, " ", "");  // Remove space chars.
       options.ofilename += "_" + mstr;
     }
     if(options.outputformat != Options::COMMANDS)
       options.ofilename += ".lst";
     else
       options.ofilename += ".sh";
  }

  if(options.ofilename == "-")	// Print to cout
  {  process(cout, filelist, listBuildTime, options);
  }
  else			// Print to file.
  { ofstream of(options.ofilename.c_str());
    if( !of)
    { cerr << "Could not open output file: " << options.ofilename << endl;
       return(1);
    }

    process(of, filelist, listBuildTime, options);
    of.close();

    if((options.outputformat == Options::COMMANDS) && options.execute)
      err = executeScript(options.ofilename);
  }
  return err;
}
