/**
 * Scan the flat file ingest directories and track file sizes and dates.
 * Compute a guesstimate of how fast the files are being produced.
 *
 * @author: Harold Ravlin
 *
 *
 * @usage mprate [-b <start>] [-e <end>] [-g|-l|-s] [-ofile outfile] [-match str] [-nmatch str]
 *
 * @description Scan flat file directories tracking sizes and times.
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
 *  The default values for start is the 7 days before the last timestamp.
 *  The default values for end is the last time stamp.
 *
 * @key start 0	string	Start time
 *
 * @key end  0	string	End time
 *
 * @key ofile	""	string	Output filename.
 *
 * @key format groups	string	Output format [summary|groups|list].
 *
 * @key match	""	string	Only output files with the string in the name.
 *
 * @key nmatch	""	string	Only output files without string in the name.
 *
 * @key conffile dbms/dbms.conf string file from which to get database
 *                                     configuration info as well as
 *                                     directories where data files are
 *                                     located; the conffile location is
 *                                     interpreted by Program::getConfFile()
 * @key sdpconffile sdp/sdp.conf string SDP configuration file.
 *
 * @logger DEFAULT_FACILITY carma.dbms.mprate
 * $CarmaCopyright$
 */


#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <time.h>

#if 0
#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#endif

#include <map>
#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
//#include <cstdlib>
#include <vector>
#include <sstream>


#if !defined(MPRATE_STANDALONE)
#include <log4cpp/Priority.hh>
#include "carma/util/Program.h"
#include "carma/dbms/DBConfigurator.h"
#include <carma/util/KeyValueConfigFile.h>
#include "carma/util/IllegalStateException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Trace.h"
using namespace carma;
using namespace carma::util;
using namespace carma::dbms;
#endif

using namespace std;

/*
  for monitorData/
   foreach [frame] minute slcorrel wbcorrel
    foreach numeric complex short string
     get list of files with their sizes and time stamps.
     add file to map indexed by <daysinceepoch>.

  Do same for:
    /opt/visbrick
    /opt/sdp/astroheader/{SLCorrelIntegrated, WBCorrelIntegrated}

Go through list getting summary data on a day by day basis.

When done, print list.

Compression:
 For visbricks, assume compressed files are 60% the size of uncompressed.
 For monitor data, assume a multiplier of 0.1 for the approximation.
 Files in astroheaders/SLCorrelIntegrated, which vary wildly in size, tend
compress to about 16.5% of their original size. (WBCorrelIntegrated is
empty).

*/

#if 0
static const string DONESUFFIX = ".done";
static const string ERRORSUFFIX = ".error";
static const string BINARYSUFFIX = ".bin";
#endif

static const string UNKNOWN = "UNKNOWN";
static const time_t ONEDAY = 60*60*24; // # of seconds in 1 day.
static const ulong KB = 1024;
static const ulong MB = KB*1024;
static const ulong GB = MB*1024;

/* Flags for the different dirs.*/
typedef unsigned FileFlag;

// Convert index to mask.
#define Index2Mask(i) ( 1 << i)

// Every subdirectory has been assigned an identifier.
// These values may be used as indexes.
// What sort of directories are there?
static const unsigned MDI		= 0;
static const unsigned SDPI		= 1;
static const unsigned VISI		= 2;

static const unsigned ASTROI	= 3;

// Subdirs
static const unsigned FRAMEI	= 4;
static const unsigned MINUTEI	= 5;
static const unsigned SLCORRELI	= 6;
static const unsigned WBCORRELI	= 7;
static const unsigned SLCORRELINTI	= 8;
static const unsigned WBCORRELINTI	= 9;
static const unsigned COMPLEXI	= 10;
static const unsigned NUMERICI	= 11;
static const unsigned SHORTI	= 12;
static const unsigned STRINGI	= 13;

// Typical amount that gzip compresses a type of file.
static float MONITORDATACOMPRESSION = 0.10;
static float VISBRICKCOMPRESSION = 0.60;
static float ASTROHEADERCOMPRESSION = 0.165;


class FileGroup;

typedef map<time_t,FileGroup> FileGroupMap;

static FileGroupMap averagemap, astromap, vismap;


// Subdirectories
typedef struct SUBD
 {string name; FileFlag flag; const SUBD *subdir; unsigned nsubdirs;} Subdirectory;

// The top level directory structure contains some other things.
typedef struct {string title; string dirname; string topdir;
  FileFlag flag; float compression;
  const Subdirectory *subdir; unsigned nsubdirs; FileGroupMap &map;} TopDir;

////////////////////////////////////////////////////////////////
//			Monitor data
////////////////////////////////////////////////////////////////
// List of directories/what kind of data are stored under each AverageType
static const Subdirectory AggregateType[] = {
  {"numeric", NUMERICI, 0, 0},
  {"complex", COMPLEXI, 0, 0},
  {"short", SHORTI, 0, 0},
  {"string", STRINGI, 0, 0}
};
static const unsigned NAGGREGATETYPES =
	sizeof(AggregateType)/sizeof(*AggregateType);

// List of subdirectories under Directory
static const Subdirectory DataAverageType[] = {
  {"frame", FRAMEI, AggregateType, NAGGREGATETYPES},
  {"minute", MINUTEI, AggregateType, NAGGREGATETYPES},
  {"slcorrel", SLCORRELI, AggregateType, NAGGREGATETYPES},
  {"wbcorrel", WBCORRELI, AggregateType, NAGGREGATETYPES}
  };
static const unsigned NAVERAGETYPES =
	sizeof(DataAverageType)/sizeof(*DataAverageType);


////////////////////////////////////////////////////////////////
//			Astroheaders
////////////////////////////////////////////////////////////////
static const Subdirectory AstroSubdirs[] = {
  {"SLCorrelIntegrated", SLCORRELINTI, 0, 0},
  {"WBCorrelIntegrated", WBCORRELINTI, 0, 0}
};
static const unsigned NUMASTROSUBDIRS = sizeof(AstroSubdirs)/sizeof(*AstroSubdirs);

static const Subdirectory Astrodirs[] = {
  {"astroheader", ASTROI, AstroSubdirs, NUMASTROSUBDIRS}
};
static const unsigned NUMASTRODIRS = sizeof(Astrodirs)/sizeof(*Astrodirs);

////////////////////////////////////////////////////////////////
//			visbricks
////////////////////////////////////////////////////////////////
static const Subdirectory Visdirs[] = {
   { "visbrick", VISI, 0, 0}
};
static const unsigned NUMVISDIRS = sizeof(Visdirs)/sizeof(*Visdirs);

// The real value of the topdir field is filled in at run time.
static TopDir topdir[] = {
  {"Monitor Data", "monitorData", "/opt", MDI, MONITORDATACOMPRESSION,
   DataAverageType, NAVERAGETYPES, averagemap},
  {"Astro Headers", "sdp",  "/opt", SDPI, ASTROHEADERCOMPRESSION,
   Astrodirs, NUMASTRODIRS, astromap},
  {"Visbricks", "",  "/opt", VISI, VISBRICKCOMPRESSION,
   Visdirs, NUMVISDIRS, vismap},
};
static const unsigned NUMTOPDIRS = sizeof(topdir)/sizeof(*topdir);


typedef struct { string name; unsigned index; unsigned mask;} IndexMasks;

/*
   0   0        1       MONITORDATA
   1   1        2       SDP
   2   2        4       VIS
   3   3        8       ASTRO
   4   4        10      FRAME
   5   5        20      MINUTE
   6   6        40      SLCORREL
   7   7        80      WBCORREL
   8   8        100     SLCORRELINT
   9   9        200     WBCORRELINT
  10   a        400     COMPLEX
  11   b        800     NUMERIC
  12   c        1000    SHORT
  13   d        2000    STRING
*/

// This is arranged so the above can be used as indexes into it.
IndexMasks indexmasks[] = {
  { "MONITORDATA", MDI, Index2Mask(MDI)},
  { "SDP", SDPI, Index2Mask(SDPI)},
  { "VIS", VISI, Index2Mask(VISI)},
  { "ASTRO", ASTROI, Index2Mask(ASTROI)},
  { "FRAME", FRAMEI, Index2Mask(FRAMEI)},
  { "MINUTE", MINUTEI, Index2Mask(MINUTEI)},
  { "SLCORREL", SLCORRELI, Index2Mask(SLCORRELI)},
  { "WBCORREL", WBCORRELI, Index2Mask(WBCORRELI)},
  { "SLCORRELINT", SLCORRELINTI, Index2Mask(SLCORRELINTI)},
  { "WBCORRELINT", WBCORRELINTI, Index2Mask(WBCORRELINTI)},
  { "COMPLEX", COMPLEXI, Index2Mask(COMPLEXI)},
  { "NUMERIC", NUMERICI, Index2Mask(NUMERICI)},
  { "SHORT", SHORTI, Index2Mask(SHORTI)},
  { "STRING", STRINGI, Index2Mask(STRINGI)}
};
static const unsigned NUMINDEXMASKS = sizeof(indexmasks)/sizeof(*indexmasks);

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
//			Various support routines for above.
////////////////////////////////////////////////////////////////

// Return a string containing all entries represented in flags.
static const string fileMasks2String(unsigned flags)
{unsigned short mask = 1;
 string s = "";

 for(unsigned index=0; index < NUMINDEXMASKS; index++, mask += mask)
 { FileFlag m = mask & flags;
   flags &= ~mask;
#if 0
 cout << "index= " << index
      << " mask = " << hex << mask
      << " flags = " << hex << flags << endl;
#endif
   if(m == 0) continue;
   if(s != "")
     s += " ";
   s += indexmasks[index].name; // Index
   if(flags == 0) break;
 }
 // cout << hex << "Flags:" << oflags << " <" << s << ">\n";
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

// Print bytes per second. Prints nothing if seconds is <= 0.
static void printBytesPerSecond(ostream &os, double numbytes, double seconds)
{  if(seconds > 0)
   { double bps = numbytes/seconds;
     unsigned op = os.precision(3);
     if(bps >= GB)
       os << bps/GB << "GB/s";
     else if(bps >= MB)
       os << bps/MB << "MB/s";
     else if(bps >= KB)
       os << bps/KB << "KB/s";
     else
       os <<  bps << " bytes/s";
     os.precision(op);
   }
}

static void printBytes(ostream &os, double numbytes)
{ ostringstream oss; // Want to return a single string.
  oss.precision(3);
   if(numbytes >= GB)
     oss << numbytes/GB << "GB";
   else if(numbytes >= MB)
     oss << numbytes/MB << "MB";
   else if(numbytes >= KB)
     oss << numbytes/KB << "KB";
   else
     oss <<  numbytes << " bytes";
   os << oss.str();
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

static double getCurrentTime(const double start=0.0)
{
  double rtn = static_cast<double>(time(0)) - start;
 return rtn;
}

// Convert a time_t value to days since the epoch.
static inline time_t timeToDay(time_t t)
{  time_t v = (t/ONEDAY);
   return v;
}

// Convert days since the epoch value to seconds since the epoch.
static inline time_t dayToTime(time_t t)
{  time_t v = t*ONEDAY;
   return v;
}

////////////////////////////////////////////////////////////////
//			Something to hold program options
////////////////////////////////////////////////////////////////

// Various options that affect what gets printed.
class Options {
public:
  enum ACTIONS { SUMMARY=1, GROUPS=2, LIST=3};

  Options()
  {starttime = endtime = 0;
   ignoremask = Index2Mask(FRAMEI);
   outputformat_ = GROUPS;
  }
  ~Options(){};
  // Is time within this Option's time range?
  bool inRange(time_t time, bool timeisDayofEpoch=false)const;
  void addMatchString(const string &str, bool isMatch=true)
  { if(isMatch)
      matchStrings_.push_back(str);
    else
      nmatchStrings_.push_back(str);
  }
  void addNMatchString(const string &str)
  { addMatchString(str, false);
  }

  // Does the string appear in any matchString? Also returns true if there is
  // no match string. (If isMatch is false, returns false if no string).
  bool isMatch(const string &, bool isMatch=true)const;
  bool isNMatch(const string &s)const { return isMatch(s, false);}

  bool haveMatchString(bool isMatch=true)const
  {   unsigned len = isMatch ? matchStrings_.size() : nmatchStrings_.size();
      return (len != 0);
  }

#if 0
  const string &matchString(unsigned index=MATCH)const
  {static const string badindex("badindex");
   if(index < MAXMATCHSTRINGS)
        return matchString_[index];
      else
	return badindex;
  }
#endif

  ACTIONS outputFormat()const{return outputformat_;}
  ACTIONS setOutputFormat(ACTIONS action)
  {ACTIONS old=outputformat_; outputformat_= action; return old;}
  bool onlySummary()const{return outputformat_ == SUMMARY;}

  void print(ostream &)const;

  const char *outputFormat2String(ACTIONS of)const;
  //  string outputFormat2String(unsigned of)const;
  string ofilename;
  time_t starttime, endtime;	// Ignore time stamps outside this range.
  unsigned ignoremask;	// Ignore any directory who's mask is here.
private:
  ACTIONS outputformat_;	// Print format.
  vector<string> matchStrings_;
  vector<string> nmatchStrings_;
};

void Options::print(ostream &str)const
{ ostringstream os;
  string times, timee;

  os << "Today is " << time2String(time(0)) << endl;
  time2String(starttime, times);
  time2String(endtime, timee);
  os << "Time range: " << times << " - " << timee << endl;
  os << "Ignore Directories: " << fileMasks2String(ignoremask) << endl;

  if(matchStrings_.size() > 0)
    { os << "Only filenames containing: ";
      for(vector<string>::const_iterator iter = matchStrings_.begin();
	  iter != matchStrings_.end(); iter++)
	{ if(iter != matchStrings_.begin())
	   os << " | ";
	  os << *iter;
	}
      os << endl;
    }

  if(nmatchStrings_.size() > 0)
    { os << "No filenames containing: ";
      for(vector<string>::const_iterator iter = nmatchStrings_.begin();
	  iter != nmatchStrings_.end(); iter++)
	{ if(iter != nmatchStrings_.begin())
	   os << " | ";
	  os << *iter;
	}
      os << endl;
    }

  os << "Output format = " << outputFormat2String(outputformat_) << endl;
  os << endl;  // An extra line.
  str << os.str();
}

#if 0
void Options::setMatchString(const string &str, unsigned index)
{
  if(index < MAXMATCHSTRINGS)
  {  matchString_[index] = str;
     haveMatchString_[index] = (str != "");
  }
}

void Options::setMatchString(const char *str, unsigned index)
{
  if(index >= MAXMATCHSTRINGS)
    return;
  if(str != 0)
  {  matchString_[index] = str;
     haveMatchString_[index] = (strlen(str) > 0);
  }
  else
  { matchString_[index] = "";
    haveMatchString_[index] = false;
  }
}
#endif

#if 0
bool Options::getMatchString(string &str, unsigned index)const
{
  if(index >= MAXMATCHSTRINGS)
    return false;
  str = matchString_[index];
  return haveMatchString_[index];
}
#endif

// Returns true is any string in strs appears in s.
// Returns ifempty if strs is empty.
static bool search(const vector<string> &strs, const string &s,
		   bool ifempty)
{bool rtn = false;

 if(strs.size() == 0)
   rtn = ifempty;
 else
  for(vector<string>::const_iterator iter = strs.begin();
      iter != strs.end(); iter++)
    if(s.find(*iter) != string::npos)
      { rtn = true;
      //      cout << "found: <" << *iter << "> in " << s << endl;
        break;
      }

 return rtn;
}

// For isAMatch = true:
//  Returns true if any match string appears in true or if there are
//  no match strings.
// For isAMatch = false:
//  Returns true if any nmatch string appears in true. Returns false
//  if there no nmatch strings.
bool Options::isMatch(const string &str, bool isAMatch)const
{bool rtn;

 if(isAMatch)
   rtn = search(matchStrings_, str, true);
 else
   rtn = search(nmatchStrings_, str, false);
  return rtn;
}

// Returns true if time is within the option's time range.
bool Options::inRange(time_t time, bool timeIsDayofEpoch)const
{ bool inrange;
    
 if(timeIsDayofEpoch) 
   time = dayToTime(time);

 if((time >= starttime) && (time <= endtime))
   inrange = true;
 else
   inrange = false;

  return inrange;
}

const char *Options::outputFormat2String(ACTIONS of)const
{
 switch(of) {
 case SUMMARY:
   return "SUMMARY";
   break;
 case GROUPS:
   return "GROUPS";
   break;
 case LIST:
   return "LIST";
   break;
 default:
   return "UNKNOWN";
   break;
 }
}

#if 0
static void addStr(string &str, const string &add)
{
  if(add.size() == 0)
    return;
  if(str.size() > 0)
    str += "|";
  str += add;
}

string Options::outputFormat2String(unsigned of)const
{ string ofs;

 if(of&SUMMARY)
   ofs += "SUMMARY";
 if(of&GROUPS)
   addStr(ofs,"GROUPS");
 if(of&LIST)
   addStr(ofs, "LIST");
 return ofs;
}
#endif

////////////////////////////////////////////////////////////////
// Keeps track of various statistics over all files

class UsageSummary{
public:
  UsageSummary(){ end = 0; numfiles=0; numbytes=0.0;
  		  start = time(0) + ONEDAY;
  }

  void timecheck(const time_t time); // Update min/max time.
  void update(unsigned nfiles, unsigned nbytes){numfiles += nfiles;numbytes+=nbytes;}
  time_t seconds()const{return end-start;}
  // Write info to stream.
  void print(ostream &os, double compression=0.0);
  void print2(ostream &os, double compression=0.0);
  static void printHeader(ostream &os, const Options &options);
  time_t start;  // Earliest & latest time stamps.
  time_t end;
  unsigned numfiles;
  double numbytes;
  static string colsep;
};


string UsageSummary::colsep("    ");

void UsageSummary::timecheck(const time_t time)
{
  if(time < start)
    start = time;
  if(time > end)
    end = time;
}

// Write the statistics to the stream.
void UsageSummary::print(ostream &os, double compression)
{int dtime = end - start;
   string tstart, tend;
   time2String(start, tstart);
   time2String(end, tend);
   os << "Start time = " << tstart;
   os << " End time = " << tend << endl;

   os << "# secs: " << end - start;
   os << " # files: " << numfiles;
   os << " Total space: ";
   printBytes(os, numbytes);
   os << " ";
   //   printBytesPerSecond(os, numbytes, dtime);
   if(dtime && (numfiles > 1))
   { double seconds2 = dtime + (dtime/(numfiles-1));
     os << " Est. write rate: ";
       printBytesPerSecond(os, numbytes, seconds2);
       os << "";
       if(compression > 0.0)
       { os << " Est. compressed write rate:";
         printBytesPerSecond(os, numbytes*compression, seconds2);
	 //	 os << "";
       }
   }
   os << endl;
}

void UsageSummary::printHeader(ostream &os, const Options &options)
{

  if(options.onlySummary())
    return;
 streamsize width = os.width();
 streamsize timelength = 16;
 os << setw(timelength) << "Start time" << colsep;
 os << setw(timelength) << "End time" << colsep;
 os << setw(6) << "# secs" << colsep;
 os << setw(6) << "# files" << colsep;

 string tspace("Total Space");
 os << setw(tspace.size()) << tspace << colsep;
 os << setw(20) << "Bytes Per Second" << colsep;
 os << "Est. Compressed rate";
 os << endl;
 os.width(width);
}

// Write the statistics to the stream.
void UsageSummary::print2(ostream &os, double compression)
{int dtime = end - start;
 stringstream oss;
 string tstart, tend;
 time2String(start, tstart);
 time2String(end, tend);
 streamsize owidth = os.width();
 double seconds;

 os << setw(16) << tstart << colsep;
 os << setw(16) << tend << colsep;

 os << setw(6) << end - start << colsep;
 os << setw(6) << numfiles << colsep;
 os.width(11);
 printBytes(os, numbytes);
 os << colsep;

 // printBytesPerSecond(oss, numbytes, dtime);
 // If there are at least 2 files, approximate the time it took to
 // write the first one.
 seconds = dtime;
 if(dtime && (numfiles > 1))
 { seconds += (seconds/(numfiles-1));  // Increase by avg. of the other files.
   oss << " ";
   printBytesPerSecond(oss, numbytes, seconds);
   //   oss << ")";
 }
 os << setw(20) << oss.str();

 if((seconds > 0.0) && (compression > 0.0))
 { os << colsep << " ";
   printBytesPerSecond(os, numbytes*compression, seconds);
   //   os << "]";
 }

   os << endl;
   os.width(owidth);
}


// This is updated when the directories are first processed.
static UsageSummary globalFileStats;

class AverageUsage {
public:
  AverageUsage(){ seconds = 0; numfiles=0; numbytes=0;}
  ~AverageUsage(){}
  void update(time_t dseconds, unsigned nfiles, unsigned nbytes)
  {seconds += dseconds; numfiles += nfiles;numbytes+=nbytes;}
  void update(const UsageSummary &usage);
  double bytesPerSecond()const;
  double bytesPerDay()const{ return bytesPerSecond()*ONEDAY;}
  void print(ostream &os, double compression=0.0);
  time_t seconds;
  unsigned numfiles;
  double numbytes;
};

void AverageUsage::update(const UsageSummary &usage)
{
  int dtime = usage.seconds();
  if(dtime < 0)	// A directory with only 1 file will show up as 0 seconds.
    return;
  seconds += dtime;
  numfiles += usage.numfiles;
  numbytes += usage.numbytes;
}

double AverageUsage::bytesPerSecond()const
{
 if(seconds > 0) 
   return numbytes/seconds;
else
   return -1.0;
}

// Write the statistics to the stream.
void AverageUsage::print(ostream &os, double compression)
{
  os << " # files = " << numfiles;
  os << " # seconds " << seconds;
  os << " Total space: ";
  printBytes(os, numbytes);

  if(seconds > 0) {
    double seconds2 = 0.0;
    if(numfiles > 1)
      seconds2 = seconds + (seconds/(numfiles-1));

    os << " Est. write rate: ";
    printBytesPerSecond(os, numbytes, seconds2);
    if(compression > 0.0) {
      os << " Est. compressed write rate: ";
      printBytesPerSecond(os, numbytes*compression, seconds2);
    }
  }

  os << endl;
}

////////////////////////////////////////////////////////////////
// Information about a particular file.

class FileStat {
public:
  FileStat();
  FileStat(const string filename, const time_t time, const size_t size,
	   unsigned flags)
  { addStats(filename, time, size, flags);}

  void addStats(const string filename, const time_t time, const size_t size,
		unsigned flags);

  FileStat &operator=(const FileStat &rhs);

  ~FileStat(){}
  const string &fileName()const{return name_;}
  size_t size()const { return size_;}
  time_t mtime()const { return time_;}
  unsigned flags()const{ return flags_;}
  // Returns true if something is put into the ostream.
  bool print(ostream &, UsageSummary &stats, const Options &options)const;
private:
  string name_;
  time_t time_;
  size_t size_;
  unsigned	 flags_;
};

FileStat::FileStat()
{
  name_ = "?";
  time_ = 0;
  size_ = 0;
  flags_ = 0;
}

FileStat &FileStat::operator=(const FileStat &rhs)
{
  if(this != &rhs)
  {  name_ = rhs.name_;
       time_ = rhs.time_;
       size_ = rhs.size_;
       flags_ = rhs.flags_;
  }
  return *this;
}


void FileStat::addStats(const string filename, const time_t time,
			const size_t size,   unsigned flags)
{ name_ = filename; time_ = time; size_=size; flags_ = flags;
 globalFileStats.timecheck(time);
 globalFileStats.update(1, size);
}


bool FileStat::print(ostream &os, UsageSummary &stats,
		     const Options &options)const
{ bool rtn = false;

 if(!options.inRange(time_))
   return false;

 // Check {N}match.

 stats.timecheck(time_);
 stats.update(1, size_);

 if(options.outputFormat() == Options::LIST)
 { string name = name_;
   string time;

   for(unsigned i=0; i < NUMTOPDIRS; i++)
     if(Index2Mask(topdir[i].flag) & flags_)
     {   string::size_type n;
         string maindir = topdir[i].topdir;

	 if((n = name.find(maindir)) == 0)
	   name.erase(0, maindir.size()+1); // Remove topdir + "/";
	 break;
     }

   time2String(time_, time);
   os << "  " << setw(64) << left << name << " " << time;
   os << " " << right;
   os.width(8);
   printBytes(os, size_);
#if 0
   os << "\t" << left << fileMasks2String(flags_)
      << " (" << hex << flags_ << ")";
#else
   os << "\t" << left << fileMasks2String(flags_);
#endif
   rtn = true;
 }

  return rtn;
}

////////////////////////////////////////////////////////////////
// Holds FileStats for each file in a group. ie. All file
// and all links to it.
// eg. For files under frame/numeric, there should be at most two entries:
// One for the data file under monitorData and one for the link under
// dbLoad.
// For files that get archived &/or loaded into visbricks, there would be
// entries for those links also.
// Files are added if the "basename" matches. Where basename is the filename
// with anything after the ".mpdat" removed. {eg. .bin, .done, .write, etc.


typedef vector<FileStat> FileStats;

class FileGroup{
public:
  FileGroup() {dayofepoch_ = 0;}
  ~FileGroup() {}
  // Add a file to this group.
  void addFile(const string &filename, time_t time, size_t size, unsigned flags);
  const FileStats &files()const{return files_;}
  //unsigned numFiles()const{ return files_.size();}

  bool printGroup(ostringstream &os, const Options &options,
		  const time_t dayofepoch, double compression,
		  AverageUsage &usage);
  // Sets dayofepoch_ if it's 0. Otherwise, compares the two and returns
  // true if they're the same.
  bool dayCheck(time_t dayofepoch)
  { if(dayofepoch_ == 0)
     dayofepoch_ = dayofepoch;
    return (dayofepoch_ == dayofepoch);
  }
private:
  time_t	dayofepoch_;
  vector<FileStat> files_;	// Entries from scandir.
};

// Add a file to the group.
// Update global stats.

void FileGroup::addFile(const string &filename, time_t time,
			size_t size, unsigned masks)
{ FileStat stat(filename, time, size, masks);
  files_.push_back(stat);
}

// Returns true if file info was written into the stream.
bool FileGroup::printGroup(ostringstream &os, const Options &options,
  			   const time_t dayofepoch,
			   double compression, AverageUsage &usage)

{ unsigned numprinted = 0;
  ostringstream filebuf;
  UsageSummary mystats;

    // Make a pass through the files checking time ranges and collecting
    // flags.
    unsigned size = files_.size();

    if(size == 0)
      return false;

    if((options.outputFormat() == Options::LIST) && size > 0)
      filebuf << setw(27) << "File"
	      << setw(27) << " "
	      << setw(16) << "Date" << "\t"
	      << setw(10) << "Size" << "\t\tFlags" << endl;

   for(unsigned i=0; i< size; i++)
   {const FileStat &fs = files_[i];
     
      // Print info for this file.
      if(fs.print(filebuf, mystats, options))
      {  filebuf << endl;
         numprinted++;
      }
   }

   if((options.outputFormat() == Options::LIST) && size > 0)
     filebuf << endl;	// An extra line after the list of files.

   // Don't actually print anything if no info was printed.
    bool printedFiles = (numprinted > 0);
    bool printedSummary = false;

    if((mystats.numfiles) && !options.onlySummary())
    {  //os << time2String(dayofepoch*ONEDAY) << " ";
       mystats.print2(os, compression);
       printedSummary = true;
    }

    if(printedFiles)
      os << filebuf.str();

    usage.update(mystats);
    return printedFiles || printedSummary;
}


// Build a string containing path to a directory and get list of
// the files in it. Add each file to the list of files.
static bool processDir(const string &oldpath, const Subdirectory &subdir,
		       unsigned oldmask, const Options &options,
		       FileGroupMap &filelist)
{string path = oldpath + "/" + subdir.name;
 unsigned mask = oldmask | Index2Mask(subdir.flag);
 unsigned ndirs = subdir.nsubdirs;

 if(options.ignoremask & mask)
   return false;

 if(ndirs)
 { bool ok;
   for(unsigned i = 0; i < ndirs; i++)
     ok = processDir(path, subdir.subdir[i], mask, options, filelist);
   return ok;
 }

#if 0
cout << "Path is " << path
     << " [" << fileMasks2String(mask) << "]"<< "\t" << hex << mask << endl;
#endif

 struct dirent **namelist = 0;
 struct stat statbuf;

 // List of files in the directory.
  int nentries = scandir(path.c_str(), &namelist, 0, alphasort);
  if(nentries < 0)
  { cerr << "Error doing directory scan for " << path << " ("
	 << strerror(errno) << ")" << endl;
    return false;
  }

 for(int i = 0; i < nentries; i++)
 {string filename = namelist[i]->d_name;
  free(namelist[i]);
  if((filename == ".") || (filename == ".."))
    continue;

  if(filename.rfind(".write") != string::npos)
    continue;

  string fullname = path + "/" + filename;

  // If full filename doesn't contain any match string, ignore it.
  if(!options.isMatch(fullname))
    continue;

  // If the filename contains any nmatch string, ignore it.
  if(options.isNMatch(fullname))
    continue;

  // Get file info.
  int status = stat(fullname.c_str(), &statbuf);
  if(status < 0)
  {  perror(fullname.c_str());
     continue;
  }

  // Ignore directories, sym links and anything else that's not a file.
  if(!S_ISREG(statbuf.st_mode))
    continue;

  // Get an index and add the file data.
  time_t dayindex = timeToDay(statbuf.st_mtime);
  FileGroup &info = filelist[dayindex];
  if(info.dayCheck(dayindex))
    info.addFile(fullname, statbuf.st_mtime, statbuf.st_size, mask);
  else
    cerr << "Attempt to add " << fullname << " to wrong day\n";
 }
 free(namelist);

 return true;
}

static void buildFileList(const string &topdir, TopDir &td,
			  const Options &options)
{string path = topdir;
 unsigned ndirs = td.nsubdirs; 
 unsigned mask = Index2Mask(td.flag);

 td.topdir = topdir;

 if(td.dirname.size() > 0)
   path += "/" + td.dirname;

 for(unsigned i = 0; i< ndirs; i++)
   processDir(path, td.subdir[i], mask, options, td.map);
}

static void printFileList(ostream &outs, FileGroupMap &filelist,
			  double compression,
			  const Options &options)
{ ostringstream os;
 AverageUsage stats;

 UsageSummary::printHeader(outs, options);
  for(FileGroupMap::iterator iter=filelist.begin();
      iter != filelist.end(); iter++) {
    time_t dayofepoch = iter->first;
    FileGroup &group = iter->second;
    os.str("");
    if(!group.printGroup(os, options, dayofepoch, compression, stats))
      continue;
    outs << os.str();
    //      outs << os.str() << endl;
  }

  os.str("");

  stats.print(os, compression);

  outs << os.str();
}

static void printResults(ostream &of, const TopDir *td, unsigned ntds,
		    double listBuildTime, Options &options)
{double printStartTime = getCurrentTime();
 double printTime;
 unsigned numdirs = ntds;

 options.print(of);

 for(unsigned i = 0; i< numdirs; i++)
   { of << "\t\t" << td[i].title;
     float kmp = static_cast<float>(td[i].compression*100);
     if(td[i].compression > 0.0)
      of << " (Estimated compression: " << kmp << "%)";
     of << endl;
     printFileList(of, td[i].map, td[i].compression, options);
     of << "------------------------------------------------------------\n\n";
   }
 printTime = getCurrentTime(printStartTime);
 of.precision(2);
 of << "Global file stats\n";
 globalFileStats.print(of);

 of << "# Read time = " << listBuildTime/60.0 << " min"
    << " Print time = " << printTime/60.0 << " min\n";
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
  cout << "Usage: " << name << " [h l g s <date> e<date> m <str>]" << endl;
  cout << " Prints estimated write rates for monitorData, astroheader and visbrick files.\n";
  cout << " -h	- Prints this message and exits.\n";
  cout << " -l	- Generate long list (including filenames)." << endl;
  cout << " -g	- Generate medium list (daily summary) default." << endl;
  cout << " -s	- Only print summary information.\n";
  cout << " -b <date> - Begin printing at <date>.\n";
  cout << " -e <date> - End printing at <date>.\n";
  cout << " -o <file> - file will be used for the output filename." << endl;
  cout << " -m <string> - Only process filenames containing <string(s)>\n";
  cout << " -n <string> - Don't process filenames containing <string(s)>\n";
  cout << " -[A|V|M] - Toggle whether Astroheaders, Visbricks or monitor data are shown.\n";
  cout << "	  Default is to show all.\n";
  cout << " -F	- Toggle whether monitor/frame data are shown. default is no.\n";
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

  cout << "The default values for -b and -e is to print information for the last 7 days.\n";

}

static const char *optstring="AVMFahlsgb:e:m:n:o:";

static void processNormalArgs(int argc, char *argv[], Options &options,
			      TimeInfo &starttime, TimeInfo &endtime)
{int rtn;
 unsigned mask;

  while((rtn=getopt(argc, argv, optstring)) >= 0)
    if( rtn != '?')
    { // printf("|%c| %s\n", rtn, optarg);

      switch(rtn) {
      case 's':				// Only print summaries.
	options.setOutputFormat(Options::SUMMARY);
	break;
      case 'l':				// Print info about anything.
	options.setOutputFormat(Options::LIST);
	break;
      case 'g':				// Print summary + group info.
	options.setOutputFormat(Options::GROUPS);
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
      case 'A':				// Toggle ignore ASTROHEADERS
	mask = Index2Mask(ASTROI);
	options.ignoremask = options.ignoremask ^ mask;
	break;
      case 'V':				// Toggle ignore Visbricks
	mask = Index2Mask(VISI);
	options.ignoremask = options.ignoremask ^ mask;
	break;
      case 'M':				// Toggle ignore monitordata
	mask = Index2Mask(MDI);
	options.ignoremask = options.ignoremask ^ mask;
	break;
      case 'F':				// Toggle ignore FRAME.
	mask = Index2Mask(FRAMEI);
	options.ignoremask = options.ignoremask ^ mask;
	break;
      case 'h':
	help(argv[0]);
	exit(0);
	break;
      case 'm':
	options.addMatchString(optarg);
	break;
      case 'n':
	options.addNMatchString(optarg);
	break;
      default:
	break;
      }
   }
}


#if !defined(MPRATE_STANDALONE)
int Program::main()
#else
int main(int argc, char *argv[])
#endif
{FileGroupMap filelist;
 string mdtopdir, visBrickDir, astroHeaderDir;
 int err=0;
 Options options; 
 TimeInfo starttime, endtime;

////////////////////////////////////////////////////////////////
// Default to starting 7 days ago and going to present.
 string2Time("-7", starttime);
 string2Time("-0", endtime);

////////////////////////////////////////////////////////////////
#if defined(MPRATE_STANDALONE)
#if 0
 mdtopdir = "/scr1/spica/carma/dbms/dbIngest";
 visBrickDir = astroHeaderDir = "/scr1/spica/carma/dbms";
#else
 mdtopdir = "/opt/dbIngest";
 visBrickDir = astroHeaderDir = "/opt";
#endif
#else
 const string conffile = getConfFile( getStringParameter("conffile"));
 const string sdpconffile = getConfFile( getStringParameter("sdpconffile"));

    auto_ptr<DBConfigurator> dbconf;
    try {
        auto_ptr<DBConfigurator> dbconftmp 
            (new DBConfigurator(conffile));
        dbconf = dbconftmp;
	mdtopdir = dbconf->getTopDir();

	// Read configuration file parameters
	std::map<std::string, std::string> sdpconf = 
	  carma::util::KeyValueConfigFile::load(sdpconffile);

	// Get the astroheader and sdp top directories
	astroHeaderDir = visBrickDir = sdpconf["top"];
    } catch (const NotFoundException & exc) {
        ostringstream msg;
        msg << "Unable to read configation file " << conffile;
        CARMA_CPTRACE (Trace::TRACE1, msg);
	//        getLogger() << log4cpp::Priority::ERROR << msg.str();
        exc.report();
        return EXIT_FAILURE;
    }

{string s;
// bool b; 
  s = getStringParameter("ofile");
  if(s != "")
    options.ofilename = s;
  s = getStringParameter("format");
  if(s == "list")
    options.setOutputFormat(Options::LIST);
  else if(s.find("summary") == 0)
    options.setOutputFormat(Options::SUMMARY);
  else if(s == "groups")
    options.setOutputFormat(Options::GROUPS);
  else
    { cerr << "Unrecognized command: " << s << endl;
    exit(1);
    }

  s = getStringParameter("match");
  if(s != "")
    options.addMatchString(s);

  s = getStringParameter("nmatch");
  if(s != "")
    options.addNMatchString(s);
}

  int argc = getExtraArgc( );
  char **argv = getExtraArgv();
#endif

////////////////////////////////////////////////////////////////

  processNormalArgs(argc, argv, options, starttime, endtime);

  double listStartTime = getCurrentTime();
#if 0
    buildFileList(mdtopdir, topdir[i], options);
    buildFileList(visBrickDir, topdir[1], options);
    buildFileList(astroHeaderDir, topdir[2], options);
#else
 string toppath[NUMTOPDIRS];
 toppath[MDI] = mdtopdir;
 toppath[VISI] = visBrickDir;
 toppath[SDPI] = astroHeaderDir;

    for(unsigned i=0; i < NUMTOPDIRS; i++)
    buildFileList(toppath[i], topdir[i], options);
#endif
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
     options.ofilename = "mprate_" + s1 + "_" + s2;

#if 0
     if(options.haveMatchString())
     { string mstr = options.matchString();
       gsub(mstr, "/", "_");  // Replace '/' chars with '_'.
       gsub(mstr, " ", "");  // Remove space chars.
       options.ofilename += "_" + mstr;
     }
#endif
     options.ofilename += ".lst";
  }

  if(options.ofilename == "-")	// Print to cout
      printResults(cout, topdir, NUMTOPDIRS, listBuildTime, options);
  else			// Print to file.
  { ofstream of(options.ofilename.c_str());
    if( !of)
    { cerr << "Could not open output file: " << options.ofilename << endl;
       return(1);
    }
	printResults(of, topdir, NUMTOPDIRS, listBuildTime, options);
    of.close();

  }
  return err;
}
