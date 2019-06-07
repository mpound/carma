#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/FileUtils.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/services/Angle.h"
#include "carma/services/HourAngle.h"
#include "carma/services/ephemFunctions.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Frequency.h"
#include "carma/services/Location.h"
#include "carma/services/Source.h"
#include "carma/services/SourceChecker.h"
#include "carma/services/stringConstants.h"
#include "carma/services/Table.h"
#include "carma/services/Velocity.h"
#include "carma/services/UvTrack.h"

#include <cmath>
#include <iomanip>
#include <iostream>
#include <fstream>
#include <map>
#include <string>
#include <vector>
#include <list>
#include <algorithm>

/*
 * Date: Thu, 7 Feb 2008 14:55:24 -0800
 *
 * DB/MP conversation on "mksched".
 *
 * Vision:  PDB/mksched will provide 3mm and 1mm master schedules updated
 * regularly so the observers can see the next project that logically
 * should be run
 *
 * - scheduler will allow the high-dec projects to drift within a "good"
 * HA range.  DONE
 * - small leftover pieces <= 1 hr will be added to the ends of high-dec 
 * projects where possible. DONE
 *
 * - otherwise, small leftover pieces >1 hr will be padded to 4 hrs or 2
 * hrs (flex tracks). DONE
 * - DB will remind proposers (and JMC will remind script writers) that
 * "flexible HA tracks should have a quick calibration method, or the
 * projects may not be suitable for flexible scheduling.
 * - NV is free to add extra time to flexible projects that get
 * fragmented into many shorter tracks (sensitivity calculation will
 * assume that they are done as long tracks). Not feasible at this stage
 * to do this automatically.
 */

//
// @version $Revision: 1.52 $ 
// @author Marc Pound
//
// @usage  Create an array schedule and report on scheduling
// statistics
//
// @description
// This program makes an initial array schedule from
// in input list of ranked projects.   The elevation limits
// for scheduling tracks are based on the declination of the source
// and the frequency of the observation.  The algorithm is 
// described in CARMA Memo #41.  The SSC policy states that 
// tracks are limited to 8 hrs maximum, even if the optimal 
// tracks are longer. 
//
// The program will attempt to schedule tracks centered on transit,
// if it cannot do this it will try to fit the track somewhere
// in the optimal range.  If scheduling a track would result
// in less than 1 hr remaining to be allocated, the remaining will
// be added to the track if possible, even if it means going 
// over the 8 hr limit.   The piece will be as evenly split
// between before and after the track as is possible given
// what may already be scheduled.
//
// The size of the entire schedule in days is controlled by the 
// days keyword.  If days=0, then days will be added to 
// the schedule as needed to accomodate all projects that meet the
// grade and cutoff criteria.  If days >0, then projects (or parts thereof)
// that do not fit in the fixed length schedule, will not be scheduled.
// 
// The minimum allowable schedulable block (in days) is controlled via the
// chunksize keyword (but see also flexchunk).  For projects with Flexible
// Hour Angle set (FlexHA=1), the flexchunk keyword is used instead.
// Blocks smaller than this size are padded out to this size, UNLESS
// then are less than 1 hr, in which case they are just dropped.
//
// Only one array configuration should be scheduled at a time, use
// the config keyword to choose one configuration from the input file.
//
// Only one band should be scheduled at a time, use
// the band keyword to choose one configuration from the input file.
//
// Input priorities MUST be unique.  Program will exit with error if not.
//
// The output is an ASCII calendar showing the projects scheduled
// each day for the range of days specified in the input.
// There follows scheduling statistics on the overall calendar as well a
// breakdown by project and institution.  (To see what the maximum
// allocation per insitutional would be use days=0).
//
// There are two scheduling modes:
//        priority - means give high priority tracks longer slots even if it extends the 
//                   schedule length.
//        compact - means try to make the shortest schedule possible, even if it means 
//                  breaking up or shortening high priority tracks.
//
// Scheduling fixed blocks (e.g., VLBI):  You can have apriori fixed blocks input 
// into the schedule before the input projects get scheduled.  The file
// with fixed blocks is given with the 'fixed' keyword.  Every run of
// the program regardless of whether fixed is specified or not outputs
// a file <input_filename>.fixed_blocks which is a representation of 
// the final schedule in a simple CARMA-format table.  You can edit 
// this file to create your input file for fixed.
//
// The output ASCII calendar can be marked with day and date by giving
// the date of the first calendar day with the start keyword.
//
// The sunLimit keyword allows for eliminating from the schedule sources 
// that are too close to the Sun.
//
// @key   file  @mandatory s  The input list of projects, must be in CARMA 
//                            table format.  Does not need to be sorted 
//                            by grade, the program will do this.
//
// @key   config @mandatory s  Name of the array configuration to 
//                             schedule (allows for input lists with multiple 
//                             array configs in them). Case insensitive.
//
// @key   band   @mandatory s  Which band to schedule. 
//                             One of: 3mm, 1mm, 1cm.  Case insensitive.
//
// @key   days      0      i  The maximum number of days to schedule, 
//                            0 means no limit
//
// @key   mode    priority s  Case-insensitive string indicating mode for 
//                            scheduling tracks: priority or compact
//
// @key   cutoff  0.1      d  Grade below which no projects will be scheduled.
//
// @key   pivot   0.0      d  Grade below which all projects are scheduled 
//                            in COMPACT mode.
//
// @key   fixed   none     s  File name for fixed blocks to be scheduled BEFORE 
//                            any new blocks are scheduled. Useful for, e.g. TOO or 
//                            VLBI runs. The file can be the output of a previous 
//                            run of the program. (See description of fixed blocks 
//                            in program description). 
//                            Default=none which means no fixed blocks.
//
// @key   chunksize 4      d  Allow breaking up of projects into chunks no 
//                            smaller than chunksize. Zero means don't break up
//                            projects. Units are hours.  Leftover chunks
//                            greater than 1 hour but less than chunksize will 
//                            be padded out to chunksize.
//
// @key   flexchunk 2      d  Alternate chunksize for flexHA projects.  
//
// @key   start     ""     s  Start date corresponding to Day 1 (local 
//                            timezone). Optional, will be used to mark the 
//                            schedule with actual dates. 
//                            Format is YYYY-MMM-DD e.g. 2007-Mar-19.
//
// @key   verbose   0      i  Verbose output level while scheduling.  
//                            Levels mean: 
//                             0 - default: Only print out ASCII calendar.
//                             1 - Print out the calendar and the statistics, including
//                                 scheduling efficiency and institutional allocation.
//                                 Note it is possible for a institution to go over
//                                 their allocation because of expanding small chunks
//                                 to the chunksize.
//                             2 - debug level, lots of spew.
//
// @key   expand    false  b  expand or don't expand projects 
//                            to eliminate short pieces.
//
// @key   startLST  0      d   Local sidereal time at which to start the 
//                             schedule.  This is useful if you want to 
//                             start the schedule at a time you
//                             know a previous script is ending.
// @key   subarray  sci1    s  Subarray for which this program is 
//                             being run: sci1, sci2, eng1, eng2.
// @key   queue   queue.out s  Output file in Carpenter's queue format.
// @key   sunLimit  0       d  Sun-source angular separation limit, in degrees, 
//                             below which the project will not be scheduled. 
//                             Todo:If scheduling multiple bands, this should be a comma-separated
//                             list of limits.
//
//
// @logger DEFAULT_FACILITY carma.services.mksched
//

using namespace ::std;
using namespace carma::services;
using namespace carma::util;

namespace { //anonymous

//***********************************
//Some useful classes and constants
//***********************************


// Block days up into small pieces so that we
// can hit the LST start/stop times closely and
// also to allow for partially allocated hours
// (e.g the TAC allocates 6.3 hrs).
// Units here are minutes.
static const unsigned short BLOCK_SIZE = 15; 
static const unsigned short BLOCKS_PER_HOUR = 4; 
// don't schedule pieces of size 1 hr or less.
static const unsigned short MINBLOCKS_TO_SCHEDULE = 5;
static const float MINHOURS_TO_SCHEDULE = MINBLOCKS_TO_SCHEDULE*BLOCK_SIZE/60.0;

// number of BLOCK_SIZE-minute blocks in 24 hours
static const unsigned short BLOCKS_PER_DAY = 96;

enum verboseEnum {
    OUTPUT_BRIEF,
    OUTPUT_STATS,
    OUTPUT_DEBUG
} ;

// global output verboseness level
static int verbose = OUTPUT_BRIEF;
static bool userExpand = false;


typedef enum bandEnum {
    BAND_3MM,
    BAND_1MM,
    BAND_1CM,
    BAND_UNKNOWN
} bandType ;

typedef enum modeEnum {
    MODE_PRIORITY,
    MODE_COMPACT
} modeType;

// institutional codes
const string CIT("C");
const string UMD("M");
const string UCB("B");
const string UIUC("I");
const string CHICAGO("Z");
const string CARMA("D"); // Director's discretionay time
const string VISITOR("V");

// Reference source for solar angle
const string SUN("sun");

// A chunk of time. start and end are in units of
// blocks. good indicates this is a usable chunk.
struct Chunk {

    // start is always between 0 and BLOCKS_PER_DAY-1
    // and marks the first used block.
    unsigned short start;

    // end marks the last used block.
    // if crossing a day boundary,
    // end will be greater than BLOCKS_PER_DAY
    unsigned short end;

    // true if this is a usable chunk.
    bool good;

    // guaranteed positive because end is semi-unbounded.
    unsigned short size() const 
    {
        return ( end - start );
    }
};

// A sidereal pass of a source
// (aka a track).  It is a chunk
// of time on a given day.
struct Pass {
    unsigned short day; // day number
    Chunk chunk;        // the Chunk for this pass
};

// *******************
//  utility methods
// *******************

unsigned short
findBlock( double hour ) {
    while ( hour >= 24.0 ) hour -= 24.0 ;
    while ( hour < 0.0 )  hour += 24.0 ;
    // number of 15 minute chunks
    hour *= BLOCKS_PER_DAY/24.0;
    // round to nearest int 
    return static_cast<unsigned short>( floor( hour ) );
}

float getLst( const unsigned short block )
{
    float lst = ( 24.0 * block/ BLOCKS_PER_DAY );
    // use >= here so that 24H is always returned as 0H
    while (lst >= 24.0) lst-=24.0;
    return lst;
}

float 
getStartLst( const Chunk & chunk ) {
    return getLst( chunk.start );
}

float 
getStopLst( const Chunk & chunk ) {
    return getLst( chunk.end );
}

/**
 * Class describing a project and its disposition
 */
class Project {
public:
    Project();
    ~Project();

    /**
     * Print info about this project
     */
    void print();

    /**
     * @return A string containing "name[obsblock#]"
     */
    ::std::string disambiguatedName();

    /** @return true if there is stull unassigned time in thie project */
    bool hasUnassignedBlocks() const;

    /** add a scheduled sidereal pass 
     * @param dayNumber the day on which this pass falls
     * @param chunk The Chunk that was assigned.
     */ 
    void addPass(unsigned short dayNumber, const Chunk & chunk);

    /** 
     * @return  number of sidereal passes ultimately assigned 
     */
    unsigned short numPasses() const;

    /**
     * print out statistics about this project
     */
    void stats();

    /**
     * Decide if sun is too close to source to observe.
     * @param chunk The current chunk
     * @param mjd the MJD at LST=0 of the Day in which this chunk resides.
     * @return true if Sun-source separation angle is greater than sun limit,
     * meaning source can be observed.
     */
    bool solarAngleIsGood(const Chunk & chunk, double mjd );

    /**
     * @return the institutional code for this project.
     */
    std::string getInstCode( void );

    /**
     * @return the policy length in blocks
     */
    unsigned short blockLen( void );

    string name;     // project name
    string institution; // institutional assignment
    string config;      // array configuration
    string source;      // source name
    string obsblockId;  // fully qualified obsblock name,
                        // to get script name, change "." to
                        // "_" and append ".obs"
    string scriptName;
    float grade;     // numeric grade
    float lstStart;  // earliest possible LST start time, based on UvTrack optimal limits
    float lstStop;   // earliest possible LST stop time, based on UvTrack optimal limits
    float upTime ;   // start - stop, in hrs
    float optimalLength;//hrs
    float policyStart;  //hrs 
    float policyStop;   // hrs
    float policyLength; // hrs
    bool  expandable; // true if optimalLength > policyLength
    int obsblock; // obsblock #
    double ra;
    double dec;
    double freq; // ghz
    unsigned short LSTstartBlock ; // rise time in blocks, optimal
    unsigned short LSTstopBlock;  //  set  time in blocks, optimal
    unsigned short startBlock ; // actual start time in blocks
    unsigned short stopBlock;  // actual stop time in blocks, may be < startBlock if
                               // crossing a day boundary
    unsigned short transitBlock;  // transit time in blocks
    bool  crossesDayBoundary; // true if lstStart > lstStop
    bool  flexible; // schedule flexibly?
    float allocatedHrs; // number hours allocated by TAC
    unsigned short allocatedBlocks; // number blocks allocated by TAC
    float minHrs;       // minimum acceptable hours
    float assignedHrs;  // number hours assigned by this program
    unsigned short assignedBlocks;  // number blocks assigned so far
    bool  success; // fully scheduled
    bool  partial; // partially scheduled
    string rawRow; // unprocessed table row because Douglas wants 
                   // it printed back out!
    float sunLimit; // The Sun-source angular separation limit, in degrees
    Ephemeris * pEphem;

private:
    // all the passes for this Project.
    vector< Pass > passes_;
    // map of institutional codes. yeah, this should be global but it's tiny.
    map< string, string > instCode_;
    /// create institutional code map
    void makeInstMap( void );

    // For use when we allow multiple bands per invocation:
    // map of bands vs solar limit angles (again could be global but it is small).
    std::map<string,double> sunLimitMap_;

    /**
     * Compute the angular separation between the Sun
     * and the source at the start of a given chunk of ime
     * @param chunk The current chunk
     * @param mjd the MJD at LST=0 of the Day in which this chunk resides.
     * @return the angular separation in degrees
     */
    double solarAngle( const Chunk & chunk, double mjd );

};

// default constructor
Project::Project() 
{ 
    name        = "none";
    institution = "none";
    source      = "none";
    obsblockId  = "none";
    config      = "X";
    grade    = 0.0;
    lstStart = 0.0;
    lstStop  = 0.0;
    upTime    = 0.0;
    obsblock = 0;
    ra = 0.0;
    dec = 0.0;
    LSTstartBlock = 0;
    LSTstopBlock  = 0;
    startBlock = 0;
    stopBlock  = 0;
    transitBlock = 0;
    crossesDayBoundary = false;
    flexible  = false;
    allocatedHrs = 0.0;
    allocatedBlocks = static_cast<ushort>((allocatedHrs * 60/BLOCK_SIZE));
    minHrs       = 0.0;
    assignedHrs  = 0.0;
    assignedBlocks = 0;
    success = false;
    partial = false;
    makeInstMap();
}

// destructor
Project::~Project() { }

bool 
Project::hasUnassignedBlocks() const
{
    return ( allocatedBlocks > assignedBlocks );
}

string 
Project::disambiguatedName() 
{
    if ( name == "none") return name;

    string dname = scriptName;
    // erase ending .obs
    const size_t len = dname.length();
    const size_t loc = dname.rfind('.');
    dname.erase(loc,len);
    return dname;
    /**
    ostringstream nameOs;
    nameOs << name
       << "[" << obsblock << "] ("
       << getInstCode()
       << ")";
    return nameOs.str();
    */
}

void
Project::print() 
{
    cout << "----------------------------------" << endl
         << "Project " << disambiguatedName()
         << setprecision(2)
         << " obsblockId " << obsblockId
         << " script " << scriptName
         << " grade: " << grade
         << " allocated hrs: " << allocatedHrs 
         << " scheduled hrs: " << assignedHrs
         << " allocBlocks: " << allocatedBlocks
         << " assignedBlocks: " << assignedBlocks 
         << endl
         << " startBlock: " << startBlock
         << " transitBlock: " << transitBlock
         << " stopBlock: " << stopBlock
         << " LSTstartBlock: " << LSTstartBlock
         << " LSTstopBlock: " << LSTstopBlock
         << " LSTstart(h): " << lstStart
         << " LSTstop(h): " << lstStop
         << " crosses day:  " << boolalpha << crossesDayBoundary
         << " uptime hrs:  " << upTime
         << " numpasses():  " << numPasses()
         << endl;

    for(ushort i = 0 ;i< numPasses(); i++ ) {

        ushort clen = passes_[i].chunk.end-passes_[i].chunk.start;
        float flen = static_cast<float>(clen);
        float blen = static_cast<float>(BLOCKS_PER_HOUR);
        float ctime = flen/blen;
        cout << "Pass " << i 
             << " on day " << passes_[i].day
             << " has " <<  clen 
             << " blocks, equalling " << ctime << " hours"
             << endl;
    }
    cout << "----------------------------------" << endl;
}

void
Project::stats()  
{
    // Name  Pass  Day#  StartLST  StopLST StartEl StopEl HrsAlloc HrsSched
    const string spacer("    ");
    const ushort np = numPasses();

    SourceChecker checker;
    Source s( disambiguatedName(),
              Angle(ra,"radians"),
              Angle(dec, "radians"),
              Velocity(0.0,"km/s"),
              Angle(0.0,"radians")
            );
    checker.setSource( s );

    float hoursSoFar = 0.0;
    // note if numpasses=0, then no stats are printed. This is by design.
    //double now = startMJD;
    //if  ( startMJD > 0.0 ) {
    double now = Time::MJD();
    //}
    checker.setMJD(now);
    for(ushort i = 0 ;i< np; i++ ) {
        float hoursThisPass = 0.0;
        float startLst = getStartLst( passes_[i].chunk );
        float stopLst  = getStopLst( passes_[i].chunk );
        // find the starting and stopping elevation.
        // Brute force way is to just loop over range of
        // mjds and find the lst that match stop/start,
        // then grab elevation from SourceChecker.
        float startEl = -1;  
        float stopEl = -1;
        for ( double mjd = now; mjd< now+1.0; mjd+=0.005 ) {
            checker.setMJD( mjd );
            double lst = checker.lst();
            // if within 10 minutes, take it.
            if ( fabs( lst - startLst ) < .16 )
                startEl = checker.getElevation().degrees();

            // if within 10 minutes, take it.
            if ( fabs( lst - stopLst ) < .16 )
                stopEl= checker.getElevation().degrees();
        }

        if ( stopLst < startLst )
            hoursThisPass = (24.0 + stopLst - startLst);
        else
            hoursThisPass = (stopLst - startLst);

        hoursSoFar += hoursThisPass;

        cout << disambiguatedName() 
             << spacer << i 
             << setprecision(2)
             << spacer
             << spacer <<  passes_[i].day
             << spacer <<  startLst
             << spacer <<  stopLst
             << spacer <<  startEl
             << spacer <<  stopEl
             << spacer
             << spacer <<  hoursThisPass
             << spacer
             << spacer
             << spacer <<  hoursSoFar
             << spacer
             << spacer
             << spacer <<  allocatedHrs
             << endl;
    }
}

void 
Project::addPass(unsigned short dayNumber, const Chunk & chunk)
{
    Pass pass;
    pass.chunk = chunk;
    pass.day   = dayNumber;
    passes_.push_back( pass );
}

unsigned short 
Project::numPasses() const {
    return passes_.size();
}

void
Project::makeInstMap() {
    if ( instCode_.empty() ) 
    {
        instCode_.insert( make_pair( "CIT", CIT ) );
        instCode_.insert( make_pair( "UCB", UCB     ) );
        instCode_.insert( make_pair( "UIUC", UIUC    ) );
        instCode_.insert( make_pair( "UMD", UMD     ) );
        instCode_.insert( make_pair( "CARMA", CARMA ) );
        instCode_.insert( make_pair( "CHICAGO", CHICAGO) );
        instCode_.insert( make_pair( "VISITOR", VISITOR ) );
    }
}

std::string 
Project::getInstCode() 
{
    const string unknown("U");
    const string key 
        = StringUtils::lowASCIIAlphaNumericToUpper( institution );
    if ( instCode_.find( key ) == instCode_.end() )
        return unknown;
    else 
        return instCode_[key];
}


unsigned short
Project::blockLen()
{
    if ( crossesDayBoundary )
        return ( BLOCKS_PER_DAY + stopBlock - startBlock ) ;
    else
        return ( stopBlock - startBlock ) ;
}

double
Project::solarAngle(const Chunk & c, double mjd) {
    double lst = getStartLst(c);
    //cout << " LST=" << lst << " MJD=" << mjd << endl;
    // add the LST of the chunk start to the MJD@LST=0 
    //cout << "setting mjd to " << mjd+lst/24.0 <<endl;
    pEphem->setMJD(mjd+lst/24.0);
    // get az,el of the sun
    //cout << "setting source to SUN" <<endl;
    pEphem->setSource(SUN);
    //cout << "getaz" <<endl;
    //double az0 = checker.getAzimuth().radians();
    //cout << "getel" <<endl;
    //double el0 = checker.getElevation().radians();
    double az0 = pEphem->getAz();
    double el0 = pEphem->getEl();

    // get az,el of the source
    Angle srcRA ( ra, "radians" );
    Angle srcDec( dec, "radians") ;
    //cout << "setting source to " <<name <<endl;
    Source s(name, srcRA, srcDec, Velocity(0.0,"km/s"), Angle(0.0,"radians"));
    pEphem->setSource( s );
    //double az1 = checker.getAzimuth().radians();
    //double el1 = checker.getElevation().radians();
    double az1 = pEphem->getAz();
    double el1 = pEphem->getEl();

    // compute angular separation
    double cosd = sin(el0)*sin(el1) + cos(el0)*cos(el1)*cos(az0-az1);
    return ( acos(cosd) * 180.0/M_PI );
}

bool
Project::solarAngleIsGood(const Chunk & c, double mjd ) 
{
    double sa = solarAngle(c, mjd);
    if ( sa >= sunLimit ) return true;

    if ( verbose >= OUTPUT_DEBUG ) {
        cout << disambiguatedName() 
             << " is too close to the Sun to schedule on " << Time::getDateString(mjd)
             << ". Solar angle = "
             << setprecision(2) << setiosflags(ios::fixed) << sa
             << " degrees"
             << endl;
    }
    return false;
}


/**
 * Used to sort Projects by grade (priority) value
 */
class PriorityCmp {
    public:
        bool operator() (const Project & p, const Project & q) const;
};

bool PriorityCmp::operator() (const Project & p, const Project & q) const
{
    return ( p.grade > q.grade );
}

/** 
 * Class describing a single 24 hr day, broken up
 * into 15 minute schedulable slots.
 */
class Day {
public:
    bool blockAvailable[ BLOCKS_PER_DAY ];

    // default constructor 
    Day();

    // destructor
    ~Day();

    /** 
     * Try to fit a project or part in this day.
     * @param p Input project, may be modified so non-const
     * @param chunkSize if > 0, try to schedule a chunk of size
     * chunkSize if the full project won't fit.
     * @param mode scheduling mode
     * If a chunk around transit can't be found, a chunk
     * anywhere in the project LST range is tried.
     * @return true if successful, fall otherwise
     *
     */
    bool tryToFit( Project & p, 
                   const unsigned short chunkSize, 
                   const modeType mode  );

    /**
     * @return true if at last one project has been scheduled
     * on this Day
     */
    bool isUsed();

    /**
     * Mark a range of contiguous 15 minute time slots as 
     * available (unscheduled), inclusive range
     * @param  begin Starting slot 0 thru 95
     * @param  end   Ending slot 0 thru 95*2.  If end < begin,
     * or end > 95 then the marks will cross the day boundary to the
     * next day.
     */
    void markAsAvailable(const unsigned short begin, 
                         const unsigned short end);

    /**
     * Mark a range of contiguous 15 minute time slots as 
     * unavailable (scheduled), inclusive range
     * @param  begin Starting slot 0 thru 95
     * @param  end   Ending slot 0 thru 95*2.  If end < begin,
     * or end > 95 then the marks will cross the day boundary to the
     * next day.
     * @param  project The Project taking this slot
     */
    void markAsUnavailable(const unsigned short begin, 
                           const unsigned short end,
                           Project & project );

    /**
     * Mark a range of contiguous 15 minute time slots as 
     * unavailable (scheduled), inclusive range
     * @param  begin Starting slot 0 thru 95
     * @param  end   Ending slot 0 thru 95*2.  If end < begin,
     * or end > 95 then the marks will cross the day boundary to the
     * next day.
     * @param  name  The name of the project taking this slot
     * @param instcode Institutional code
     */
    void markAsUnavailable(const unsigned short begin, 
                           const unsigned short end,
                           const std::string & name,
                           const std::string & instCode);

    /**
     * Expand a project chunk to fill space on either side, if possible.
     * First tries a balanced expansion -- with same amount on
     * either side of transit.  If that fails it will try unbalanced.
     * @param  p The Project taking this slot
     * @param  c the input Chunk to try to expand
     * @param  maxBlocks the maximum number of blocks to expand this
     *         chunk.
     * @return true if Chunk was expanded.
     */
    bool expandIfPossible( Project & p, Chunk & c, const unsigned short maxBlocks);

    /*
     * Used to pad out a Chunk
     * @param start  c The Chunk to search BEFORe. The search starts at c.start
     * and proceeds backwards in time.
     * @param maxSize -- the maxSize space to look for.
     * @return the new end of the expanded Chunk.
     */
    unsigned short maxChunkEndingAt( const Chunk & c,
                                     const unsigned short maxSize );

    /**
     * Used to pad out a Chunk
     * @param start  c The Chunk to search AFTER. The search starts at c.end
     * and proceeds forwards in time.
     * @param maxSize -- the maxSize space to look for.
     * @return the new start of the expanded Chunk.
     */
    unsigned short maxChunkStartingAt( const Chunk & c,
                                       const unsigned short maxSize );

    /**
     * Mark Chunk c as unavailable in project P and
     * update the project's assigned data.
     * @param  c the input Chunk
     * @param  p The Project taking this slot
     */
    void fillItIn( const Chunk & c, Project & p );

    /**
     * @return true if the entire block is available,
     * inclusive range 
     * @param  begin Starting slot 0 thru 95
     * @param  end   Ending slot 0 thru 95*2.  
     * If end < begin || end > 95,
     * then the check will cross the day boundary to the
     * next day.
     */
    bool contiguouslyAvailable( const unsigned short begin,
                                const unsigned short end);

    /**
     * See if there is a chunk of size chunkSize
     * available anywhere on this Day.
     * @param  begin Starting slot 0 thru 95
     * @param  chunk size to look for, in blocks
     * @param  end   Ending slot 0 thru 95.  If end < begin,
     * then the check will cross the day boundary to the
     * next day.
     * @return A Chunk with Chunk.good == true is returned if
     * one is found, otherwise a Chunk with chunk.good==false
     * is returned
     */
    Chunk chunkAvailable( const unsigned short begin,
                          const unsigned short end,
                          const unsigned short chunkSize);

    /**
     * @return the Project name that is assigned to
     * the input block. "none" is returned if there 
     * is no project assigned.
     * @param The input block, 0 to BLOCKS_PER_DAY-1
     */
    std::string getName(unsigned short block);

    /** 
     * @return institutional code assigned to input block
     * @param The input block, 0 to BLOCKS_PER_DAY-1
     */
    std::string getInstCode(unsigned short block);

    /**
     * @return string representation of this day's blocks
     * @param showUnscheduled True if also want to list  unscheduled ("none") blocks.
     */
    std::string print( bool showUnscheduled = false );

    /**
     * @return a representation of this Day as a CARMA-style Table
     * @param showUnscheduled True if also want to list  unscheduled ("none") blocks.
     */
    carma::services::Table asTable( bool showUnscheduled = false );

    std::string asQueue( const std::string & subarray );

    /**
     * The number of this Day. A running count in the Calendar
     */
    unsigned short number;

    /**
     * The MJD of this day computed from startMJD of Calendar
     */
    double mjd;

    /**
     * An iterator pointing to the nextDay in the Calendar
     */
    std::list<Day>::iterator nextDay;

    /** set the iterator to magic value indicating this is the
     * last day of Calendar
     */
    void noNextDay();

private:
    std::string projectMap_[ BLOCKS_PER_DAY ]; 
    // institutional map
    std::string instMap_[ BLOCKS_PER_DAY ]; 
    bool used_;
    std::list<Day> kNullList_;
};

Day::Day() 
    : nextDay( kNullList_.begin() ), used_(false)
{
    try {
        CARMA_CPTRACE(Trace::TRACE3, "In DAY CONSTRUCTOR");
        markAsAvailable( 0, BLOCKS_PER_DAY-1 );
    } catch ( ... ) {
        throw CARMA_EXCEPTION( ErrorException, 
            "Unclassified exception in Day contructor");
    }
}

Day::~Day() 
{ 
}

void
Day::noNextDay() {
    nextDay = kNullList_.begin();
}

std::string
Day::print( bool showUnscheduled )
{
    ostringstream os;
    os << asTable( showUnscheduled );
    return os.str();
}

std::string
Day::asQueue(const std::string & subarray) 
{
   ostringstream os;
   string prevname = "XHASHDHA";
   string prevStop ="00:00";
   for (int i=1 ; i<BLOCKS_PER_DAY; i++) {
     string name = getName(i);
     if ( prevname != name ) {
         double lststop = 0;
         int k=i+1;
         for (k=i+1; k<BLOCKS_PER_DAY;k++) {
             string nextName = getName(k);
             if ( nextName != name ) {
                 lststop = getLst(k-1);
                 i=k;
                 break;
             }
         }
         if ( k!=BLOCKS_PER_DAY ) {
             string lstop = HourAngle::hms(lststop,0);

             string currStop = lstop.substr(0,lstop.size()-3);
             // If a "none" block, ask observers to manually schedule something
             if ( StringUtils::equalsIgnoreCase(name,"none")  ) {
                 os << "# *** WARNING: No science scripts found for "<< subarray 
                    << " between LST "<<prevStop << " and " << currStop <<"."
                    << endl
                    << "# *** Please schedule array health or other until LST "
                    << currStop
                    //<< " ("<<prevname<<","<<name<<")"
                    << endl;
             } else {
                 os << subarray 
                    << "  "
                    << setw(20)
                    << name 
                    << "  endtrack='"
                    << currStop
                    << "' "
                    //<< " ("<<prevname<<","<<name<<")"
                    << endl;
             }
             prevStop = currStop;
             prevname = name;
         } else {
             // This covers a "none" block that ends at LST 24.
             string lstop = HourAngle::hms(lststop,0);
             string currStop = lstop.substr(0,lstop.size()-3);
             if ( StringUtils::equalsIgnoreCase(name,"none")  ) {
                 os << "# *** WARNING: No science scripts found for "<< subarray 
                    << " between LST "<<prevStop << " and " << currStop <<"."
                    << endl
                    << "# *** Please schedule array health or other until LST "
                    << currStop
                    //<< " ("<<prevname<<","<<name<<")"
                    << endl;
                  i=BLOCKS_PER_DAY;
             }
         }
    }
   } // for
   const string q = os.str();
   //cout << q << endl;
   return q;

}

carma::services::Table Day::asTable( bool showUnscheduled )
{
    // write it in CARMA Table format
    Table t(5);
    string prevname = "XHASHDHA";
    for (int i=1 ; i<BLOCKS_PER_DAY; i++) {
         ostringstream os;
         string name = getName(i);
         string icode = getInstCode(i);
     if ( StringUtils::equalsIgnoreCase(name,"none") 
          && !showUnscheduled ) 
         {
                 prevname = name;
                 continue;
         }
     if ( prevname != name ) {

//BUG BUG BUG. If showUnscheduled == false then
//fixed_blocks LST_START is written wrong (off by -1 block/-15 minutes LST)
//This is a kludgy fix, as I cannot find where the actual logic error is.
             if ( StringUtils::equalsIgnoreCase(prevname,"none") && !showUnscheduled) i++;

             float lststart = getLst(i-1);
             os << setw(20) << name 
                << "       " 
                << icode
                << "       " 
                << setw(2) << setfill(' ')
                << number
                << "       " 
                << setw(5) << setfill(' ') 
                << setprecision(2) << setiosflags(ios::fixed) << lststart
                << "       " 
                ;
             int k=i+1;
             for (k=i+1; k<BLOCKS_PER_DAY;k++) {
                 string nextName = getName(k);
                 if ( nextName != name ) {
                     float lststop = getLst(k-1);
                     os << setw(5) 
                            << setfill(' ') 
                    << setprecision(2) 
                    << setiosflags(ios::fixed) << lststop;
                     i=k;
                     break;
                 }
             }
             if ( k==BLOCKS_PER_DAY ) os << "23.99";
             t.addRow( os.str() );
             prevname = name;
         }
    }

    return t;

}

void 
Day::markAsAvailable(const unsigned short begin, 
                     const unsigned short end) 
{
    // throw exception if inputs are bad
    /*
    if ( begin == end )  {
    ostringstream os;
    os << "markAsAvailable: bad range ["
        << begin << ","
        << end << "] ";
    throw CARMA_EXCEPTION(ErrorException,  os.str());
    }*/

    if ( begin > BLOCKS_PER_DAY-1  
         || end > 2*(BLOCKS_PER_DAY-1) ) {
        ostringstream os;
        os << "markAsAvailable: range out of bounds ["
            << begin << ","
            << end << "] ";
        throw CARMA_EXCEPTION(ErrorException,  os.str());
    }

    if ( end >= begin && end < BLOCKS_PER_DAY )  {
        for(unsigned short i = begin ; i <= end ; i++) {
            blockAvailable[i] = true;
            // place "none" in the project map at this location
            projectMap_[i] = "none";
            // place space in institutional map at this location
            instMap_[i] = " ";
        }
    } else {
        CARMA_CPTRACE(Trace::TRACE3,
         "markAsAvailable: crossing day boundary ["
            << begin << ","
            << end << "] on Day "
            << number
            );
        // interpret as crossing a day boundary.
        unsigned short stop = end;
        if ( end > BLOCKS_PER_DAY - 1 ) 
            stop -= BLOCKS_PER_DAY;
        this->markAsAvailable( begin, BLOCKS_PER_DAY - 1 );
        if ( nextDay != kNullList_.begin() ) 
            nextDay->markAsAvailable( 0, stop );
        }

        CARMA_CPTRACE(Trace::TRACE3,
        "markAsAvailable inserted project NONE"
            << " between " << begin << " and " << end);
}

void 
Day::markAsUnavailable(const unsigned short begin, 
                       const unsigned short end,
                       Project & project) 
{
    markAsUnavailable( begin, end, 
                       project.disambiguatedName(), 
                       project.getInstCode() 
                     );
}

void 
Day::markAsUnavailable(const unsigned short begin, 
                       const unsigned short end,
                       const std::string & name,
                       const std::string & instCode ) 
{
    // throw exception if inputs are bad
    /*
    if ( begin == end ) {
    ostringstream os;
    os << "markAsUnavailable: bad range ["
        << begin << ","
        << end << "] ";
    throw CARMA_EXCEPTION(ErrorException,  os.str());
    }*/


    if ( begin > BLOCKS_PER_DAY-1  
         || end > 2*(BLOCKS_PER_DAY-1) ) {
        ostringstream os;
        os << "markAsUnAvailable: range out of bounds ["
            << begin << ","
            << end << "] "
            << " Project " 
            << name;
        throw CARMA_EXCEPTION(ErrorException,  os.str());
    }

    if ( end >= begin && end < BLOCKS_PER_DAY )  {
        for(unsigned short i = begin ; i <= end ; i++) {
            blockAvailable[i] = false;
            // mark the beginning location with a project
            projectMap_[i] = name;
            instMap_[i] = instCode;
        }
    } else {
        CARMA_CPTRACE(Trace::TRACE3,
         "markAsAvailable: crossing day boundary ["
            << begin << ","
            << end << "] on Day "
            << number
            << " Project " 
            << name;
            );
        // interpret as crossing a day boundary.
        unsigned short stop = end;
        if ( end > BLOCKS_PER_DAY - 1 ) 
            stop -= BLOCKS_PER_DAY;

        this->markAsUnavailable( begin, BLOCKS_PER_DAY - 1, name, instCode );
        if ( nextDay != kNullList_.begin() ) 
            nextDay->markAsUnavailable( 0, stop, name, instCode );
    }

    CARMA_CPTRACE(Trace::TRACE3,
        "inserted project " << getName(begin)
        << "[" << name << "]"
        << " between " << begin << " and " << end
        << " on Day " << number
        );

    used_ = true;
}

void 
Day::fillItIn( const Chunk & c, Project & p ) 
{
    ostringstream os;
    os << "Day::fillItIn("<<p.disambiguatedName()<<") Day #" 
        << number <<" - ";

    ScopedLogNdc( os.str() );
    markAsUnavailable( c.start, c.end, p );
    p.assignedBlocks += (c.end - c.start);
    CARMA_CPTRACE(Trace::TRACE3, " assignedBlocks = " 
         << p.assignedBlocks
         << " start " << c.start
         << " end   " << c.end
         << " remaining " << p.allocatedBlocks - p.assignedBlocks
         );
    p.assignedHrs = static_cast<float>(p.assignedBlocks*BLOCK_SIZE/60.0);
    p.addPass(this->number, c);
}

unsigned short
Day::maxChunkStartingAt( const Chunk & c, const unsigned short maxBlocks )
{
    bool found = false;
    unsigned short i = 1;
    for ( i = 1; i <= maxBlocks ; i++ ) {
        found = contiguouslyAvailable( c.end, c.end+i );
        if ( !found ) return (c.end+i-1);
    }

    // if maxBlocks =0, then we return c.end
    return c.end+i-1;
}


unsigned short
Day::maxChunkEndingAt( const Chunk & c, const unsigned short maxBlocks )
{

    unsigned short start = c.start;
    bool found = false;
    for ( unsigned short i = 1; i <= maxBlocks; i++ ) {
         short signedStart = c.start - i;
         if ( signedStart < 0 ) {
             ostringstream os;
             os << "Day::maxChunkEndingAt() - "
            << "On day #"<< number
            << ": I can't deal with backwards crossing of day boundaries.";
             throw CARMA_EXCEPTION( ErrorException, os.str() );
         }
         start = signedStart;
         found = contiguouslyAvailable( start, c.start );
         if ( !found ) return start+1;
    }

    // if maxBlocks =0, then we return c.start
    return start;
}

bool 
Day::expandIfPossible( Project & p, Chunk & c, unsigned short maxBlocks )
{
    // We already know that the Chunk c was available, so just look
    // for blocks before and after it.
    ostringstream os ;
    os  << "Day::expandIfPossible("
        << p.name 
        << "["
        << c.start 
        << "," 
        << c.end
        << "],"
        << maxBlocks
        << ") - "
        ;

    ScopedLogNdc ndc( os.str() );
    CARMA_CPTRACE(Trace::TRACE3," Entering ");

    bool expandSuccess = false;

    // if maxBlocks is odd, increase it by one to 
    // make addition easier.
    float fHalfBlocks = static_cast<float>(maxBlocks)/2.0;
    unsigned short halfBlocks = maxBlocks/2;
    bool subtractOneBlock = false;
    if ( fHalfBlocks != halfBlocks ) {
        maxBlocks++;
        halfBlocks = maxBlocks/2;
        subtractOneBlock = true;
        CARMA_CPTRACE(Trace::TRACE3, 
            " Incremented maxBlocks by 1; halfBlocks = " << halfBlocks);
    }


    unsigned short newEnd   = maxChunkStartingAt( c, maxBlocks );
    unsigned short newStart = maxChunkEndingAt( c, maxBlocks );

    if ( ( newEnd != c.end ) || ( newStart != c.start ) )
        expandSuccess = true;

    unsigned short blocksBefore  = ( c.start - newStart );
    unsigned short blocksAfter =  ( newEnd - c.end );
    unsigned short additionalBlocksFound = blocksBefore + blocksAfter;

    if ( additionalBlocksFound <= maxBlocks ) {
        // we didn't find all we needed to just set
        // the input chunk's new start and end points.
        c.start = newStart;
        c.end   = newEnd;
        CARMA_CPTRACE(Trace::TRACE3,"Did a balanced expansion on " << p.name);
    } else {
        // we found more than we needed. attempt to
        // balance before and after.
        short diffBlocksBefore = blocksBefore - halfBlocks;
        short diffBlocksAfter  = blocksAfter  - halfBlocks;

        if ( blocksBefore < halfBlocks && blocksAfter < halfBlocks )
            throw CARMA_EXCEPTION(ErrorException, "That's unpossible!");

        if ( diffBlocksBefore >= halfBlocks && diffBlocksAfter >= halfBlocks ) {
            c.start -= halfBlocks;
            c.end   += halfBlocks;
            // If we incremented maxBlocks to get an
            // even number, now take the extra block 
            // off the end.
            if ( subtractOneBlock ) c.end--;
        } else {
            if ( blocksBefore < halfBlocks ) {
            c.start  = newStart;
            c.end   += blocksBefore;
            // If we incremented maxBlocks to get an
            // even number, now take the extra block 
            // off the longer side.
                if ( subtractOneBlock ) c.start++;
            if ( c.end > newEnd ) 
                throw CARMA_EXCEPTION(ErrorException, "DRAT!");
            }
            if ( blocksAfter < halfBlocks ) {
            c.end   = newEnd;
            c.start -= blocksAfter;
            // If we incremented maxBlocks to get an
            // even number, now take the extra block 
            // off the longer side.
                if ( subtractOneBlock ) c.end--;
            if ( c.start < newStart )
                throw CARMA_EXCEPTION(ErrorException, "SPLAT!");
            }
        }


        CARMA_CPTRACE(Trace::TRACE3,"Did a unbalanced expansion on " << p.name);
    }

    CARMA_CPTRACE( Trace::TRACE3," Returning " << boolalpha << expandSuccess );

    return expandSuccess;

}

bool 
Day::contiguouslyAvailable( const unsigned short begin,
                            const unsigned short end )
{
    ostringstream nos;
    nos << "Day::contiguouslyAvailable(): day #"<<number << " - ";
    const string method( nos.str() );
    ScopedLogNdc ndc( method );
    CARMA_CPTRACE(Trace::TRACE3,"Entering. Range ["<<begin<<","<<end<<"]");

    /*
    if ( begin == end ) {
        ostringstream os;
        os << "contiguouslyAvailable: bad range ["
            << begin << ","
            << end << "] ";
        throw CARMA_EXCEPTION(ErrorException,  os.str());
    }*/

    if ( end > 2*(BLOCKS_PER_DAY-1) ) {
        ostringstream os;
        os  << method
            << "range out of bounds ["
            << begin << ","
            << end << "] ";
        throw CARMA_EXCEPTION(ErrorException,  os.str());
    }

    if ( begin >= BLOCKS_PER_DAY ) {
        if ( end >= begin && nextDay != kNullList_.begin() ) {
            return nextDay->contiguouslyAvailable( begin-BLOCKS_PER_DAY, 
                                               end-BLOCKS_PER_DAY );
        }
        else {
            ostringstream os;
            os  << method
            << "range out of bounds ["
            << begin << ","
            << end << "] ";
            throw CARMA_EXCEPTION(ErrorException,  os.str());
        }
    }

    if ( end >= begin && end < BLOCKS_PER_DAY )  {
        for(unsigned short i = begin ; i <= end ; i++) {
            if (blockAvailable[i] == false ) {
            CARMA_CPTRACE(Trace::TRACE3,"Returning false.");
            return false;
            }
        }
    } else {
        if ( nextDay == kNullList_.begin() )  {
            ostringstream os;
            os << "Nextday is null on Day " << number
               << ". Returning false.";
            CARMA_CPTRACE(Trace::TRACE3, os.str() );
            return false;
        }
            // interpret as crossing a day boundary.
        unsigned short stop = end;
            if ( end > BLOCKS_PER_DAY - 1 ) 
            stop-= BLOCKS_PER_DAY ;
        /*
            cout << "DOING RECURSIVE CALL [" 
             <<begin 
             <<","
             << BLOCKS_PER_DAY-1
             << "] && nextDay [0,"
             << stop 
             << "]"
             << endl;
             */

        // nextDay will be defined here so ok to call it's method
        return (   contiguouslyAvailable( begin, BLOCKS_PER_DAY-1 ) 
                && nextDay->contiguouslyAvailable( 0, stop ) 
               );
    }

    CARMA_CPTRACE(Trace::TRACE3,"Returning true.");
    return true;
}

Chunk 
Day::chunkAvailable( const unsigned short begin,
                     const unsigned short end,
                     const unsigned short chunkSize) 
{
    const string method("Day::chunkAvailable - ");
    ScopedLogNdc ndc( method );
    if ( end >= BLOCKS_PER_DAY ) {
        ostringstream os;
        os << method
            << "requested end value too large ["
            << end << "] on Day " << number ;
        throw CARMA_EXCEPTION(ErrorException,  os.str());
    }

    /*
    if ( begin == end ) {
    ostringstream os;
    os << "chunkAvailable: bad range ["
        << begin 
        << ","
        << end 
        << "] ";
    throw CARMA_EXCEPTION(ErrorException,  os.str());
    }
    */


    Chunk chunk;
    chunk.start = 0;
    chunk.end  = 0;
    chunk.good  = false;

    // Note the range in which to search can cross the
    // day boundary while the found range may not.
    bool rangeCross = ( end < begin );
    /*
    cout << method 
        << "requested size "
        << chunkSize 
        << " in range ["
        << begin 
        << ","
        << end 
        << "] on Day " << number 
        << " cross = " << boolalpha << cross;
    */
    ostringstream os;
    os  << "requested size "
        << chunkSize 
        << " in range ["
        << begin 
        << ","
        << end 
        << "] on Day " << number 
        << " range cross = " << boolalpha << rangeCross;
    // We need the loop control variable to be monotonically increasing
    unsigned short loopStop = ( rangeCross ) ? end+BLOCKS_PER_DAY : end;

    if ( chunkSize > ( loopStop - begin ) )  {
        os << " was not available because chunk is bigger than input range.";
    //cout << foo << endl;
        CARMA_CPTRACE(Trace::TRACE3, os.str() );
        return chunk;
    }



    // Slide a chunk of the requested size over the
    // requested range and see if we find a spot.
    for(unsigned short i = begin ; i < loopStop; i++) {
        unsigned short startChunk = i;
        unsigned short endChunk = startChunk+chunkSize;
        if ( endChunk > loopStop || startChunk == BLOCKS_PER_DAY ) {
            // we've run out of space
            os << " was not available. [" << endChunk <<">" << loopStop << "]";
                CARMA_CPTRACE(Trace::TRACE3, os.str() );
            return chunk;
        }

        // endChunk will be passed to contiguouslyAvaiable,
        // which interprets end < start as crossing
        // a day boundary.  So subtract off appropriately.
        /*
        cout << " CALLING IN LOOP "
             << " begin = "<< begin
             << " loopStop = " << loopStop
             << " range = ["
             << startChunk
             << "," << endChunk
             << "]"
             <<endl;
             */
        if ( endChunk > BLOCKS_PER_DAY-1 )
             endChunk -=  BLOCKS_PER_DAY ;
            // Is resultant chunk good?
        bool good = contiguouslyAvailable(startChunk,endChunk);
        CARMA_CPTRACE(Trace::TRACE3, "Calling contig("<<startChunk<<","<<endChunk<<") returned "<<boolalpha<<good);

        if ( good ) {
            // we found chunkSize contiguous blocks;
            chunk.start = startChunk;
            chunk.end   = endChunk;
            // If the found range crosses the day boundary
            // increase the end location by one day.
            if ( chunk.end < chunk.start ) 
                chunk.end += BLOCKS_PER_DAY;
            
            chunk.good  = true; 
            os << " was available. 1" ;
                CARMA_CPTRACE(Trace::TRACE3, os.str() );

            return chunk;
        }
    }

    if ( chunk.good ) 
        os << " was available. 2" ;
    else 
        os << " was not available. 3";

    CARMA_CPTRACE(Trace::TRACE3, os.str() );
    return chunk;
}

bool 
Day::isUsed() {
    return used_;
}

bool
Day::tryToFit( Project & p, const unsigned short chunkSize, modeType mode )
{
    ostringstream nos;
    nos << "Day::tryToFit(" << p.name << "): day #"<<number;
    ScopedLogNdc ndc( nos.str() );
    CARMA_CPTRACE(Trace::TRACE6,"Entering");

    // The (policy) length we may try to schedule.
    unsigned short blockLen = p.blockLen();
    // The length that is left to schedule in this project.
    unsigned short blocksLeft = (p.allocatedBlocks - p.assignedBlocks);

    CARMA_CPTRACE(Trace::TRACE3," max block length to attempt this pass = " 
            << blockLen 
            << " blocks remaining to schedule = " << blocksLeft 
            << " assigned so far = " << p.assignedBlocks
            << " allocated = " << p.allocatedBlocks );


    // Pad out to chunkSize if smaller than chunkSize,
    // but don't bother to try to pad it if
    // it is smaller than the minimum.
    // Note if blocksLeft is less than chunkSize
    // then we have already tried to expand the previous
    // track of this project and failed  OR this is
    // the first time through and this track was allocated
    // a very small chunk. 
    if ( blocksLeft < MINBLOCKS_TO_SCHEDULE )  {
        if ( verbose >= OUTPUT_DEBUG ) {
            cout << " Will not schedule <1hr chunk of "
             << p.disambiguatedName()
             << endl;
        }
        return false;
    }

    if ( blocksLeft < chunkSize ) 
    {
        CARMA_CPTRACE( Trace::TRACE3,
                   "Padding out blocks remaining to chunksize." );
        blocksLeft = chunkSize ;
    }

    // What will remain to allocated if we manage to schedule the
    // requested blocksLen track.  Note this is unsigned, so 
    // if blockLen > blocksLeft then blocksRemaining will 
    // be much greater than MINBLOCKS_TO_SCHEDULE, so the
    // if-test below will correctly be false.
    unsigned short blocksRemainingAfterSched = blocksLeft - blockLen;


    // do we desire to try and schedule a full sidereal pass?
    bool needsFullTrack = ( blockLen <= blocksLeft );

    // try to schedule the extra bit here, if possible
    bool isExpandable = false;
    float optDiff     = p.optimalLength - p.policyLength;
    if (   blocksRemainingAfterSched < MINBLOCKS_TO_SCHEDULE 
        && blocksRemainingAfterSched > 0 ) {
            if ( optDiff >= MINHOURS_TO_SCHEDULE ) {
            isExpandable = true;
            if ( verbose >= OUTPUT_DEBUG ) {
             cout << p.disambiguatedName() 
                  << " can be expanded." << endl;
            }
        } else { 
            if ( verbose >= OUTPUT_DEBUG ) {
             cout <<p.disambiguatedName() 
              << " cannot be expanded because expansion would go beyond"
              << " optimal track length."
              << endl;
            }
        }
    }

    // first try the whole thing
    if ( needsFullTrack ) {
        CARMA_CPTRACE(Trace::TRACE3,"trying transit centered full track...");
        // note since blockLen == p.stopBlock-p.startBlock, this is
        // a transit-centered chunk.  Same as policyAvaiable contig call
        // above.
        Chunk c = chunkAvailable(p.startBlock, p.stopBlock, blockLen);
        const unsigned short startSize = c.size();
        if ( ! p.solarAngleIsGood(c,mjd) ) {
            c.good = false;
        }
        if ( c.good ) {
            bool expandSuccess = false;
            if ( isExpandable && userExpand ) {
                expandSuccess = expandIfPossible( p, c, blocksRemainingAfterSched );
            }

            {
                ostringstream tos;
                tos << "Day " << number
                    << " had full policy range "
                    << " for " << p.disambiguatedName()
                    << " LST "
                    << p.policyStart 
                    << " ["
                    << p.startBlock
                    << "] to "
                    << p.policyStop 
                    << " ["
                    << p.stopBlock
                    << "] available ";
                if ( expandSuccess ) {
                    tos << "and was expanded by "
                    << c.size() - startSize
                    << " blocks.";
                } else {
                    tos << "and was NOT expanded.";
                }
                const string tosStr = tos.str();
                CARMA_CPTRACE( Trace::TRACE3, tosStr );
                if ( verbose >= OUTPUT_DEBUG )
                    cout << tosStr << endl;
            }

            fillItIn( c, p );
            return true;
        }
    } // if needsFullTrack

    // if there is less than a full track left, try to schedule it all together
    if ( !needsFullTrack ) {
        CARMA_CPTRACE(Trace::TRACE3,"trying partial track in policy block");
        Chunk c = chunkAvailable(p.startBlock, p.stopBlock, blocksLeft);
        if ( ! p.solarAngleIsGood(c,mjd) ) {
            c.good = false;
        }

        // We could not find a track in the policy block,
        // so search for one over the entire optimal block
        // if optimal > policy.
        if ( !c.good && p.expandable ) {
             CARMA_CPTRACE(Trace::TRACE3,"trying partial track in optimal block");
             c = chunkAvailable(p.LSTstartBlock, p.LSTstopBlock, blocksLeft);
        }
        if ( c.good ) {

            {
            ostringstream tos;
                tos << "Day " << number
                << " had a blocksLeft chunk "
                << c.start
                << " to "
                << c.end
                << " available.";
            const string tosStr = tos.str();
            CARMA_CPTRACE( Trace::TRACE3, tosStr );
            }

            fillItIn( c, p );

            return true;
        }
    } // if ! needsFullTrack


    // if no chunks allowed or in PRIORITY mode, we are done.
    if ( chunkSize == 0 || mode == MODE_PRIORITY ) return false;

    // Decrease the number of blocks to attempt to schedule.
    // If we have arrived here, then we already tried to schedule
    // one of blockLen if we needed a full track or we have tried
    // to schedule one of blocksLeft if we needed a partial track.
    // So decrement the attempted blocks from them.
    unsigned short blocksToSched 
        = needsFullTrack ?  blockLen-1 : blocksLeft-1 ;
    bool enoughBlocksRemainingAfterSched = true; 

    while ( blocksToSched >= chunkSize ) {

        // Don't try to assign time if we wind up with a small
        // leftover piece. Small means < chunkSize/2.
        // In this case force full scheduling of the entire 
        // block somewhere or not at all.
        blocksRemainingAfterSched = ( blocksLeft - blocksToSched );
        if (   blocksRemainingAfterSched > 0 )
        {
              enoughBlocksRemainingAfterSched = false; 
        } else {
              enoughBlocksRemainingAfterSched = true; 
        }

        enoughBlocksRemainingAfterSched = true; 

        if ( !enoughBlocksRemainingAfterSched ) {
            blocksToSched--;
            continue;
        }


        // if flexHA track go straight to finding any 
        // contiguous block
        if ( !p.flexible ) { 
            // first try a chunk around transit
            unsigned short half = (blocksToSched+1)/2;
            short signedStart = p.transitBlock - half;
            if ( signedStart < 0 ) 
                signedStart += BLOCKS_PER_DAY;
            unsigned short start = signedStart;
            unsigned short stop  = p.transitBlock + half;
            if ( stop  > BLOCKS_PER_DAY-1 )
                stop -= BLOCKS_PER_DAY;
            CARMA_CPTRACE(Trace::TRACE3, 
                "IN LOOP trying transit-centered chunk of size "
                << blocksToSched << " for project " 
                <<  p.disambiguatedName() 
                );
            Chunk c = chunkAvailable( start, stop, blocksToSched );
            if ( ! p.solarAngleIsGood(c,mjd) ) {
                c.good = false;
            }
            if ( c.good ) {
                {
                    ostringstream tos;
                    tos << "Day " << number
                    << " had a transit-centered chunk "
                    << start
                    << " to "
                    << stop
                    << " available."
                    ;

                    const string tosStr = tos.str();
                    CARMA_CPTRACE( Trace::TRACE3, tosStr );
                }

                fillItIn( c, p );
                return true;
            }
        } 

        {
            // now try a chunk anywhere
            CARMA_CPTRACE(Trace::TRACE3,"IN LOOP trying partial track in policy block");
            Chunk c = chunkAvailable(p.startBlock, p.stopBlock, blocksToSched );
            if ( !c.good && p.expandable ) {
             CARMA_CPTRACE(Trace::TRACE3,"IN LOOP trying partial track in optimal block");
             c = chunkAvailable(p.LSTstartBlock, p.LSTstopBlock, blocksToSched );
            }
            if ( ! p.solarAngleIsGood(c,mjd) ) {
                c.good = false;
            }
            if ( c.good ) {
                {
                    ostringstream tos;
                    tos << "Day " << number
                    << " had a regular chunk "
                    << c.start
                    << " to "
                    << c.end
                    << " available."
                    ;
                    const string tosStr = tos.str();
                    CARMA_CPTRACE( Trace::TRACE3, tosStr );
                }

                fillItIn( c, p );
                return true;
            }
        } 

        blocksToSched--;

    } // while ( blocksToSched >= chunkSize )

    return false;
}

string 
Day::getName(unsigned short block) 
{
    return projectMap_[block];
}

string 
Day::getInstCode( unsigned short block ) 
{
    return instMap_[block];
}
    
//---------------- END DAY METHODS -----------------------------------

// ----- Calendar class  ---------------

class Calendar {
public:
    /**
     * @param arrayConfig - String representing array configuration
     * @param maxDays - maximum number of days in the calendar
     * @param mjd - the start MJD of this calendar
     * zero means no maximum
     */
    Calendar(const std::string & arrayConfig, 
             const unsigned short maxDays,
             const double mjd);
    ~Calendar();

    /** Try to schedule a project.
     * @param p Input project, may be modified so non-const
     * @param chunkSize if > 0, try to schedule a chunk of size
     * chunkSize if the full project won't fit.
     * @param scheduling mode
     * @return true if successful, fall otherwise
     *
     */
    bool schedule( Project & p, 
                   const unsigned short chunkSize, 
                   const modeType mode );

    /** print the calendar **/
    void print();

    /** number of days that have projects scheduled */
    unsigned int size();

    /** @return true we are allowed to add a day to this calendar */
    bool canIncrease();

    /** add an empty Day to the end of the calendar */
    void addDay();

    /**
     * Add scheduled hours to the running tally.
     * @param the number of hours that were just scheduled.
     */
    void incrementHoursScheduled(float hrs);

    /**
     * @return the total number of hours scheduled in this
     * Calendar
     */
    float getHoursScheduled() const
    {
        return hrsScheduled_;
    };

    /**
     * After instantiating the Calendar and before scheduling any new 
     * days, add in the fixed/previously scheduled blocks from an
     * earlier run
     * @param file the input file with the block data
     */
    void readFixed( const std::string & file);

    /**
     * Write out the calendar in a fixed ASCII format that can be
     * editted and read in to a future iteration to block out
     * certain times/days.
     * @param file the output file with the block data
     */
    void writeFixed( const std::string & file) ;

    /**
     * Write out the calendar in Carpenter's queue format.
     * @param file the output file for the queue
     * @param subarray The subarray string, e.g. "sci1"
     */
    void writeQueue( const std::string & file,
                     const std::string & subarray ) ;

    /**
     * start MJD of the Calendar (for print out of date)
     */
    double mjd;

    /**
     * block out up to starting LST
     */
    void blockUpTo( const double startLST );

private:
    void compact();
    std::string createCal();
    void dumpGlobalProjectMap();

    std::list<Day> days_;
    std::map<unsigned short,string> globalProjectMap_;
    std::string arrayConfig_;
    unsigned short maxDays_;
    float hrsScheduled_;
    std::string fillChar_;
    std::string unfillChar_;
    std::string projectCol_;
    std::string instCol_;
    std::string dayCol_;
    std::string lstStartCol_;
    std::string lstStopCol_;
};


Calendar::Calendar( const std::string & arrayConfig, 
                    const unsigned short maxDays,
                    const double mjd ) :
    mjd(mjd),
    arrayConfig_( 
        StringUtils::lowASCIIAlphaNumericToUpper( arrayConfig )
        ),
    maxDays_(maxDays),
    hrsScheduled_(0.0),
    fillChar_("="),
    unfillChar_("."),
    projectCol_("PROJECT"),
    instCol_("INST"),
    dayCol_("DAY"),
    lstStartCol_("LST_START"),
    lstStopCol_("LST_STOP")
{
    try {
        int sz = ( maxDays_ > 0 ? maxDays_ : 100 );
        for ( int i = 0 ; i < sz ; i++ ) {
            Day d;
            CARMA_CPTRACE(Trace::TRACE5, "FINISHED DAY CONSTRUCTOR " << i);
            d.number = i+1;
            d.mjd = mjd+static_cast<double>(d.number);
            days_.push_back( d );
            CARMA_CPTRACE(Trace::TRACE5, "PUSHED BACK DAY " << i);
        }

        // make link to next day
        list<Day>::iterator di = days_.begin();
        list<Day>::iterator dii = days_.begin();
        list<Day>::iterator de = days_.end();
        while ( di != de ) {
            ++di;
            dii->nextDay = di;
            ++dii;
        }
        // tell the last Day of the Calendar that it is
        // the last day of the calendar
        de--;
        de->noNextDay();


    } catch ( const out_of_range & oor ) {
        ostringstream os;
        os << "Calendar construction  - got out of range exception on days_ vector: "
           << oor.what();
        throw CARMA_EXCEPTION( ErrorException, os.str() );
    } catch ( ... ) {
        throw CARMA_EXCEPTION( ErrorException, "Unclassified exception in Calendar contructor");
    }
}

Calendar::~Calendar() 
{ 
    days_.clear();
}

unsigned int 
Calendar::size() 
{
    unsigned int count = 0;
    list<Day>::iterator di = days_.begin();
    list<Day>::iterator de = days_.end();
    while( di != de ) {
       if ( di->isUsed() ) 
           count++;
       ++di;
    }

    return count;
}

bool 
Calendar::canIncrease() 
{
    return ( maxDays_ == 0 || days_.size() < maxDays_ );
}

void 
Calendar::addDay()
{
   try {
        Day day;
        Day & lastDay = days_.back();
        day.number =  lastDay.number+1;
        day.mjd = mjd+static_cast<double>(day.number);
        days_.push_back( day );
        std::list<Day>::iterator di = days_.end();
        di--;
        lastDay.nextDay = di;
        Day & newLastDay = days_.back();
        newLastDay.nextDay = days_.end();
   } catch ( const out_of_range & oor ) {
        ostringstream os;
        os << "Calendar::addDay() - got out of range exception on days_ vector: "
           << oor.what();
        throw CARMA_EXCEPTION( ErrorException, os.str() );
   }
}

string 
Calendar::createCal()
{
    compact();
    //dumpGlobalProjectMap();
    ostringstream os;
    unsigned short sz = globalProjectMap_.size();
    string curProj  = globalProjectMap_[1];
    string lastProj = "foo";
    // must start at index one because we have the beginning /
    // at LST 0 on the first day
    for (unsigned short i=1; i < sz; i++) {
        curProj = globalProjectMap_[i];
        if ( curProj != lastProj ) {
            os << "/";
            //i++;
            // locate the index of the project change.
            string tproj = curProj;
            unsigned short idx = i;
            while( tproj == curProj ) {
                tproj = globalProjectMap_[idx];
                CARMA_CPTRACE(Trace::TRACE4,
                    " GPM.curproj is /" << tproj 
                    << "/"<<curProj<<"/ at " << idx );
                idx++;
            }
            //idx--; // we overstepped by 1 in the while loop
            CARMA_CPTRACE(Trace::TRACE4,
                " located next project " << tproj 
                << " at " << idx);
            const size_t numBlocksAllocated = idx-i;
            const size_t cpsize = curProj.size();
            const size_t spaceAvailable = numBlocksAllocated - 1;
            if ( cpsize <= numBlocksAllocated ) {
                os << curProj;
                i+=cpsize;
                CARMA_CPTRACE(Trace::TRACE4,
                    " using full project name " << curProj
                    << " with size " << cpsize 
                    << " allocBlocks = " << numBlocksAllocated);
            } else {
                try {
                    string shortname = curProj.substr(0, spaceAvailable);
                    os << shortname;
                    const size_t shortsize = shortname.size();
                    i+=shortsize;
                    CARMA_CPTRACE(Trace::TRACE4,
                        " using short project name " << shortname 
                        << " instead of " << curProj 
                        << " with size " << shortsize 
                        << " allocBlocks = " << numBlocksAllocated);
                } catch ( const std::exception & ex ) {
                    cerr << " caught exception in createCal " <<endl;
                    cerr << "curproj: " << curProj
                         << " substr(0," << spaceAvailable << ")" 
                         << endl;
                    throw;
                }
            }
            // only go to idx - 1 because we
            // used a space for the / between projects
            while(i<idx-1) {
            if ( curProj == "none")
                os << unfillChar_;
            else
                os << fillChar_;
            ++i;
            }
        }
        lastProj = curProj;
    }

    return os.str();

}

void 
Calendar::print() 
{
    const string FMT("%a %b %d");
    int dayCount = 1;
    ostringstream os;
    string cal = createCal();
    //cout << "CAL:"<<cal<<":"<<endl;
    int LST = 0;
    os   << "CARMA " 
         << arrayConfig_
         << "-Array Schedule. " 
         << "Maximum Length: "; 
    if (maxDays_ == 0 ) 
        os << " unlimited. " ;
    else 
        os << maxDays_ << " days." ;
    os << endl 
       << endl 
       << "The '/' marks the BEGINNING of each track." 
       << endl 
       << "The last '=' marks the END of each track." 
       << endl;

    os << endl << endl <<  "LST    | ";
    double omjd = mjd;
    for(unsigned short i=0;i<BLOCKS_PER_DAY;i+=BLOCKS_PER_HOUR) {
        os << setw(2) << LST;
        os << "  ";
        LST++;
    }
    os << endl;

    ushort sz =size();
    for(unsigned short i = 0; i < sz; i++) {

        // leader
        for(unsigned short j=0;j<10;j++) {
            os << "-";
        }

        // time line
        for(unsigned short k = 0; k < BLOCKS_PER_DAY; k++) {
            (k % 4 == 0) ? os << "+"  : os << "-";
        }
        os << endl;

        os << "Day  "<<setw(2)<<dayCount <<"|  ";
        try {
            os << cal.substr(i*BLOCKS_PER_DAY,BLOCKS_PER_DAY);
        } catch ( const std::exception & ex ) {
            cerr << "caught exception in Calendar::print()" <<endl;
            cerr << "cal.substr(" 
             << i*BLOCKS_PER_DAY
             << ","
             << BLOCKS_PER_DAY
             << ") cal.size() = " << cal.size()
             << endl;
            throw;
        }
        // now add the date if requested at the beginning
        if ( omjd > 0.0 ) {
            os << "  " << Time::getDateString( omjd , FMT );
            omjd += 1.0;
        }

        os << endl;
        dayCount++;
    }

    // leader
    for(unsigned short j=0;j<10;j++) {
        os << "-";
    }

    // time line
    for(unsigned short k = 0; k < BLOCKS_PER_DAY; k++) {
        (k % 4 == 0) ? os << "+"  : os << "-";
    }
    os << endl;

    LST=0;
    os << "LST    | ";
    for(unsigned short i=0; i<BLOCKS_PER_DAY; i+=BLOCKS_PER_HOUR) {
        os << setw(2) << LST;
        os << "  ";
        LST++;
    }

    os << endl;
    cout << os.str() << endl;

}

bool 
Calendar::schedule( Project & p, const unsigned short chunkSize, const modeType mode ) 
{
    list<Day>::iterator di = days_.begin();
    list<Day>::const_iterator dEnd = days_.end();
    ostringstream os;
    os << "Calendar::schedule(" << p.disambiguatedName() << ") - ";
    ScopedLogNdc ndc( os.str() );
    CARMA_CPTRACE(Trace::TRACE3,"Entering. Crosses day =" << boolalpha << p.crossesDayBoundary );
    // The length that is left to schedule in this project.
    unsigned short blocksLeft = (p.allocatedBlocks - p.assignedBlocks);

    if ( blocksLeft < MINBLOCKS_TO_SCHEDULE ) {

        float minHrs = static_cast<float>(MINBLOCKS_TO_SCHEDULE*BLOCK_SIZE/60.0);
        float blockHrs = static_cast<float>(blocksLeft*BLOCK_SIZE/60.0);
        ostringstream os;
        os <<"Remaining time left for " << p.disambiguatedName() 
            << " [" << setprecision(1) << blockHrs
            << " hrs] is less than minimum allowable"
            << " for any project [" << setprecision(1) << minHrs
            << " hrs]. Will not schedule it.";
        const string msg = os.str();

        if ( verbose > OUTPUT_DEBUG ) {
            cout << msg << endl;
        }

        CARMA_CPTRACE(Trace::TRACE3, msg );

        return false;
    }

    const float ah = p.assignedHrs;
    while ( di != dEnd ) {
      CARMA_CPTRACE(Trace::TRACE3," trying day " << di->number);
        if ( di->tryToFit( p, chunkSize, mode ) == true )  {
            incrementHoursScheduled(p.assignedHrs - ah);
            //cout << " WORKED for day " << di->number << endl;
            return true; 
        }
       di++;
    } // while (di != dEnd )

    if ( canIncrease() ) {
        // Nothing available and schedule not yet full.
        // Add another day
        if ( verbose >= OUTPUT_DEBUG )
            cout << "Making new day for project " << p.disambiguatedName() <<endl;
        addDay();
        // try again with the added day
        return schedule( p, chunkSize, mode );
    }

    // schedule is full, sorry
    return false;
}

void 
Calendar::incrementHoursScheduled(float hrs) {
    ScopedLogNdc ndc("Calendar::incrementHoursScheduled - ");
    CARMA_CPTRACE(Trace::TRACE3," incrementing by " << hrs << " hours ");

    hrsScheduled_ += hrs;
}

void 
Calendar::compact()
{
    list<Day>::iterator di = days_.begin();
    list<Day>::iterator de = days_.end();
    unsigned short index;
    unsigned int short i = 0;
    while( di != de ) {
        for (unsigned short j = 0; j < BLOCKS_PER_DAY; j++) {
            index = i * BLOCKS_PER_DAY + j;
            string name = di->getName(j);
            globalProjectMap_
            .insert( make_pair( index, name ) );
        }
        ++i;
        ++di;
    }

}

// debug method
void Calendar::dumpGlobalProjectMap()
{
    unsigned short sz = size();
    unsigned short index;
    cout << " DAY      BLOCK     INDEX     NAME   LSTblock LSTindex" << endl;
    for( unsigned short i = 0; i<sz;i++ ) {
        for (unsigned short j = 0; j < BLOCKS_PER_DAY; j++) {
            index = i * BLOCKS_PER_DAY + j;
            cout << i << "    " << j  << "    " << index << "    "
             << globalProjectMap_[index]
             << setprecision(2) << setiosflags(ios::fixed) 
             << "    " << getLst(j)
             << "    " << getLst(index)
             << endl;
        }
    }

}

void
Calendar::readFixed( const std::string & fixed ) 
{
    Table table;
    // Table.open will throw if file can't be opened
    table.open( fixed );
    // fixedblocks file was empty or commented out, just return.
    if ( table.getNrows() == 0 ) return;

    vector<string> names    = table.getColumn( projectCol_ );
    vector<string> inst     = table.getColumn( instCol_ );
    vector<double> lstStart = table.getDoubleColumn( lstStartCol_ );
    vector<double> lstStop  = table.getDoubleColumn( lstStopCol_ );
    vector<int>    dayNum   = table.getIntColumn( dayCol_ );

    std::vector<int>::iterator result;
    result = max_element(dayNum.begin(),dayNum.end());
    unsigned int numDays = *result;
    unsigned int dsize = days_.size();
    if ( numDays > dsize ) {
        ostringstream errOs;
        errOs << " Length of input fixed data exceeds number of days ("
              << numDays << " > " << dsize  << "). "
              << " Increase input calendar length with 'days' keyword."
              ;
        throw CARMA_EXCEPTION( ErrorException, errOs.str() );
    }

    string lastgood = "none";
    try {
        unsigned int size=dayNum.size();
        for ( unsigned int i = 0 ; i < size ; i++ ) {
            list<Day>::iterator di = days_.begin();
            // index for days_ is zero-based.
            unsigned int n = dayNum.at(i) - 1;
        /* debug
        cout << "Doing day " << dayNum.at(i) 
             << " project " << names.at(i) 
             << " lst start " << lstStart.at(i) 
             << " lst stop " << lstStop.at(i) 
             << " inst " << inst.at(i) 
             << endl;
             */
            if ( n > dsize ) {
            ostringstream errOs;
            errOs << " Input fixed data contains day number larger "
                  << "than number of days in calendar ("
                  << n << " > " << dsize  << "). "
                  << " Increase input calendar length with 'days' keyword."
                  ;
            throw CARMA_EXCEPTION( ErrorException, errOs.str() );
            }

            for ( unsigned int cn = 0; cn< n;cn++, di++ ); // null loop

            unsigned short bstart = findBlock( lstStart.at(i) );
            unsigned short bend   = findBlock( lstStop.at(i) );
            string modName = "F:" + names.at(i);
            di->markAsUnavailable( bstart, bend, modName, inst.at(i));
            lastgood = modName;
        }

    } catch ( const out_of_range & ex ) {
        ostringstream os;
        os << "Error interpreting fixed data. "
           << "Last successful fixed project was " 
           << lastgood
           << " Exception message: " 
           << ex
           ;
        throw CARMA_EXCEPTION( ErrorException, os.str() );
    }

}

void
Calendar::writeFixed( const std::string & fixed ) 
{
    list<Day>::iterator di = days_.begin();
    list<Day>::const_iterator dEnd = days_.end();
    // write it in CARMA Table format
    Table t(5);
    t.setColumnName( 0, projectCol_ );
    t.setColumnType( 0, "s" );
    t.setColumnName( 1, instCol_ );
    t.setColumnType( 1, "s" );
    t.setColumnName( 2, dayCol_ );
    t.setColumnType( 2, "i" );
    t.setColumnName( 3, lstStartCol_ );
    t.setColumnType( 3, "r" );
    t.setColumnName( 4, lstStopCol_ );
    t.setColumnType( 4, "r" );
    ostringstream os;

    while ( di != dEnd ) {
        if ( di->isUsed() ) {
            Table dayTable = di->asTable();
            int nrows = dayTable.getNrows();
            for( int i=0; i< nrows;i++ )
                t.addRow( dayTable.getRow(i) );
        }
        di++;
    }

    ofstream outStream( fixed.c_str() );
    outStream << "# ************ FIXED DATA ************** " << endl;
    outStream << t;
    outStream.close();
}

void
Calendar::writeQueue( const std::string & queue,
                      const std::string & subarray ) 
{
    const string FMT("%a %b %d %Y");
    list<Day>::iterator di = days_.begin();
    list<Day>::const_iterator dEnd = days_.end();
    ostringstream os;

    double omjd = mjd;
    while ( di != dEnd ) {
        if ( di->isUsed() ) {
            os << "# Day " << di->number ;
               // only write out if mjd started as non-zero
               if ( mjd > 0 ) 
                   os << " / " << Time::getDateString( omjd , FMT );
           os << endl << di->asQueue(subarray) << endl;
        }
        di++;
        omjd+=1.0;
    }

    ofstream outStream( queue.c_str() );
    outStream << os.str();
    outStream.close();
}

void
Calendar::blockUpTo( const double startLST )
{
    if ( startLST != 0 ) {
        unsigned short bend   = findBlock( startLST );
        // subtract one block so that we start exactly on the LST
        bend--;
        const string xname = "XXXX";
        const string xinst = "X";
            list<Day>::iterator di = days_.begin();
        di->markAsUnavailable( 0 , bend, xname, xinst);
    }
}

//** end Calendar methods **/



} // end anonymous namespace


//************************************************************8
// PROGRAM MAIN BODY
//************************************************************8
int 
Program::main()
{
  const string VERSION = "$Revision: 1.52 $" ;
  try {

    const double HRS_PER_RAD = 12.0/M_PI;
    const string ONEMM       = "1mm";
    const string ONECM       = "1cm";
    const string THREEMM     = "3mm";

    const string infile    = getStringParameter("file");
    const int maxDays      = getIntParameter("days");
    const double chunkHrs  = getDoubleParameter("chunksize");
    const double cutoff    = getDoubleParameter("cutoff");
    const double schedStartLST  = getDoubleParameter("startLST");
    const string arrayconfig = getStringParameter("config");

    const string bandStr     = getStringParameter("band");
    bandType scheduleBand = BAND_UNKNOWN;
    if ( StringUtils::equalsIgnoreCase( bandStr, THREEMM ) )
        scheduleBand = BAND_3MM;
    if ( StringUtils::equalsIgnoreCase( bandStr, ONEMM ) )
        scheduleBand = BAND_1MM;
    if ( StringUtils::equalsIgnoreCase( bandStr, ONECM ) )
        scheduleBand = BAND_1CM;

    if ( scheduleBand == BAND_UNKNOWN ) {
        ostringstream os;
        os << " Unrecognized input band string ["<<bandStr<<"]."
           << " Must be one of 3mm, 1mm, 1cm ";
        throw CARMA_EXCEPTION( ErrorException, os.str() );
    }

    if ( schedStartLST < 0 || schedStartLST >= 24 ) 
        throw CARMA_EXCEPTION(ErrorException,
            "Start LST must be in range 0 to 24");

    const unsigned short userChunkSize 
        = static_cast<unsigned short>(rint(chunkHrs*BLOCKS_PER_HOUR));
    verbose = getIntParameter("verbose");
    userExpand = getBoolParameter("expand");
    const string modestr = getStringParameter("mode");
        const double pivot = getDoubleParameter("pivot") ; 
        const double flexchunkHrs = getDoubleParameter("flexchunk") ; 
    const unsigned short flexChunkSize
        = static_cast<unsigned short>(rint(flexchunkHrs*BLOCKS_PER_HOUR));
    modeType mode;


    // Compute MJD at LST=0 of given start date or at LST=0 if MJD of 
    // program invocation.
    // Note startMJD is not the same as startLST.   We later block out
    // sections of the calendar from startMJD(@LST=0) to startLST.
    double startMJD; 
    string startDate = getStringParameter("start");
    if ( startDate != "" ) {
        startDate += " 00:00:00" ;
        startMJD = Time::computeMJD( startDate, "%Y-%b-%d %H:%M:%S" );
    } else { 
        startMJD = Time::MJD();
        Location CARMAOBSERVATORY("carma");
        AstroTime a(CARMAOBSERVATORY);
        double lst = a.localSiderealTime(startMJD);
        startMJD -= lst/24.0;
    }
    cout << " startDate = " << startDate << " startMJD = " << startMJD << endl;

    if ( StringUtils::equalsIgnoreCase(modestr,"priority") ) {
        mode = MODE_PRIORITY;
    } else if ( StringUtils::equalsIgnoreCase(modestr,"compact") ) {
        mode = MODE_COMPACT;
    } else {
        throw CARMA_EXCEPTION(ErrorException, 
            "Unrecognized value for mode: must be 'priority' or 'compact'");
    }

    const string fixed = getStringParameter("fixed");
    if ( ! StringUtils::equalsIgnoreCase(fixed,"none") ) {
        if ( !FileUtils::exists(fixed) ) {
        ostringstream fos;
        fos <<"File " << fixed << " not found." ;
        throw CARMA_EXCEPTION(FileNotFoundException, fos.str() );
        }
    }

    const string queueFile = getStringParameter("queue");
    const string subarray 
        = StringUtils::lowASCIIAlphaNumericToLower( 
                               getStringParameter("subarray") 
                            );

    //const string sunLimitKW = getStringParameter("sunLimit");
    //vector<string> slStr = StringUtils::tokenize(sunLimitKW,",");
    const double sunLimit = getDoubleParameter("sunLimit");

    cout << getArg0() << " " << VERSION << endl;
    cout << " band=" << bandStr
         << " days=" << maxDays 
         << " subarray=" << subarray 
         << " config=" << arrayconfig
         << " start=" << startDate
         << " startLST=" << schedStartLST
         << " fixed= " << fixed
         << " queue= " << queueFile
         << " mode=" << modestr 
         << " chunksize=" << chunkHrs 
         << " flexchunk=" << flexchunkHrs
         << " cutoff=" << cutoff
         << " pivot=" << pivot 
         << " sunLimit=" << sunLimit
         << endl;
    if (maxDays < 0 )
        throw CARMA_EXCEPTION(ErrorException, "days must be non-negative");

    UvTrack uvTrackLowFreq( arrayconfig, Frequency( 100.0, "GHz" ) );
    UvTrack uvTrackHiFreq( arrayconfig,  Frequency( 220.0, "GHz" ) );

    // read the project Table
    Table projectTable;
    // Table.open will throw if file can't be opened
    projectTable.open( infile );
    vector<string> names  = projectTable.getColumn("project_name");
    vector<double> grades  = projectTable.getDoubleColumn("numeric_grade");
    vector<double> allocHr = projectTable.getDoubleColumn("TAC_allocation");
    vector<int>    obsblocks = projectTable.getIntColumn("obsblock");
    // RA is in radians
    vector<double> ra      = projectTable.getHMSColumn("RA");
    // DEC is in radians
    vector<double> dec     = projectTable.getDMSColumn("Dec");
    vector<string> institutions = projectTable.getColumn("Institution");
    vector<string> configs = projectTable.getColumn("configuration");
    vector<string> srcs    =  projectTable.getColumn("source_name");
    vector<string> obsblockIds = projectTable.getColumn("obsblockId");
    vector<int>    flexHA  =  projectTable.getIntColumn("Flex_HA");
    vector<double> freq    =  projectTable.getDoubleColumn("Freq");

    int nrows = projectTable.getNrows();
    vector<Project> projects;
        set< Project, PriorityCmp > sortedProjects;

    SourceChecker checker;
    checker.showHeader(true);

    float totalHours = 0.0;
    // institutal request totals
    float citReq = 0.0;
    float umdReq = 0.0;
    float ucbReq = 0.0;
    float uiucReq = 0.0;
    float chicagoReq = 0.0;
    float carmaReq   = 0.0;
    float visReq = 0.0;
    float unkReq = 0.0;

    Ephemeris ephemeris;
    ephemeris.setMJD( Time::MJD() );
    const double RAD2DEG = 180.0/M_PI;
    const double RAD2HR  = 12/M_PI;

    // load up the projects
    for(int i=0; i<nrows; i++) {

        bandType obsBand  = BAND_UNKNOWN;
        if ( freq[i] >= 72.0 && freq[i] <= 120.0 ) 
        obsBand = BAND_3MM;
        if ( freq[i] > 200.0 )
        obsBand = BAND_1MM;
        if ( freq[i] < 40.0 )
        obsBand = BAND_1CM;

        // This is for deciding which UvTrack object to
        // use.  All we care about is if the observation
        // is greater than 113 GHz or not.
        bool hiFreq  = freq[i] > 113.0 ? true : false;

        if (i>0) checker.showHeader(false);
        // only schedule projects that meet the cutoff
        // are in the requested array config and band and
        // have allocated hours
        if (   grades[i] < cutoff 
        || allocHr[i] == 0.0 
        || ( obsBand != scheduleBand )
        || !StringUtils::equalsIgnoreCase(configs[i], arrayconfig)
           )  {
        if( verbose >= OUTPUT_DEBUG ) {
            cout << "Skipping Project "
             << names[i]
             <<"["
             << obsblocks[i]
             <<"] because it does not meet grade cutoff "
             <<"or has no TAC allocation or is the "
             <<"wrong array configuration or is the wrong band."
             << endl;
        }
        continue;
        }
        Project p;
        p.name = names[i];
        p.institution = institutions[i];
        p.config = configs[i];
        p.source = srcs[i];
        p.obsblockId = obsblockIds[i];
        p.scriptName = p.obsblockId;
        // replace all instances of . with _
        p.scriptName = StringUtils::replace( p.obsblockId,".","_");
        p.scriptName += ".obs";
        p.grade = static_cast<float>( grades[i] );
        p.allocatedHrs = static_cast<float> ( allocHr[i] );
        totalHours += p.allocatedHrs;
        p.allocatedBlocks 
            = static_cast<ushort>((p.allocatedHrs * 60/BLOCK_SIZE));
        p.minHrs = p.allocatedHrs;
        p.obsblock = obsblocks[i];
        // if the source is a planet reset the RA/DEC to the RA/Dec
        // at the time this program is run. See bug #690.
        if ( isPlanet( p.source ) ) {
            ephemeris.setSource( p.source );
            ra[i]  = ephemeris.getRa() ;
            dec[i] = ephemeris.getDec();
            if ( verbose > OUTPUT_BRIEF ) {
                cout << "### NOTE: Substituting current RA/DEC for planet "
                 << p.source 
                 << " in project " << p.disambiguatedName()
                 << ". New RA/DEC (hr,deg): " << ra[i]*RAD2HR 
                     << " , " << dec[i]*RAD2DEG
                 << endl;
            }
        }
        p.pEphem = &ephemeris;

        p.ra = ra[i];
        p.dec = dec[i];

        p.freq = freq[i];
        p.flexible = ( flexHA[i] == 1 ? true : false );
        p.rawRow = projectTable.getRow(i);
        const string icode = p.getInstCode();
        // sum up the institutional request totals
        if ( icode == CIT ) 
            citReq += p.allocatedHrs;
            else if ( icode == UMD )
                umdReq += p.allocatedHrs;
                else if ( icode == UCB )
                    ucbReq += p.allocatedHrs;
                    else if ( icode == UIUC )
                        uiucReq += p.allocatedHrs;
                        else if ( icode == VISITOR )
                            visReq += p.allocatedHrs;
                            else if ( icode == CARMA )
                                carmaReq += p.allocatedHrs;
                                else if ( icode == CHICAGO )
                                    chicagoReq += p.allocatedHrs;
                                        else 
                                        unkReq += p.allocatedHrs;


        Angle srcRA ( p.ra, "radians" );
        Angle srcDec( p.dec, "radians") ;
        double sourceDec = srcDec.degrees();
        if ( hiFreq ) {
            p.optimalLength = uvTrackHiFreq.optimalLength( sourceDec );
            p.policyLength = uvTrackHiFreq.policyLength( sourceDec );
        } else {
            p.optimalLength = uvTrackLowFreq.optimalLength( sourceDec );
            p.policyLength = uvTrackLowFreq.policyLength( sourceDec );
        }
        p.expandable = ( p.optimalLength > p.policyLength );

        // warn about projects with less than minimum
        if ( p.allocatedHrs < p.policyLength ) {
            ostringstream os;
            os  << "Warning: Project " << p.disambiguatedName()
                << " has been allocated fewer hours ["
                << p.allocatedHrs
                << "] than the policy minimum ["
                << p.policyLength
                << "] for its declination and frequency."
                ;
            const string msg = os.str();
            if ( verbose > OUTPUT_BRIEF )
                cout << "### NOTE: " << msg << endl;

            programLogNoticeIfPossible( msg );
        }


        if ( verbose >= OUTPUT_DEBUG )  {
            Source s( p.disambiguatedName(),
                  srcRA,
                  srcDec,
                  Velocity(0.0,"km/s"),
                  Angle(0.0,"radians")
                );

            checker.setSource( s );
            cout << checker.info() << endl;
        }

        double raHr = p.ra*HRS_PER_RAD;
        /*
        cout << " p.ra " << p.ra << "raHR " << raHr
         << " p.optimalLength " << p.optimalLength
         << " opt/2 " << p.optimalLength/2.0
         <<endl
         ;
         */
        p.lstStart = raHr - p.optimalLength/2.0;
        if ( p.lstStart < 0 ) p.lstStart += 24.0;
        if ( p.lstStart >= 24.0 ) p.lstStart -= 24.0;

        p.lstStop  = raHr + p.optimalLength/2.0;
        if ( p.lstStop >= 24.0 ) p.lstStop -= 24.0;

        p.policyStart = raHr - p.policyLength/2.0;
        /*
        cout 
         << " p.policyLength " << p.policyLength
         << " pol/2 " << p.policyLength/2.0
         <<endl
         ;
         */
        if ( p.policyStart< 0 ) p.policyStart += 24.0;
        if ( p.policyStart>= 24.0 ) p.policyStart -= 24.0;

        p.policyStop = raHr + p.policyLength/2.0;
        if ( p.policyStop >= 24.0 ) p.policyStop -= 24.0;

        // try to center around transit, using policy length track
        p.transitBlock    = findBlock( raHr );
        p.LSTstartBlock   = findBlock( p.lstStart );
        p.LSTstopBlock    = findBlock( p.lstStop );
        p.startBlock      = findBlock( p.policyStart );
        p.stopBlock       = findBlock( p.policyStop );
        /*
        cout << "LSTSTART " << p.lstStart << " "
             << "LSTSTOP "  << p.lstStop << " "
             << "LSTSTARTBLOCK "  << p.LSTstartBlock << " "
             << "LSTSTOPBLOCK " << p.LSTstopBlock << " "
             << "POLICYSTART" << p.policyStart << " "
             << "POLICYSTOP" << p.policyStop << " "
             << "POLICYSTARTBLOCK" << p.startBlock << " "
             << "POLICYSTOPBLOCK" << p.stopBlock << " "
         << endl;
         */

        if ( p.startBlock > p.stopBlock )
            p.crossesDayBoundary = true;
        else
            p.crossesDayBoundary = false;

        p.sunLimit = sunLimit;

        // Sort the projects by descending grade.
        // Exit with error if input grades are not unique.
        // (See bugzilla #653)
        set<Project>::iterator spcheck;
        set<Project>::const_iterator spEnd = sortedProjects.end();
        if ( ( spcheck = sortedProjects.find( p )) != spEnd ) {
            ostringstream errMsg;
            Project badp = *spcheck;
            errMsg << "Projects " << p.disambiguatedName()
                   << " and " 
                   << badp.disambiguatedName()
                   << " have identical priorities. This is not allowed."
                   ;
            throw CARMA_EXCEPTION(ErrorException, errMsg.str() );
        }
        sortedProjects.insert( p );

        if ( verbose >= OUTPUT_DEBUG ) p.print();

    }

    // Now stuff them into the vector in sorted order.
    // We have to do this because std::set keys are const so
    // we can't use them in non-const method calls or pass
    // them to methods that will change them (or the
    // key could become invalid, if the grade were changed).
    //
    set<Project>::iterator spi         = sortedProjects.begin();
    set<Project>::const_iterator spEnd = sortedProjects.end();
    if ( verbose >= OUTPUT_DEBUG ) cout << "ORDERED LIST" << endl;
    for (; spi != spEnd; spi++ ) {
        Project myp = *spi;
        if ( verbose >= OUTPUT_DEBUG )  {
        cout << myp.disambiguatedName() << setprecision(4) << " " << myp.grade << endl;
        }
        projects.push_back( *spi );
    }

    Calendar cal( arrayconfig, maxDays, startMJD );

    // Block out time on Day 1 up to user-requested startLST
    if ( schedStartLST != 0 ) {
        cal.blockUpTo( schedStartLST );
    }

        // read in an old schedule if requested
    if ( ! StringUtils::equalsIgnoreCase(fixed,"none") ) {
        cal.readFixed( fixed );
    }

    vector<Project>::iterator pi         = projects.begin();
    vector<Project>::const_iterator pEnd = projects.end();
    unsigned int numProjects             = projects.size();

    unsigned short couldNotSchedule = 0;
    //cout << " STARTING SCHEDULE " << endl;
    while ( pi != pEnd ) {
        bool keepgoing = true;
        
        while ( pi->hasUnassignedBlocks() && keepgoing ) {

        bool tried_compact = false;
        unsigned short inChunk = userChunkSize;
        if ( pi->flexible ) {
            if( verbose >= OUTPUT_DEBUG ) {
            cout << "Using FlexHA for " 
                 << pi->disambiguatedName() 
                 << endl;
            }
            inChunk = flexChunkSize;

        }

        if ( pi->grade < pivot ) {
            pi->success = cal.schedule( *pi, inChunk , MODE_COMPACT );
            tried_compact = true;
        } else {
            pi->success = cal.schedule( *pi, inChunk , mode );
        }

        if ( mode == MODE_PRIORITY && pi->success == false && !tried_compact ) {
            // We could not find a full track length anywhere in
            // the calendar, so now try scheduling it in COMPACT mode
            if( verbose >= OUTPUT_DEBUG ) {
                cout << "Could not schedule project " << pi->disambiguatedName()
                     << " in PRIORITY mode. Trying COMPACT mode"
                     << endl;
                    // NB: CAN get false conflict here if
                    // schedule() returned false because the remaining
                    // chunk was too small to schedule.
                    unsigned short remaining = pi->allocatedBlocks - pi->assignedBlocks;
                    if ( remaining >= inChunk ) {
                      if ( cal.canIncrease() )
                          throw CARMA_EXCEPTION(ErrorException,"Internal error: PRIORITY/canIncrease conflict!");
                    }
            }
            pi->success = cal.schedule( *pi, inChunk , MODE_COMPACT );
        }

        if ( pi->success ){
            if( verbose >= OUTPUT_DEBUG ) cout << "Successfully scheduled project ";
        } else {
            if( verbose >= OUTPUT_DEBUG ) cout << "***No room to schedule ";
            couldNotSchedule++;
            keepgoing = false;
        }

        pi->partial = ( pi->assignedHrs < pi->allocatedHrs );
        if (verbose>= OUTPUT_DEBUG ) cout << pi->disambiguatedName() << endl;
        }

        if (verbose>= OUTPUT_DEBUG ) pi->print();
        pi++;
    }

    cout << endl ;
    cal.print();

    if ( verbose >= OUTPUT_STATS ) {
        // Print out some statistics.
        float dayHours = cal.size()*24.0;
        float hoursScheduled = cal.getHoursScheduled();
        cout.setf(ios::fixed) ;
        cout << "For "
         << numProjects 
         << setprecision(2)
         << " input projects, "
         << hoursScheduled
         << " hours out of " 
         << totalHours
         << " allocated were successfully scheduled. " 
         << endl
         << couldNotSchedule
         << " projects were partially or totally unschedulable."
         << endl
         << "The total days required is " << cal.size() << "."
         << " The total hours required is " 
         << dayHours
         << ", giving an scheduling efficiency of " 
         <<  (hoursScheduled/dayHours)*100.0
         << "%."
         << endl;

        cout << endl <<endl
         << "FULLY OR PARTIALLY SCHEDULED" << endl
         << "Project       Pass#   Day#  StartLST  StopLST  StartEL  StopEl  HrsThisPass    ScheduledSoFar    TAC_Allocation " <<endl;

        // institutional totals
        float cit = 0.0;
        float umd = 0.0;
        float ucb = 0.0;
        float uiuc = 0.0;
        float vis = 0.0;
        float chicago = 0.0;
        float carma = 0.0;
        for (unsigned short i=0; i < numProjects; i++ ) {
            // note if numpasses=0, then no stats are printed
            projects[i].stats();
            if ( projects[i].numPasses() > 0 ) {
                if ( projects[i].getInstCode() == CIT ) 
                cit += projects[i].assignedHrs;
                if ( projects[i].getInstCode() == UMD )
                umd += projects[i].assignedHrs;
                if ( projects[i].getInstCode() == UCB )
                ucb += projects[i].assignedHrs;
                if ( projects[i].getInstCode() == UIUC )
                uiuc += projects[i].assignedHrs;
                if ( projects[i].getInstCode() == CHICAGO )
                chicago += projects[i].assignedHrs;
                if ( projects[i].getInstCode() == CARMA )
                carma += projects[i].assignedHrs;
                if ( projects[i].getInstCode() == VISITOR )
                vis += projects[i].assignedHrs;
            }
        }

        cout << endl <<endl
         << "COMPLETELY UNSCHEDULED" << endl
         << "Project        TAC_Allocation"  
         << endl;
        float unschedHours = 0;
        for (unsigned short i=0; i < numProjects; i++ ) {
        if ( projects[i].numPasses() == 0 ) {
            cout << projects[i].disambiguatedName() << "     " << projects[i].allocatedHrs
             << endl;
            unschedHours += projects[i].allocatedHrs;
        }
        }
        cout << "Total hours for completely unscheduled projects: " 
         <<  setprecision(2) << unschedHours << endl;

        float citalloc = ( citReq > 0) ?  100*cit/citReq : 0 ;
        float umdalloc = ( umdReq > 0) ?  100*umd/umdReq : 0 ;
        float ucballoc = ( ucbReq > 0) ?  100*ucb/ucbReq : 0 ;
        float uiucalloc = (uiucReq > 0) ? 100*uiuc/uiucReq : 0 ;
        float chicagoalloc = (chicagoReq > 0) ? 100*chicago/chicagoReq : 0 ;
        float carmaalloc = (carmaReq> 0) ? 100*carma/carmaReq: 0 ;
        float visalloc = (visReq > 0) ?  100*vis/visReq : 0 ;
        cout << endl
         << "INSITUTIONAL BREAKDOWN"
         << endl
         << "           Hours     %Total_Scheduled     %Inst_Allocation"
         << endl
         << setprecision(2)
         << "CIT:       " << cit 
                << "     " 
                << "     " 
                << 100*cit/hoursScheduled 
                << "     " 
                << "     " 
                << "     " 
                << citalloc
                << endl
         << "UMD:       " << umd 
                << "     " 
                << "     " 
                << 100*umd/hoursScheduled 
                << "     " 
                << "     " 
                << "     " 
                << umdalloc
                << endl
         << "UCB:       " << ucb 
                << "     " 
                << "     " 
                << 100*ucb/hoursScheduled 
                << "     " 
                << "     " 
                << "     " 
                << ucballoc
                << endl
         << "UIUC:      " << uiuc 
                << "     " 
                << "     " 
                << 100*uiuc/hoursScheduled 
                << "     " 
                << "     " 
                << "     " 
                << uiucalloc
                << endl
         << "CHICAGO:   " << chicago
                << "     " 
                << "     " 
                << 100*chicago/hoursScheduled 
                << "     " 
                << "     " 
                << "     " 
                << chicagoalloc 
                << endl
         << "CARMA/DDT: " << carma
                << "     " 
                << "     " 
                << 100*carma/hoursScheduled 
                << "     " 
                << "     " 
                << "     " 
                << carmaalloc
                << endl
         << "VISITOR:   " << vis 
                << "     " 
                << "     " 
                << 100*vis/hoursScheduled 
                << "     " 
                << "     " 
                << "     " 
                << visalloc
                << endl
         ;

    } // STATS print

    const string fixed_out = infile+".fixed_blocks";
    cal.writeFixed( fixed_out );
    cal.writeQueue( queueFile, subarray );

    } catch (const ErrorException & be) {
      cerr << getArg0() << " " << VERSION 
       << " *** CAUGHT AN EXCEPTION: " << be.getMessage() << endl;
      return EXIT_FAILURE;
    } catch ( const std::exception & stdex ) {
      cerr << getArg0() << " " << VERSION 
           << " *** CAUGHT AN EXCEPTION " << stdex.what() << endl;
    } catch (...) {
      cerr << getArg0() << " " << VERSION 
           << " *** CAUGHT AN UNKNOWN ERROR" << endl;
      return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

/*
 * may be of interest

Sent to you by Peter via Google Reader: The optimal schedule for pulsar
timing array observations via Monthly Notices of the Royal Astronomical
Society by K. J. Lee, C. G. Bassa, G. H. Janssen, R. Karuppusamy, M.
Kramer, R. Smits, B. W. Stappers on 5/21/12
ABSTRACT
In order to maximize the sensitivity of pulsar timing arrays to a
stochastic gravitational wave background, we present computational
techniques to optimize observing schedules. The techniques are
applicable to both single- and multitelescope experiments. The
observing schedule is optimized for each telescope by adjusting the
observing time allocated to each pulsar while keeping the total amount
of observing time constant. The optimized schedule depends on the
timing noise characteristics of each individual pulsar as well as the
performance of instrumentation. Several examples are given to
illustrate the effects of different types of noise. A method to select
the most suitable pulsars to be included in a pulsar timing array
project is also presented.

Things you can do from here:
- Subscribe to Monthly Notices of the Royal Astronomical Society using
Google Reader
- Get started using Google Reader to easily keep up with all your
favorite sites

*/
