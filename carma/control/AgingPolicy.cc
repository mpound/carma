#include "carma/control/AgingPolicy.h"
#include "carma/control/SystemStateManager.h"
#include "carma/util/FileUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/IllegalStateException.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <boost/filesystem.hpp>

using namespace boost::filesystem;
using namespace boost::gregorian;
using namespace boost::posix_time;
using namespace carma::control;
using namespace carma::util;
using namespace std;

namespace {

const Trace::TraceLevel TRACE_AGING_LOGIC = Trace::TRACE3;

ptime ptimeFromFrame( const frameType frame )
{
    const double frameMjd = Time::MJD( frame );
    const time_t secondsSinceEpoch = static_cast< time_t >( 
        floor( Time::SECONDS_PER_DAY * (frameMjd - Time::MJD1970) ) );
    ptime answer = from_time_t( secondsSinceEpoch );
    if ( frame % 2 == 0 ) 
        return answer;
    else
        return answer + milliseconds( 500 );
}

frameType
parseFrameFromFilename( const string & filename, 
                        const string & prefix,
                        const string & suffix )
{
    // Tokenize filename of form prefix + frame + suffix.
    const string::size_type prefixPos = filename.find( prefix );
    const string::size_type suffixStartPos = filename.size() - suffix.size();
    const string::size_type suffixPos = filename.find( suffix, suffixStartPos );

    if ( prefixPos != string::npos && suffixPos != string::npos ) {
        const string::size_type framePos = prefixPos + prefix.size();
        const string::size_type frameSize = suffixPos - framePos;
        const string frameToken =  filename.substr( framePos, frameSize );
                                               
        istringstream mjdIn( frameToken ); 
        frameType frame;  
        mjdIn >> frame;
        return frame;
    }

    return 0;
}

FileTimeMap createFileTimeMap( const string & dir )
{
    typedef vector<string>::iterator sIterator;

    vector<string> fileNames = FileUtils::scandir( dir );

    FileTimeMap answer;

    for ( sIterator i = fileNames.begin(); i != fileNames.end(); ++i ) {

        const frameType frame = parseFrameFromFilename( *i, "state-", ".txt" ); 

        if ( frame != 0 ) {
            ptime filePtime = ptimeFromFrame( frame );

            answer.insert( FileTimeMap::value_type( filePtime, *i ) );
        }
    }
        
    return answer;
}

}

PolicyChain carma::control::createAgingPolicyChain( const string & dirname ) 
{
    PolicyChain answer;

    // Initial policy, interval = duration for completeness but the interval
    // is effectively ignored on the first policy.  
    // Store everything for a day.
    answer.push_back( AgingPolicy( dirname + "/current",
                                   time_duration( 0, 0, 0, 0 ), 
                                   1, 0, 0, // 1 day interval
                                   1, 0, 0  ) );
    // Save state every 20 minutes for 1 month
    answer.push_back( AgingPolicy( dirname + "/one-day-ago",
                                   time_duration( 0, 20, 0, 0 ), // 20 minutes
                                   0, 0, 0, 
                                   0, 1, 0  ) );
    // Save state every 4 hours for 2 months 
    answer.push_back( AgingPolicy( dirname + "/one-month-ago",
                                   time_duration( 4, 0, 0, 0 ), // 4 hours
                                   0, 0, 0,
                                   0, 2, 0  ) );
    // Save state once a day for 9 months
    answer.push_back( AgingPolicy( dirname + "/three-months-ago",
                                   time_duration( 0, 0, 0, 0 ), 
                                   1, 0, 0,
                                   0, 9, 0 ) );
    // Save state weekly for 2 years
    answer.push_back( AgingPolicy( dirname + "/one-year-ago",
                                   time_duration( 0, 0, 0, 0 ), 
                                   0, 1, 0,
                                   0, 0, 2 ) );
    // Last policy.  Duration is 100 years for completeness but this too
    // is effectively ignored (nothing ages out of the last policy).
    // Save everything else on a monthly timescale.
    answer.push_back( AgingPolicy( dirname + "/three-years-ago",
                                   time_duration( 0, 0, 0, 0 ), 
                                   0, 0, 1,
                                   0, 0, 100 ) );
    return answer;                                 
}

string
carma::control::retrieveFilenameForFrame( const PolicyChain & policies,
                                          const carma::util::frameType frame )
{
    PolicyChain::const_iterator p = policies.begin();
    const PolicyChain::const_iterator pEnd = policies.end();
    while ( p != pEnd ) {
        
        if ( p->frameIsInPolicy( frame ) ) 
            return p->getFilenameForFrame(frame); 

        ++p;
    }

    ostringstream errMsg;
    errMsg << "There's no saved state for frame " << frame << ".";
    throw CARMA_EXCEPTION( IllegalStateException, errMsg.str() );
}

AgingPolicy::AgingPolicy( 
        const string & directory,
        boost::posix_time::time_duration interval,
        int intervalDays, int intervalWeeks,int intervalMonths,
        int durationDays, int durationMonths, int durationYears ) :
    directory_( directory ),
    interval_( interval ),
    intervalDays_( intervalDays ),
    intervalWeeks_( intervalWeeks ),
    intervalMonths_( intervalMonths ),
    durationDays_( durationDays ),
    durationMonths_( durationMonths ),
    durationYears_( durationYears ),
    fileTimeMap_( ),
    policyPeriod_( second_clock::local_time(), time_duration(0,0,0,0) )
{ 
    path dirpath( directory );

    if ( !exists( dirpath ) ) {
        // Try creating the directory
        programLogInfoIfPossible( "Directory " + directory + " doesn't exist."
            + "  Attempting to create." );
        if ( !create_directories( dirpath ) ) { 
            ostringstream err;
            err << "Unable to create " << dirpath;
            programLogErrorIfPossible( err.str() );
            throw CARMA_EXCEPTION( IllegalStateException, err.str() ); 
        } 
    } 

}

string 
AgingPolicy::getDirectory( ) const
{
    return directory_;
}

ptime
AgingPolicy::subtractTotalDuration( const ptime & to ) const
{
    ptime answer = to;
    answer -= durationDays_; 
    answer -= durationMonths_;
    answer -= durationYears_;
    return answer;
}

ptime 
AgingPolicy::subtractTotalInterval( const ptime & to ) const
{
    ptime answer = to;
    answer -= interval_;
    answer -= intervalDays_;
    answer -= intervalWeeks_;
    answer -= intervalMonths_;
    return answer;
}

void 
AgingPolicy::syncToTime( const ptime & policyEndDate ) 
{
    policyEndDate_ = policyEndDate;
    policyStartDate_ = subtractTotalDuration( policyEndDate );
    policyPeriod_ = time_period( policyStartDate_, policyEndDate_ );

    fileTimeMap_ = createFileTimeMap( directory_ );
    
    updateFirstOpenTimeSlots( );
}

FileTimeMap 
AgingPolicy::reapAgedCandidates( )
{
    // Assume that any files in this directory belong there from previous
    // processing and thus aged files are those less than the start date.
    FileTimeMap aged;

    // Start from the beginning of the file map since oldest files age first.
    FileTimeMap::iterator fBegin = fileTimeMap_.begin();
    FileTimeMap::iterator fEnd = fileTimeMap_.end();
    for ( FileTimeMap::iterator f = fBegin; f != fEnd; ++f ) {
        if ( f->first < policyStartDate_ ) {
            aged.insert( *f );
        } else {
            break; // Since fileTimeMap is sorted chronologically, we're done.
        }
    }

    if ( !aged.empty() ) {
        ostringstream oss; 
        oss << aged.size() << " file";
        if ( aged.size() > 1 ) oss << "s";
        oss << " aged out of policy attached to " << directory_ << ": ";
        FileTimeMap::iterator a = aged.begin();
        while ( a != aged.end() ) { 
            oss << a->second;
            ++a;
            if ( a == aged.end() ) 
                oss << ".";
            else 
                oss << ", ";
        }

        programLogInfoIfPossible( oss.str( ) );
    }

    return aged;
}

void
AgingPolicy::traceDeletion( const FileTimeMap::value_type & candidate,
                            const AgingPolicy & previousPolicy,
                            const string & spiel )
{
    ostringstream oss;
    oss << "Candidate " << previousPolicy.directory_ << "/" 
        << candidate.second << " deleted.  Candidate date "
        << candidate.first << ", policy date " 
        << policyStartDate_ << " - " << policyEndDate_
        << ". " << spiel;
    CARMA_CPTRACE( TRACE_AGING_LOGIC, oss.str() ); 
}

void 
AgingPolicy::removeFileFromPolicy( FileTimeMap::value_type entry,
                                   StateManager & manager )
{
    FileTimeMapIter pos = fileTimeMap_.find( entry.first );
    if ( pos == fileTimeMap_.end() ) {
        ostringstream error;
        error << "AgingPolicy::removeFileFromPolicy - Requested removal of "
            << entry.second << " from directory " << directory_ << " but "
            << "file does not exist in fileTimeMap_!";
        programLogWarnIfPossible( error.str() );
    }

    // Remove candidate frame from the manager first...  The manager
    // retains an index of frames and monitor points - if the file were
    // to be deleted prior to removing the frame from the index, the table
    // could become corrupt if we shutdown or crash in the interim.  
    // However, if it is the other way around, the old file will simply be
    // deleted on the next go around.
    const frameType deadFrame = parseFrameFromFilename( entry.second,
                                                        "state-", ".txt" ); 
    manager.removeFrameFromIndex( deadFrame );

    FileUtils::removeFile( directory_ + '/' + entry.second );

    fileTimeMap_.erase( pos );
}


void
AgingPolicy::handleCandidate( const FileTimeMap::value_type candidate,
                              AgingPolicy & previousPolicy,
                              StateManager & manager )
{
    CARMA_CPTRACE( TRACE_AGING_LOGIC, "Considering candidate " << 
        candidate.second << " for aging." ); 

    const string & candidateDir = previousPolicy.directory_;

    if ( !policyPeriod_.contains( candidate.first ) ) {
        traceDeletion( candidate, previousPolicy, "Not in policy." );

        previousPolicy.removeFileFromPolicy( candidate, manager );
        return;
    } 

    TimeSlots::iterator sBegin = openTimeSlots_.begin();
    const TimeSlots::iterator sEnd = openTimeSlots_.end();
    for ( TimeSlots::iterator s = sBegin; s != sEnd; ++s ) {
        const time_period & slot = *s;
        if ( slot.contains( candidate.first ) ) { // Add to policy
            FileUtils::rename( candidateDir + '/' + candidate.second,
                               directory_ + '/' + candidate.second ); 
            {
                ostringstream oss;
                oss << "Candidate " << candidateDir << "/" << candidate.second
                    << " moved to " << directory_ << "/" << candidate.second 
                    << " for time slot " << slot;
                programLogInfoIfPossible( oss.str() );
            }
            openTimeSlots_.erase( s ); 
            fileTimeMap_.insert( candidate );
            previousPolicy.fileTimeMap_.erase( candidate.first );
            return; // We're done.
        } 
    }
    
    traceDeletion( candidate, previousPolicy, "Not in any open slot." );
    previousPolicy.removeFileFromPolicy( candidate, manager );
}

void 
AgingPolicy::traceOpenTimeSlots( )
{
    ostringstream oss;
    oss << "Open time slots: ";

    TimeSlots::iterator sBegin = openTimeSlots_.begin();
    const TimeSlots::iterator sEnd = openTimeSlots_.end();
    for ( TimeSlots::iterator s = sBegin; s != sEnd; ++s ) 
        oss << *s << ", ";
    
    CARMA_CPTRACE( TRACE_AGING_LOGIC, oss.str( ) );
}
    
bool
AgingPolicy::frameIsInPolicy( const carma::util::frameType frame ) const
{
    const ptime pt = ptimeFromFrame( frame );

    {
        ostringstream dbg;
        dbg << "Checking that frame (" << frame << ") with ptime ("
            << pt << ") is in policy (" << policyPeriod_ << ").";
        programLogDebugIfPossible( dbg.str() );
    }

    if ( policyPeriod_.contains( pt ) ) {

        if ( fileTimeMap_.find( pt ) == fileTimeMap_.end() ) {
            ostringstream err;
            err << "Frame (" << frame << ") ptime (" << pt << ") is within "
                << "policy bounds (" << policyPeriod_ << ") but no entry "
                << "exists in file time map.  Either the function is being " 
                << "misused or internal tables are fragmented.";
            throw CARMA_EXCEPTION( IllegalStateException, err.str() ); 
        }

        return true;
    } 
    
    return false;
}

string
AgingPolicy::getFilenameForFrame( const carma::util::frameType frame ) const
{
    if ( !frameIsInPolicy( frame ) )
        throw CARMA_EXCEPTION( IllegalArgumentException,
                               "Frame is not in policy!" );

    const ptime pt = ptimeFromFrame( frame );

    const FileTimeMap::const_iterator f = fileTimeMap_.find( pt );
    if ( f == fileTimeMap_.end() ) {
        throw CARMA_EXCEPTION( IllegalStateException,
            "FileTimeMap entry not found despite "
            "confirmation from AgingPolicy::frameIsInPolicy that it would be!"
            "  Something is very wrong." );
    }
    
    return directory_ + "/" + f->second;
}

void
AgingPolicy::updateFirstOpenTimeSlots( )
{
    ptime newestInPolicy;
    if ( !fileTimeMap_.empty() ) {
        newestInPolicy = fileTimeMap_.rbegin()->first;
    } else {
        newestInPolicy = policyStartDate_;
    }

    openTimeSlots_.clear();

    ptime endOfSlot = policyEndDate_;
    ptime startOfSlot = subtractTotalInterval( endOfSlot ); 

    while ( startOfSlot >= newestInPolicy ) {
        time_period timeSlot( startOfSlot, endOfSlot );
        openTimeSlots_.push_back( timeSlot );
        endOfSlot = startOfSlot;
        startOfSlot = subtractTotalInterval( endOfSlot );
    }
}
