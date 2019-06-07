#ifndef CARMA_CONTROL_AGINGPOLICY_H
#define CARMA_CONTROL_AGINGPOLICY_H

#include "carma/util/types.h"

#include <boost/date_time/gregorian/gregorian_types.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <map>
#include <string>
#include <vector>

namespace carma {
namespace control {

class StateManager;

// Map to resolve string names into posix time objects.
// Note list is automatically sorted by frame with the oldest files first.
typedef std::map< boost::posix_time::ptime, std::string > FileTimeMap;
typedef FileTimeMap::iterator FileTimeMapIter;
typedef FileTimeMap::const_iterator FileTimeMapConstIter;
    
/**
 * Class to describe an aging policy and associated methods for adding
 * and removing files from it.  A policy is defined by a time interval and
 * duration.  The interval defines the desired time resolution of files
 * within the policy and the duration defines how long the policy should
 * be applied.  A policy is complete only when it is defined in relation to
 * other policies via a policy chain.  Files are aged out of policies and
 * become candidates for inclusion into the next adjacent policy in the chain.
 * An obvious side effect of this is that the interval of the first policy
 * and the duration of the last policy don't apply and aren't used.
 * There are holes in the way this all works - notably I don't exhaustively 
 * attempt to pair a candidate with every possible open slot in subsequent
 * policies.  Rather a file aged from a policy is a candidate for inclusion 
 * into the first open slots of the next policy only.  This is fair since this 
 * program both produces the state files and ages them (e.g they are always
 * both running together).  Another weakness is that I don't attempt to 
 * validate policies relative to one another, thus one could easily define 
 * policies in a way that would starve subsequent policies - this case is 
 * left to the user's good common sense.  Finally I could automatically
 * name the associated directories but have put this on the backburner for now.
 */
class AgingPolicy {
public:
    
    AgingPolicy( const std::string & directory,
                 ::boost::posix_time::time_duration interval,
                 int intervalDays, int intervalWeeks,int intervalMonths,
                 int durationDays, int durationMonths, int durationYears );
    
    std::string getDirectory( ) const;

    // Sync internal data structures to input time.
    void syncToTime( const boost::posix_time::ptime & policyEndDate ); 

    // Apply the policy to retrieve a map of files which are candidates
    // for deletion and no longer fit the policy.  Aged files are 
    // candidates for ingestion into subsequent policies.
    FileTimeMap reapAgedCandidates( );

    // Given a single candidate, either move it into the policy or delete it.
    // If deleted, the corresponding frame is removed from the manager's index.
    void
    handleCandidate( const FileTimeMap::value_type candidate,
                     AgingPolicy & previousPolicy,
                     StateManager & manager );

    boost::posix_time::ptime 
    subtractTotalDuration( const boost::posix_time::ptime & to ) const;

    boost::posix_time::ptime 
    subtractTotalInterval( const boost::posix_time::ptime & to ) const;
    
    bool frameIsInPolicy( carma::util::frameType frame ) const; 

    std::string getFilenameForFrame( const carma::util::frameType frame ) const;

private:
    

    // Vector of open time slots for this policy 
    typedef std::vector<boost::posix_time::time_period> TimeSlots;
    void updateFirstOpenTimeSlots( );

    void traceDeletion( const FileTimeMap::value_type & candidate,
                        const AgingPolicy & previousPolicy,
                        const std::string & spiel );

    void traceOpenTimeSlots( );

    void removeFileFromPolicy( FileTimeMap::value_type entry,
                               StateManager & manager );

    std::string directory_;
    boost::posix_time::time_duration interval_;
    boost::gregorian::days intervalDays_;
    boost::gregorian::weeks intervalWeeks_;
    boost::gregorian::months intervalMonths_;
    boost::gregorian::days durationDays_;
    boost::gregorian::months durationMonths_;
    boost::gregorian::years durationYears_;

    FileTimeMap fileTimeMap_; 
    TimeSlots openTimeSlots_;
    boost::posix_time::ptime policyStartDate_; // Valid any given update cycle.
    boost::posix_time::ptime policyEndDate_; // Valid any given update cycle. 
    boost::posix_time::time_period policyPeriod_; // Ditto 

}; // Class AgingPolicy

typedef std::vector< AgingPolicy > PolicyChain;
            
PolicyChain createAgingPolicyChain( const std::string & dirname );

std::string retrieveFilenameForFrame( const PolicyChain & policies,
                                      carma::util::frameType frame ); 

} // namespace control
} // namespace carma
#endif
