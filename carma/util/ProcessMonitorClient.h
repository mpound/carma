#ifndef CARMA_UTIL_PROCESSMONITORCLIENT_H
#define CARMA_UTIL_PROCESSMONITORCLIENT_H

#include <memory>

namespace carma {
namespace util {

class Orb;

/**
 * Class to continuously ping the process monitor.
 * An instance of this class will notify the process monitor it is alive at 
 * the specified update period in a background thread. If the processMonitor
 * (e.g. it's not running yet, has died, etc), we just sit and retry at the 
 * specified interval.
 * @see carma::util::ProcessMonitor 
 */
class ProcessMonitorClient {
public:

    /** 
     * Constructor
     * @pre serverId must be specified on the command line with -ORBServerId.
     * @pre imr must be specified on the command line.
     * @throw carma::util::ErrorException if preconditions aren't met.
     */
    explicit ProcessMonitorClient( );

    explicit ProcessMonitorClient( carma::util::Orb * orb );
    
    /**
     * Destructor
     */
    virtual ~ProcessMonitorClient( );

private:
     
    ProcessMonitorClient( const ProcessMonitorClient & );
    ProcessMonitorClient & operator=( const ProcessMonitorClient & );

    class Private;
    std::auto_ptr< Private > private_;

}; 

}} // namespace carma::util

#endif
