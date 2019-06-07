/**@file
 * Class responsible for communicating with the IMR and adding/configuring
 * any servers described in the XML config files. 
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.17 $
 * $Date: 2013/04/09 22:48:02 $
 * $Id: ConfigureImr.h,v 1.17 2013/04/09 22:48:02 abeard Exp $
 */

#ifndef CARMA_UTIL_CONFIGUREIMR_H
#define CARMA_UTIL_CONFIGUREIMR_H

#include "carma/util/ImrClient.h"
#include "carma/util/ImrConfigHandlers.h"

namespace carma {
namespace util {

/**
 * This class is responsible for taking server information retrieved from
 * the ImrConfigHandlers class (which reads it from an XML file) and adding
 * the servers to the IMR.  
 */
class ConfigureImr {
public:
    
    /**
     * Constructor
     * @param client Initialized corba::Client reference. 
     * @param imrHost hosntame of IMR
     * @param domain Structure containing oads/servers to be added to the IMR.
     */
    ConfigureImr(carma::corba::Client & client,
                 std::string imrHost,
                 carma::util::domainType domain);

    /**
     * Destructor
     */
    virtual ~ConfigureImr();

    /**
     * Add OADs
     * Take all oads input to this class and add them to the IMR.
     */
    void addOads();

    /**
     * Add Servers
     * Take all servers specific to the input OAD and add them to the IMR.
     * @param oad Hostname of oad to start servers on.
     * @param sleepyTime Time in milliseconds to sleep between adding servers.
     */
    void addServers( const std::string &oad, 
                     const std::string prioritySpec = "0-9",
                     unsigned long SleepyTimeMs = 0 );

    /**
     * Start servers
     * Start all servers associated with input OAD.
     * @param oad Hostname of oad to start servers on.
     * @param prioritySpec String specifying priority of machines to stop. 
     * Priority is specified by a number 0-9.  Ranges are specified with 
     * a '-' and are inclusive.  Startup is from highest (0) to lowest (9)
     * priority.
     * @param waitForRunningMs Time in milliseconds to wait for servers to 
     * transition to the RUNNING state.  Waiting starts AFTER all servers
     * in given priority range have been instructed to start.
     * @param sleepyTimeMs Time in ms to sleep between starting servers.
     * @param waitForEachPriorityStage Wait at most waitForRunningMs for all
     * servers in each priority level specified in prioritySpec to start.
     */
    void startServers( const std::string &oad, 
                       const std::string prioritySpec = "0-9",
                       unsigned int waitForRunningMs = 0,
                       unsigned long sleepyTimeMs = 0,
                       bool waitForEachPriorityStage = false,
                       unsigned long waitAfterEachStageMs = 0,
                       bool verbose = true );

    /**
     * Stop servers
     * Stop all servers associated with input OAD.
     * @param oad String identifying OAD.
     * @param prioritySpec String specifying priority of machines to stop. 
     * Priority is specified by a number 0-9.  Ranges are specified with 
     * a '-' and are inclusive. Shutdown is from lowest (9) to highest (0)
     * priority.
     * @param waitForStoppedMs Max number of milliseconds to wait for a server
     * to transition to STOPPED state. Waiting starts AFTER all servers 
     * in a given priority range have been instructed to terminate.
     * @param waitForEachPriorityStage Wait at most waitForRunningMs for all
     * servers in each priority level specified in prioritySpec to stop.
     */
    void stopServers( const std::string &oad, 
                      const std::string prioritySpec = "9-0",
                      unsigned int waitForStoppedMs = 0,
                      bool waitForEachPriorityStage = false );

    /**
     * Reset servers
     * Reset all servers associated with input OAD.
     */
    void resetServers(const std::string &oad);
    
    /**
     * Clean IMR of currently registered servers and non-running oads
     * This routine stops all currently running servers on the imr and then
     * removes the servers and oad entries from the IMR.
     */
    void clean(const std::string &oad);

protected:
    
    // Nothing here

private:
    
    void waitForServers( const std::string & oad, 
                         ImrClient::ServerStatus state, // State to wait for.
                         const std::pair<int, int> & inPriorityRange,
                         long waitMaxMs );

    std::vector<OADConfig>::iterator findOAD(const std::string &oad);

    // Reference to IMR Client
    carma::util::ImrClient imr_;
    std::string imrHost_;
    domainType domain_;

};  // End class ConfigureImr
}}  // End namespace carma::util;
#endif
