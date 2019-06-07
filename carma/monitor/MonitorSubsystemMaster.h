
#ifndef CARMA_MONITOR_MONITORSUBSYSTEMMASTER_H
#define CARMA_MONITOR_MONITORSUBSYSTEMMASTER_H

/**
 * @file
 *
 * Instantiates a monitor subsystem based on name.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorSubsystemMaster.h,v 1.4 2008/09/24 17:32:58 abeard Exp $
 * $CarmaCopyright$
 *
 */
 
#include "carma/monitor/MonitorSubsystem.h"


namespace carma {
    namespace monitor {


/**
 *
 * A factory to produce monitor subsystems based on name.
 * It has a single static method to produce a subsystem on the heap.
 *
 */
class MonitorSubsystemMaster {
public:

    /**
     * Destructor
     */
    virtual ~MonitorSubsystemMaster() {};
        
    /**
     * Produce a monitor subsystem on the heap
     * The subsystems  can be upcast if needed.
     * Note subsystems returned by this static method are leaked!
     * @param subsystemName case insensitive
     * @throw carma::util::IllegalArgumentException on invalid subsystemName.
     */
    static MonitorSubsystem& makeSubsystem(const std::string& subsystemName);

private:
    static std::string toLower(const std::string& input); 
};

} }  // End namespace carma::monitor  


#endif  // CARMA_MONITOR_MONITORSUBSYSTEMMASTER_H









