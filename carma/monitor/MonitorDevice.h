
#ifndef CARMA_MONITOR_MONITORDEVICE_H
#define CARMA_MONITOR_MONITORDEVICE_H


/**
 * @file
 *
 * A monitor system device, used in the construction of monitor
 * points and containers that are based on physical devices.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorDevice.h,v 1.5 2004/03/04 16:16:55 scott Exp $
 * $CarmaCopyright$
 *
 */


#include "carma/monitor/MonitorContainer.h"
#include "carma/monitor/PhysicalDevice.h"


namespace carma {
    namespace monitor {


/**
 * A monitor system device, used in the construction of monitor
 * points and containers that are based on physical devices.
 */
class MonitorDevice: public MonitorContainer {
public:

    /**
     * Constructor
     * @param name MonitorDevice name
     */
    MonitorDevice(const std::string& name);

    /**
     * Constructor
     * @param name MonitorDevice name
     * @param physicalDevice physical device
     */
    MonitorDevice(const std::string& name, 
            PhysicalDevice& physicalDevice);

    /**
     * Destructor
     */
    virtual ~MonitorDevice() ;

    /**
     * Get the physical device
     */
    PhysicalDevice& getPhysicalDevice() const ;

    /**
     * Set the physical device
     * @param physicalDevice pointer to the physical device for this sense point
     */
    void setPhysicalDevice(PhysicalDevice& physicalDevice);

    // Virtual - inherits documentation
    std::string getPhysicalDeviceName() const ;
private:
    PhysicalDevice*  physicalDevice_;
    MonitorDevice(); // Default constructor is private!
};

} }  // End namespace carma::monitor  


#endif  // CARMA_MONITOR_MONITORDEVICE_H








