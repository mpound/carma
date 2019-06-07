/**
 *
 * Implementation for a monitor system device.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorDevice.cc,v 1.5 2004/03/04 16:16:55 scott Exp $
 * $CarmaCopyright$
 *
 */



#include <iomanip>

#include "carma/monitor/MonitorDevice.h"


using namespace std;
using namespace carma::monitor;



MonitorDevice::MonitorDevice(const string& name) : 
    MonitorContainer(name)
{
}

MonitorDevice::MonitorDevice(const string& name, 
        PhysicalDevice& physicalDevice) : 
    MonitorContainer(name), physicalDevice_(&physicalDevice)
{
}

MonitorDevice::~MonitorDevice()
{    
}

PhysicalDevice& MonitorDevice::getPhysicalDevice() const
{
	return *physicalDevice_;
}

void MonitorDevice::setPhysicalDevice(PhysicalDevice& physicalDevice)
{
	physicalDevice_ = &physicalDevice;
}

string MonitorDevice::getPhysicalDeviceName() const 
{
    if (physicalDevice_ == 0) return "";
    return physicalDevice_->toString();
}











