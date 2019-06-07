
/**
 *
 * Implementation for the monitor system container class.
 * This class is mainly an interface, so there is not much to
 * the implementation.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorSystemContainer.cc,v 1.7 2011/04/19 23:35:28 abeard Exp $
 * $CarmaCopyright$
 *
 */


#include <string>

#include "carma/monitor/MonitorSystemContainer.h"
#include "carma/util/Time.h"


using namespace carma::monitor;
using namespace carma::util;



MonitorSystemContainer::MonitorSystemContainer(const std::string& systemName):
	MonitorContainer(systemName)
{
} 

MonitorSystemContainer::~MonitorSystemContainer()
{
} 


bool
MonitorSystemContainer::readNewestIfStale()
{
    bool result = false;
    
    if ( isCurrent() == false )
        result = readNewest();
        
    return result;
}


bool MonitorSystemContainer::isActive()
{
    if (!readNewest()) return false;
    int timeDiff = Time::computeCurrentFrame() - getFrameCount();
    if (abs(timeDiff) > 8) return false;
    return true;
} 
   








