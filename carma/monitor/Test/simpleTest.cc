
/**
 *
 * Unit test for monitor hierarchy.
 *
 * @author: Steve Scott
 *
 * $Id: simpleTest.cc,v 1.1 2003/12/11 00:46:02 scott Exp $
 * $CarmaCopyright$
 *
 */



#include <iomanip>

#include "carma/util/Program.h"
#include "carma/monitor/OvroSubsystem.h"


using namespace std;
using namespace carma::monitor;



int Program::main() 
{
    MonitorPointSet&      a= MonitorPointSet::getMonitorPointSet(14);

    unsigned short ssid = a.getSubsystemID();	
    cout<<"SSid:"<<ssid<<endl;

    return 0;
}












