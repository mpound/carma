
/**
 *
 * Simple test for comparing the free-standing subsystem with the 
 * subsystem from the carma tree.
 *
 * @author: N. S. Amarnath
 *
 * $Id: dumpSubsystemInstances.cc,v 1.14 2012/01/18 18:48:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.14 $ $Date: 2012/01/18 18:48:43 $
// @usage       dumpSubsystemInstances 
// @description
//      Simple test program for comparing data in a subsystem frames with
//      data from the corresponding frames in the carma frame.
//
// @noKeys
// @logger TEST_FACILITY carma.test.monitor.dumpSubsystemInstances
//

#include <iostream>
#include <iomanip>

#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/programLogging.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorSubsystemMaster.h"

#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/CentralIfSubsystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/DelayEngineSubsystem.h"
#include "carma/monitor/LineLengthSubsystem.h"
#include "carma/monitor/LoberotatorSubsystem.h"
#include "carma/monitor/LoRefSubsystem.h"
#include "carma/monitor/MasterClockSubsystem.h"
#include "carma/monitor/OpacityMonitorSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/PhaseMonitorSubsystem.h"
#include "carma/monitor/CarmaSlcBandSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/SzaSubsystem.h"
#include "carma/monitor/TestSubsystemExt.h"
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/monitor/WbcBandSubsystem.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"
#include "carma/monitor/WeatherSubsystem.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::monitor;


string 
toLower(const string& input)
{
    string output;
    output.resize(input.size());
    for (size_t i=0; i<input.size(); i++) {
        output[i] = tolower(input[i]);
    }
    return output;
}



MonitorSubsystem&
getSubsystemFromTree (CarmaMonitorSystem& carma, string subsystemName)
{
    string name = toLower(subsystemName);

    if (name == "bima1") return carma.bima (0);
    if (name == "bima2") return carma.bima (1);
    if (name == "bima3") return carma.bima (2);
    if (name == "bima4") return carma.bima (3);
    if (name == "bima5") return carma.bima (4);
    if (name == "bima6") return carma.bima (5);
    if (name == "bima7") return carma.bima (6);
    if (name == "bima8") return carma.bima (7);
    if (name == "bima9") return carma.bima (8);

    if (name == "ovro1") return carma.ovro (0);
    if (name == "ovro2") return carma.ovro (1);
    if (name == "ovro3") return carma.ovro (2);
    if (name == "ovro4") return carma.ovro (3);
    if (name == "ovro5") return carma.ovro (4);
    if (name == "ovro6") return carma.ovro (5);

    if (name == "sza1") return carma.sza (0);
    if (name == "sza2") return carma.sza (1);
    if (name == "sza3") return carma.sza (2);
    if (name == "sza4") return carma.sza (3);
    if (name == "sza5") return carma.sza (4);
    if (name == "sza6") return carma.sza (5);
    if (name == "sza7") return carma.sza (6);
    if (name == "sza8") return carma.sza (7);

    
    if (name == "centralif")    return carma.centralIf();
    if (name == "control")      return carma.control();
    if (name == "delayengine")  return carma.delay();
    if (name == "linelength")   return carma.lineLength();
    if (name == "loberotator")  return carma.loberotator();
    if (name == "loref")        return carma.loRef();
    if (name == "masterclock")  return carma.masterclock();
    if (name == "opacitymonitor") return carma.opacityMonitor();
    if (name == "phasemonitor") return carma.phaseMonitor();
    if (name == "sldc")         return carma.sldc();
    if (name == "wbdc")         return carma.wbdc();
    if (name == "weather")      return carma.weather();

    
    if (name == "slcband1")  return carma.carmaSlcBand (0);
    if (name == "slcband2")  return carma.carmaSlcBand (1);
    if (name == "slcband2")  return carma.carmaSlcBand (2);
    if (name == "slcband3")  return carma.carmaSlcBand (3);
    if (name == "slcband4")  return carma.carmaSlcBand (4);
    if (name == "slcband5")  return carma.carmaSlcBand (5);
    if (name == "slcband6")  return carma.carmaSlcBand (6);
    if (name == "slcband7")  return carma.carmaSlcBand (7);


    if (name == "wbcband1")  return carma.wbcBand (0);
    if (name == "wbcband2")  return carma.wbcBand (1);
    if (name == "wbcband3")  return carma.wbcBand (2);
    if (name == "wbcband4")  return carma.wbcBand (3);
    if (name == "wbcband5")  return carma.wbcBand (4);
    if (name == "wbcband6")  return carma.wbcBand (5);
    if (name == "wbcband7")  return carma.wbcBand (6);
    if (name == "wbcband8")  return carma.wbcBand (7);
    if (name == "wbcband9")  return carma.wbcBand (8);
    if (name == "wbcband10")  return carma.wbcBand (9);
    if (name == "wbcband11")  return carma.wbcBand (10);
    if (name == "wbcband12")  return carma.wbcBand (11);
    if (name == "wbcband13")  return carma.wbcBand (12);
    if (name == "wbcband14")  return carma.wbcBand (13);
    if (name == "wbcband15")  return carma.wbcBand (14);
    if (name == "wbcband16")  return carma.wbcBand (15);

    // Added pipeline subsystems later - Amar 01/13/2005
    if (name == "wbpipeline")  return carma.wbPipeline();
    if (name == "slpipeline")  return carma.slPipeline();

    if (name == "test")  return carma.test();

    {
        string msg;
    
        msg += "MonitorSubsystemMaster::makeSubsystem(\"";
        msg += subsystemName;
        msg += "\"): subsystem name not found.";
    
        programLogErrorIfPossible( msg );
    }
    
    MonitorSubsystem* ss = 0;
    return *ss;
}




int Program::main() 
{
    //bool canonical        = true;
    //bool verbose          = false;
    //bool value            = false;
    Time time;
    string subsys = "test";


    // Define system
    CarmaMonitorSystem&   carma = *new CarmaMonitorSystem();
    
    // Get input...
    string input = "";    
    cout << "Type in name of subsystem to dump (*=Test)" << endl;
    cin>>input;
    if (input != "*") {
       subsys = input;
    }
    MonitorSubsystem& subsystem = MonitorSubsystemMaster::makeSubsystem (subsys);
    
    // It is convenient to use a reference for the top of the tree
    // Get reference to subsystem from carma monitor tree
    const MonitorSubsystem&       t = getSubsystemFromTree (carma, subsys);

    cout.setf(ios::fixed);

    cout<<"------------------Test subsystem from carma tree-------------"<<endl;
    cout << t.hierarchyToString(true, true, true) << endl;
    cout<<"------------------End Test subsystem from carma tree-------------"<<endl;

    cout<<"-----------------Free-standing test subsystem----------------"<<endl;
    cout << subsystem.hierarchyToString(true, true, true) << endl;
    cout<<"-----------------End free-standing test subsystem----------------"<<endl;

    subsystem.configure();
    cout<<"---------Free-standing test subsystem - after reconfiguring--------"<<endl;
    cout << subsystem.hierarchyToString(true, true, true) << endl;
    cout<<"-------End Free-standing test subsystem - after reconfiguring------"<<endl;

    return 0;
}












