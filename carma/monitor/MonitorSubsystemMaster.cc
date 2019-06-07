
/**
 *
 * Implementation for a class to create monitor subsystems
 *
 * @author: Steve Scott
 *
 * $Id: MonitorSubsystemMaster.cc,v 1.39 2014/06/20 15:55:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

#include <algorithm>

//#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/monitor/MonitorSubsystemMaster.h"

#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/C3gMax8PipelineSubsystemExt.h"
#include "carma/monitor/C3gMax23PipelineSubsystemExt.h"
#include "carma/monitor/CentralIfSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/DataflowSubsystem.h"
#include "carma/monitor/DelayEngineSubsystem.h"
#include "carma/monitor/ImrSubsystem.h"
#include "carma/monitor/AlarmSubsystem.h"
#include "carma/monitor/LineLengthSubsystem.h"
#include "carma/monitor/LoberotatorSubsystem.h"
#include "carma/monitor/LoRefSubsystem.h"
#include "carma/monitor/MasterClockSubsystem.h"
#include "carma/monitor/OpacityMonitorSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/PhaseMonitorSubsystem.h"
#include "carma/monitor/ProjectDatabaseManagerSubsystem.h"
#include "carma/monitor/CarmaSlcBandSubsystem.h"
#include "carma/monitor/Carma3GSubsystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/SzaSubsystem.h"
#include "carma/monitor/SystemStatus.h"
#include "carma/monitor/TestSubsystem.h"
#include "carma/monitor/VlbiSubsystem.h"
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/monitor/WbcBandSubsystem.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"
#include "carma/monitor/WeatherSubsystem.h"
#include "carma/monitor/WbDataflowSubsystem.h"
#include "carma/monitor/SlDataflowSubsystem.h"
#include "carma/monitor/C3gDataflowSubsystem.h"
#include "carma/monitor/WbRemapperSubsystem.h"
#include "carma/monitor/SlRemapperSubsystem.h"
#include "carma/monitor/C3gRemapperSubsystem.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;


MonitorSubsystem&
MonitorSubsystemMaster::makeSubsystem(const string& subsystemName)
{
    // When you add a comparison for a new monitor subsystem,
    // make sure the comparison is to a purely lower case string!

    string name = toLower(subsystemName);

    if (name == "astro") return *new AstroSubsystem();
    if (name == "bima1") return *new BimaSubsystem(1);
    if (name == "bima2") return *new BimaSubsystem(2);
    if (name == "bima3") return *new BimaSubsystem(3);
    if (name == "bima4") return *new BimaSubsystem(4);
    if (name == "bima5") return *new BimaSubsystem(5);
    if (name == "bima6") return *new BimaSubsystem(6);
    if (name == "bima7") return *new BimaSubsystem(7);
    if (name == "bima8") return *new BimaSubsystem(8);
    if (name == "bima9") return *new BimaSubsystem(9);

    if (name == "ovro1") return *new OvroSubsystem(1);
    if (name == "ovro2") return *new OvroSubsystem(2);
    if (name == "ovro3") return *new OvroSubsystem(3);
    if (name == "ovro4") return *new OvroSubsystem(4);
    if (name == "ovro5") return *new OvroSubsystem(5);
    if (name == "ovro6") return *new OvroSubsystem(6);

    if (name == "sza1") return *new SzaSubsystem(1);
    if (name == "sza2") return *new SzaSubsystem(2);
    if (name == "sza3") return *new SzaSubsystem(3);
    if (name == "sza4") return *new SzaSubsystem(4);
    if (name == "sza5") return *new SzaSubsystem(5);
    if (name == "sza6") return *new SzaSubsystem(6);
    if (name == "sza7") return *new SzaSubsystem(7);
    if (name == "sza8") return *new SzaSubsystem(8);

    if (name == "c3gmax8pipeline")  return *new C3gMax8PipelineSubsystem(); 
    if (name == "c3gmax23pipeline") return *new C3gMax23PipelineSubsystem(); 
    if (name == "centralif")      return *new CentralIfSubsystem();
    if (name == "control")        return *new ControlSubsystem();
    if (name == "dataflow")       return *new DataflowSubsystem();
    if (name == "delayengine")    return *new DelayEngineSubsystem();
    if (name == "alarm")          return *new AlarmSubsystem();
    if (name == "linelength")     return *new LineLengthSubsystem();
    if (name == "loberotator")    return *new LoberotatorSubsystem();
    if (name == "loref")          return *new LoRefSubsystem();
    if (name == "masterclock")    return *new MasterClockSubsystem();
    if (name == "opacitymonitor") return *new OpacityMonitorSubsystem();
    if (name == "phasemonitor")   return *new PhaseMonitorSubsystem();
    if (name == "signalpath")     return *new SignalPathSubsystem();
    if (name == "sldc")           return *new SldcSubsystem();
    if (name == "systemstatus")   return *new SystemStatusSubsystem();
    if (name == "wbdc")           return *new WbdcSubsystem();
    if (name == "weather")        return *new WeatherSubsystem();

    if (name == "carmaslcband1")  return *new CarmaSlcBandSubsystem(1);
    if (name == "carmaslcband2")  return *new CarmaSlcBandSubsystem(2);
    if (name == "carmaslcband3")  return *new CarmaSlcBandSubsystem(3);
    if (name == "carmaslcband4")  return *new CarmaSlcBandSubsystem(4);
    if (name == "carmaslcband5")  return *new CarmaSlcBandSubsystem(5);
    if (name == "carmaslcband6")  return *new CarmaSlcBandSubsystem(6);
    if (name == "carmaslcband7")  return *new CarmaSlcBandSubsystem(7);
    if (name == "carmaslcband8")  return *new CarmaSlcBandSubsystem(8);

    if (name == "carma3gband1")  return *new Carma3GBandSubsystem(1);
    if (name == "carma3gband2")  return *new Carma3GBandSubsystem(2);
    if (name == "carma3gband3")  return *new Carma3GBandSubsystem(3);
    if (name == "carma3gband4")  return *new Carma3GBandSubsystem(4);
    if (name == "carma3gband5")  return *new Carma3GBandSubsystem(5);
    if (name == "carma3gband6")  return *new Carma3GBandSubsystem(6);
    if (name == "carma3gband7")  return *new Carma3GBandSubsystem(7);
    if (name == "carma3gband8")  return *new Carma3GBandSubsystem(8);
    if (name == "carma3gband9")  return *new Carma3GBandSubsystem(9);
    if (name == "carma3gband10")  return *new Carma3GBandSubsystem(10);
    if (name == "carma3gband11")  return *new Carma3GBandSubsystem(11);
    if (name == "carma3gband12")  return *new Carma3GBandSubsystem(12);
    if (name == "carma3gband13")  return *new Carma3GBandSubsystem(13);
    if (name == "carma3gband14")  return *new Carma3GBandSubsystem(14);
    if (name == "carma3gband15")  return *new Carma3GBandSubsystem(15);
    if (name == "carma3gband16")  return *new Carma3GBandSubsystem(16);

    if (name == "wbcband1")   return *new WbcBandSubsystem(1);
    if (name == "wbcband2")   return *new WbcBandSubsystem(2);
    if (name == "wbcband3")   return *new WbcBandSubsystem(3);
    if (name == "wbcband4")   return *new WbcBandSubsystem(4);
    if (name == "wbcband5")   return *new WbcBandSubsystem(5);
    if (name == "wbcband6")   return *new WbcBandSubsystem(6);
    if (name == "wbcband7")   return *new WbcBandSubsystem(7);
    if (name == "wbcband8")   return *new WbcBandSubsystem(8);
    if (name == "wbcband9")   return *new WbcBandSubsystem(9);
    if (name == "wbcband10")  return *new WbcBandSubsystem(10);
    if (name == "wbcband11")  return *new WbcBandSubsystem(11);
    if (name == "wbcband12")  return *new WbcBandSubsystem(12);
    if (name == "wbcband13")  return *new WbcBandSubsystem(13);
    if (name == "wbcband14")  return *new WbcBandSubsystem(14);
    if (name == "wbcband15")  return *new WbcBandSubsystem(15);
    if (name == "wbcband16")  return *new WbcBandSubsystem(16);

    // Added pipeline subsystems later - Amar 01/13/2005
    if (name == "wbpipeline")  return *new WbPipelineSubsystem();
    if (name == "slpipeline")  return *new SlPipelineSubsystem();

    if (name == "imr") return *new ImrSubsystem();

    if (name == "test")  return *new TestSubsystem();

    if (name == "vlbi")  return *new VlbiSubsystem();
    
    if (name == "projectdatabasemanager")
        return *new ProjectDatabaseManagerSubsystem();

    if ( name == "wbdataflow1" ) return *new WbDataflowSubsystem(1);
    if ( name == "wbdataflow2" ) return *new WbDataflowSubsystem(2);
    if ( name == "wbdataflow3" ) return *new WbDataflowSubsystem(3);
    if ( name == "wbdataflow4" ) return *new WbDataflowSubsystem(4);
    if ( name == "wbdataflow5" ) return *new WbDataflowSubsystem(5);
    if ( name == "wbdataflow6" ) return *new WbDataflowSubsystem(6);
    if ( name == "wbdataflow7" ) return *new WbDataflowSubsystem(7);
    if ( name == "wbdataflow8" ) return *new WbDataflowSubsystem(8);
    if ( name == "wbdataflow9" ) return *new WbDataflowSubsystem(9);
    if ( name == "wbdataflow10" ) return *new WbDataflowSubsystem(10);
    if ( name == "wbdataflow11" ) return *new WbDataflowSubsystem(11);
    if ( name == "wbdataflow12" ) return *new WbDataflowSubsystem(12);
    if ( name == "wbdataflow13" ) return *new WbDataflowSubsystem(13);
    if ( name == "wbdataflow14" ) return *new WbDataflowSubsystem(14);
    if ( name == "wbdataflow15" ) return *new WbDataflowSubsystem(15);
    if ( name == "wbdataflow16" ) return *new WbDataflowSubsystem(16);

    if ( name == "sldataflow1" ) return *new SlDataflowSubsystem(1);
    if ( name == "sldataflow2" ) return *new SlDataflowSubsystem(2);
    if ( name == "sldataflow3" ) return *new SlDataflowSubsystem(3);
    if ( name == "sldataflow4" ) return *new SlDataflowSubsystem(4);
    if ( name == "sldataflow5" ) return *new SlDataflowSubsystem(5);
    if ( name == "sldataflow6" ) return *new SlDataflowSubsystem(6);
    if ( name == "sldataflow7" ) return *new SlDataflowSubsystem(7);
    if ( name == "sldataflow8" ) return *new SlDataflowSubsystem(8);

    if ( name == "c3gdataflow1" ) return *new C3gDataflowSubsystem(1);
    if ( name == "c3gdataflow2" ) return *new C3gDataflowSubsystem(2);
    if ( name == "c3gdataflow3" ) return *new C3gDataflowSubsystem(3);
    if ( name == "c3gdataflow4" ) return *new C3gDataflowSubsystem(4);
    if ( name == "c3gdataflow5" ) return *new C3gDataflowSubsystem(5);
    if ( name == "c3gdataflow6" ) return *new C3gDataflowSubsystem(6);
    if ( name == "c3gdataflow7" ) return *new C3gDataflowSubsystem(7);
    if ( name == "c3gdataflow8" ) return *new C3gDataflowSubsystem(8);
    if ( name == "c3gdataflow9" ) return *new C3gDataflowSubsystem(9);
    if ( name == "c3gdataflow10" ) return *new C3gDataflowSubsystem(10);
    if ( name == "c3gdataflow11" ) return *new C3gDataflowSubsystem(11);
    if ( name == "c3gdataflow12" ) return *new C3gDataflowSubsystem(12);
    if ( name == "c3gdataflow13" ) return *new C3gDataflowSubsystem(13);
    if ( name == "c3gdataflow14" ) return *new C3gDataflowSubsystem(14);
    if ( name == "c3gdataflow15" ) return *new C3gDataflowSubsystem(15);
    if ( name == "c3gdataflow16" ) return *new C3gDataflowSubsystem(16);

    if ( name == "wbremapper1" )  return *new WbRemapperSubsystem(1);
    if ( name == "wbremapper2" )  return *new WbRemapperSubsystem(2);
    if ( name == "wbremapper3" )  return *new WbRemapperSubsystem(3);
    if ( name == "wbremapper4" )  return *new WbRemapperSubsystem(4);
    if ( name == "wbremapper5" )  return *new WbRemapperSubsystem(5);
    if ( name == "wbremapper6" )  return *new WbRemapperSubsystem(6);
    if ( name == "wbremapper7" )  return *new WbRemapperSubsystem(7);
    if ( name == "wbremapper8" )  return *new WbRemapperSubsystem(8);
    if ( name == "wbremapper9" )  return *new WbRemapperSubsystem(9);
    if ( name == "wbremapper10" ) return *new WbRemapperSubsystem(10);
    if ( name == "wbremapper11" ) return *new WbRemapperSubsystem(11);
    if ( name == "wbremapper12" ) return *new WbRemapperSubsystem(12);
    if ( name == "wbremapper13" ) return *new WbRemapperSubsystem(13);
    if ( name == "wbremapper14" ) return *new WbRemapperSubsystem(14);
    if ( name == "wbremapper15" ) return *new WbRemapperSubsystem(15);
    if ( name == "wbremapper16" ) return *new WbRemapperSubsystem(16);

    if ( name == "slremapper1" )  return *new SlRemapperSubsystem(1);
    if ( name == "slremapper2" )  return *new SlRemapperSubsystem(2);
    if ( name == "slremapper3" )  return *new SlRemapperSubsystem(3);
    if ( name == "slremapper4" )  return *new SlRemapperSubsystem(4);
    if ( name == "slremapper5" )  return *new SlRemapperSubsystem(5);
    if ( name == "slremapper6" )  return *new SlRemapperSubsystem(6);
    if ( name == "slremapper7" )  return *new SlRemapperSubsystem(7);
    if ( name == "slremapper8" )  return *new SlRemapperSubsystem(8);

    if ( name == "c3gremapper1" )  return *new C3gRemapperSubsystem(1);
    if ( name == "c3gremapper2" )  return *new C3gRemapperSubsystem(2);
    if ( name == "c3gremapper3" )  return *new C3gRemapperSubsystem(3);
    if ( name == "c3gremapper4" )  return *new C3gRemapperSubsystem(4);
    if ( name == "c3gremapper5" )  return *new C3gRemapperSubsystem(5);
    if ( name == "c3gremapper6" )  return *new C3gRemapperSubsystem(6);
    if ( name == "c3gremapper7" )  return *new C3gRemapperSubsystem(7);
    if ( name == "c3gremapper8" )  return *new C3gRemapperSubsystem(8);
    if ( name == "c3gremapper9" )  return *new C3gRemapperSubsystem(9);
    if ( name == "c3gremapper10" ) return *new C3gRemapperSubsystem(10);
    if ( name == "c3gremapper11" ) return *new C3gRemapperSubsystem(11);
    if ( name == "c3gremapper12" ) return *new C3gRemapperSubsystem(12);
    if ( name == "c3gremapper13" ) return *new C3gRemapperSubsystem(13);
    if ( name == "c3gremapper14" ) return *new C3gRemapperSubsystem(14);
    if ( name == "c3gremapper15" ) return *new C3gRemapperSubsystem(15);
    if ( name == "c3gremapper16" ) return *new C3gRemapperSubsystem(16);

    // When you add a comparison for a new monitor subsystem,
    // make sure the comparison is to a purely lower case string!

    const string errorMsg = "MonitorSubsystemMaster::makeSubsystem(\""
        + subsystemName + "\"): subsystem name not found.";

    util::programLogErrorIfPossible( errorMsg );

    throw CARMA_EXCEPTION( util::IllegalArgumentException, errorMsg );
}


string MonitorSubsystemMaster::toLower(const string& input) 
{
    string output;
    output.resize(input.size());
    for (size_t i=0; i<input.size(); i++) {
        output[i] = tolower(input[i]);
    }
    return output;
}
