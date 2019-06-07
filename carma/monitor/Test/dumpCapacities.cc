
/**
 * @file dumpCapacities.cc
 *
 * Dumps capacity related information for the monitor system (and for any 
 * specified subsystem). Prints the following information to stdout -
 *   o maximum available subsystems
 *   o maximum available monitor points (across all subsystems)
 *   o maximum available monitor samples (across all subsystems)
 *   o maximum available size, in bytes (across all subsystems)
 *   o maximum allocated subsystems
 *   o cumulative maximum monitor points (across all subsystems)
 *   o cumulative maximum monitor samples (across all subsystems)
 *   o cumulative max size, in bytes (across all subsystems)
 *   o actual number of subsystems
 *   o actual number of monitor points (across all subsystems)
 *   o actual number of monitor samples (across all subsystems)
 *   o actual size, in bytes (across all subsystems)
 *
 * @author: Amar Amarnath
 *
 * $Id: dumpCapacities.cc,v 1.20 2010/07/21 17:58:28 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */

// Keyword setup...
// @version     $Revision: 1.20 $ $Date: 2010/07/21 17:58:28 $
// @usage dumpCapacities [mode=\"raw\"/\"final\"] [subsystem=none|all|subsystem name] [types=f/t] [addresses=f/t] [verbose=f/t]
// @key mode final string \"raw\" or \"final\" - reads \"raw\" IPQ or \"final\" IPQ
// @key subsystem none string all subsystems, or a specific subsystem, or none
// @key types false bool if true, dump monitor point information categorized by type
// @key addresses false bool if true, and subsystems != none dump subsystem frame start addresses, 
// @key verbose false bool if true, dump all storage related info
//
// @description
// Dumps capacity related information for the monitor system.
// Prints the following information to stdout -
//   o maximum available subsystems
//   o maximum available monitor points (across all subsystems)
//   o maximum available monitor samples (across all subsystems)
//   o maximum available size, in bytes (across all subsystems)
//   o maximum allocated subsystems
//   o cumulative maximum # of monitor points (across all subsystems)
//   o cumulative maximum # of monitor samples (across all subsystems)
//   o cumulative maximum size, in bytes (across all subsystems)
//   o actual number of subsystems
//   o actual number of monitor points (across all subsystems)
//   o (optionally) actual number of monitor points (segregated by monitor sample value type)
//   o actual number of monitor samples (across all subsystems)
//   o actual size, in bytes (across all subsystems).
//
// @logger MONITOR_FACILITY carma.monitor.dumpCapacities
//

#include <iomanip>
#include <iostream>

#include "carma/util/checking.h"
#include "carma/util/Program.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SubsystemFrame.h"

#define EXIT_SUCCESS  0

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


typedef struct {
    MonitorValueType type;
    string      typeName;
} EnumTabEntry;


const EnumTabEntry kEnumTab[ ] = {
    { MONITOR_VALUE_TYPE_BYTE,           "Byte"       },
    { MONITOR_VALUE_TYPE_SHORT,          "Short"      },
    { MONITOR_VALUE_TYPE_INTEGER,        "Integer"    },
    { MONITOR_VALUE_TYPE_BOOLEAN,        "Boolean"    },
    { MONITOR_VALUE_TYPE_FLOAT,          "Float"      },
    { MONITOR_VALUE_TYPE_DOUBLE,         "Double"     },
    { MONITOR_VALUE_TYPE_COMPLEX,        "Complex"    },
    { MONITOR_VALUE_TYPE_STRING,         "String"     },
    { MONITOR_VALUE_TYPE_SERIAL_NUMBER,  "Serial No." }
};


::size_t
numElems( )  {
    return (sizeof( kEnumTab ) / sizeof( kEnumTab[ 0 ] ));
}


MonitorValueType
valueType( const ::size_t index ) {
    CARMA_CHECK( index < numElems( ) );

    return kEnumTab[ index ].type;
}


const string &
valueType2String( const MonitorValueType type ) {
    const ::size_t nElems = numElems( );

    ::size_t i = 0;
    
    while ( (i < nElems)  &&  (type != kEnumTab[i].type) )
        ++i;

    CARMA_CHECK( i < nElems );

    return kEnumTab[ i ].typeName;
}


} // namespace < anonymous >


int
Program::main( ) {
    const bool typedInfoReqd = getBoolParameter ("types");
    const bool printAddresses = getBoolParameter ("addresses");
    const bool verbose = getBoolParameter ("verbose");
    const string subsystemName = getStringParameter ("subsystem");

     // Define monitor system
    MonitorSystem* ms;
    
    if (getStringParameter ("mode") == string ("raw")) {
        ms = new RawCarmaMonitorSystem();
    }  else  {
        ms = new CarmaMonitorSystem();
    }

    const string indent ("      "); // 6 spaces
    cout << "Carma Monitor System capacities and usage"
              << "\n" << indent
              << "Total available subsystems = " << ms->getTotalNumSubsystems()
              << "\n" << indent
              << "Total available monitor points = " << ms->getTotalNumMonitorPoints()
              << "\n" << indent
              << "Total available monitor samples = " << ms->getTotalNumMonitorSamples()
              << "\n" << indent
              << "Total available size (in bytes) = " << ms->getTotalMonitorSystemSize()
              << "\n" << indent
              << "Cumulative allocated subsystems = " << ms->getAllocatedNumSubsystems()
              << "\n" << indent
              << "Cumulative max monitor points = " << ms->getMaxNumMonitorPoints()
              << "\n" << indent
              << "Cumulative max monitor samples = " << ms->getMaxNumMonitorSamples()
              << "\n" << indent
              << "Cumulative max size (in bytes) = " << ms->getMaxMonitorSystemSize()
              << "\n" << indent
              << "Cumulative actual subsystems = " << ms->getActualNumSubsystems()
              << "\n" << indent
              << "Cumulative actual monitor points = " << ms->getActualNumMonitorPoints()
              << "\n" << indent
              << "Cumulative actual multi-sample monitor points = " << ms->getActualNumMultiSampleMonitorPoints()
              << "\n" << indent
              << "Cumulative actual monitor samples = " << ms->getActualNumMonitorSamples()
              << "\n" << indent
              << "Cumulative actual size (in bytes) = " << ms->getActualMonitorSystemSize() ;
    if (printAddresses)  {
#if 0
        SubsystemHeader * ptr =
            ((SubsystemHeader *)&(ms->systemFrameBuffer().getFrame().getSystemFrame().subsystemFrameOffset[ms->getTotalNumSubsystems()-1]) +
            sizeof(size_t));
        cout << "\n" << indent << "Starting address of subsystems = " << ptr;
#else
        cout << "\n" << indent << "Starting address of subsystems = ???";
#endif
    }
    cout << endl;

    if (typedInfoReqd)  {
       cout << "\nCarma Monitor System usage by monitor value type:";
       
       const ::size_t nElems = numElems( );
       for ( ::size_t i = 0;  i < nElems;  ++i )  {
             const MonitorValueType type = valueType( i );
             
             cout << "\n" << indent
                       << "Cumulative actual monitor points of type " 
                       << valueType2String (type) << " = "
                       << ms->getActualNumMonitorPoints (type)
                       << "\n" << indent
                       << "Cumulative actual monitor samples of type " 
                       << valueType2String (type) << " = "
                       << ms->getActualNumMonitorSamples (type);
       }

       cout << endl;
    }

    if (verbose)  {
        cout << "\nMore details : "
                  << "\n" << indent
                  << "Cumulative allocated # of subsystems " << ms->getAllocatedNumSubsystems()
                  << "\n" << indent
                  << "Cumulative allocated # of monitor points " << ms->getAllocatedNumMonitorPoints()
                  << "\n" << indent
                  << "Cumulative allocated # of monitor samples " << ms->getAllocatedNumMonitorSamples()
                  << "\n" << indent
                  << "Cumulative allocated size (in bytes) " << ms->getAllocatedMonitorSystemSize()
                  << "\n" << indent
                  << "Cumulative count of monitor points " << ms->getCountedNumMonitorPoints()
                  << "\n" << indent
                  << "Cumulative count of monitor samples " << ms->getCountedNumMonitorSamples()
                  << "\n" << indent
                  << "Cumulative counted system size (in bytes) " << ms->getCountedMonitorSystemSize()
                  << endl;
    }
    // check cumulative size against total frame size
    size_t cumulativeSize = 0;
    for (int i = 0;  i < ms->getNumChildren();  i++)  {
        MonitorSubsystem * const msPtr = ms->getChild(i).subsystemPtr();
        if (msPtr != NULL)  {
            SubsystemFrame& ssFrame = 
                                msPtr->monitorPointSet().getSubsystemFrame();
            long maxMonitorPoints = ssFrame.getMaxNumMonitorPoints();
            long maxMonitorSamples = ssFrame.getMaxNumSamples();
            size_t maxSubsystemSize = 
                SubsystemFrame::sizeFrame(maxMonitorPoints, maxMonitorSamples);
            cumulativeSize += maxSubsystemSize;
        }
    }
    if (cumulativeSize > ms->getTotalMonitorSystemSize())  {
        cerr << "WARNING ! Total available system frame size exceeded\n";
    }
    cout << "\nTotal available size = " 
              << ms->getTotalMonitorSystemSize() << " bytes\n"
              << "  and cumulative subsystem size = "
              << cumulativeSize << " bytes"
              << endl;
    
    int start = 0;
    int end   = ms->getNumChildren();
    if (subsystemName != "all") {
        if (subsystemName == "none")  {
            end = start;
        }  else  {
            // we have to check a specific subsystem
            MonitorComponent * comp = 
                ms->getComponentPtr( subsystemName, false );
                
            if ( comp == 0 )  {
                cerr << "dumpCapacities : ERROR "
                          << "\n"
                          << "\"" << subsystemName << "\""
                          << " is not a valid subsystem name."
                          << endl;
                          
                return EXIT_FAILURE;
            }
            
            int i;
            for (i = 0;  i < ms->getNumChildren()
                              && (ms->getChild(i).componentRef().getCanonicalName() 
                                      != comp->getCanonicalName());  
                             i++) { }

            start = i;
            end   = std::min ((i+1), ms->getNumChildren());
        }
    }
     
    for (int j = start;  j < end;  j++)  {
        MonitorSubsystem * const msPtr = ms->getChild(j).subsystemPtr();
        if (msPtr != NULL)  {
            SubsystemFrame& ssFrame = msPtr->monitorPointSet().getSubsystemFrame();
            long maxMonitorPoints = ssFrame.getMaxNumMonitorPoints();
            long maxMonitorSamples = ssFrame.getMaxNumSamples();
            size_t maxSubsystemSize = 
                    SubsystemFrame::sizeFrame (maxMonitorPoints, maxMonitorSamples);
            long actualNumMonitorPoints = ssFrame.getNumMonitorPoints();
            long actualNumSamples = ssFrame.getNumMonitorSamples();
            size_t actualSize = sizeof (MonitorSubsystem) 
                      + actualNumMonitorPoints*(sizeof(MonitorHeader) + sizeof(int))
                      +actualNumSamples*sizeof(MonitorSampleValue);

            cout << "\nStorage numbers for subsystem " 
                      << msPtr->getName()
                      << "\n" << indent
                      << "Max monitor points = " << maxMonitorPoints
                      << "\n" << indent
                      << "Max monitor samples = " << maxMonitorSamples
                      << "\n" << indent
                      << "Max size (in bytes) = " << maxSubsystemSize
                      << "\n" << indent
                      << "Counted actual monitor points = " 
                      << actualNumMonitorPoints
                      << "\n" << indent
                      << "Counted actual multi-sample monitor points = " 
                      << (ssFrame.getNumMonitorPoints() - ssFrame.getNumSingleSamplePoints())
                      << "\n" << indent
                      << "Counted actual monitor samples = " << actualNumSamples
                      << "\n" << indent
                      << "Counted actual size (in bytes) = " << actualSize;
            if ( printAddresses ) {
                const void * const ptr = &(ssFrame.getSubsystemHeader());
                
                cout << "\n"
                     << indent << "Starting address of subsystem = " << ptr;
            }
            cout << endl;

            if (typedInfoReqd)  {
               cout << "\nCarma Monitor Subystem usage by monitor value type:";

           const ::size_t nElems = numElems( );
               for ( ::size_t i = 0; i < nElems; ++i )  {
                     const MonitorValueType type = valueType( i );
                     
                     cout << "\n" 
                               << indent
                               << "Cumulative actual monitor points of type " 
                               << valueType2String (type) << " = "
                               << ssFrame.getNumMonitorPoints (type)
                               << "\n" << indent
                               << "Cumulative actual monitor samples of type " 
                               << valueType2String (type) << " = "
                               << ssFrame.getNumMonitorSamples (type);
               }

               cout << endl;
            }
        }
    }

    return EXIT_SUCCESS;
}
