/**
 *  Program to read system frame from shred memeory and dump 
 *  contents. Doesnt dump everything as yet, just some essential 
 *  information to ensure that subsystem frames are reaching the
 *  ACC.  
 */

#include "carma/corba/corba.h"
#include "carma/monitor/types.h"
#include "carma/monitor/MonitorPointSample.h"
#include "carma/monitor/MonitorPointHeader.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace CORBA;
using namespace carma;
using namespace carma::util;
using namespace carma::monitor;


/**
 * Print monitor point sample value with the appropriate type
 */
void
printMonitorValue (MonitorValueType valueType, MonitorValue& value)
{
    switch ( valueType )  {
        case MONITOR_VALUE_TYPE_BYTE: 
            cout << "Byte type " << endl;
            cout << "Value = " <<  value.byte  << endl;
            break;

        case MONITOR_VALUE_TYPE_SHORT:
            cout << "Short type " << endl;
            cout << "Value = " <<  value.sh  << endl;
            break;

        case MONITOR_VALUE_TYPE_INTEGER:
            cout << "Integer type " << endl;
            cout << "Value = " <<  value.lo  << endl;
            break;

        case MONITOR_VALUE_TYPE_BOOLEAN:
            cout << "Boolean type " << endl;
            cout << "Value = " <<  value.bo  << endl;
            break;

        case MONITOR_VALUE_TYPE_FLOAT:
            cout << "Float type " << endl;
            cout << "Value = " <<  value.fl  << endl;
            break;

        case MONITOR_VALUE_TYPE_DOUBLE:
            cout << "Double type " << endl;
            cout << "Value = " <<  value.db  << endl;
            break;

        case MONITOR_VALUE_TYPE_COMPLEX:
            cout << "Complex type " << endl;
            // must get each element of complex number
            {
                float* complex = value.complex;
                cout << "Real value = " << complex[0] << endl;
                cout << "Im value = " << complex[1] << endl;
            }
            break;

        case MONITOR_VALUE_TYPE_STRING:
            cout << "String type " << endl;
            cout << "String value print not implemented" << endl;
            break;

        case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
            cout << "Serial number type " << endl;
            cout << "Value = " << value.sn << endl;
            break;

        default:
            CARMA_CPTRACE( Trace::TRACE1, "Unknown monitor value type (" <<
                                          valueType <<
                                          ")." );
                                          
            throw CARMA_ERROR( "Unknown monitor value type" );
            break;
    }
}



/**
 * Print monitor point header, the print first monitor point sample.
 */
void 
printMonitorPoint (MonitorPointHeader&  header)
{
    tagIDType   tagID = header.getTagID();
    ushort      subsystemID = SubsystemFrame::getSubsystemID (tagID);
    ushort      pointID = SubsystemFrame::getPointID (tagID);

    cout << "Monitor Point tagID - "<< tagID << " (" << subsystemID << "/" << pointID << ")" << endl;
    cout << "# of samples per cycle = " << header.getNumSamplesPerCycle() << endl;
    cout << "# of samples per cycle = " << header.getNumSamplesPerCycle() << endl;

    if (header.getNumSamplesPerCycle() > 0)  {
        MonitorPointSample sample = header.getMonitorPointSample (1);
        cout << "------------------ Begin sample # 1 -------------------- " << endl;
        printMonitorValue (header.getValueType(), sample.getMonitorValue());
        cout << "------------------ End sample # 1 -------------------- " << endl;
        // cout << "Sample value is " << sample.getMonitorValue() << endl;
    }
}



/**
 * Print subsystem frame header, the print each monitor point.
 */
void
printSubsystemFrame (SubsystemFrame&    frame)
{
    cout << "============== Begin Subsystem Frame " << frame.getSubsystemID() << " =============" << endl;
    cout << "# of monitor points is " << frame.getNumMonitorPoints () << endl;
    cout << "# of samples is " << frame.getNumMonitorSamples()  << endl;
    for (int  i = 0;  i < frame.getNumMonitorPoints();  i++)  {
        MonitorPointHeader header = frame.getHeader (i);
        cout << "----------- Monitor Point # " << i << " ----------------" << endl;
        printMonitorPoint (header);
    }

    cout << "============== End Subsystem Frame " << frame.getSubsystemID() << " =============" << endl;
}



/**
 * Print frame header, the print each subsystem frame.
 */
void
printFrame (SystemFrameBuffer&  sysFrameBuffer)
{
    SystemFrame&        sysFrame = sysFrameBuffer.getFrame();
    int                 numSubsystems = sysFrame.getNumSubsystemFrames();

    cout << "System frame output : " << endl;
    cout << "# of subsystem frames is " << numSubsystems << endl;
    cout << "Frames count is " << sysFrame.getFrameCount() << endl;
    cout << endl;
    cout << "------------------ Begin Subsystem Frames ----------------" << endl;
    cout << endl;

    for ( int i = 0; i < numSubsystems; ++i ) {
        const auto_ptr< SubsystemFrame >
            frame( sysFrame.makeSubsystemFrameForIndex( i ) );
            
        if ( frame.get() != 0 )
            printSubsystemFrame( *(frame.get()) );
    }

    cout << endl;
    cout << "------------------ End Subsystem Frames ----------------" << endl;
    cout << endl;
}



/**
 *  Test code that gets a system frame buffer object,
 *  reads system frame values from shared memory,
 *  dumps the resulting frame.
 */
int
Program::main( ) {
    CarmaMonitorSystem& carma = *new CarmaMonitorSystem();
    SystemFrameBuffer&  sysFrame = carma.systemFrameBuffer();

    cout << "Frame just after construction" << endl  << endl;
    printFrame (sysFrame);

    carma.read();

    cout << "Frame after reading..." << endl;
    printFrame (sysFrame);

    return 0;
}
