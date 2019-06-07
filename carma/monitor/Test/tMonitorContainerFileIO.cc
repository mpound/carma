#include "carma/monitor/MonitorContainerFileIO.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorSubsystemMaster.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <string>
#include <fstream>
#include <iostream>

using namespace carma::monitor;
using namespace carma::util;
using namespace std;

/**
 * @version $Revision: 1.2 $
 *
 * @description Read or write control subsystem from/to a named file. Run \
 *              multiple times with different filenames to verify correctness.
 *
 * @usage tMonitorContainerFileIO file=<filename> 
 *
 * @key file @noDefault string Filename to write -  defaults to subsystem name.
 * @key read false bool Read from file and set subsystem.
 * @key write true bool Write file from instance of Carma Monitor System.
 * @key reps 100 int When writing, repeat this many times and output timing info
 *
 * @logger DEFAULT_FACILITY carma.monitor.tSubsystemToFile
 */
int 
Program::main( ) 
{
    string outputFilename;
    if ( parameterWasSpecified( "file" ) ) 
        outputFilename = getStringParameter( "file" );
    else
        outputFilename = "ControlSubsystem.txt";
    
    const bool read = getBoolParameter( "read" );
    const bool write = getBoolParameter( "write" );
    const int maxReps = getIntParameter( "reps" );

    if ( write ) {
        const double initSubsysStart = Time::MJD();

        CarmaMonitorSystem cms;

        MonitorSubsystem & control = cms.control();

        const double initSubsysTotal = 
            ( Time::MJD() - initSubsysStart ) * Time::SECONDS_PER_DAY;

        cout << "CMS creation in " << initSubsysTotal << " seconds." << endl;

        cms.read();

        double totalTime = 0.0;
        for ( int reps = 0; reps < maxReps; ++reps ) {

            const double writeSubsystemStart = Time::MJD();

            writeContainerToFile( control, outputFilename ); 

            totalTime += 
                ( Time::MJD() - writeSubsystemStart ) * Time::SECONDS_PER_DAY;
        }

        cout << "Average write in " << totalTime / maxReps << " secs." << endl;
    }

    if ( read ) {
        // Now read the file back into the control subsystem. 
        ControlSubsystem controlMonSubsys;

        double totalReadTime = 0.0;
        for ( int reps = 0; reps < maxReps; ++reps ) {

            const double readFileStart = Time::MJD();
            
            setContainerFromFile( controlMonSubsys, outputFilename ); 

            totalReadTime += ( Time::MJD() - readFileStart );
        }

        controlMonSubsys.write();
        
        totalReadTime *= Time::SECONDS_PER_DAY;

        cout << "Average read in " << totalReadTime / maxReps << " s." << endl;
    }

    return 0;
}
