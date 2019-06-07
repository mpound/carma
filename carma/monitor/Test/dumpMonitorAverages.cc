/**
 * Accumulates average monitor data by type, and writes the averages
 * to specified output streams.
 *
 * @author: Amar Amarnath
 *
 * $Id: dumpMonitorAverages.cc,v 1.9 2012/01/18 18:48:43 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <iostream>

#include <stdio.h>

#include "carma/corba/corba.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/AverageAccumulator.h"

using namespace std;
using namespace CORBA;
using namespace carma;
using namespace carma::monitor;


/**
 * $Id: dumpMonitorAverages.cc,v 1.9 2012/01/18 18:48:43 mpound Exp $
 */

//
// @version	$Revision: 1.9 $ $Date: 2012/01/18 18:48:43 $ 
//
// @description	accumulate monitor sample values and write averages to 
//              specified output streams.
//
// @usage       dumpMonitorAverages [--keywords] [--help] [--usage] [imr=[imr]] [mode=[final]] [-- [-ORBDefaultInitRef corbaloc::<imrhost>]]
//              If using ORBDefaultInitRef, then explictly set imr=-
//	
// @key mode final string if mode=final, then we're reading thresholded monitor 
//                 data, else if mode=raw, we're reading raw uncalibrated 
//                 monitor data.
// @key file /dev/stdout string name of file to dump output - /dev/stdout
//                 by default
// @key frames 2   int     Number of frames to dump
// @key priority   normal   string  archive priority of points to write, 
//                 points with priorities at or higher than this will be 
//                 archived; valid values are vital, useful, normal, debug, 
//                 verbose
//
// @logger TEST_FACILITY carma.test.monitor.dumpMonitorAverages



int
::carma::util::Program::main()
{
    int status = EXIT_SUCCESS;
    try {
        std::string mode (getStringParameter ("mode"));
        const ::std::string priorityStr (getStringParameter( "priority" ));
        const ::std::string fileName (getStringParameter( "file" ));
        const int numFrames = getIntParameter( "frames" );
        bool rawMode = (mode == "raw");

        ::carma::monitor::MonitorSystem & carma =
            (rawMode ? (*(new ::carma::monitor::RawCarmaMonitorSystem)) :
                       (*(new ::carma::monitor::CarmaMonitorSystem)));

        monitor::MonitorPoint::ARCHIVE_PRIORITY priority;
        if(priorityStr == "verbose") {
            priority =     monitor::MonitorPoint::VERBOSE;
        } else if(priorityStr == "debug") {
            priority =     monitor::MonitorPoint::DEBUG;
        } else if(priorityStr == "normal") {
            priority =     monitor::MonitorPoint::NORMAL;
        } else if(priorityStr == "useful") {
            priority =     monitor::MonitorPoint::USEFUL;
        } else if(priorityStr == "vital") {
            priority =     monitor::MonitorPoint::VITAL;
        } else {
            throw CARMA_ERROR("Unknown priority " + priorityStr);
        }
        
        // I'm assuming that the following filestreams are available as 
        // FILE* objects -
        //
        // For averages across multiple averages
        // intAveFile    - long timeframe numeric averages
        // shortAveFile  - long timeframe short averages
        // stringAveFile - long timeframe string averages
        // complexAveFile - long timeframe complex averages
        //

        FILE* outFile = fopen (fileName.c_str(), "w");  
        
        ::carma::monitor::AverageAccumulator aveAccumulator (carma, priority);

        // open streams for minute averages
        FILE*  shortAveFile = outFile;
        FILE*  intAveFile = outFile;
        FILE*  stringAveFile = outFile;
        FILE*  complexAveFile = outFile;


        carma.read();

        for (int i = 0;  i < numFrames;  i++)  {
            long frameCount = carma.getFrameCount();
            aveAccumulator.accumulate();
            aveAccumulator.writeLongAveragesToFile 
                                           (frameCount, shortAveFile, intAveFile,
                                                  stringAveFile, complexAveFile);
            carma.read();
        }
    } catch ( util::ErrorException & ee ) {
        cerr << "Error caught: " << ee.getMessage();
        status = EXIT_FAILURE;
    }
    
    return status;
}



