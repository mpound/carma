#include "carma/monitor/MonitorSystemPipelineSync.h"
#include "carma/monitor/MonitorSystemSelector.h"
#include "carma/util/Program.h"

#include <string>

using namespace carma::monitor;
using namespace carma::util;
using namespace std;

/**
 * @version $Id: monitorPipelineSync.cc,v 1.3 2008/01/15 17:26:42 abeard Exp $
 *
 * @usage \nUsage: monitorPipelineSync [system keywords]
 *
 * @description
 * \nResponsible for resynchronizing pipeline monitor data with the frame
 * it was originally calculated from.  Monitor data arrives at the pipeline
 * several frames later than the sample frame.  The monitor data is then 
 * used to calculate pipeline calibration and other data which in turn is put
 * back into the monitor system.  All in all, pipeline monitor data can be
 * several frames out of sync with the data it was derived from.  This 
 * application resynchronizes and outputs to the final monitor system.
 *
 * @key inputCMS "intermediate" string Monitor system to use for input. One of
 *                                     {raw, final, intermediate}.
 * @key outputCMS "final" string Monitor system to use for output. One of  
 *                               {raw, final, intermediate}.  
 *
 * @logger MONITOR_FACILITY carma.monitor.monitorPipelineSync
 */
int Program::main( )
{
    const string inputCMS = getStringParameter( "inputCMS" );
    const string outputCMS = getStringParameter( "outputCMS" );

    const CmsSelector inCmsSelector = convertStringToCmsSelector( inputCMS );
    const CmsSelector outCmsSelector = convertStringToCmsSelector( outputCMS );

    MonitorSystemPipelineSync sync( inCmsSelector, outCmsSelector );

    while ( !imrTerminationRequested( ) ) {
        sync.syncNextValidFrame( );
    }

    return 0;
}
