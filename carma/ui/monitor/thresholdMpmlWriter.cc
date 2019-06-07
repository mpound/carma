/**
 * @file
 * $Id: thresholdMpmlWriter.cc,v 1.3 2007/03/12 04:53:47 tcosta Exp $
 *
 * thresholdMpmlWriter
 *
 * @description writes out information for all monitor points with set
 *              thresholds to stdout in xml format
 *
 * @usage thresholdMpmlWriter
 *
 * @noKeys
 *
 * @logger DEFAULT_FACILITY carma.ui.thresholdMpmlWriter
 */

#include "carma/util/Program.h"
#include "carma/monitor/ThresholdFileWriter.h"
#include "carma/monitor/MonitorSystem.h"

using namespace carma;
using namespace carma::monitor;
using namespace carma::util;

int
Program::main( )
{
    CarmaMonitorSystem cms;
    ThresholdFileWriter tfw( cms );

    tfw.createXmlFile();
    
    return 0;
}
