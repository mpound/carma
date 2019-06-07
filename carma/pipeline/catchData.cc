#include "carma/pipeline/CatchData.h"
#include "carma/pipeline/CorrelatorIpqWriter.h"
#include "carma/pipeline/pipelineUtils.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

#include <algorithm>
#include <iostream>
#include <string>

using namespace carma;
using namespace carma::correlator::lib;
using namespace carma::pipeline;
using namespace carma::util;
using namespace ::std;

/**
 *  @description
 *  \nCatches Correlator Data from individual astroband channels, combines them
 *  into a single CorrelatorData object and publishes it to a shared memory IPQ.
 *
 *  @usage \nUsage: catchData <keywords>
 *
 *  @key mode @mandatory s 
 *       Mode of operation: Sl, Wb, C3g23 or C3g8 (not case sensitive). 
 *  @key channelPrefix "carma.correlator.astroband" s 
 *       Correlator data notification channel prefix (band # appended to).
 *  @key channelSuffix ".data" s
 *       Correlator data notification channel suffix (appended to band #).
 *  @key monitorWriteDelay 250  i 
 *       Millisecs after frame to write monitor data.
 *  @key ipqname @noDefault s 
 *       Output ipq filename. If not specified use a default name\n\t
 *       of "catch" + Mode + "DataIPQ".  This paremeter must match\n\t
 *       the pipeline (the defaults do).
 *  @key ipqmaxsize @noDefault i 
 *       Max bytes of IPQ element. If not specified use an internal default\n\t
 *       based on the mode.  This parameter must match the pipeline (the\n\t
 *       defaults do).
 *  @key ipqdepth 5 i 
 *       Depth of ipq (number of elements). This parameter must\n\t
 *       match the pipeline (the defaults do).
 * 
 *  @logger DEFAULT_FACILITY carma.catchdata
 *
 *  @author Rick Hobbs (original)
 *  @author Andy Beard (upgrades for C3g).
 *  @version $Id: catchData.cc,v 1.12 2012/12/13 23:46:32 abeard Exp $
 */
int Program::main( )
try {

    if ( !haveImrHostname( ) ) {
        cerr << "This application requires the imr keyword." << endl;
        programLogErrorIfPossible( "Required imr keyword not specified." );
        return 1;
    }

    const string mode( getStringParameter( "mode" ) );
    const pipeline::PipelineType pt = stringToPipelineType( mode );

    setInstanceLogname( "carma." + pipelineTypeToString( pt ) + "CatchData" );

    // This value would normally be a command line option but needs to be the 
    // same for all catchData instances.  If it isn't, the monitor
    // pipeline sync application will not be able to properly synchronize.
    const int corrDataWriteDelayMs = 1700;

    const int monitorDelayMs = getIntParameter( "monitorWriteDelay" );

    const string channelPrefix( getStringParameter( "channelPrefix" ) );
    const string channelSuffix( getStringParameter( "channelSuffix" ) );
    CatchData cd( pt, monitorDelayMs, corrDataWriteDelayMs,
                  channelPrefix, channelSuffix,
                  getCorbaServer() );

    string ipqFilename;
    if ( parameterWasSpecified( "ipqname" ) ) 
        ipqFilename = getStringParameter( "ipqname" );
    else
        ipqFilename = getDefaultCatchDataIpqName( pt );

    int ipqMaxElementSize;
    if ( parameterWasSpecified( "ipqmaxsize" ) ) 
        ipqMaxElementSize = getIntParameter( "ipqmaxsize" );
    else
        ipqMaxElementSize = getDefaultCatchDataIpqElementBytes( pt );

    const int ipqDepth = getIntParameter( "ipqdepth" );
        
    CorrelatorIpqWriter ciw( ipqFilename, ipqMaxElementSize, ipqDepth );
                             
    cd.addCorrelatorListener( &ciw );

    cd.run(); // start things running. shouldn't return.
  
    programLogInfoIfPossible( "catchData - Exiting main cleanly!" );

    return 0;

} catch ( ... ) {
    const string errMsg = getStringForCaught();
    cerr << errMsg << endl;
    programLogErrorIfPossible( errMsg );
    return 1;
}
