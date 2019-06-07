// $Id: pipeline.cc,v 1.19 2014/06/04 17:09:27 mpound Exp $

#include "carma/corba/Server.h"
#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/PipelineMonitorInput.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/monitor/PipelineSubsystemC3G.h"
#include "carma/monitor/PipelineSubsystemSL.h"
#include "carma/monitor/PipelineSubsystemWB.h"
#include "carma/pipeline/PipelineControlImpl.h"
#include "carma/pipeline/pipelineControl_skel.h"
#include "carma/pipeline/pipelineControl_skel_tie.h"
#include "carma/pipeline/pipelineUtils.h" // For PipelineType
#include "carma/pipeline/BlankFlagStage.h"
#include "carma/pipeline/CoherenceStage.h"
#include "carma/pipeline/IntegratorStage.h"
#include "carma/pipeline/VisBrickWriterStage.h"
#include "carma/pipeline/PublisherStage.h"
#include "carma/pipeline/DecimatorStage.h"
#include "carma/util/IPQbasicTypeBuffer.h"
#include "carma/pipeline/Pipeline.h"
#include "carma/pipeline/SelfCalStage.h"
#include "carma/pipeline/TsysStage.h"
#include "carma/services/Global.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/posixErrors.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

#include <boost/algorithm/string.hpp>
#include <boost/thread.hpp>
#include <memory>
#include <string>

using namespace carma;
using namespace carma::correlator::lib;
using namespace carma::pipeline;
using namespace carma::util;
using namespace std;

//
//  @description
//  Reads from an IPQ containing raw Correlator data and then processes the
//  data in various stages.  

//  @usage Usage: pipeline <keywords>
//
//  @key mode @mandatory s 
//       Mode of operation: Sl, Wb, C3g23 or C3g8 (not case sensitive). 
//  @key vbdir @mandatory s Directory to store visbricks in.
//  @key monitorDelay 300 i
//       Millisecs after the frame boundary to write monitor data.
//  @key autowrite true b 
//       Use monitor system autowriter if true.
//  @key ipqname @noDefault s 
//       Output ipq filename. If not specified use a default name\n\t
//       of "catch" + Mode + "DataIPQ".  This paremeter must match\n\t
//       catchData (the defaults do).
//  @key ipqmaxsize @noDefault i 
//       Max bytes of IPQ element. If not specified use an internal default\n\t
//       based on the mode.  This parameter must match catchData (the\n\t
//       defaults do).
//  @key ipqdepth 5 i 
//       Depth of ipq (number of elements). This parameter must match\n\t
//       catchData (the defaults do).
//
//  @logger DEFAULT_FACILITY carma.pipeline
//
//  @author Original: Rick Hobbs
//  @author Rewrite: Andy Beard 
//
//  @version $Revision: 1.19 $, $Date: 2014/06/04 17:09:27 $
//
int Program::main() 
try {


    if ( !haveImrHostname( ) ) {
        cerr << "This application requires the imr keyword." << endl;
        programLogErrorIfPossible( "Required imr keyword not specified." );
        return 1;
    }

    const string mode( boost::to_lower_copy( getStringParameter( "mode" ) ) );
    const pipeline::PipelineType pt = stringToPipelineType( mode );

    // set logger name and facility as well as DO name
    setInstanceLogname( "carma." + mode + "pipeline" );

    const string doNamePubInt( "carma.correlator." + mode + "Integrator" );
    const string channelName( "carma.correlator." + mode + "PubInt" );
    
    const size_t reserveBands( getAstrobandCount( pt ) );

    const string visbrickDir( getStringParameter( "vbdir" ) );
    const string visBrickFilenamePrefix( visbrickDir + "/" + mode + "VisBrickData" );

    string ipqName;
    if ( parameterWasSpecified( "ipqname" ) ) 
        ipqName = getStringParameter( "ipqname" );
    else
        ipqName = getDefaultCatchDataIpqName( pt );

    int ipqElementSize;
    if ( parameterWasSpecified( "ipqmaxsize" ) ) 
        ipqElementSize = getIntParameter( "ipqmaxsize" );
    else
        ipqElementSize = getDefaultCatchDataIpqElementBytes( pt );

    const int ipqElementCount = getIntParameter( "ipqdepth" );

    monitor::AstroSubsystem astroSubsystem;
    auto_ptr<monitor::PipelineSubsystem> pipelineSubsystem;
    std::string pipelineDoName;

    // Dynamically instantiate our monitor subsystem and pipeline DO names.
    const MonitorCorrelatorDesignation corrDes( getMonCorrDes( pt ) );
    if ( pt == SL ) {
        pipelineSubsystem = auto_ptr<monitor::PipelineSubsystem>( 
                new monitor::PipelineSubsystemSL( ) );
        pipelineDoName = SPECTRAL_PIPELINE_NAME;
    } else if ( pt == WB ) {
        pipelineSubsystem = auto_ptr<monitor::PipelineSubsystem>( 
                new monitor::PipelineSubsystemWB( ) );
        pipelineDoName = WIDEBAND_PIPELINE_NAME;
    } else if ( pt == C3G8 ) {
        pipelineSubsystem = auto_ptr<monitor::PipelineSubsystem>( 
                new monitor::PipelineSubsystemC3gMax8( ) );
        pipelineDoName = C3GMAX8_PIPELINE_NAME;
    } else if ( pt == C3G23 ) {
        pipelineSubsystem = auto_ptr<monitor::PipelineSubsystem>( 
                new monitor::PipelineSubsystemC3gMax23( ) );
        pipelineDoName = C3GMAX23_PIPELINE_NAME;
    } else { 
        cerr << "Invalid pipeline type." << endl;
        programLogErrorIfPossible( "Invalid pipeline type." );
        return 1; 
    }

    if ( pipelineSubsystem.get( ) == 0 ) {
        programLogErrorIfPossible( "Unable to create monitor subsystem." );
        return 1; 
    }

    monitor::PipelineMonitorInput plMonitorInput( corrDes );
    
    // Instantiate the Stages
    CoherenceStage coherence( *pipelineSubsystem, 
                              plMonitorInput, 
                              astroSubsystem,
                              corrDes );

    Decimator decimator( *pipelineSubsystem, plMonitorInput, pt );
    carma::pipeline::TsysStage tsys( *pipelineSubsystem, 
                                     plMonitorInput,
                                     pt,
                                     astroSubsystem );

    BlankFlag blankFlag(*pipelineSubsystem, astroSubsystem, plMonitorInput, pt);

    Integrator integrator( *pipelineSubsystem, plMonitorInput );
    SelfCalStage selfCal( *pipelineSubsystem, tsys, astroSubsystem,
                          plMonitorInput, pt );
    VisBrickWriter visBrick( visBrickFilenamePrefix, *pipelineSubsystem);
    Publisher cpubInt( *pipelineSubsystem, channelName, doNamePubInt, pt );


    const bool autowrite = getBoolParameter( "autowrite" );
    long monitorDelayMs = getIntParameter( "monitorDelay" ); 
    if ( autowrite ) {
        astroSubsystem.startAutoWriter( monitorDelayMs * 0.001 ) ;
        pipelineSubsystem->startAutoWriter( monitorDelayMs * 0.001 );
        monitorDelayMs -= 15; // Set pl mon thread to fire first. 
    }


    Pipeline pipeline( *pipelineSubsystem, astroSubsystem,
                       monitorDelayMs, autowrite ); 

    // The pipeline will process correlator data in the below order.
    pipeline.pushStageBack( coherence );
    pipeline.pushStageBack( decimator ); 
    pipeline.pushStageBack( tsys );
    pipeline.pushStageBack( blankFlag );
    pipeline.pushStageBack( integrator );
    pipeline.pushStageBack( selfCal );
    pipeline.pushStageBack( cpubInt );
    pipeline.pushStageBack( visBrick );

    // Create control server implementation DO
    PipelineControlImpl pc( coherence, blankFlag, integrator,
                visBrick, decimator,
                selfCal, tsys,
                cpubInt );

    namespace POA_plc = POA_carma::pipeline;
    corba::Server & pcs = Program::getProgram().getCorbaServer( );


    pcs.addServant< POA_plc::PipelineControl_tie >( pc, pipelineDoName );
    pcs.run( true );

    // Open input IPQ (creating if necessary) using a data vector.
    vector< char > data( ipqElementSize );

    util::IPQbasicTypeBuffer ipq_( &( data.at( 0 ) ),
            ipqElementSize,
            ipqName,
            true, // create
            ipqElementCount );

    FrameAlignedTimer frameTimer;

    boost::shared_ptr< CorrelatorData > cd;
    frameType dataFrame = 0;
    frameType lastProcessedDataFrame = 0;
    long framesElapsed = 0;
    bool inputAvailable = true;


    ipq_.setNoneAvailable( ); // Start at the top of the queue
    while (!pcs.terminated() && pipelineSubsystem->autoWriterIsAlive() ) {

        // IPQ's are not termination points so one risks blocking on them
        // forever when shutting down.  Rather, I use non blocking IPQ reads
        // and poll for data availability at 100Hz.  This should provide
        // snappy processing without jeopardizing much CPU.
        if ( !ipq_.isDataAvailable() ) {
            boost::this_thread::sleep( boost::posix_time::milliseconds( 10 ) );
            continue;    
        }

        cd.reset( new CorrelatorData( ) ); 

        cd->reserveBands( reserveBands );

        // Note this IPQ read implicitly transfers bytes into the 'data' vector.
        // Read newest correlator data element from ipq using non-blocking
        // read.  This should succeed (see note above), but if it doesn't
        // we loop around and try again.  
        if ( !ipq_.readNewest( ) ) 
            continue;

        cd->deserial( data );

        dataFrame = Time::computeClosestFrame( cd->getHeader( ).getMJD( ) ); 

        framesElapsed = dataFrame - lastProcessedDataFrame;

        if ( framesElapsed > 0 ) {

            inputAvailable = plMonitorInput.waitForInput( dataFrame );

            if ( !inputAvailable ) {
                pipeline.incrementMissedMonitorInfo();
                continue; 
            }

            pipeline.processCorrelatorData( cd );
            
            lastProcessedDataFrame = dataFrame;
        }
    }

    return 0;

} catch ( ... ) {
    logCaughtAsError( );
    return 1;
}
