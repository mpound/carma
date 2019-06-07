/** @file
 * MonitorSystemPipelineSync class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.10 $
 * $Date: 2014/06/04 17:09:24 $
 * $Id: MonitorSystemPipelineSync.cc,v 1.10 2014/06/04 17:09:24 mpound Exp $
 */

#include "carma/monitor/MonitorSystemPipelineSync.h"

#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/monitorPointSpecializations.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/C3gMax8PipelineSubsystemExt.h"
#include "carma/monitor/C3gMax23PipelineSubsystemExt.h"
#include "carma/monitor/SubsystemFrameBuffer.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

#include <iostream>
#include <string>

using namespace carma::monitor;
using namespace carma::util;
using namespace std;

MonitorSystemPipelineSync::MonitorSystemPipelineSync( 
    const CmsSelector inputCMS,
    const CmsSelector outputCMS )
{
    // Maintain two copies of input CMS - the first to read and query for 
    // pipeline information, the second to act as a buffer.  
    inputCMS_ = makeCms( inputCMS, inputCmsName_ );  
    bufferCMS_ = makeCms( inputCMS, bufferCmsName_ ); 
    outputCMS_ = makeCms( outputCMS, outputCmsName_ );

    bufferCMS_->synchronize( *( inputCMS_.get( ) ) );

    inputCMS_->readNewest( ); 
    bufferCMS_->readNewest( ); // May contain newer frame(s) than inputCMS 

    // Read input CMS and bufferCMS and verify that they are synchronized.
    const int inputFrameCount = inputCMS_->getFrameCount( );
    const int bufferFrameCount = bufferCMS_->getFrameCount( );

    // Note it's fine if the buffer frame count is > than input frame count (the
    // two race for new data above) as syncNextValidFrame will sync 'em up. 
    if ( inputFrameCount > bufferFrameCount ) {
        ostringstream err;
        err << "MonitorSystemPipelineSync constructor - " 
            << "Input CMS frame " << inputFrameCount << " should be >= to "
            << "buffer CMS frame " << bufferFrameCount << ", " 
            << "something is terribly wrong.";
        programLogErrorIfPossible( err.str( ) );

        throw CARMA_ERROR( err.str( ) );
    }

    ostringstream msg;

    msg << "MonitorSystemPipelineSync created with "
        << "input CMS \"" << inputCmsName_ << "\" "
        << "@ frame " << inputFrameCount << ", "
        << "buffer CMS \"" << bufferCmsName_ << "\" "
        << "@ frame " << bufferFrameCount << ", "
        << "and output CMS \"" << outputCmsName_ << "\".";

    programLogInfoIfPossible( msg.str( ) );
} 

MonitorSystemPipelineSync::~MonitorSystemPipelineSync( )
{

}

void
MonitorSystemPipelineSync::syncNextValidFrame( )
{
    // This is cumbersome so an explanation of the basic idea is in order:
    // The pipeline processes data several frames after the data was produced,
    // thus pipeline monitor data is typically out of sync at least 1 frame 
    // relative to the data it was derived from.  Here we resynchronize
    // pipeline data with the original data it was derived from by:
    // a) Reading from the input carma monitor system and noting the real 
    //    frame it's pipeline data corresponds to (itself contained in a MP). 
    // b) Using another instance of a carma monitor system to track and access 
    //    that older frame.
    // c) Writing the older pipeline data to it's corresponding frame in 
    //    this second CMS instance.
    // d) Writing it all out to the 'Final Carma Monitor System'.

    // Loop until input CMS pipeline subsystem data frame count is >= bufferCMS
    // Nominally these are already pointing correctly except at startup so
    // track the number of reads required and warn if > 1.
    int bufferCMSFrameCount = bufferCMS_->getFrameCount( ); 
    int pipelineDataFrameCount = 0;

    while ( pipelineDataFrameCount < bufferCMSFrameCount ) {
        
        inputCMS_->read( ); 

        SlPipelineSubsystem & pl = inputCMS_->slPipeline( );
        MonitorPointInt & plFrame = pl.pipelineStatus( ).dataFrame( );

        if ( plFrame.getValidity( ) >= MonitorPoint::VALID ) 
            pipelineDataFrameCount = plFrame.getValue( );
    }

    // Three cases here.
    // 1) Pipeline data frame count matches the buffer CMS frame count:
    //    this is the everything is working correctly nominal case.
    // 2) The pipeline data frame is > than the buffer CMS frame:
    //    there was missing pipeline data for the current buffer CMS frame.
    //    In this case we need to catch the buffer CMS up while writing 
    //    dummy pipeline monitor systems and write pl to proper frame. 
    // 3) Same as two except we never encounter the proper frame in the 
    //    buffer CMS - in which case we do nothing.

    int outputFramesWritten = 0;
    while ( pipelineDataFrameCount > bufferCMSFrameCount ) {
        // TODO: The question here is do I want to write out a completely 
        // empty pipeline subsystem to the outputCMS or do I want to just
        // write what's already there?  For now just write out what is there,
        // if we later find that we are missing a substantial amount of 
        // frames, we can go from there.
        outputCMS_->synchronize( *( bufferCMS_.get( ) ) );
        outputCMS_->write( );

        const unsigned int lost = bufferCMS_->read( );
        bufferCMSFrameCount = bufferCMS_->getFrameCount( ); 
        ++outputFramesWritten;
        
        if ( lost > 0 ) {
            ostringstream errMsg;
            errMsg << "Lost " << lost;
            if ( lost > 1 ) {
                errMsg << " frames (" << bufferCMSFrameCount - lost 
                    << "-" << bufferCMSFrameCount - 1;
            } else {
                errMsg << " frame (" << bufferCMSFrameCount - 1;
            }
            errMsg << ") between CMS reads. These frames are permanently gone.";

            programLogErrorIfPossible( errMsg.str() );
        }
    }

    if ( pipelineDataFrameCount == bufferCMSFrameCount ) {

        // Write input CMS pipeline data to output CMS
        TransportSubsystemFrame slTransportFrame;
        TransportSubsystemFrame wbTransportFrame;
        TransportSubsystemFrame c3gMax8TransportFrame;
        TransportSubsystemFrame c3gMax23TransportFrame;
        TransportSubsystemFrame astroTransportFrame;

        // This is pure crap but sans any real way to copy subsystems it works.
        inputCMS_->slPipeline().monitorPointSet( ).getSubsystemFrame().
            writeToTransport( slTransportFrame );
        bufferCMS_->slPipeline().monitorPointSet( ).getSubsystemFrame().
            writeFromTransportFrame( slTransportFrame );
        inputCMS_->wbPipeline().monitorPointSet( ).getSubsystemFrame().
            writeToTransport( wbTransportFrame );
        bufferCMS_->wbPipeline().monitorPointSet( ).getSubsystemFrame().
            writeFromTransportFrame( wbTransportFrame );
        inputCMS_->astro().monitorPointSet( ).getSubsystemFrame().
            writeToTransport( astroTransportFrame );
        bufferCMS_->astro().monitorPointSet( ).getSubsystemFrame().
            writeFromTransportFrame( astroTransportFrame );
        inputCMS_->c3gMax8Pipeline().monitorPointSet( ).getSubsystemFrame().
            writeToTransport( c3gMax8TransportFrame );
        bufferCMS_->c3gMax8Pipeline().monitorPointSet( ).getSubsystemFrame().
            writeFromTransportFrame( c3gMax8TransportFrame );
        inputCMS_->c3gMax23Pipeline().monitorPointSet( ).getSubsystemFrame().
            writeToTransport( c3gMax23TransportFrame );
        bufferCMS_->c3gMax23Pipeline().monitorPointSet( ).getSubsystemFrame().
            writeFromTransportFrame( c3gMax23TransportFrame );

        outputCMS_->synchronize( *( bufferCMS_.get() ) );
        outputCMS_->write( );
    
        { // Verify that the sync succeeded
            SlPipelineSubsystem & slpl = outputCMS_->slPipeline( );
            MonitorPointInt & slplFrame = slpl.pipelineStatus( ).dataFrame( );
            if ( slplFrame.getValue( ) != outputCMS_->getFrameCount( ) ) {
                ostringstream msg;

                msg << "Pipeline data frame count " << slplFrame.getValue( )
                    << " does NOT match output CMS pipeline data frame count "
                    << "of " << outputCMS_->getFrameCount( ) << "!";
                programLogError( msg.str( ) ); 
            }
            
            WbPipelineSubsystem & wbpl = outputCMS_->wbPipeline( );
            MonitorPointInt & wbplFrame = wbpl.pipelineStatus( ).dataFrame( );
            if ( wbplFrame.getValue() != slplFrame.getValue() ) {
                ostringstream err;

                err << "SlPipeline data frame count " << slplFrame.getValue( )
                    << " does NOT match WbPipeline data frame count "
                    << wbplFrame.getValue() << " which implies that the "
                    << "Astro subsystem is not time synchronized!";

                programLogError( err.str( ) ); 
            }

            C3gMax8PipelineSubsystem & c3gm8 = outputCMS_->c3gMax8Pipeline( );
            MonitorPointInt & c3gm8Frame = c3gm8.pipelineStatus( ).dataFrame( );
            if ( c3gm8Frame.getValue() != slplFrame.getValue() ) {
                ostringstream err;

                err << "SlPipeline data frame count " << slplFrame.getValue( )
                    << " does NOT match C3gMax8Pipeline data frame count "
                    << c3gm8Frame.getValue() << " which implies that the "
                    << "Astro subsystem is not time synchronized!";

                programLogError( err.str( ) ); 
            }

            C3gMax23PipelineSubsystem & c3gm23 = outputCMS_->c3gMax23Pipeline( );
            MonitorPointInt & c3gm23Frame = c3gm23.pipelineStatus( ).dataFrame( );
            if ( c3gm23Frame.getValue() != slplFrame.getValue() ) {
                ostringstream err;

                err << "SlPipeline data frame count " << slplFrame.getValue( )
                    << " does NOT match C3gMax8Pipeline data frame count "
                    << c3gm23Frame.getValue() << " which implies that the "
                    << "Astro subsystem is not time synchronized!";

                programLogError( err.str( ) ); 
            }
        }

        const unsigned int lost = bufferCMS_->read( );
        
        if ( lost > 0 ) {
            ostringstream errMsg;
            errMsg << "Lost " << lost;
            if ( lost > 1 ) {
                errMsg << " frames (" << bufferCMSFrameCount - lost 
                    << "-" << bufferCMSFrameCount - 1;
            } else {
                errMsg << " frame (" << bufferCMSFrameCount - 1;
            }
            errMsg << ") between CMS reads. These frames are permanently gone.";

            programLogErrorIfPossible( errMsg.str() );
        }
        
        ++outputFramesWritten;
    
        if ( outputFramesWritten > 1 ) {

            const int unsyncedFrames = outputFramesWritten - 1;

            ostringstream msg;
            
            msg << "Wrote " << unsyncedFrames << " CarmaMonitorSystem ";
           
            if ( unsyncedFrames > 1 ) {
                msg << "frames ";
            } else {
                msg << "frame ";
            }

            msg << " to \"" << outputCmsName_ << "\" with invalid pipeline "
                << "data, and 1 frame synchronized to valid pipeline data.";

            programLogErrorIfPossible( msg.str( ) );
        }
    } else {
        ostringstream msg;

        msg << "Wrote " << outputFramesWritten << " CarmaMonitorSystem ";

        if ( outputFramesWritten > 1 ) {
            msg << "frames ";
        } else {
            msg << "frame ";
        }

        msg << " to \"" << outputCmsName_ << "\" CMS with invalid pipeline "
            << "data, but am still NOT synchronized to valid pipeline data.";

        programLogErrorIfPossible( msg.str( ) );
    }
}
