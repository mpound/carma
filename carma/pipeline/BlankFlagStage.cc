// $Id: BlankFlagStage.cc,v 1.22 2014/07/08 21:00:25 scott Exp $

#include "carma/pipeline/BlankFlagStage.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/monitor/AstroSubsystem.h"
#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/PipelineMonitorInput.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/fault/BlankFlagConstants.h"
#include "carma/services/Global.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <boost/foreach.hpp>

using namespace carma;
using namespace carma::correlator::lib;
using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::services;
using namespace carma::util;
using namespace std;

namespace {

const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;

namespace CCL = carma::correlator::lib;
            
typedef carma::monitor::NoiseStatusMonitorPointEnum NoiseStatusMPE;

void 
addSideband( CorrelatorBaseline & baseline, 
             const CorrelatorSideband::Flavor flav ) 
{
    CorrelatorSideband sb( flav );
    sb.blankData( CorrelatorSideband::CORR_DATA_MISSING );
    baseline.addSideband( sb );
}

void 
fillCorrBaseline( CorrelatorBaseline & baseline )
{
    //COUT("inside fillCorrBaseline...");
    typedef CorrelatorSideband CS;
    const int i1No = baseline.getInput1Number();
    const int i2No = baseline.getInput2Number();

    if ( i1No == i2No ) {

        if ( !baseline.containsSideband( CS::AUTO_FLAVOR ) ) {
            addSideband( baseline, CS::AUTO_FLAVOR );
        } 

    } else {

        if ( !baseline.containsSideband( CS::UPPER_FLAVOR ) ) {
            addSideband( baseline, CS::UPPER_FLAVOR );
        }

        if ( !baseline.containsSideband( CS::LOWER_FLAVOR ) ) {
            addSideband( baseline, CS::LOWER_FLAVOR );
        }
    } 
    //COUT("insigied fillCorrBaseline...DONE");

}

void
fillCorrBand( CorrelatorBand & band,
              const monitor::PipelineMonitorInput & plmi )
{
    //COUT("Inside fillCorrBand...");
    typedef monitor::AntPolPair AntPolPair;

    // Loop through mapped baselines and add those in which don't exist.
    const int abNo = band.getBandNumber();
    const set< int > inputs = plmi.getMappedInputNumbers();

    BOOST_FOREACH( const int input1No, inputs ) {

        set<int>::const_iterator input2it = inputs.lower_bound(input1No);
        for ( ; input2it != inputs.end(); ++input2it ) {

            const int input2No = *input2it;

            if ( !band.hasBaseline( input1No, input2No ) ) {

                CorrelatorBaseline baseline;

                const AntPolPair antpol1 = plmi.getAntPolPair( abNo, input1No );
                const AntPolPair antpol2 = plmi.getAntPolPair( abNo, input2No );

                baseline.setInput1Number( input1No );
                baseline.setInput2Number( input2No );
                baseline.setAntPol1( antpol1.first, 
                        CCL::monPolToCorrPol( antpol1.second ) );
                baseline.setAntPol2( antpol2.first, 
                        CCL::monPolToCorrPol( antpol2.second ) );

                band.addBaseline( baseline );
            }

            fillCorrBaseline( band.getBaseline( input1No, input2No ) );
        }
    }
    //COUT("Inside fillCorrBand... DONE");
}

void
fillInMissingCorrData( CorrelatorDataPtr cd, 
                       const monitor::PipelineMonitorInput & plmi )
{
    const bool db = false;
    if (db) COUT("Inside fillInMissingCorrData");
    const vector< int > astrobands = plmi.getMappedAstroBandNumbers();
    BOOST_FOREACH( const int astroband, astrobands ) {

      if (db) COUT("Inside fillInMissingCorrData: astroband = " << astroband);

        if (!cd->hasBand( astroband)) {
	        if (db) COUT("Here 0");
            CorrelatorBand band;
            
            // Fill in as much as we can about the empty band in order to 
            // not trip anybody up downstream.  This is especially important
            // if the blank frame is the first in the integration as 
            // the info is kept for the whole integration.
	        if (db) COUT("Here 1");
            const ControlBandPoints& cbps(plmi.controlBandPoints(astroband));
            band.setBandNumber( astroband );
            band.setSelfTest( false );
            band.setSimulation( false );
	        if (db) COUT("Here 2");
            band.setSequenceNumber( cd->getHeader().getSequenceNumber() );
	        if (db) COUT("Here 2a");
            band.setMJD( cd->getHeader().getMJD() );
	        if (db) COUT("Here 2ba");
	        if (db) COUT("Here: " << cbps.bandwidth().getValue());
	        if (db) COUT("Here 2bb");
            band.setBandwidth( cbps.bandwidth().getValue() );
	        if (db) COUT("Here 2c");
            band.setNumberOfInputs( 
                    plmi.getMappedAntPolPairs( astroband ).size() );
	        if (db) COUT("Here 2d");
            band.setValid( false, CorrelatorSideband::CORR_DATA_MISSING );
	        if (db) COUT("Here 3");
            cd->addBand( band );
	        if (db) COUT("Here 4");
        } 

	     if (db) {
	        COUT("Inside fillInMissingCorrData: astroband = " 
	              << astroband << " ... DONE");
	     }         

        fillCorrBand( cd->getBand( astroband ), plmi);
    }
    if (db) COUT("Inside fillInMissingCorrData...done");
}

} // namespace <unnamed>

/**.......................................................................
 * Constructor.
 */
BlankFlag::BlankFlag( 
        carma::monitor::PipelineSubsystem & monitor, 
        carma::monitor::AstroSubsystem & astroMonitor, 
        const carma::monitor::PipelineMonitorInput & plmi,
        const carma::pipeline::PipelineType plType ) :
    Stage( monitor.getBlankFlagStageStats( ), "BlankFlag" ),
    blankCount_( 0 ),
    flagCount_( 0 ),  
    totalDataCount_( 0 ),
    plmi_( plmi ),
    monitorData_( monitor ),
    astroMonitor_( astroMonitor ),
    shadowing_( &( plmi.getCarmaMonitorSystem() ) ),
    plType_( plType )
{
    // Nothing
}

/**.......................................................................
 * Destructor.
 */
BlankFlag::~BlankFlag() 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "BlankFlag::~BlankFlag() - D'tor." );
}

void
BlankFlag::preprocess( const CorrelatorDataPtr cd )
{
    pctBlanked_ = 0;
    pctFlagged_ = 0;
    
    fillInMissingCorrData( cd, plmi_ );

    shadowing_.update();

    svShadowedAnts_ = shadowing_.getSweptVolumeShadowing();
    blShadowedAnts_ = shadowing_.getInternalShadowing();

    if ( ( svShadowedAnts_.size() != blShadowedAnts_.size() ) || 
         ( svShadowedAnts_.size() != Global::maxAntennas() ) )
        throw CARMA_ERROR( "Shadowed antenna vector sizes bad." );

    const int numBands   = cd->getNumberOfBands();

    if ( numBands == 0 ) return;
        
    const vector<CorrelatorBand>& cb = cd->getBands();

    if ( static_cast<int>( cb.size( ) ) != numBands ) {
        ostringstream msg;
        msg << "CorrelatorData::getNumberOfBands returned " << numBands
            << " which does NOT match " << cb.size( ) 
            << ", the size of the returned vector!";
        programLogErrorIfPossible( msg.str( ) );
    }
}

void
BlankFlag::processBand( CorrelatorBand * cb )
try {
  std::cout << "Inside BF processBand..." << std::endl;

    const int abNo = cb->getBandNumber();

    vector< CorrelatorBaseline > & baselines = cb->getBaselines();
    BOOST_FOREACH( CorrelatorBaseline & bl, baselines ) {
        const int input1 = bl.getInput1Number( );
        const int input2 = bl.getInput2Number( );

        monitor::AntPolPair antPol1 = plmi_.getAntPolPair( abNo, input1 );
        monitor::AntPolPair antPol2 = plmi_.getAntPolPair( abNo, input2 );
            
        typedef CorrelatorSideband SB;
        typedef MonitorPoint MP;

        if ( !plmi_.signalPathMapped( abNo, antPol1 ) ||
             !plmi_.signalPathMapped( abNo, antPol2 ) ) 
        {
            bl.blankData( SB::UNMAPPED_SIGNAL );
        }
        
        BOOST_FOREACH( CorrelatorSideband & sb, bl.getSidebands() ) 
        {
            if ( sb.isAuto() ) continue;
                
            pair< bool, bool > rxInSb1 = plmi_.isRxInSideband( abNo,
                                                               antPol1,
                                                               sb.isUSB() );
            pair< bool, bool > rxInSb2 = plmi_.isRxInSideband( abNo,
                                                               antPol2,
                                                               sb.isUSB() );

            // If noise source is enabled we don't want to flag no rx in sb.
            const NoiseStatusMPE & noiseStatus = plmi_.noiseStatus( 
                getMonCorrDes( plType_ ) );

            const bool noiseDisabled = 
                ( noiseStatus.getValue() == NoiseStatusMPE::DISABLED );

            if ( !noiseStatus.isValid() || !rxInSb1.first || !rxInSb2.first ) 
            {
                bl.blankData( SB::MONITOR_DATA_BAD );
            } 
            else if ( noiseDisabled && (!rxInSb1.second || !rxInSb2.second ) ) 
            {
                sb.flagData( SB::NO_RX_IN_SIDEBAND );
            }
        }

        const MonitorPointByte &bf1mp = plmi_.blankStatus(abNo, antPol1);
        const MonitorPointByte &bf2mp = plmi_.blankStatus(abNo, antPol2);

        const unsigned char bf1 = bf1mp.getValue();
        const unsigned char bf2 = bf2mp.getValue();

        if ( !bf1mp.isValid() || !bf2mp.isValid() ) {
            // The blanking/flagging info from the fault system is bad
            bl.blankData( SB::MONITOR_DATA_BAD );
        } 
        else if(((carma::monitor::PipelineMonitorInput&)(plmi_)).isManuallyFlagged(abNo, input1, input2)) {
            //	  std::cout << "Blanking data for abNo = " << abNo << " input1 = " << input1 << " input2 = " << input2 << std::endl;
            bl.blankData( SB::MANUAL_FLAG);
        } 
        else {
            /*
             * Macro to save some typing and ensure that all bits sent
             * from the fault system are handled uniformly, without
             * accidental typos.
             * The FAULTNUM##_BLANK substitutes the variable for FAULTNUM,
             * giving you something like DRIVE_BLANK to indicate the user
             * preference (blank or flag) for drive errors.
             */
#define BLANK_FLAG_HELPER(VAR, FAULTENUM, CORRENUM)                             \
            if ((VAR) & carma::fault::BlankFlagBits::FAULTENUM) {               \
                if ((VAR) & carma::fault::BlankFlagBits::FAULTENUM##_BLANK) {   \
                    bl.blankData(SB::CORRENUM);                                 \
                } else {                                                        \
                    bl.flagData(SB::CORRENUM);                                  \
                }                                                               \
            }

            // first input
            BLANK_FLAG_HELPER(bf1, DRIVE,       A1_MINOR_TRACKING);
            BLANK_FLAG_HELPER(bf1, MONITORDATA, MONITOR_DATA_BAD);
            BLANK_FLAG_HELPER(bf1, OFFLINE,     A1_OFFLINE);
            BLANK_FLAG_HELPER(bf1, PHASELOCK,   A1_PHASELOCK);

            // second input
            BLANK_FLAG_HELPER(bf2, DRIVE,       A2_MINOR_TRACKING);
            BLANK_FLAG_HELPER(bf2, MONITORDATA, MONITOR_DATA_BAD);
            BLANK_FLAG_HELPER(bf2, OFFLINE,     A2_OFFLINE);
            BLANK_FLAG_HELPER(bf2, PHASELOCK,   A2_PHASELOCK);

#undef BLANK_FLAG_HELPER

        } // Else if monitor data bad

    } // Loop over baselines

} catch ( ... ) {
    logCaughtAsError( );
}

CorrelatorDataPtr 
BlankFlag::postprocess( CorrelatorDataPtr cd )
{
    BOOST_FOREACH( CorrelatorBand & cb, cd->getBands() ) {
        if ( !plmi_.astroBandOnline( cb.getBandNumber() ) ) 
            cb.setValid( false, CorrelatorSideband::BAND_OFFLINE );
    }

    blankCount_ = 0; 
    flagCount_ = 0;  
    totalDataCount_ = 0; 

    BOOST_FOREACH( const CorrelatorBand & cb, cd->getBands( ) ) 
    {
      BOOST_FOREACH( const CorrelatorBaseline & bl, cb.getBaselines() ) 
      {
        BOOST_FOREACH( const CorrelatorSideband & sb, bl.getSidebands() ) 
        {
          ++totalDataCount_;
          const MonitorPoint::BLANKING_FLAGGING bf = sb.getBlankFlagStatus();
          switch ( bf ) {
            case MonitorPoint::UNDETERMINED:
            case MonitorPoint::OK:
            case MonitorPoint::BLANKED_FLAGGED:
                break;
            case MonitorPoint::FLAGGED:
                ++flagCount_;
                break;
            case MonitorPoint::BLANKED:
                ++blankCount_;
                break;
            default:
                break;
          }
        }
      }
    }
                
    if ( totalDataCount_ > 0 ) {
        pctBlanked_ = ( blankCount_ * 100 ) / totalDataCount_;
        pctFlagged_ = ( flagCount_ * 100 ) / totalDataCount_;
    }    
    return cd;
}

void BlankFlag::fillMonitorData() {
    monitorData_.getBlankFlagStage().pctBlanked().setValue( pctBlanked_ );
    monitorData_.getBlankFlagStage().pctFlagged().setValue( pctFlagged_ );

    // Each pipeline contains a shadowing calculator and calculates the exact
    // same results for all antennas regardless of subarray ownership.  
    // I arbitrarily chose to only write these to the monitor system from 
    // the sl pipeline to avoid duplicate setting of the antenna MPs.
    if ( plType_ == pipeline::SL ) {
        for ( vector<bool>::size_type i = 0; i < Global::maxAntennas(); ++i ) 
        {
            MonitorPointBool & shadowMP = astroMonitor_.antenna( i ).shadowed();
            shadowMP.setValue( svShadowedAnts_.at(i) || blShadowedAnts_.at(i) );
        }
    }
}
