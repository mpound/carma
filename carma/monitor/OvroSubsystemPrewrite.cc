/**
 * @file 
 * Definition of carma::monitor::OvroSubsystemPrewrite class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.31 $
 * $Date: 2011/09/27 21:44:00 $
 * $Id: OvroSubsystemPrewrite.cc,v 1.31 2011/09/27 21:44:00 abeard Exp $
 */

#include "carma/monitor/OvroSubsystemPrewrite.h"

// CARMA includes
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/Varactor.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

// C++ Standard Library Includes
#include <iomanip>

using namespace carma::monitor;
using namespace carma::util;
using namespace std;

namespace { // Anonymous namespace for local riffraff

    typedef carma::monitor::AntennaCommon AC;
    typedef AntennaCommon::CurrentRxMonitorPointEnum CurrentRxMPE;
    typedef carma::monitor::GunnPll::LockStateMonitorPointEnum LockStateEnum;
    typedef carma::monitor::AntennaCommon::LoStateMonitorPointEnum LoStateEnum;
    typedef carma::monitor::GunnPll::SweepStatMonitorPointEnum SweepStatEnum;
    typedef carma::monitor::AntennaCommon::LoSweepMonitorPointEnum LoSweepEnum;
    typedef AC::RxTsysStateMonitorPointEnum RxTsysStateEnum;

    // Convert GunnPll lock state enumeration to antenna common lo state enum.
    LoStateEnum::LOSTATE
    convertLockStateToLoState( const LockStateEnum::LOCKSTATE lockState )
    {
        LoStateEnum::LOSTATE answer = LoStateEnum::FAILED;
        switch ( lockState ) {
            case LockStateEnum::WAIT_TUNER:
            case LockStateEnum::WAIT_BCKSHRT:
            case LockStateEnum::WAIT_ATTEN:
            case LockStateEnum::SEARCHING:
                answer = LoStateEnum::SEARCH;
                break;
            case LockStateEnum::REDUCING:
            case LockStateEnum::ADJUSTING:
                answer = LoStateEnum::OPTIMIZING;
                break;
            case LockStateEnum::LOCKED:
                answer = LoStateEnum::LOCK;
                break;
            case LockStateEnum::LOCKSTATE_COUNT:
            case LockStateEnum::UNLOCKED:
            default:
                answer = LoStateEnum::FAILED;        
                break;
        }
        return answer;
    } // End convertLockStateToLoState 
    
    // Convert GunnPll sweep state to antenna common lo sweep state enum.
    LoSweepEnum::LOSWEEP
    convertSweepStatToLoSweep( const SweepStatEnum::SWEEPSTAT sweepStat )
    {
        LoSweepEnum::LOSWEEP answer = LoSweepEnum::ON;
        switch ( sweepStat ) {
            case SweepStatEnum::ON:
                answer = LoSweepEnum::ON;
                break;
            case SweepStatEnum::OFF:
                answer = LoSweepEnum::OFF;
                break;
            case SweepStatEnum::SWEEPSTAT_COUNT:
            default:
                break; // Nothing.
        }
        return answer;
    } // End convertSweepStatToLoSweep

    AntennaCommon::CurrentRxMonitorPointEnum::CURRENTRX 
    currentRxFromIfSwitchStat( 
        const AntennaIF::IfSwitchStatMonitorPointEnum::IFSWITCHSTAT ifSwitchStat )
    {
        typedef carma::monitor::AntennaIF::IfSwitchStatMonitorPointEnum IfStat;
        switch ( ifSwitchStat ) {
            case IfStat::CHANGED: return CurrentRxMPE::MOVING;
            case IfStat::POS_1: return CurrentRxMPE::RX1CM;
            case IfStat::POS_2: return CurrentRxMPE::RX3MM;
            case IfStat::POS_3: return CurrentRxMPE::RX1MM;
            case IfStat::POS_4: return CurrentRxMPE::RXANY;
            case IfStat::STUCK: return CurrentRxMPE::ERROR;
            default: return CurrentRxMPE::ERROR;
        }
    } // ifSwitchStatFromRxType

    AntennaCommon::CurrentRxMonitorPointEnum::CURRENTRX
    currentRxFromRxSelect( 
        OvroSubsystem::RxSelectStateMonitorPointEnum::RXSELECTSTATE rxSelect )
    {
        typedef OvroSubsystem::RxSelectStateMonitorPointEnum RxSelectState;
        typedef AntennaCommon::CurrentRxMonitorPointEnum CurrentRx;

        switch ( rxSelect ) {
            case RxSelectState::RX_CM: return CurrentRx::RX1CM;
            case RxSelectState::RX_3MM: return CurrentRx::RX3MM;
            case RxSelectState::RX_1MM: return CurrentRx::RX1MM;
            case RxSelectState::GRID: return CurrentRx::RXANY;
            case RxSelectState::MOVING: return CurrentRx::MOVING;
            case RxSelectState::STUCK: return CurrentRx::ERROR;
            default: return CurrentRx::ERROR;
        }
    } // currentRxFromRxSelect
        
    void
    setCurrentRx( carma::monitor::OvroSubsystem & mon ) 
    {
        // The current receiver is determined by the values of the selected 
        // receiver as well as the selected IF switch position. If the two
        // don't match, the current receiver is set to ERROR.
        // Note we use the first (left pol) IF module only because the 
        // right pol IF module doesn't have a switch.
        typedef OvroSubsystem::RxSelectStateMonitorPointEnum RxSelectState;
        typedef AntennaIF::IfSwitchStatMonitorPointEnum IfSwitchStat;

        const RxSelectState::RXSELECTSTATE selectedRx = 
            mon.optics( ).rxSelectState( ).getValue( );
        const IfSwitchStat::IFSWITCHSTAT selectedIf = 
            mon.antennaIfContainer( 0 ).antennaIF().ifSwitchStat().getValue();
        
        AntennaCommon::Receivers & comRx = mon.antennaCommon().receivers();

        CARMA_CPTRACE( Trace::TRACE7, "setCurrentRx( ) - selectedRx="
                << mon.optics( ).rxSelectState( ).getValueToString( )
                << ", selectedIf=" << mon.antennaIfContainer( 0 ).antennaIF().
                   ifSwitchStat().getValueToString( ) 
                << ", called at " << Time::getTimeString( 3 ) << "." );

        CurrentRxMPE::CURRENTRX ifSwitchAsRx = 
            currentRxFromIfSwitchStat( selectedIf );      
        CurrentRxMPE::CURRENTRX currentRx = 
            currentRxFromRxSelect( selectedRx );
             
        if ( ifSwitchAsRx == currentRx ) {
            comRx.currentRx( ).setValue( currentRx );
        } else if ( currentRx == CurrentRxMPE::MOVING ) {
            comRx.currentRx( ).setValue( CurrentRxMPE::MOVING );
        } else if ( currentRx == CurrentRxMPE::RXANY ) {
            comRx.currentRx( ).setValue( CurrentRxMPE::RXANY );
        } else {
            comRx.currentRx( ).setValue( CurrentRxMPE::ERROR );
        }

    } // setCurrentRx 

    void 
    setCommonGunnMpsFromVaractor( carma::monitor::OvroSubsystem & mon )
    {
        const VaractorModule & varmon = mon.gunn1cm().varactor();
        AntennaCommon & common = mon.antennaCommon( );
        
        typedef VaractorModule::LockStatusMonitorPointEnum LockStat;
        const LockStat & lockstatMP = varmon.lockStatus();

        typedef AntennaCommon::LoStateMonitorPointEnum CommonLoState;

        if ( lockstatMP.getValidity() >= MonitorPoint::VALID ) {
            const LockStat::LOCKSTATUS lockstat = lockstatMP.getValue();
            switch ( lockstat ) {
                case LockStat::UNLOCKED:
                    common.lO().loState().setValue( CommonLoState::FAILED );
                    break;
                case LockStat::LOCKED:
                    common.lO().loState().setValue( CommonLoState::LOCK );
                    break;
                default:
                    break;
            }
        }

        typedef AntennaCommon::LoSweepMonitorPointEnum CommonSweep;

        if ( varmon.sweepEnabled().getValidity() >= MonitorPoint::VALID ) {
            if ( varmon.sweepEnabled().getValue() )
                common.lO().loSweep().setValue( CommonSweep::ON );
            else 
                common.lO().loSweep().setValue( CommonSweep::OFF );
        }

        if ( varmon.commandedFreq().getValidity() >= MonitorPoint::VALID ) {
            common.lO().oscFreq().setValue( varmon.commandedFreq().getValue() );
            common.lO().loFreq().setValue( varmon.commandedFreq().getValue() );
        }

    }



    void 
    setCommonGunnMonitorPoints( carma::monitor::OvroSubsystem & mon )
    {
        AntennaCommon & common = mon.antennaCommon( );
        AntennaCommon::Receivers & commonRx = common.receivers( );
        CurrentRxMPE::CURRENTRX currentRx = commonRx.currentRx( ).getValue( );
        GunnPll * gunn;

        // Get ovro gunn monitor point container. 
        switch ( currentRx ) {
            case AC::CurrentRxMonitorPointEnum::RX1MM:
            case AC::CurrentRxMonitorPointEnum::RXANY:
                gunn = &( mon.gunn1mm( ).gunnPll( ) );
                break;
            case AC::CurrentRxMonitorPointEnum::RX3MM:
                gunn = &( mon.gunn3mm( ).gunnPll( ) );
                break;
            case AC::CurrentRxMonitorPointEnum::RX1CM:
                setCommonGunnMpsFromVaractor( mon );
                return;
            default:
                return;
        }

        const int multiplier = gunn->multiplier().getValue( );
        const double freq = gunn->gunnFreq().getValue( );
        LockStateEnum::LOCKSTATE lockState = gunn->lockState().getValue( );
        SweepStatEnum::SWEEPSTAT sweepStat = gunn->sweepStat().getValue( );
        
        // Based on values of input OvroSubsystem Gunn module, set the
        // AntennaCommon values.
        if ( gunn->gunnFreq().getValidity() >= MonitorPoint::VALID ) {
            common.lO().oscFreq().setValue( freq );
            if ( gunn->multiplier().getValidity() >= MonitorPoint::VALID ) {
                common.lO().loFreq().setValue( multiplier * freq );
            }
        }
        if ( gunn->sweepStat().getValidity() >= MonitorPoint::VALID ) {
            common.lO().loSweep().setValue( 
                                    convertSweepStatToLoSweep( sweepStat ) );
        }
        if ( gunn->lockState().getValidity() >= MonitorPoint::VALID ) {
            common.lO().loState().setValue( 
                                    convertLockStateToLoState( lockState ) );
        }

    } // setCommonGunnMonitorPoints

    void 
    setRxStateIfGoodAndNotTuning( carma::monitor::OvroSubsystem & mon ) 
    {
        // This routine is designed to catch if the gunn or yig falls out of 
        // lock and mirror this error in the Rx state. 
        typedef AntennaCommon::RxStateMonitorPointEnum RxState;
        typedef OvroSubsystem::LockStateMonitorPointEnum YigLockState;
        typedef GunnPll::LockStateMonitorPointEnum GunnLockState;
        typedef SisReceiver::TuneStateMonitorPointEnum RxTuneState;

        AntennaCommon::Receivers & comRx = mon.antennaCommon().receivers();

        enum RxState::RXSTATE currentRxState = comRx.rxState( ).getValue( );
        
        CARMA_CPTRACE( Trace::TRACE7,
                       "setRxStateIfGoodAndNotTuning( ) - rxState="
                       << comRx.rxState( ).getValueToString( ) << "." );

        if ( currentRxState == RxState::TUNE ) {
            return;
        } 

        // The below states are set exclusively in the tune thread.  These 
        // states correspond to conditions that will generally not 
        // spontaneously fix themselves and are considered fatal. 
        if ( currentRxState == RxState::IFSEL_BAD ) {
            return;
        }

        if ( currentRxState == RxState::RXSEL_BAD ) {
            return;
        }
        
        carma::monitor::GunnPll * gunn;
        carma::monitor::SisReceiver * receiver;
        const enum CurrentRxMPE::CURRENTRX 
        currentRx = comRx.currentRx( ).getValue( );

        bool loNotLocked = false;
        bool rxNotTuned = false;
        switch ( currentRx ) {
            case CurrentRxMPE::RX1MM:
                gunn = &mon.gunn1mm( ).gunnPll( );
                loNotLocked = ( 
                    gunn->lockState( ).getValue( ) != GunnLockState::LOCKED );
                receiver = &mon.rx1mm( 0 ).sisReceiver( ); 
                rxNotTuned = 
                    ( receiver->tuneState().getValue() != RxTuneState::TUNED &&
                      receiver->tuneState().getValue() != RxTuneState::SET_IJ );
                break;
            case CurrentRxMPE::RX3MM:
                gunn = &mon.gunn3mm( ).gunnPll( );
                loNotLocked = ( 
                    gunn->lockState( ).getValue( ) != GunnLockState::LOCKED );
                receiver = &mon.rx3mm( ).sisReceiver( );
                rxNotTuned = 
                    ( receiver->tuneState().getValue() != RxTuneState::TUNED &&
                      receiver->tuneState().getValue() != RxTuneState::SET_IJ );
                break;
            case CurrentRxMPE::RX1CM:
                loNotLocked = 
                    mon.gunn1cm().varactor().lockStatus().getValue() != 
                    VaractorModule::LockStatusMonitorPointEnum::LOCKED;
            default: 
                break;
        }

        if ( mon.yig( ).lockState( ).getValue( ) != YigLockState::LOCKED ) {

            comRx.rxState( ).setValue( RxState::YIG_BAD );

        } else if ( loNotLocked ) {

            comRx.rxState( ).setValue( RxState::GUNN_BAD );


        } else if ( rxNotTuned ) {

            comRx.rxState( ).setValue( RxState::RX_BAD );

        } else {

            comRx.rxState( ).setValue( RxState::GOOD );

        }

    } // setRxStateIfGoodAndNotTuning

    void 
    setCryoCompressorState( carma::monitor::OvroSubsystem & mon ) 
    {
        typedef OvroSubsystem::FridgeDriveStateMonitorPointEnum OvroCompState;
        typedef AC::CompressorStateMonitorPointEnum CommonCompState;
        
        AntennaCommon::Receivers & commonRx = mon.antennaCommon( ).receivers( );

        MonitorPoint::VALIDITY driveStateValidity = 
            mon.cryo( ).compressor( ).fridgeDriveState( ).getValidity( );

        if ( driveStateValidity >= MonitorPoint::VALID ) {
            OvroCompState::FRIDGEDRIVESTATE compState =
                mon.cryo().compressor().fridgeDriveState().getValue();
            if ( compState == OvroCompState::ON )
                commonRx.compressorState().setValue( CommonCompState::ON );
            else if ( compState == OvroCompState::OFF )
                commonRx.compressorState().setValue( CommonCompState::OFF );
        }
    }

} // End namespace <unnamed>

// -----------------------------------------------------------------------------
OvroSubsystemPrewrite::OvroSubsystemPrewrite( OvroSubsystem & subsys ) 
    : mon_( subsys )
{    
    CARMA_CPTRACE(Trace::TRACE6, "OvroSubsystemPrewrite - C'tor.");
} 

// -----------------------------------------------------------------------------
OvroSubsystemPrewrite::~OvroSubsystemPrewrite()
{
    // Note: This should be safe now since Tom modified CARMA_CPTRACE
    // to use Trace::getProgramTraceIfAvailable().  Prior to this, 
    // this d'tor could get called after Program was torn down leading to 
    // segfaults.
    CARMA_CPTRACE(Trace::TRACE6, "OvroSubsystemPrewrite - D'tor");
}

// -----------------------------------------------------------------------------
int OvroSubsystemPrewrite::execute() const
{
    // Fill AntennaCommon MPs based on the values of OVRO monitor points. 
    CARMA_CPTRACE(Trace::TRACE7, "OvroSubsystemPrewrite::execute().");

    setCurrentRx( mon_ );

    setRxStateIfGoodAndNotTuning( mon_ );

    setCommonGunnMonitorPoints( mon_ );

    setCryoCompressorState( mon_ );
    
    return 0;
}
