/**
 * @file
 * Class definition of CmRxControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.19 $
 * $Date: 2013/07/08 21:42:01 $
 * $Id: CmRxControlImpl.cc,v 1.19 2013/07/08 21:42:01 scott Exp $
 */

#include "carma/antenna/ovro/control/CmRxControlImpl.h"

// Carma includes
#include "carma/antenna/common/loggingUtils.h"
#include "carma/antenna/common/FrontEndControl_skel.h"
#include "carma/antenna/common/FrontEndControl_skel_tie.h"
#include "carma/antenna/common/IFControl_skel.h"
#include "carma/antenna/common/IFControl_skel_tie.h"
#include "carma/antenna/common/OpticsControl.h"
#include "carma/antenna/common/OpticsControl_skel.h"
#include "carma/antenna/common/OpticsControl_skel_tie.h"
#include "carma/antenna/ovro/canbus/OvroMaster.h"
#include "carma/antenna/ovro/control/CalibratorControlImpl.h"
#include "carma/antenna/ovro/control/IFControlImpl.h"
#include "carma/antenna/ovro/control/CmLOControlImpl.h"
#include "carma/antenna/ovro/control/OpticsControlImpl.h"
#include "carma/antenna/ovro/control/ovroCmLOControl.h"
#include "carma/antenna/ovro/control/ovroCmLOControl_skel.h"
#include "carma/antenna/ovro/control/ovroCmLOControl_skel_tie.h" 
#include "carma/antenna/ovro/control/ovroPolarizationControl.h"
#include "carma/antenna/ovro/control/ovroPolarizationControl_skel.h"
#include "carma/antenna/ovro/control/ovroPolarizationControl_skel_tie.h"
#include "carma/antenna/ovro/control/PolarizationControlImpl.h"
#include "carma/corba/Server.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/BaseException.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadAttr.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Trace.h"
#include "carma/services/Table.h"

// Carma tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// STL
#include <iostream>
#include <vector>

using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace { // Anonymous namespace for local constants, typedefs and riffraff.

    const unsigned int FRAMETIMER_OFFSET_NANOS = 200000;
    const bool CONTINUE_ON_TUNE_ERRORS = true;
    const unsigned IF_SWITCH_TIMEOUT = 10; // Frames
    const unsigned RX_SELECT_TIMEOUT = 360; // 360 Frames == 3 minutes.
    const unsigned YIG_LOCK_TIMEOUT = 30; // Frames
    const unsigned YIG_LOCK_MIN = 10; // Wait at least frames
    const unsigned GUNN_LOCK_TIMEOUT = 60; // Frames
    const unsigned GUNN_LOCK_MIN = 2; // Wait at least frames.
    const unsigned RX_LOCK_TIMEOUT = 60; // Frames
    const unsigned RX_LOCK_MIN = 30;
    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE5;
    const Trace::TraceLevel TRACE_SET_FREQ = Trace::TRACE5;

    // Aliases for wordy class::innerclass::enumeration monitor points.
    typedef OvroSubsystem::RxSelectStateMonitorPointEnum RxSelectSPE;
    typedef enum RxSelectSPE::RXSELECTSTATE RxSelect;

    // Constant maps for converting between RxType enumeration and all
    // other various Rx mapped enumerations both monitor and control.
    typedef map<RxControl::Type, OvroMaster::GunnType> RxToLoMap;
    typedef map<RxControl::Type, OvroMaster::SisRxType> RxToSisMap;
    typedef map<RxControl::Type, RxSelect> RxToRxSelectMap;

    const RxToLoMap::value_type rxToLoArray[] =
    {
        RxToLoMap::value_type(RxControl::RX1CM, OvroMaster::LO1CM),
        RxToLoMap::value_type(RxControl::RX1MM, OvroMaster::LO1MM),
        RxToLoMap::value_type(RxControl::RX3MM, OvroMaster::LO3MM),
        RxToLoMap::value_type(RxControl::RXANY, OvroMaster::LOANY)
    };

    const RxToSisMap::value_type rxToSisArray[] =
    {
        RxToSisMap::value_type(RxControl::RX1MM, OvroMaster::SIS_RX1MM),
        RxToSisMap::value_type(RxControl::RX3MM, OvroMaster::SIS_RX3MM)
    };

    const RxToRxSelectMap::value_type rxToRxSelectArray[] =
    {
        RxToRxSelectMap::value_type(RxControl::RX1CM, RxSelectSPE::RX_CM),
        RxToRxSelectMap::value_type(RxControl::RX1MM, RxSelectSPE::RX_1MM),
        RxToRxSelectMap::value_type(RxControl::RX3MM, RxSelectSPE::RX_3MM),
        RxToRxSelectMap::value_type(RxControl::RXANY, RxSelectSPE::GRID)
    };

    RxToLoMap rx2lo(
            rxToLoArray,
            rxToLoArray + sizeof(rxToLoArray)/sizeof(rxToLoArray[0]) );

    RxToSisMap rx2sis(
            rxToSisArray,
            rxToSisArray + sizeof(rxToSisArray)/sizeof(rxToSisArray[0]) );

    RxToRxSelectMap rx2rxselect(
            rxToRxSelectArray,
            rxToRxSelectArray +
            sizeof(rxToRxSelectArray)/sizeof(rxToRxSelectArray[0]) );

    template<typename T, typename E>
    bool
    waitForStateChange(
        T & mon,
        E state,
        unsigned maxFrames,
        unsigned minFrames = 0 )
    {
        unsigned frames = 0;
        FrameAlignedTimer frametimer( FRAMETIMER_OFFSET_NANOS );
        frametimer.ResetNextFireTime();
        do {
            if ( mon.getValue() == state && frames >= minFrames) {
                CARMA_CPTRACE(Trace::TRACE2, "waitForStateChange()" " - State "
                        "changed as expected after " << frames << " frames.");
                return false;
            } else {
                CARMA_CPTRACE(Trace::TRACE2, "waitForStateChange()" " - State "
                        "not changed after " << frames << " frames.");
            }
            frametimer.WaitForNextFireTime();
        } while ( ++frames < maxFrames );
        return true;
    } // End waitForStateChange

    template<typename T, typename E>
    bool
    waitForStateChange(
        T & mon,
        vector< E > states,
        unsigned maxFrames,
        unsigned minFrames = 0 )
    {
        unsigned frames = 0;
        FrameAlignedTimer frametimer( FRAMETIMER_OFFSET_NANOS );
        frametimer.ResetNextFireTime();
        do {
            if ( frames >= minFrames) {
                const E monValue = mon.getValue( );
                typename vector< E >::const_iterator i = states.begin( );
                const typename vector< E >::iterator iEnd = states.end( );
                for ( ; i != iEnd; ++i ) {
                    if ( monValue == (*i) ) {
                        CARMA_CPTRACE( Trace::TRACE2, "waitForStateChange()"
                            " - State changed as expected after "
                            << frames << " frames.");
                        return false;
                    }
                }
            }
            frametimer.WaitForNextFireTime();
        } while ( ++frames < maxFrames );

        return true;
    } // End waitForStateChange

    // Exception purely for timeouts
    class InternalTuneFailException {
    public:

        explicit InternalTuneFailException( ) { };

        /* virtual */ ~InternalTuneFailException( ) { };

    };

} // End namespace <unnamed>

CmRxControlImpl::CmRxControlImpl(
    carma::antenna::ovro::OvroMaster & master,
    carma::antenna::ovro::CalibratorControlImpl & cal,
    OvroSubsystem & ovroSubsys,
    corba::Server & server,
    const unsigned short antennaId,
    const std::string & confDir ) :   
        frontEnd_( master.getCMReceiver() ),
        if_( master.getAntennaIF( OvroMaster::IF1 ),
             carma::antenna::common::RxControl::RX1CM,
             carma::antenna::common::RxControl::IF1 ),
        lo_( master.getYigPll(),
             master.getGunn( OvroMaster::LO3MM ),
             master.getVaractor( ),
             master.getLOReferenceMonitor() ),
        optics_( master.getOptics(), 
                 common::RxControl::RX1CM ),
        polarization_( master.getOptics(), 
                       common::RxControl::RX1CM ),
        cal_( cal ), 
        log_( Program::getLogger( ) ),
        mon_( ovroSubsys ),
        comMon_( ovroSubsys.antennaCommon( ) ),
        rxType_( carma::antenna::common::RxControl::RX1CM ),
        antennaId_( antennaId ),
        confDir_( confDir )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR,
                   "CmRxControlImpl() - Creating receiver control object for "
                   << rxType_.rxAsString() << ", " 
                   << rx2lo[ common::RxControl::RX1CM ] );

    namespace POA_cac = POA_carma::antenna::common;
    namespace POA_cao = POA_carma::antenna::ovro;

    server.addServant< POA_cac::FrontEndControl_tie >( frontEnd_,
                                                       frontEndPtr_ );
    server.addServant< POA_cac::IFControl_tie >( if_, ifPtr_ );
    server.addServant< POA_cao::CmLOControl_tie >( lo_, loPtr_ );
    server.addServant< POA_cac::OpticsControl_tie >( optics_, opticsPtr_ );
    server.addServant< POA_cao::PolarizationControl_tie >( polarization_, 
                                                           polPtr_ );
}

CmRxControlImpl::~CmRxControlImpl( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR,
                   "CmRxControlImpl::~RxControlImpl( ) - Dtor." );
}

carma::antenna::common::LOControl_ptr
CmRxControlImpl::LO( )
try {
    logInfoWithRxNdc( rxType_, "CmRxControlImpl::LO( ) - Retrieving LO." );

    return common::LOControl::_duplicate( loPtr_ );

} catch ( const CORBA::SystemException &cex ) {
    throw; // Rethrow
    return LOControl::_nil( ); // Squash compiler warnings.
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
    return LOControl::_nil( ); // Squash compiler warnings.
}

carma::antenna::common::IFControl_ptr
CmRxControlImpl::IF( carma::antenna::common::RxControl::IF_Type pol )
try {

    // Commented out the exceptions when IF does not match Rx so that
    // this interface can be used to independently test the IF switch.
    ostringstream oss;
    oss << "CmRxControlImpl::IF(pol=";
    if ( pol == carma::antenna::common::RxControl::IF1 ) {
        oss << "IF1";
    }
    else if ( pol == carma::antenna::common::RxControl::IF2 ) {
        oss << "IF2";
        //throw CARMA_EXCEPTION( UserException, "CmRxControlImpl::IF() - IF2 "
        //    "is only available when tuned to 1mm." );
    }
    else if ( pol == carma::antenna::common::RxControl::BOTH ) {
        oss << "IFBOTH";
        //throw CARMA_EXCEPTION( UserException, "CmRxControlImpl::IF() - BOTH "
        //    "is only available when tuned to 1mm." );
    }
    else {
        throw CARMA_EXCEPTION( UserException, "CmRxControlImpl::IF() - "
            "Invalid IF type." );
    }

    oss << ") => Retrieving specified IF.";
    logInfoWithRxNdc( rxType_, oss.str() );

    return common::IFControl::_duplicate( ifPtr_ );

} catch ( const CORBA::SystemException & cex ) {
    throw; // Rethrow
    return IFControl::_nil( ); // Squash compiler warnings.
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
    return IFControl::_nil( ); // Squash compiler warnings.
}

carma::antenna::common::FrontEndControl_ptr
CmRxControlImpl::FrontEnd(
        carma::antenna::common::RxControl::Pol_Type pol )
try {
    // No front end control for cm receiver.
    logInfoWithRxNdc( rxType_, "CmRxControlImpl::FrontEnd( ) - "
                      "Retrieving Rx front end." );

    return common::FrontEndControl::_duplicate( frontEndPtr_ );

} catch ( const CORBA::SystemException & cex ) {
    throw; // Rethrow
    return FrontEndControl::_nil( );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
    return FrontEndControl::_nil( );
}

carma::antenna::common::OpticsControl_ptr
CmRxControlImpl::Optics( )
try {
    logInfoWithRxNdc( rxType_, "RxControlImpl::Optics() - Retrieving Optics." );

    return common::OpticsControl::_duplicate( opticsPtr_ );

} catch ( const CORBA::SystemException & cex ) {
    throw; // Rethrow
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
    return OpticsControl::_nil( ); // Squash compiler warnings.
}

carma::antenna::common::PolarizationControl_ptr
CmRxControlImpl::Polarization( )
try {
    logInfoWithRxNdc( rxType_, "RxControlImpl::Polarization() - "
                      "Retrieving Polarization." );

    return common::PolarizationControl::_duplicate( polPtr_ );

} catch ( const CORBA::SystemException & cex ) {
    throw; // Rethrow
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
    return PolarizationControl::_nil( ); // Squash compiler warnings
}

void
CmRxControlImpl::setFrequency( ::CORBA::Double yigFreq,
                               ::CORBA::Double LOfreq,
                               ::CORBA::Boolean endWithAbsorberInBeam,
                               ::CORBA::Boolean optimizeReceiver,
                               ::CORBA::Boolean forceRelock, // Not used by ovro
                               ::CORBA::ULong seqNo )
try {
    // Verify that frequencies are in range..
    if ( lo_.yigFreqOutOfRange( yigFreq ) ) {
        throw CARMA_EXCEPTION( UserException,
                "Yig frequency is out of range." );
    }

    if ( lo_.loFreqOutOfRange( LOfreq ) ) {
        throw CARMA_EXCEPTION( UserException,
                "LO frequency is out of range." );
    }

    ostringstream msg;
    msg << "CmRxControlImpl::setFrequencyWithSeqNo( yigFreq=" << yigFreq
        << " , LOFreq=" << LOfreq << " , seqNo=" << seqNo << " )";

    logInfoWithRxNdc( rxType_, msg.str( ) );

    SetFreqArgType args = { *this,
                            yigFreq,
                            LOfreq,
                            seqNo,
                            false,
                            endWithAbsorberInBeam };

    StartPthreadWithCopy(
        setFrequencyEntryPoint,
        args,
        "CmRxControlImpl::setFrequencyEntryPoint",
        & (PthreadAttr( PTHREAD_CREATE_DETACHED ).InternalPthreadAttr( ) ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
CmRxControlImpl::setObservingFrequency( ::CORBA::Double freq,
                                        ::CORBA::ULong seqNo )
try {

    ostringstream msg;
    msg << "CmRxControlImpl::setObservingFrequency( freq=" << freq
         << ", seqNo=" << seqNo << " ).";

    logInfoWithRxNdc( rxType_, msg.str( ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
CmRxControlImpl::measureTotalPower(
    carma::antenna::common::CalibratorControl::Position position,
    ::CORBA::ULong seqNo )
try {
    ostringstream msg;
    msg << "CmRxControlImpl::measureTotalPower( position="
        << position  << ", seqNo=" << seqNo << " )";

    logInfoWithRxNdc( rxType_, msg.str( ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
CmRxControlImpl::toggleFastSampling( CORBA::ULong channel,
                                     CORBA::Boolean start )
try {
    ostringstream msg;
    msg << "CmRxControlImpl::toggleFastSampling( channel=" << channel
        << ", start=" << boolalpha << start << noboolalpha << " ) "
        << "- Fast sampling IS NOT IMPLEMENTED.";

    logInfoWithRxNdc( rxType_, msg.str( ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
CmRxControlImpl::setIFPresetPower( )
try {

    if_.setPresetPower( );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
CmRxControlImpl::setIFAtten(
    const CORBA::Float atten,
    const carma::antenna::common::RxControl::IF_Type ifType )
try {

    if ( ifType == carma::antenna::common::RxControl::IF1 ) {
        if_.setAtten( atten );
    } else {
        throw CARMA_EXCEPTION( UserException, "CmRxControlImpl::setIFatten() "
            "- Only IF1 is available from CM Rx Control." );
    }

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
CmRxControlImpl::setIFPower( CORBA::Float power )
try {

    if_.setPower( power );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
CmRxControlImpl::biasRxUsingConfFile( )
try {
    const string descTabName( confDir_ + "antenna/ovro/desc.tab" );
    const string cmrxTabName( confDir_ + "antenna/ovro/rx/cmDewars.tab" );
    ostringstream antNameOss;
    antNameOss << "ovro" << antennaId_;

    // Read the desc.tab table and determine which rx to talk to.
    services::Table descTable( descTabName );
    string rxId;
    const vector< string > rxids = descTable.getColumn( "cmdewar" );
    const vector< string > names = descTable.getColumn( "name" );

    if ( rxids.size() != names.size() ) 
        throw CARMA_EXCEPTION( UserException, "CmRxControlImpl::"
            "biasRxUsingConfFile() - name and cmdewar column sizes do "
            "not match!" );

    for ( vector< string >::size_type i = 0; i < names.size(); ++i ) {
        if ( names[ i ] == antNameOss.str() ) {
            rxId = rxids[ i ];
            break;
        }
    }

    if ( rxId == "" ) 
        throw CARMA_EXCEPTION( UserException, "CmRxControlImpl::"
            "biasRxUsingConfFile() - cmdewar rx id not found." );

    // Retrieve parameters from cmdewars table.
    services::Table cmrxTab( cmrxTabName );
    const vector< string > rxnames = cmrxTab.getColumn( "name" );
    int rowIdx = -1;
    for ( vector< string >::size_type i = 0; i < rxnames.size(); ++i ) {
        if ( rxnames[ i ] == rxId ) {
            rowIdx = i;
            break;
        }
    }

    if ( rowIdx == -1 ) 
        throw CARMA_EXCEPTION( UserException, "CmRxControlImpl::"
            "biasRxUsingConfFile() - rx id not found in cmDewars.tab." );

    const double Vd1 = cmrxTab.getDoubleColumn( "Vd1" ).at( rowIdx );
    const double Vd2 = cmrxTab.getDoubleColumn( "Vd2" ).at( rowIdx );
    const double Vd3 = cmrxTab.getDoubleColumn( "Vd3" ).at( rowIdx );
    const double Vd4 = cmrxTab.getDoubleColumn( "Vd4" ).at( rowIdx );
    const double Id1 = cmrxTab.getDoubleColumn( "Id1" ).at( rowIdx );
    const double Id2 = cmrxTab.getDoubleColumn( "Id2" ).at( rowIdx );
    const double Id3 = cmrxTab.getDoubleColumn( "Id3" ).at( rowIdx );
    const double Id4 = cmrxTab.getDoubleColumn( "Id4" ).at( rowIdx );
    const double Ig = cmrxTab.getDoubleColumn( "Ig" ).at( rowIdx );

    // Bias the receiver.
    frontEnd_.setVD( FrontEndControl::RF, FrontEndControl::FIRST, Vd1 );
    frontEnd_.setVD( FrontEndControl::RF, FrontEndControl::SECOND, Vd2 );
    frontEnd_.setVD( FrontEndControl::RF, FrontEndControl::THIRD, Vd3 );
    frontEnd_.setVD( FrontEndControl::RF, FrontEndControl::FOURTH, Vd4 );
    frontEnd_.setID( FrontEndControl::RF, FrontEndControl::FIRST, Id1 );
    frontEnd_.setID( FrontEndControl::RF, FrontEndControl::SECOND, Id2 );
    frontEnd_.setID( FrontEndControl::RF, FrontEndControl::THIRD, Id3 );
    frontEnd_.setID( FrontEndControl::RF, FrontEndControl::FOURTH, Id4 );
    frontEnd_.setID( FrontEndControl::IF, FrontEndControl::FIRST, Ig );

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
CmRxControlImpl::setFrequencyEntryPoint( const SetFreqArgType & args )
{
    CARMA_CPTRACE( TRACE_SET_FREQ, "Thread started successfully." );

    CmRxControlImpl & This = args.This;

    static bool tuneSeqInitialized = false;

    // Initialize the tune seq persistent mon point
    if (!tuneSeqInitialized) {
        This.comMon_.receivers( ).tuneSeqNum( ).setValue(0);
        tuneSeqInitialized = true;
    }

    try {

        // More complete cut at tuning algorithm:
        //    Reviewed with James Lamb 8/9/05
        //    Yet to be discovered subtleties may complicate this further.

        // Set carma::monitor::AntennaCommon monitor points.
        // See monitor/OvroSubsystemPrewrite for more on how these MPs can
        // get set.

        // For now, set the current Rx monitor point - later this will come
        // from the optics module AND IF switch.
        This.comMon_.receivers().currentRx().setValue(
                            This.rxType_.rxAsCurrentRxMonitorPointEnum( ) );
        This.comMon_.receivers().rxState().setValue(
                            AntennaCommon::RxStateMonitorPointEnum::TUNE );
        // -1) Bias the receiver
        This.biasRxUsingConfFile();

        // 0) Set the LO termination attenuation (LoReference module)
        This.lo_.setLoTerminatorPowerToPreset( );

        // 1) Select IF switch position and wait for switch
        CARMA_CPTRACE( TRACE_SET_FREQ, "Selecting IF switch position.");

        This.if_.selectRx();

        // Use the first (left pol) IF module only as the right pol module
        // does not have a switch.
        bool ifSelectTimeout = waitForStateChange(
                This.mon_.antennaIfContainer( 0 ).antennaIF( ).ifSwitchStat( ),
                This.rxType_.rxAsIfSwitchStatMonitorPointEnum( ),
                IF_SWITCH_TIMEOUT );
        if ( ifSelectTimeout ) {
            string msg = "Timedout waiting for ifSelection.";

            if ( CONTINUE_ON_TUNE_ERRORS ) {
                msg += " Ignoring error and continuing anyways.";
            }

            CARMA_CPTRACE( TRACE_SET_FREQ, msg );

            logInfoWithRxNdc( This.rxType_, msg );

            if ( !CONTINUE_ON_TUNE_ERRORS ) {
                throw InternalTuneFailException( );
            }
        }

        // 2) Move receiver into the beam.
        CARMA_CPTRACE( TRACE_SET_FREQ, "Moving receiver into beam." );

        This.optics_.selectRx( );

        bool rxSelectTimeout = waitForStateChange(
                This.mon_.optics().rxSelectState(),
                rx2rxselect[This.rxType_.rxAsRxControlType( )],
                RX_SELECT_TIMEOUT );
        if ( rxSelectTimeout ) {
            string msg = "Timedout waiting for optics rx selection.";

            if ( CONTINUE_ON_TUNE_ERRORS ) {
                msg += " Ignoring error and continuing anyways.";
            }

            CARMA_CPTRACE( TRACE_SET_FREQ, msg );

            logInfoWithRxNdc( This.rxType_, msg );

            if ( !CONTINUE_ON_TUNE_ERRORS ) {
                throw InternalTuneFailException( );
            }
        }

        // 3) Set the YIG frequency and lock.  Retry if necessary.
        CARMA_CPTRACE( TRACE_SET_FREQ, "Setting yig frequency." );

        bool yigTimeout = true;
        unsigned retries = 0;
        const unsigned MAX_RETRIES = 2;
        for ( ; retries < MAX_RETRIES && yigTimeout; ++retries ) {

            This.lo_.setYigFrequency( args.yigFreq );

            yigTimeout = waitForStateChange(
                    This.mon_.yig( ).lockState( ),
                    OvroSubsystem::LockStateMonitorPointEnum::LOCKED,
                    YIG_LOCK_TIMEOUT,
                    YIG_LOCK_MIN );
        }

        if ( yigTimeout ) {
            ostringstream msg;
            msg << "Unable to lock yig after " << retries << " attempts.";

            if ( CONTINUE_ON_TUNE_ERRORS ) {
                msg << " Ignoring error and continuing anyways.";
            }

            CARMA_CPTRACE( TRACE_SET_FREQ, msg.str( ) );

            logInfoWithRxNdc( This.rxType_, msg.str( ) );

            if ( !CONTINUE_ON_TUNE_ERRORS ) {
                throw InternalTuneFailException( );
            }
        }

        // 4) Set the LO frequency and wait for lock.
        CARMA_CPTRACE( TRACE_SET_FREQ, "Waiting for varactor to lock." );

        This.lo_.setLoFrequency( args.loFreq );

        bool gunnTimeout = waitForStateChange(
                This.mon_.gunn1cm().varactor().lockStatus(),
                carma::monitor::VaractorModule::LockStatusMonitorPointEnum::LOCKED,
                GUNN_LOCK_TIMEOUT,
                GUNN_LOCK_MIN );
        if ( gunnTimeout ) {
            string msg = "Timedout waiting for gunn to lock.";

            if ( CONTINUE_ON_TUNE_ERRORS ) {
                msg += " Ignoring error and continuing anyways.";
            }

            CARMA_CPTRACE( TRACE_SET_FREQ, msg );

            logInfoWithRxNdc( This.rxType_, msg );

            if ( !CONTINUE_ON_TUNE_ERRORS ) {
                throw InternalTuneFailException( );
            }
        }

        if ( ifSelectTimeout ) {
            This.comMon_.receivers().rxState().setValue(
                    AntennaCommon::RxStateMonitorPointEnum::IFSEL_BAD );
        } else if ( rxSelectTimeout ) {
            This.comMon_.receivers().rxState().setValue(
                    AntennaCommon::RxStateMonitorPointEnum::RXSEL_BAD );
        } else if ( yigTimeout ) {
            This.comMon_.receivers().rxState().setValue(
                AntennaCommon::RxStateMonitorPointEnum::YIG_BAD );
        } else if ( gunnTimeout ) {
            This.comMon_.receivers().rxState().setValue(
                AntennaCommon::RxStateMonitorPointEnum::GUNN_BAD );
        } else {
            This.comMon_.receivers().rxState().setValue(
                    AntennaCommon::RxStateMonitorPointEnum::GOOD);
        }

    } catch ( const InternalTuneFailException & ) {
        // Fall through
    } catch (...) {
        try {
            This.comMon_.receivers().rxState().setValue(
                    AntennaCommon::RxStateMonitorPointEnum::BAD );
            logCaughtAsError( This.log_ );
        } catch (...) {
            logCaughtAsError( This.log_ );
        }
    }

    // 6) If requested, put the absorber in the beam and set sequence number.
    //    This is done independent of the success or failure of tuning.
    try {

        if ( args.endWithAbsorberInBeam ) {

            if ( args.ignoreSeqNo )
                This.cal_.setPos( CalibratorControl::AMBIENT );
            else
                This.cal_.setPos( CalibratorControl::AMBIENT,
                                  args.seqNo,
                                  true ); // Sequence # from rx.

        } else {

            if ( !args.ignoreSeqNo )
                This.comMon_.receivers( ).tuneSeqNum( ).setValue( args.seqNo );

        }

    } catch ( ... ) {
        logCaughtAsError( This.log_ );
    }

    CARMA_CPTRACE( TRACE_SET_FREQ, "Thread finished." );
} // setFrequencyThread
