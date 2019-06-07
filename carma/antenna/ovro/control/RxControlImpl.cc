 /**
 * @file
 * Class definition of RxControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.86 $
 * $Date: 2014/09/10 21:47:03 $
 * $Id: RxControlImpl.cc,v 1.86 2014/09/10 21:47:03 scott Exp $
 */

#include "carma/antenna/ovro/control/RxControlImpl.h"


// Carma includes
#include "carma/antenna/common/loggingUtils.h"

#include "carma/antenna/ovro/control/ovroFrontEndControl.h"
#include "carma/antenna/ovro/control/ovroFrontEndControl_skel.h"
#include "carma/antenna/ovro/control/ovroFrontEndControl_skel_tie.h"
#include "carma/antenna/common/IFControl_skel.h"
#include "carma/antenna/common/IFControl_skel_tie.h"
#include "carma/antenna/ovro/control/ovroLOControl.h"
#include "carma/antenna/ovro/control/ovroLOControl_skel.h"
#include "carma/antenna/ovro/control/ovroLOControl_skel_tie.h"
#include "carma/antenna/common/OpticsControl_skel.h"
#include "carma/antenna/common/OpticsControl_skel_tie.h"
#include "carma/antenna/ovro/control/ovroPolarizationControl.h"
#include "carma/antenna/ovro/control/ovroPolarizationControl_skel.h"
#include "carma/antenna/ovro/control/ovroPolarizationControl_skel_tie.h"

#include "carma/antenna/ovro/canbus/EnvironmentalMonitor.h"
#include "carma/antenna/ovro/canbus/OvroMaster.h"
#include "carma/antenna/ovro/control/CalibratorControlImpl.h"
#include "carma/antenna/ovro/control/FrontEndControlImpl.h"
#include "carma/antenna/ovro/control/IFControlImpl.h"
#include "carma/antenna/ovro/control/LOControlImpl.h"
#include "carma/antenna/ovro/control/OpticsControlImpl.h"
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
    const unsigned RX_SELECT_TIMEOUT = 360; // 360 frames == 3 minutes.
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
    typedef carma::antenna::common::RxControl  RxCon;
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

    carma::monitor::GunnPll & getGunnPllMonitorRef(
        RxControl::Type rxtype,
        OvroSubsystem & ovroSubsys )
    {
        switch ( rxtype ) {
            case RxControl::RX1MM:
                return ovroSubsys.gunn1mm().gunnPll();
            case RxControl::RX3MM:
                return ovroSubsys.gunn3mm().gunnPll();
            case RxControl::RXANY:
            case RxControl::RX1CM:
            default:
                throw CARMA_EXCEPTION( ErrorException, "Receiver type not "
                        "implemented." );
        }
    } // End getGunnPllMonitorRef

    carma::monitor::SisReceiver & getSisRxMonitorRef(
        RxControl::Type rxtype,
        RxControl::Pol_Type pol,
        OvroSubsystem & ovroSubsys )
    {
        switch ( rxtype ) {
            case RxControl::RX1MM:
                if ( pol == RxControl::LEFTCIRCULAR )
                    return ovroSubsys.rx1mm( 0 ).sisReceiver();
                else if ( pol == RxControl::RIGHTCIRCULAR )
                    return ovroSubsys.rx1mm( 1 ).sisReceiver();
                else
                    throw CARMA_EXCEPTION( ErrorException, "Invalid pol type.");
            case RxControl::RX3MM:
                return ovroSubsys.rx3mm().sisReceiver();
            case RxControl::RXANY:
            case RxControl::RX1CM:
            default:
                throw CARMA_EXCEPTION( ErrorException, "Receiver type not "
                        "implemented." );
        }
    } // End getSisRxMonitorRef

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

    string
    ifTypeAsString( const carma::antenna::common::RxControl::IF_Type ifType )
    {
        switch ( ifType ) {
            case carma::antenna::common::RxControl::IF1:
                return "IF1";
                break;
            case carma::antenna::common::RxControl::IF2:
                return "IF2";
                break;
            case carma::antenna::common::RxControl::BOTH:
                return "BOTH";
                break;
            default:
                throw CARMA_EXCEPTION( ErrorException, "Invalid ifType" );
        }
    }


} // End namespace <unnamed>

// -----------------------------------------------------------------------------
RxControlImpl::RxControlImpl(
    carma::antenna::ovro::OvroMaster & master,
    carma::antenna::ovro::CalibratorControlImpl & cal,
    carma::antenna::common::RxControl::Type type,
    OvroSubsystem & ovroSubsys,
    carma::corba::Server & server ) 
    :   lo_( master.getYigPll(),
             master.getGunn( rx2lo[type] ),
             master.getVaractor( ),
             master.getLOReferenceMonitor(),
             type ),
        optics_( master.getOptics(), 
                 type ),
        polarization_( master.getOptics(),
                       type ),
        cal_( cal ),
        log_( Program::getLogger( ) ),
        mon_( ovroSubsys ),
        comMon_( ovroSubsys.antennaCommon( ) ),
        rxType_( type )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR,
                   "RxControlImpl() - Creating receiver control object for "
                   << rxType_.rxAsString() << ", " << rx2lo[type]);

    // Prepare yourself for a dynamic mess - it should be obvious why these
    // are being allocated dynamically (hint: it's a direct consequence of the
    // antenna API), but if not, it's because there are three separate mm 
    // receivers all dynamically accessed by this control implementation. I'm 
    // sure there are similar messes in the 6m and 3.5m implementations.

    // Setup stuff...
    namespace POA_cac = POA_carma::antenna::common;
    namespace POA_cao = POA_carma::antenna::ovro;
    carma::antenna::common::IFControl_ptr ifcp;
    ovro::AntennaIF& aif1 = master.getAntennaIF(OvroMaster::IF1);
    ovro::AntennaIF& aif2 = master.getAntennaIF(OvroMaster::IF2);
    IFControlImpl* ifci = 0;
    ifci = new IFControlImpl(aif1, type, RxCon::IF1);
    ifMap_[RxCon::IF1] = ifci;
    server.addServant<POA_cac::IFControl_tie>(*ifMap_[RxCon::IF1], ifcp);
    ifPtrMap_[RxCon::IF1] = ifcp;
    ifci = new IFControlImpl(aif2, type, RxCon::IF2);
    ifMap_[RxCon::IF2] = ifci;
    server.addServant< POA_cac::IFControl_tie >(*ifMap_[RxCon::IF2], ifcp);
    ifPtrMap_[RxCon::IF2] = ifcp;
 
   
    // Assign and/or create the subobjects this interface is responsible for.
    carma::antenna::ovro::FrontEndControl_ptr fecp;
    if (type == RxCon::RX1MM) {
        frontEndMap_[ carma::antenna::common::RxControl::LEFTCIRCULAR ] =
            new FrontEndControlImpl(
                master.getSisReceiver( rx2sis[type],
                                       OvroMaster::LEFT_CIRCULAR ),
                type,
                carma::antenna::common::RxControl::LEFTCIRCULAR,
                master.getAntennaIF( OvroMaster::IF1 ) );

        server.addServant< POA_cao::FrontEndControl_tie >(
            *frontEndMap_[ RxControl::LEFTCIRCULAR ], fecp );
        frontEndPtrMap_[ RxControl::LEFTCIRCULAR ] = fecp;

        frontEndMap_[ carma::antenna::common::RxControl::RIGHTCIRCULAR ] =
            new FrontEndControlImpl(
                master.getSisReceiver( rx2sis[type],
                                       OvroMaster::RIGHT_CIRCULAR ),
                type,
                carma::antenna::common::RxControl::RIGHTCIRCULAR,
                master.getAntennaIF( OvroMaster::IF2 ) );

        server.addServant< POA_cao::FrontEndControl_tie >(
            *frontEndMap_[ RxControl::RIGHTCIRCULAR ], fecp );
        frontEndPtrMap_[ RxControl::RIGHTCIRCULAR ] = fecp;
    } 
    else if (type == RxCon::RX3MM) {
        frontEndMap_[RxCon::SINGLE ] =
            new FrontEndControlImpl(
                master.getSisReceiver(rx2sis[type], OvroMaster::SINGLE),
                type,
                RxCon::SINGLE,
                master.getAntennaIF(OvroMaster::IF1));
        
        server.addServant< POA_cao::FrontEndControl_tie >(
            *frontEndMap_[RxCon::SINGLE], fecp );
        frontEndPtrMap_[RxCon::SINGLE] = fecp;
    } 
    else {
        throw CARMA_ERROR( "Invalid receiver type." );
    }

    // Now add additional servants
    server.addServant<POA_cao::LOControl_tie>( lo_, loPtr_ );
    server.addServant<POA_cac::OpticsControl_tie>( optics_, opticsPtr_ );
    server.addServant<POA_cao::PolarizationControl_tie>(polarization_, polPtr_);
}

// -----------------------------------------------------------------------------
RxControlImpl::~RxControlImpl( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR,
                   "RxControlImpl::~RxControlImpl( ) - Dtor." );
    const FrontEndMap::iterator iBegin = frontEndMap_.begin();
    const FrontEndMap::iterator iEnd   = frontEndMap_.end();
    for ( FrontEndMap::iterator i = iBegin; i != iEnd; ++i )
        delete i->second;
    const IFMap::iterator ifBegin = ifMap_.begin();
    const IFMap::iterator ifEnd = ifMap_.end();
    for ( IFMap::iterator ifi = ifBegin; ifi != ifEnd; ++ifi )
        delete ifi->second;
}

// -----------------------------------------------------------------------------
carma::antenna::common::LOControl_ptr
RxControlImpl::LO( )
try {
    logInfoWithRxNdc( rxType_, "RxControlImpl::LO( ) - Retrieving LO." );

    return carma::antenna::common::LOControl::_duplicate( loPtr_ );

} catch ( const CORBA::SystemException &cex ) {
    throw; // Rethrow
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
    return LOControl::_nil( ); // Squash compiler warnings.
}

// -----------------------------------------------------------------------------
// pol is currently ignored
carma::antenna::common::IFControl_ptr
RxControlImpl::IF( carma::antenna::common::RxControl::IF_Type pol )
try {
    {
        ostringstream oss;
        oss << "RxControlImpl::IF( pol=";
        if ( pol == carma::antenna::common::RxControl::IF1 )
            oss << "IF1";
        else if ( pol == carma::antenna::common::RxControl::IF2 )
            oss << "IF2";
        else
            oss << "<ERROR>";
        oss << " ) - Retrieving specified IF.";

        logInfoWithRxNdc( rxType_, oss.str() );
    }

    const IFPtrMap::iterator ifc = ifPtrMap_.find( pol );
    if ( ifc != ifPtrMap_.end() )
        return carma::antenna::common::IFControl::_duplicate( ifc->second );
    else
        throw CARMA_EXCEPTION( UserException, "Invalid polarization." );

    return IFControl::_nil( ); // Squash compiler warnings.

} catch ( const CORBA::SystemException & cex ) {
    throw; // Rethrow
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
    return IFControl::_nil( ); // Squash compiler warnings.
}

// -----------------------------------------------------------------------------
carma::antenna::common::FrontEndControl_ptr
RxControlImpl::FrontEnd(
    const ::carma::antenna::common::RxControl::Pol_Type pol )
try {
    logInfoWithRxNdc( rxType_,
                      "RxControlImpl::FrontEnd( ) - Retrieving Rx front end." );
    const FrontEndPtrMap::iterator fei = frontEndPtrMap_.find( pol );
    if ( fei != frontEndPtrMap_.end() )
        return common::FrontEndControl::_duplicate( fei->second );
    else
        throw CARMA_EXCEPTION( UserException, "Invalid polarization type." );

} catch ( const CORBA::SystemException & cex ) {
    throw; // Rethrow
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
    return FrontEndControl::_nil( );
}

// -----------------------------------------------------------------------------
carma::antenna::common::OpticsControl_ptr
RxControlImpl::Optics( )
try {
    logInfoWithRxNdc( rxType_, "RxControlImpl::Optics() - Retrieving Optics." );

    return common::OpticsControl::_duplicate( opticsPtr_ );

} catch ( const CORBA::SystemException & cex ) {
    throw; // Rethrow
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
    return OpticsControl::_nil( ); // Squash compiler warnings.
}

// -----------------------------------------------------------------------------
carma::antenna::common::PolarizationControl_ptr
RxControlImpl::Polarization( )
try {
    logInfoWithRxNdc(
        rxType_,
        "RxControlImpl::Polarization() - Retrieving Polarization." );

    return common::PolarizationControl::_duplicate( polPtr_ );

} catch ( const CORBA::SystemException & cex ) {
    throw; // Rethrow
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
    return PolarizationControl::_nil( ); // Squash compiler warnings
}

// -----------------------------------------------------------------------------
void
RxControlImpl::setFrequency(
    ::CORBA::Double yigFreq,
    ::CORBA::Double LOfreq,
    ::CORBA::Boolean endWithAbsorberInBeam,
    ::CORBA::Boolean optimizeReceiver,
    ::CORBA::Boolean forceRelock,  // Ignored for ovro
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
    msg << "RxControlImpl::setFrequencyWithSeqNo( yigFreq=" << yigFreq
        << " , LOFreq=" << LOfreq << " , seqNo=" << seqNo << " )";

    logInfoWithRxNdc( rxType_, msg.str( ) );

    const bool ignoreSeqNo = false;

    SetFreqArgType args = { *this,
                            yigFreq,
                            LOfreq,
                            seqNo,
                            ignoreSeqNo,
                            endWithAbsorberInBeam };

    StartPthreadWithCopy(
        setFrequencyEntryPoint,
        args,
        "RxControlImpl::setFrequencyEntryPoint",
        & (PthreadAttr( PTHREAD_CREATE_DETACHED ).InternalPthreadAttr( ) ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
RxControlImpl::setObservingFrequency(
    ::CORBA::Double freq,
    ::CORBA::ULong seqNo )
try {
    ostringstream msg;
    msg << "RxControlImpl::setObservingFrequency( freq=" << freq
         << ", seqNo=" << seqNo << " ).";

    logInfoWithRxNdc( rxType_, msg.str( ) );


} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void RxControlImpl::measureTotalPower(
    carma::antenna::common::CalibratorControl::Position position,
    ::CORBA::ULong seqNo )
try {
    ostringstream msg;
    msg << "RxControlImpl::measureTotalPower( position="
        << position  << ", seqNo=" << seqNo << " )";

    logInfoWithRxNdc( rxType_, msg.str( ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
RxControlImpl::toggleFastSampling(
    CORBA::ULong channel,
    CORBA::Boolean start )
try {
    ostringstream msg;
    msg << "RxControlImpl::toggleFastSampling( channel=" << channel
        << ", start=" << boolalpha << start << noboolalpha << " ) "
        << "- Fast sampling IS NOT IMPLEMENTED.";

    logInfoWithRxNdc( rxType_, msg.str( ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
RxControlImpl::setIFPresetPower( )
try {

    const IFMap::iterator ifiBegin = ifMap_.begin();
    const IFMap::iterator ifiEnd = ifMap_.end();
    for ( IFMap::iterator ifi = ifiBegin; ifi != ifiEnd; ++ifi )
    {
        ifi->second->setPresetPower( );
    }

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
RxControlImpl::setIFAtten(
    const CORBA::Float atten,
    const carma::antenna::common::RxControl::IF_Type ifType )
try {

    typedef carma::antenna::common::RxControl cacRxControl;

    if ( ifType == cacRxControl::BOTH ) {

        if ( rxType_.rxAsRxControlType() == cacRxControl::RX1MM ) {
            const IFMap::iterator ifiBegin = ifMap_.begin();
            const IFMap::iterator ifiEnd = ifMap_.end();
            for ( IFMap::iterator ifi = ifiBegin; ifi != ifiEnd; ++ifi )
            {
                ifi->second->setAtten( atten );
            }
        } else {
            ostringstream msg;
            msg << "IF_Type of BOTH is not supported "
                << " for " << rxType_.rxAsString() << ".";
            throw CARMA_EXCEPTION( UserException, msg.str().c_str() );
        }
    } else {
        const IFMap::iterator ifi = ifMap_.find( ifType );
        if ( ifi != ifMap_.end() ) {
            ifi->second->setAtten( atten );
        } else {
            ostringstream msg;
            msg << "IF_Type " << ifTypeAsString( ifType ) << " not found for "
                << rxType_.rxAsString( ) << ".";
            throw CARMA_EXCEPTION( UserException, msg.str().c_str() );
        }
    }

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
RxControlImpl::setIFPower( CORBA::Float power )
try {

    const IFMap::iterator ifiBegin = ifMap_.begin();
    const IFMap::iterator ifiEnd = ifMap_.end();
    for ( IFMap::iterator ifi = ifiBegin; ifi != ifiEnd; ++ifi )
    {
        ifi->second->setPower( power );
    }

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
RxControlImpl::setFrequencyEntryPoint( const SetFreqArgType & args )
{
    CARMA_CPTRACE( TRACE_SET_FREQ, "Thread started successfully." );

    RxControlImpl & This = args.This;

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
        // See monitor/OvroSubsystemPrewrite for more on how these MP can
        // get set.

        // For now, set the current Rx monitor point - later this will come
        // from the optics module AND IF switch.
        This.comMon_.receivers().currentRx().setValue(
                            This.rxType_.rxAsCurrentRxMonitorPointEnum( ) );
        This.comMon_.receivers().rxState().setValue(
                            AntennaCommon::RxStateMonitorPointEnum::TUNE );

        // 0) Set the LO termination attenuation (LoReference module)
        This.lo_.setLoTerminatorPowerToPreset( );

        // 1) Select IF switch position and wait for switch
        CARMA_CPTRACE( TRACE_SET_FREQ, "Selecting IF switch position.");

        if ((This.ifMap_.find(RxCon::IF1) == This.ifMap_.end()) ||
            (This.ifMap_.find(RxCon::IF2) == This.ifMap_.end()))
                throw InternalTuneFailException( );

        // Set both switches to the same position (but we only wait on IF1)
        // Because we only check IF1, set IF2 first to ensure that it is
        // probably complete when IF1, which is set second, is complete
        This.ifMap_[RxCon::IF2]->selectRx();
        This.ifMap_[RxCon::IF1]->selectRx();  

        // Note we use left pol IF module only - the right pol module is not
        // connected to a switch.
        bool ifSelectTimeout = waitForStateChange(
                This.mon_.antennaIfContainer( 0 ).antennaIF( ).ifSwitchStat( ),
                This.rxType_.rxAsIfSwitchStatMonitorPointEnum( ),
                IF_SWITCH_TIMEOUT );
        if ( ifSelectTimeout ) {
            string msg = "Timedout waiting for ifSelection.";

            if ( CONTINUE_ON_TUNE_ERRORS ) {
                msg += " Ignoring error and continuing anyway.";
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
        CARMA_CPTRACE( TRACE_SET_FREQ, "Setting lo frequency." );

        This.lo_.setLoFrequency( args.loFreq );

        bool gunnTimeout = waitForStateChange(
                getGunnPllMonitorRef(
                    This.rxType_.rxAsRxControlType( ), This.mon_ ).lockState(),
                carma::monitor::GunnPll::LockStateMonitorPointEnum::LOCKED,
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

        // 5) Set frequency on front end - tuning of mixer depends on frequency.
        CARMA_CPTRACE( TRACE_SET_FREQ, "Setting frequency on front end(s)." );

        // Loop over all front ends
        const FrontEndMap::iterator feiBegin = This.frontEndMap_.begin();
        const FrontEndMap::iterator feiEnd = This.frontEndMap_.end();
        for ( FrontEndMap::iterator fei = feiBegin; fei != feiEnd; ++fei )
        {
            fei->second->setFrequency( args.loFreq );
        }

        const FrontEndMap::iterator fei = This.frontEndMap_.begin();
        if ( fei == This.frontEndMap_.end() )
            throw InternalTuneFailException( );

        carma::monitor::SisReceiver & sisRx = getSisRxMonitorRef(
                This.rxType_.rxAsRxControlType( ),
                fei->first,
                This.mon_ );

        bool frontEndTimeout = false;
        if ( !frontEndTimeout ) {
            typedef CM::SisReceiver::TuneStateMonitorPointEnum TuneStateSPE;
            vector< TuneStateSPE::TUNESTATE > states;
            states.push_back( TuneStateSPE::TUNED );
            states.push_back( TuneStateSPE::SET_IJ );
            frontEndTimeout = waitForStateChange( sisRx.tuneState( ),
                    states,
                    RX_LOCK_TIMEOUT,
                    RX_LOCK_MIN );

            if ( frontEndTimeout ) {
                string msg = "Timedout waiting for front-end to tune.";

                if ( CONTINUE_ON_TUNE_ERRORS ) {
                    msg += " Ignoring error and continuing anyways.";
                }

                CARMA_CPTRACE( TRACE_SET_FREQ, msg );

                logInfoWithRxNdc( This.rxType_, msg );

                if ( !CONTINUE_ON_TUNE_ERRORS ) {
                    throw InternalTuneFailException( );
                }
            }
        }

        // Set rxState based on various tuning stage timeouts...
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
        } else if ( frontEndTimeout ) {
            This.comMon_.receivers().rxState().setValue(
                AntennaCommon::RxStateMonitorPointEnum::RX_BAD );
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
