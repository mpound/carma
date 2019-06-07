#include "carma/control/VlbiHandle.h"

#include <sstream>

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/VlbiSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"

typedef carma::monitor::NoiseStatusMonitorPointEnum NoiseSourceEnum;

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::correlator;
using namespace carma::correlator::obsRecord2;
using namespace carma::monitor;
using namespace carma::util;

namespace {


string
makeVlbiDoName( const unsigned int band )
{
    ostringstream oss;
    oss << "carma.vlbi.band" << band;
    return oss.str( );
}



MonitorPointInt & 
getVlbiSeqNoMp( const unsigned int bandNo, 
                MonitorSystem & monitorSystem )
{
  return monitorSystem.vlbi().band( bandNo - 1 ).control().vlbiSeqNo();
}


}  // namespace < anonymous >


VlbiHandle::VlbiHandle(
    const unsigned int                band,
    MonitorSystem &                   monitorSystem,
    ControlSubsystemBase::Reachable & reachable,
    const bool                        defaultLogIfNotReachable ) :
CorrelatorControlRemoteObjHandle(
        makeVlbiDoName( band ),
        &reachable.vlbiBand( band - 1 ),
        &monitorSystem.vlbi(),
        &monitorSystem,
        defaultLogIfNotReachable,
        true ),
online_( true ),
nextSequenceNo_( 0 ),
consecutiveErrors_( 0 ),
errLimit_( 0 ),
monitorSys_( monitorSystem ),
bandNo_( band ),
astroBandMode_(CORR_SINGLEPOL)  // NEEDED?
{
    online_ = true;
}


VlbiHandle::~VlbiHandle( )
{
}

bool
VlbiHandle::isNoiseSource(
    NoiseSourceEnum::NOISESTATUS state)
{
    // noise status can be ENABLED, CHANGING, DISABLED
    NoiseSourceEnum::NOISESTATUS noiseStatus;
    monitorSys_.readNewestConditionalCopy( );
    NoiseSource & noiseStatusMp = monitorSys_.sldc().noiseSourceContainer()
                                                  .noiseSource();
    // Try for up to 3 frames (1.5 seconds) in case the noise source 
    // has not yet arrived at the desired state.
    // If it is in the desired state on the first frame, return true
    // immediately, don't check 3 times.
    unsigned short numTries = 0;
    const unsigned short reTries = 3;
    while ( numTries < reTries ) {
        if ( noiseStatusMp.noiseStatus().isValid() ) {
            noiseStatus = noiseStatusMp.noiseStatus().getValue();
            if ( noiseStatus != state && numTries < reTries) continue;
            else return ( noiseStatus == state );
        }
        // sleep for just over 1 frame.
        usleep(600000UL);
    }
    throw CARMA_EXCEPTION(ErrorException,
                          "Unable to query noise source state");

}

bool
VlbiHandle::isNoiseSourceEnabled() 
{
    return isNoiseSource( NoiseSourceEnum::ENABLED );
}

bool
VlbiHandle::isNoiseSourceDisabled() 
{
    return isNoiseSource( NoiseSourceEnum::DISABLED );
}


void
VlbiHandle::setInputDelayTriplets(
    const vector< DelayTriplet > tripletsVec )
{
    if ( isOffline() ) return;
    if ( isObjReachable() ) {
        // Distinguish similar log messages from CorrelatorHandle.
        ScopedLogNdc ndc ("VlbiHandle::setInputDelayTriplets");

        const DelayTripletSeq tripletsSeq =
            convertVectorToSequence< DelayTripletSeq >( tripletsVec );
            
        string remoteCallString;
        {
            multiset< CORBA::Long > inputNos;
            for ( size_t i = 0; i < tripletsSeq.length(); ++i )
                inputNos.insert( tripletsSeq[ i ].inputNumber );

            ostringstream oss;
            
            oss << "Correlator_I::setInputDelayTriplets( "
                << tripletsSeq.length() << " input numbers: "
                << formatAsRanges( inputNos ) << " )";
                
            remoteCallString = oss.str();
        }
        
        try {
            
            remoteObj()->setInputDelayTriplets( tripletsSeq );

        }  catch ( const CORBA::Exception & ex )  {
            processException( remoteCallString, ex );
        }
    }
}


void
VlbiHandle::setBandwidth( 
    carma::correlator::obsRecord2::BandWidthType bw,
    const int preferredSeqNo,
    const unsigned int astroBandNo )
{
    ScopedLogNdc ndc("VlbiHandle::setBandwidth");
    if ( isOffline() ) {
        return;
    }
    if ( !isObjReachable( ) ) { 
        return;
    }

    // Note we do not necessarily send the preferredSeqNo to the correlator.
    // Rather, setNextSequenceNo determines if the preferred seq no is 
    // sufficient and sets the canonical nextSequenceNo_ which is sent.
    setNextSequenceNo( preferredSeqNo );

    correlator::obsRecord2::FpgaModeType fm = correlator::obsRecord2::SINGLEPOL;

    string remoteCallString;
    {
        ostringstream oss;
        oss << "correlator::obsRecord2::Correlator_I::setBandwidth( "
            << "bandwidth = ";
        // translate bandwidth enum to string.
        switch ( bw ) {
            case correlator::obsRecord2::BAND_500MHZ: 
                oss << "BAND_500MHZ";
                break;
            case correlator::obsRecord2::BAND_500MHZ_3BIT: 
                oss << "BAND_500MHZ_3BIT";
                break;
            case correlator::obsRecord2::BAND_500MHZ_4BIT: 
                oss << "BAND_500MHZ_4BIT";
                break;
            case correlator::obsRecord2::BAND_250MHZ:
                oss << "BAND_250MHZ";
                break;
            case correlator::obsRecord2::BAND_250MHZ_3BIT:
                oss << "BAND_250MHZ_3BIT";
                break;
            case correlator::obsRecord2::BAND_250MHZ_4BIT:
                oss << "BAND_250MHZ_4BIT";
                break;
            case correlator::obsRecord2::BAND_125MHZ:
                oss << "BAND_125MHZ";
                break;
            case correlator::obsRecord2::BAND_125MHZ_3BIT:
                oss << "BAND_125MHZ_3BIT";
                break;
            case correlator::obsRecord2::BAND_125MHZ_4BIT:
                oss << "BAND_125MHZ_4BIT";
                break;
            case correlator::obsRecord2::BAND_62MHZ:
                oss << "BAND_62MHZ";
                break;
            case correlator::obsRecord2::BAND_62MHZ_3BIT:
                oss << "BAND_62MHZ_3BIT";
                break;
            case correlator::obsRecord2::BAND_62MHZ_4BIT:
                oss << "BAND_62MHZ_4BIT";
                break;
            case correlator::obsRecord2::BAND_31MHZ:
                oss << "BAND_31MHZ";
                break;
            case correlator::obsRecord2::BAND_31MHZ_3BIT:
                oss << "BAND_31MHZ_3BIT";
                break;
            case correlator::obsRecord2::BAND_31MHZ_4BIT:
                oss << "BAND_31MHZ_4BIT";
                break;
            case correlator::obsRecord2::BAND_8MHZ:
                oss << "BAND_8MHZ";
                break;
            case correlator::obsRecord2::BAND_8MHZ_3BIT:
                oss << "BAND_8MHZ_3BIT";
                break;
            case correlator::obsRecord2::BAND_8MHZ_4BIT:
                oss << "BAND_8MHZ_4BIT";
                break;
            case correlator::obsRecord2::BAND_2MHZ:
                oss << "BAND_2MHZ";
                break;
            case correlator::obsRecord2::BAND_2MHZ_3BIT:
                oss << "BAND_2MHZ_3BIT";
                break;
            case correlator::obsRecord2::BAND_2MHZ_4BIT:
                oss << "BAND_2MHZ_4BIT";
                break;
            default:
                oss << "<error - unknown bandwidth type>";
                break;
        }

        switch ( astroBandMode_ ) {
            default:
            case CORR_SINGLEPOL:
                fm = correlator::obsRecord2::SINGLEPOL;
                oss << ", astroBandMode = SINGLEPOL";
                break;
            case CORR_DUALPOL:
                fm = correlator::obsRecord2::DUALPOL;
                oss << ", astroBandMode = DUALPOL";
                break;
            case CORR_FULLPOL:
                fm = correlator::obsRecord2::FULLPOL;
                oss << ", astroBandMode = FULLPOL";
                break;
            case CORR_CARMA23:
                fm = correlator::obsRecord2::CARMA23;
                oss << ", astroBandMode = CARMA23";
                break;
        }
        oss << ", seqNo=" << nextSequenceNo_ 
            << ", astroBandNo " << astroBandNo
            << " )";
        remoteCallString = oss.str();
    }

    // Now make the remote call.
    try {
        double sendTime = Time::MJD();

        remoteObj( )->setBandwidth( bw, fm, nextSequenceNo_, astroBandNo );

        logSentCommandIfNeeded( remoteCallString, sendTime );
        sendTime = Time::MJD();

    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }

}

void
VlbiHandle::optimizeThresholds( const int preferredSeqNo )
{

    ScopedLogNdc ndc("VlbiHandle::optimizeThresholds");
    if ( isOffline() ) {
        return;
    }
    if ( !isObjReachable( ) ) { 
        return;
    }

    setNextSequenceNo( preferredSeqNo );

    string remoteCallString;
    {
        ostringstream oss;
        oss << "correlator::obsRecord2::Correlator_I::"
            << "optimizeThresholds([bandNo = "
            << bandNo_ 
            << "], seqNo = "
            <<nextSequenceNo_ <<")";
        remoteCallString = oss.str();
        programLogInfoIfPossible( remoteCallString );
    }
    
    if ( isNoiseSourceEnabled() ) {
        try {
            const double sendTime = Time::MJD();

            remoteObj()->optimizeThresholds( nextSequenceNo_ );
            logSentCommand( remoteCallString, sendTime );

        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    } else {
        throw CARMA_ERROR( "Noise source was not enabled!");
    }
}

void
VlbiHandle::flattenPhases( const int preferredSeqNo )
{

    ScopedLogNdc ndc("VlbiHandle::flattenPhases");
    if ( isOffline() ) {
        return;
    }
    if ( !isObjReachable( ) ) { 
        return;
    }

    setNextSequenceNo( preferredSeqNo );

    string remoteCallString;
    {
        ostringstream oss;
        oss << "correlator::obsRecord2::Correlator_I::"
            << "flattenPhases([bandNo = " 
            << bandNo_ 
            << "], seqNo = "
            <<nextSequenceNo_ <<")";
        remoteCallString = oss.str();
        programLogInfoIfPossible( remoteCallString );
    }

    if ( isNoiseSourceEnabled() ) {
        try {
            const double sendTime = Time::MJD();

            remoteObj()->flattenPhases( nextSequenceNo_ );
            logSentCommand( remoteCallString, sendTime );

        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    } else {
        throw CARMA_ERROR( "Noise source was not enabled!");
    }
}

void 
VlbiHandle::setNextSequenceNo( const int preferredSequenceNo )
{   
    monitorSys_.readNewestConditionalCopy( );

    const int currentSeqNo = 
        getVlbiSeqNoMp( bandNo_, monitorSys_ ).getValue( );

    if ( currentSeqNo == preferredSequenceNo ) {
        nextSequenceNo_ = preferredSequenceNo + 10;
    } else {
        nextSequenceNo_ = preferredSequenceNo;
    }
}

bool
VlbiHandle::isActionComplete( const MonitorSystem & monsys,
                                    const int             monErrLimit )
{

    // If the band is offline, return true immediately here
    // so that wait() does not wait for this band.
    if ( isOffline() ) return true;

    const bool debug = true;
    const MonitorPointInt & completionMP =
        getVlbiSeqNoMp( bandNo_, monitorSys_ );
    
    const int oldErrLimit = errLimit_;
    const int oldConsecErrors = consecutiveErrors_;

    // Non-zero limit triggers resetting
    if (monErrLimit > 0) {
        errLimit_ = monErrLimit;
        consecutiveErrors_ = 0;
    }
    
    if (!completionMP.isValid()) {
        consecutiveErrors_++;
        if (consecutiveErrors_ >= errLimit_) {
            ostringstream o;

            o << "isActionComplete: " 
              << "number of consecutive invalid monitor frames "
              << "equals limit of "
              << errLimit_;
              
            o << " ("
              << "monErrLimit=" << monErrLimit << ", "
              << "oldErrLimit=" << oldErrLimit << ", "
              << "errLimit_ =" << errLimit_ << ", "
              << "oldConsecErrors=" << oldConsecErrors << ", "
              << "consecutiveErrors_=" << consecutiveErrors_ << ")";

            throw CARMA_ERROR(o);
        }
                   
        return false;
    }
    
    // Valid monitor point
    if (debug && (consecutiveErrors_ > 0)) {
        ostringstream o;
        o << "isActionComplete: band#" << bandNo_ 
          << " had "<< consecutiveErrors_
          << " consecutive invalid monitor frames";
        programLogInfoIfPossible( o.str() );
    }
    consecutiveErrors_ = 0;
    
    return (completionMP.getValue() == nextSequenceNo_);
}
