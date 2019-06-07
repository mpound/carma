#include "carma/control/CorrelatorHandle.h"

#include <sstream>

#include "carma/correlator/obsRecord2/obsRecordUtils.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/CarmaSlcBandSubsystem.h"
#include "carma/monitor/WbcBandSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/Carma3GSubsystem.h"
#include "carma/util/CorrelatorSet.h"
#include "carma/util/ErrorException.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"

#include <boost/foreach.hpp>

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::correlator;
using namespace carma::correlator::obsRecord2;
using namespace carma::monitor;
using namespace carma::util;

typedef carma::monitor::NoiseStatusMonitorPointEnum NoiseSourceEnum;
namespace {


string
makeBandDoName( const unsigned int astroBandNo, 
    const carma::util::CorrelatorType corrType)
{
    ScopedLogNdc ndc("CorrelatorHandle - makeBandDOName:");
    ostringstream oss;
    
    // yeesh, this should come from the configuration files 
    // slcorrelator.conf and wbcorrelator.conf for consistency!
    switch ( corrType ) {
        case carma::util::CORR_SPECTRAL:
                oss << "carma.correlator.slcControl" << astroBandNo;
                break;
        case carma::util::CORR_WIDEBAND:
                oss << "carma.correlator.wbcControl" << astroBandNo - 8;
                break;
        // For C3G we use "Subarray Servers", only one per subarray
        // so no band number as part of the DO name
        case carma::util::CORR_C3GMAX8:
                oss << "carma.correlator.c3gmax8Control" ;
                break;
        case carma::util::CORR_C3GMAX23:
                oss << "carma.correlator.c3gmax23Control" ;
                break;
        default:
        case HARDWARE_TYPE_UNKNOWN:
            { 
                ostringstream os;
                os << "Unknown hardware type for Astroband " << astroBandNo;
                throw CARMA_EXCEPTION( ErrorException, os.str() );
            }
            break;
    }
 //   programLogInfo("returning " +oss.str());
    return oss.str( );
}


MonitorPointBool &
getBandReachableMp( const unsigned int                 astroBandNo,
                    ControlSubsystemBase::Reachable &  reachable,
                    const carma::util::CorrelatorType corrType )
{
    ScopedLogNdc ndc("CorrelatorHandle - getBandReachableMp:");
    // reachable indices start at zero.
    switch ( corrType ) {
        case carma::util::CORR_SPECTRAL:
            return reachable.slcorBand(  astroBandNo - 1 );
            break;
        case carma::util::CORR_WIDEBAND:
            return reachable.wbcorBand( astroBandNo - 9 );
            break;
        case carma::util::CORR_C3GMAX23:
            //programLogInfo("Got C3GMAX23 reachable");
            return reachable.carma3gMax23();
            break;
        case carma::util::CORR_C3GMAX8:
            //programLogInfo("Got C3GMAX8 reachable");
            return reachable.carma3gMax8();
            break;
        default:
            {
             ostringstream os;
             os << "Unknown hardware type for Astroband " << astroBandNo;
             throw CARMA_EXCEPTION( ErrorException, os.str() );
            }
            break;
    }
}

//@FIXME:   This will need a rewrite when the C3GBAND monitor system is
//          finalized.  The problem is that we have only one correlatorhandle
//          per subarray in C3G not one per band.  So depending on a 
//          band number based monitor subsystem is not a good idea. The final C3G 
//          subsystem may not even be band number based!
//          The workaround for now is to instantiate the C3GMAX8 handle with astrobandNo=25
//          and the C3GMax23 handle with astrobandNo=33.
MonitorSubsystem &
getBandSubsystem( const unsigned int    astroBandNo,
                  const MonitorSystem & monitorSystem )
{
    // monitor indices start at zero
    switch ( util::hwType( astroBandNo ) ) {
        case HARDWARE_TYPE_CARMA:
            return monitorSystem.carmaSlcBand(  astroBandNo - 1 );
            break;
        case HARDWARE_TYPE_COBRA:
            return monitorSystem.wbcBand( astroBandNo - 9 );
            break;
        case HARDWARE_TYPE_C3G:
            return monitorSystem.carma3gBand( astroBandNo - 25 );
            break;
        default:
        case HARDWARE_TYPE_UNKNOWN:
            { 
                ostringstream os;
                os << "Unknown hardware type for Astroband " << astroBandNo;
                throw CARMA_EXCEPTION( ErrorException, os.str() );
            }
            break;
        }
}

// @see note above about band-based monitor systems
MonitorPointInt & 
getCorrSeqNoMp( const unsigned int astroBandNo, 
                MonitorSystem & monitorSystem )
{
   // monitor indices start at zero
    switch ( util::hwType(astroBandNo) ) {
        case HARDWARE_TYPE_CARMA:
            return monitorSystem.carmaSlcBand( astroBandNo - 1 ).corrSeqNo();
            break;
        case HARDWARE_TYPE_COBRA:
            return monitorSystem.wbcBand( astroBandNo - 9 ).corrSeqNo();
            break;
        case HARDWARE_TYPE_C3G:
            return monitorSystem.carma3gBand( astroBandNo - 25 ).corrSeqNo();
            break;
        default:
        case HARDWARE_TYPE_UNKNOWN:
            { 
                ostringstream os;
                os << "Unknown hardware type for Astroband " << astroBandNo;
                throw CARMA_EXCEPTION( ErrorException, os.str() );
            }
            break;
    }

}


}  // namespace < anonymous >


CorrelatorHandle::CorrelatorHandle(
    const unsigned int                astroBandNo,
    const ControlCorrelatorDesignation corrType,
    MonitorSystem &                   monitorSystem,
    ControlSubsystemBase::Reachable & reachable,
    const bool                        defaultLogIfNotReachable ) :
CorrelatorControlRemoteObjHandle(
        makeBandDoName( astroBandNo, corrType ),
        &(getBandReachableMp( astroBandNo, reachable, corrType )),
        // note: this call can throw ErrorException
        &(getBandSubsystem( astroBandNo, monitorSystem )),
        &monitorSystem,
        defaultLogIfNotReachable,
        true ),
online_( true ),
nextSequenceNo_( 0 ),
consecutiveErrors_( 0 ),
errLimit_( 0 ),
monitorSys_( monitorSystem ),
astroBandNo_(astroBandNo),
astroBandMode_(carma::util::CORR_SINGLEPOL),
corrType_(corrType)
{
    switch ( util::hwType( astroBandNo ) ) {
        case HARDWARE_TYPE_CARMA:
            bandNo_ =  astroBandNo;
            break;
        case HARDWARE_TYPE_COBRA:
            bandNo_ = astroBandNo - 8;
            break;
        case HARDWARE_TYPE_C3G:
            bandNo_ = astroBandNo - 24 ; //??? MWP FIX. bandNo has no meaning here.
            break;
        default:
        case HARDWARE_TYPE_UNKNOWN:
            { 
                ostringstream os;
                os << "Unknown hardware type for Astroband " << astroBandNo;
                throw CARMA_EXCEPTION( ErrorException, os.str() );
            }
            break;
    }
    online_ = true;
}


CorrelatorHandle::~CorrelatorHandle( )
try {
    // nothing to do here.
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
CorrelatorHandle::setInputDelayTriplets(
    const vector< DelayTriplet > tripletsVec )
{
    if ( isOffline() ) return;
    if ( isObjReachable() ) {
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

bool
CorrelatorHandle::isNoiseSource(
    NoiseSourceEnum::NOISESTATUS state)
{
    //programLogInfo("CorrelatorHandle::isNoiseSource");
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
        numTries++;
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
CorrelatorHandle::isNoiseSourceEnabled() 
{
    return isNoiseSource( NoiseSourceEnum::ENABLED );
}

bool
CorrelatorHandle::isNoiseSourceDisabled() 
{
    return isNoiseSource( NoiseSourceEnum::DISABLED );
}


void
CorrelatorHandle::flattenPhases( const int preferredSeqNo )
{

    ScopedLogNdc ndc("CorrelatorHandle::flattenPhases");
    if ( isOffline() ) {
        return;
    }
    if ( !isObjReachable( ) ) { 
        return;
    }

    // no-op for COBRA bands
    // Put this BEFORE setNextSequenceNo, since a no-op is considered
    // success and we don't want to increment the expected sequence number
    // otherwise wait() will wait the entire timeout period.
    CorrelatorSet corrset(corrType_);
    if ( corrset.includesWideband() ) {
        ostringstream oss;
        oss << "flattenPhases(seqNo = "<<preferredSeqNo<<")"
            << " returning immediately because Band " << bandNo_
            << " (AstroBand " << astroBandNo_ 
            << ") is not a CARMA or C3G band."
            ;
        programLogInfoIfPossible( oss.str() );
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
            logSentCommandIfNeeded( remoteCallString, sendTime );

        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    } else {
        throw CARMA_ERROR( "Noise source was not enabled!");
    }
}

void
CorrelatorHandle::optimizeThresholds( const int preferredSeqNo )
{

    ScopedLogNdc ndc("CorrelatorHandle::optimizeThresholds");
    if ( isOffline() ) {
        return;
    }
    if ( !isObjReachable( ) ) { 
        return;
    }

    // no-op for COBRA bands
    // Put this BEFORE setNextSequenceNo, since a no-op is considered
    // success and we don't want to increment the expected sequence number
    // otherwise wait() will wait the entire timeout period.
    CorrelatorSet corrset(corrType_);
    if ( corrset.includesWideband() ) {
        ostringstream oss;
        oss << "optimizeThresholds(seqNo = "<<preferredSeqNo<<")"
            << " returning immediately because Band " << bandNo_
            << " (AstroBand " << astroBandNo_ 
            << ") is not a CARMA or C3G band."
            ;
        programLogInfoIfPossible( oss.str() );
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
            logSentCommandIfNeeded( remoteCallString, sendTime );

        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    } else {
        throw CARMA_ERROR( "Noise source was not enabled!");
    }
}

void
CorrelatorHandle::calibrateSpectra( const bool noiseEnabled,
                    const float intTime,
                    const bool cache,
                    const bool enable,
                    const int preferredSeqNo
                                  )

{
    ScopedLogNdc ndc("CorrelatorHandle::calibrateSpectra");
    if ( isOffline() ) {
        return;
    }
    if ( !isObjReachable( ) ) { 
        return;
    }

    // no-op for COBRA bands
    // Put this BEFORE setNextSequenceNo, since a no-op is considered
    // success and we don't want to increment the expected sequence number
    // otherwise wait() will wait the entire timeout period.
    CorrelatorSet corrset(corrType_);
    if ( corrset.includesWideband() ) {
        ostringstream oss;
        oss << "calibrateSpectra("
            << ", noiseEnabled = " << boolalpha << noiseEnabled
            << ", intTime = " << intTime
            << ", cache = " << boolalpha << cache 
            << ", enable = " << boolalpha << enable
            << ", seqNo = " << preferredSeqNo  
            << ") returning immediately because Band " << bandNo_
            << " (AstroBand " << astroBandNo_ 
            << ") is not a CARMA or C3G band."
            ;
        programLogInfoIfPossible( oss.str() );
        return;
    }

    setNextSequenceNo( preferredSeqNo );

    // round to nearest number of half-second frames to integrate
    const long count = static_cast<long>(floor(intTime*2+0.5));
    string remoteCallString;
    {
        ostringstream oss;
        oss << "correlator::obsRecord2::Correlator_I::"
            << "calibrateSpectra([bandNo = "
            << bandNo_ 
            << "],"
            << " enable = " << boolalpha << enable
            << ", cache = " << boolalpha << cache 
            << ", count = " << count
            << ", seqNo = " << nextSequenceNo_ 
            <<")";
        remoteCallString = oss.str();
        programLogInfoIfPossible( remoteCallString );
    }

    // check the desired noise source state.
    NoiseSourceEnum::NOISESTATUS noiseStatus
         = (  noiseEnabled 
            ? NoiseSourceEnum::ENABLED 
            : NoiseSourceEnum::DISABLED
           );
    if ( isNoiseSource( noiseStatus ) ) {
        try {
            const double sendTime = Time::MJD();

            remoteObj()->calibrateSpectra( 
                    enable,
                    cache,
                    count,
                    nextSequenceNo_ );
            logSentCommandIfNeeded( remoteCallString, sendTime );

        } catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    } else {
        ostringstream errOs;
        errOs << "Noise source was not " 
              << (noiseEnabled ? "enabled!" :"disabled!") ;

        throw CARMA_ERROR( errOs.str() );
    }
    
}

void
CorrelatorHandle::setBandwidth( 
    carma::correlator::obsRecord2::BandWidthType bw,
    const int preferredSeqNo,
    const unsigned int astroBandNo )
{
    ScopedLogNdc ndc("CorrelatorHandle::setBandwidth");
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
    correlator::obsRecord2::FpgaModeType fm = utilFpgaModeToObsrecordFpgaMode( astroBandMode_ );

    // Wideband always gets singlepol.
    if ( isWideband() ) fm = correlator::obsRecord2::SINGLEPOL;

    string remoteCallString;
    {
        ostringstream oss;
        oss << "correlator::obsRecord2::Correlator_I::setBandwidth( "
            << "bandwidth = "
            << correlator::obsRecord2::getStringForBandWidthType( bw )
            << ", astroBandMode = "
            << correlator::obsRecord2::getStringForFpgaModeType( fm );

       oss << ", seqNo=" << nextSequenceNo_ 
            << ", astroBandNo " << astroBandNo
            << " )";
        remoteCallString = oss.str();
    }

    // Now make the remote call.
    try {
        const double sendTime = Time::MJD();
        remoteObj( )->setBandwidth( bw, fm, nextSequenceNo_, astroBandNo );
        logSentCommandIfNeeded( remoteCallString, sendTime );
    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }

}

void 
CorrelatorHandle::setBandwidth( 
    vector<carma::correlator::obsRecord2::BandWidthType> bw,
    vector<carma::correlator::obsRecord2::FpgaModeType> fm,
    const int preferredSeqNo,
    vector<unsigned int> astroBandNo )
{
    ScopedLogNdc ndc("CorrelatorHandle::setBandwidth<vector>");
    if ( isOffline() ) {
        return;
    }
    if ( !isObjReachable( ) ) { 
        return;
    }
    if ( !isC3g() ) {
        programLogNotice("This method reserved for the C3G correlator");
        return;
    }

    const size_t bwsize = bw.size();
    const size_t fmsize = fm.size();
    const size_t absize = astroBandNo.size();
    if ( bwsize != absize || bwsize != fmsize ) 
    {
        ostringstream os;
        os << "CorrelatorHandle::setBandwidth<vector>"
           << " vector sizes don't match."
           << " bw vec = " << bwsize
           << " fm vec = " << fmsize
           << " ab vec = " << absize;
        throw CARMA_ERROR( os.str() );
    }

    setNextSequenceNo( preferredSeqNo );
    string remoteCallString;
    {
        ostringstream oss;
        oss << "correlator::obsRecord2::Correlator_I::setBandwidthVector( "
            << "vector size= " << bwsize
            << ", seqNo=" << nextSequenceNo_ 
            << " )";
        remoteCallString = oss.str();
    }

    // Now make the remote call.
    try {
        const BandWidthSeq  b = convertVectorToSequence<BandWidthSeq>(bw);
        const FpgaModeSeq   f = convertVectorToSequence<FpgaModeSeq>(fm);
        const util::SeqLong a = convertVectorToSequence<SeqLong>(astroBandNo);
        double sendTime = Time::MJD();
        remoteObj( )->setBandwidthVector( b, f, nextSequenceNo_, a );
        logSentCommandIfNeeded( remoteCallString, sendTime );
        sendTime = Time::MJD();
    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }
}

void 
CorrelatorHandle::setDownconverterSettings( ::std::vector<float> dcFreq )
{
    ScopedLogNdc ndc("CorrelatorHandle::setDownconverterSettings");
    if ( isOffline() ) {
        return;
    }
    if ( !isObjReachable( ) ) { 
        return;
    }
    string remoteCallString;
    {
        ostringstream oss;
        oss << "calling correlator::obsRecord2::Correlator_I::"
            << "setDownconverterSettings(["
            << getStringForHardwareType( util::hardwareType(bandNo_) )
            <<  "Correlator] vector<float>)";
        remoteCallString = oss.str();
        programLogInfoIfPossible( remoteCallString );
    }

    try {

        double sendTime = Time::MJD();

        const util::SeqFloat freqs = util::convertVectorToSequence<SeqFloat>(dcFreq);
        remoteObj( )->setDownconverterSettingsVector( freqs );

        logSentCommandIfNeeded( remoteCallString, sendTime );

    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }
}

void 
CorrelatorHandle::setWalshColumns(
                ::std::vector<int> cols90,
                ::std::vector<int> cols180,
                const int nStates90,
                const int nStates180,
                const bool noiseEnabled)
{
    ScopedLogNdc ndc("CorrelatorHandle::setWalshColumns");
    if ( isOffline() ) {
        return;
    }
    if ( !isObjReachable( ) ) { 
        return;
    }
    string remoteCallString;
    {
        ostringstream oss;
        oss << "calling correlator::obsRecord2::Correlator_I::"
            << "setWalshColumns([ "
            << getStringForHardwareType( util::hardwareType(bandNo_) )
            <<  "Correlator " 
            << " bandNo = "
            << bandNo_ 
            << "],"
            <<")";
        remoteCallString = oss.str();
        programLogInfoIfPossible( remoteCallString );
    }

    try {

        const double sendTime = Time::MJD();

        const util::SeqLong  c90 = util::convertVectorToSequence<SeqLong>(cols90);
        const util::SeqLong c180 = util::convertVectorToSequence<SeqLong>(cols180);
        remoteObj( )->setWalshColumns( c90, c180, nStates90, nStates180, noiseEnabled);

        logSentCommandIfNeeded( remoteCallString, sendTime );

    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }
}

void
CorrelatorHandle::enableCorrelation( const bool enable )
{
    ScopedLogNdc ndc("CorrelatorHandle::enableCorrelation");
    // this is only valid for COBRA boards.
    if ( isSpectral() ) { 
        return;
    }

    if ( isOffline() ) {
        return;
    }

    if ( !isObjReachable( ) ) { 
        return;
    }

    string remoteCallString;
    {
        ostringstream oss;
        oss << "calling correlator::obsRecord2::Correlator_I::"
            << "enableCorrelation([ "
            << boolalpha << enable 
            << "],"
            <<")  WB bandNo = "
            << bandNo_ ;

        remoteCallString = oss.str();
        remoteObj( )->enableCorrelation( enable );
        programLogInfoIfPossible( remoteCallString );
    }

    try {
        const double sendTime = Time::MJD();
        remoteObj()->enableCorrelation( enable );
        logSentCommandIfNeeded( remoteCallString, sendTime );
    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }

}

void 
CorrelatorHandle::setNextSequenceNo( const int preferredSequenceNo )
{   
    monitorSys_.readNewestConditionalCopy( );

    const int currentSeqNo = 
        getCorrSeqNoMp( astroBandNo_, monitorSys_ ).getValue( );

    if ( currentSeqNo == preferredSequenceNo ) {
        nextSequenceNo_ = preferredSequenceNo + 10;
    } else {
        nextSequenceNo_ = preferredSequenceNo;
    }
}

bool
CorrelatorHandle::isActionComplete( const MonitorSystem & monsys,
                                    const int             monErrLimit )
{

    // If the band is offline, return true immediately here
    // so that wait() does not wait for this band.
    if ( isOffline() ) return true;

    const bool debug = true;
    const MonitorPointInt & completionMP =
        getCorrSeqNoMp( astroBandNo_, monitorSys_ );
    
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

/*
void
CorrelatorHandle::assertConfiguration( 
     const SubarrayControlImpl::ConfigAstroBandMap * cabmap, const int preferredSeqNo )
{
     if ( !isC3g() ) {
        programLogNotice("This method reserved for the C3G correlator");
        return;
     }

     // instantiate and populate the arguments for the
     // vectorized version of setBandwidth
     vector<carma::correlator::obsRecord2::BandWidthType> bw;
     vector<carma::correlator::obsRecord2::FpgaModeType>  fm;
     vector<unsigned int> astroBandNo;
     CorrelatorSet corrset(corrType_);
     BOOST_FOREACH( SubarrayControlImpl::ConfigAstroBandPair c, *cabmap) {
         // ensure the correct astrobands  go into C3GMAX8 or C3GMAX25 
         if ( corrset.isC3gMax8() 
             && ( c.second->astroBand_.bandNo_ > 24 && c.second->astroBand_.bandNo_<= 32)
            ) {
             bw.push_back( getBandWidthType(c.second->bandwidth_,c.second->bits_));
             fm.push_back( utilFpgaModeToObsrecordFpgaMode(
                         c.second->getFpgaMode()) );
             astroBandNo.push_back(c.second->astroBand_.bandNo_);
         } else if ( corrset.isC3gMax23() 
             && ( c.second->astroBand_.bandNo_ > 32 && c.second->astroBand_.bandNo_ <= 40 )
            ) {
             bw.push_back( getBandWidthType(c.second->bandwidth_,c.second->bits_));
             fm.push_back( utilFpgaModeToObsrecordFpgaMode(
                         c.second->getFpgaMode()) );
             astroBandNo.push_back(c.second->astroBand_.bandNo_);
             }
     }
     setBandwidth( bw, fm, preferredSeqNo, astroBandNo );
}
*/
