#include "carma/control/OpticalTelHandle.h"

#include "carma/control/antennaHandleUtils.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace ::std;
using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;

namespace {

    const bool kLogSentCommands = true;

} // namespace <unnamed>

OpticalTelHandle::OpticalTelHandle(
    const unsigned short            carmaAntNo,
    MonitorSystem &                 monitorSys,
    ControlSubsystemBase::Antenna & antenna ) :
OpticalTelControlRemoteObjHandle(
    makeAntennaDoName( carmaAntNo, OPTICAL_TEL_NAME ),
    &(antenna.antennaReachable( ).opticalTel( )),
    &(getAntennaSubsystem( carmaAntNo, monitorSys )),
    &monitorSys,
    true,
    kLogSentCommands ),
carmaAntNo_( carmaAntNo )
{
    // nothing to do here?
}


OpticalTelHandle::~OpticalTelHandle( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}

void OpticalTelHandle::findCentroidWithSeqNo( 
    monitor::MonitorSystem * const monsys,
    CORBA::UShort numFramesPerImage,
    CORBA::UShort minValidCentroids,
    CORBA::UShort maxCentroidAttempts,
    CORBA::UShort numEdgePixels,
    CORBA::UShort apertureRadiusPixels,
    CORBA::Float pixelThresholdSigma,
    CORBA::Boolean subBackground,
    CORBA::Boolean normalizeMedian,
    int preferredSequenceNo )
{
    if ( !isObjReachable() ) return;

    const int seqNo = getAntennaCommon( carmaAntNo_, *monsys ).
        opticalTel( ).centroidSeqNum( ).getValue( );;

    if ( seqNo == preferredSequenceNo ) {
        // Sequence number has already been used, increment next by 10
        nextSequenceNo_ = preferredSequenceNo + 10;
    } else {
        nextSequenceNo_ = preferredSequenceNo;
    }

    consecutiveErrors_ = 0;

    string remoteCallString;
    {
        ostringstream oss;
        oss << "OpticalTelCommon::findCentroid("
            << "numFramesPerImage=" << numFramesPerImage << ", " 
            << "minValidCentroids=" << minValidCentroids << ", "
            << "maxCentroidAttempts=" << maxCentroidAttempts << ", "
            << "numEdgePixels=" << numEdgePixels << ", "
            << "apertureRadiusPixels=" << apertureRadiusPixels << ", "
            << "pixelThresholdSigma=" << pixelThresholdSigma << ", "
            << "subBackground=" << boolalpha << subBackground << ", "
            << "normalizeMedian=" << normalizeMedian << noboolalpha << ", "
            << "seqNo=" << preferredSequenceNo << " )";

        remoteCallString = oss.str( );
    }

    try {
        const double sendTime = util::Time::MJD( );

        remoteObj( )->findCentroid(
                numFramesPerImage,
                minValidCentroids,
                maxCentroidAttempts,
                numEdgePixels,
                apertureRadiusPixels,
                pixelThresholdSigma,
                subBackground,
                normalizeMedian,
                preferredSequenceNo );

        logSentCommandIfNeeded( remoteCallString, sendTime );

    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }
}
        
void OpticalTelHandle::takeBackgroundWithSeqNo(
    monitor::MonitorSystem * const monsys,
    const CORBA::UShort numFrames,
    const int preferredSequenceNo )
{
    if ( !isObjReachable() ) return;

    const int seqNo = getAntennaCommon( carmaAntNo_, *monsys ).
        opticalTel( ).centroidSeqNum( ).getValue( );;

    if ( seqNo == preferredSequenceNo ) {
        // Sequence number has already been used, increment next by 10
        nextSequenceNo_ = preferredSequenceNo + 10;
    } else {
        nextSequenceNo_ = preferredSequenceNo;
    }

    consecutiveErrors_ = 0;

    string remoteCallString;
    {
        ostringstream oss;
        oss << "OpticalTelCommon::takeBackgroundImage("
            << "numFrames=" << numFrames << ", " 
            << "seq=" << preferredSequenceNo << " )";

        remoteCallString = oss.str( );
    }
    
    try {
        const double sendTime = util::Time::MJD( );

        remoteObj( )->takeBackgroundImage(
            numFrames,
            preferredSequenceNo );

        logSentCommandIfNeeded( remoteCallString, sendTime );

    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    }
}

bool OpticalTelHandle::isActionComplete( 
    const monitor::MonitorSystem & monsys, 
    const int monDataErrorLimit )
{
    const bool debug = true;
    const MonitorPointInt & completionMP =
        getAntennaCommon( carmaAntNo_, monsys ).opticalTel( ).centroidSeqNum( );

    const int oldErrLimit = errorLimit_;
    const int oldConsecErrors = consecutiveErrors_;

    // Non zero monDataErrorLimit triggers reset
    if ( monDataErrorLimit > 0 ) {
        errorLimit_ = monDataErrorLimit;
        consecutiveErrors_ = 0;
    }

    if ( !completionMP.isValid( ) ) {

        consecutiveErrors_++;
        if (consecutiveErrors_ >= errorLimit_) {
            ostringstream o;

            o << "isActionComplete: "
                << "number of consecutive invalid monitor frames "
                << "equals limit of "
                << errorLimit_;

            o << " ("
                << "monErrLimit=" << monDataErrorLimit << ", "
                << "oldErrLimit=" << oldErrLimit << ", "
                << "errLimit_ =" << errorLimit_ << ", "
                << "oldConsecErrors=" << oldConsecErrors << ", "
                << "consecutiveErrors_=" << consecutiveErrors_ << ")";

            throw CARMA_ERROR(o);
        }

        return false;
    }

    // Valid monitor point
    if (debug && (consecutiveErrors_ > 0)) {
        ostringstream o;
        o << "isActionComplete: ant#" << carmaAntNo_
            << " had "<< consecutiveErrors_
            << " consecutive invalid monitor frames";
        Category& log = Program::getLogger();
        log << Priority::INFO << o.str();
    }
    consecutiveErrors_ = 0;

    return (completionMP.getValue() == nextSequenceNo_);
}
