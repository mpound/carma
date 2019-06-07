#include "carma/control/CalibratorHandle.h"
#include "carma/control/antennaHandleUtils.h"
#include "carma/control/SubarrayControlImpl.h"
#include "carma/monitor/AntennaCommon.h"

using namespace ::std;
using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::monitor;


CalibratorHandle::CalibratorHandle(
    const unsigned short            carmaAntNo,
    MonitorSystem &                 monitorSys,
    ControlSubsystemBase::Antenna & antenna ) :
CalibratorControlRemoteObjHandle(
    makeAntennaDoName( carmaAntNo, CALIBRATOR_NAME ),
    &(antenna.antennaReachable().calibrator()),
    &(getAntennaSubsystem( carmaAntNo, monitorSys )),
    &monitorSys,
    true,
    false ),
carmaAntNo_(carmaAntNo),
consecutiveErrorCount_(0) 
{
    // nothing to do here?
}


CalibratorHandle::~CalibratorHandle( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
CalibratorHandle::setCalibrator(
    MonitorSystem * const             monsys, 
    const CalibratorControl::Position calPos,
    const int                         preferredSequenceNo )
{
    if (!isObjReachable()) return;

    int seqNo = getAntennaCommon(carmaAntNo_, *monsys).
                    calibrator().calSeqNum().getValue();
    if (seqNo == preferredSequenceNo) {
        nextSequenceNo_ = preferredSequenceNo + 10;
    }
    else {
        nextSequenceNo_ = preferredSequenceNo;
    }

    consecutiveErrorCount_ = 0;
        
    string calPosName;
    switch ( calPos ) {
        case CalibratorControl::SKY:
            calPosName = "SKY"; break;
        case CalibratorControl::AMBIENT:
            calPosName = "AMBIENT"; break;
        case CalibratorControl::FIXEDTEMP:
            calPosName = "FIXEDTEMP"; break;
        case CalibratorControl::PARTIAL:
            calPosName = "PARTIAL"; break;
    }
    
    ostringstream remoteCall;
    remoteCall<< "CalibratorControl::setPos("
        << "pos=" << calPosName 
        << ", seqNo=" << nextSequenceNo_
        << ")";

    try {
        remoteObj( )->setPos(calPos, nextSequenceNo_);
    } catch (const CORBA::Exception & ex) {
        processException(remoteCall.str(), ex);
    }
    
}

bool
CalibratorHandle::isActionComplete(
    const MonitorSystem & monsys,
    const int             consecutiveErrorLimit )
{
    const bool debug = true;
    const MonitorPointInt & seqNoMP =
        getAntennaCommon(carmaAntNo_, monsys).calibrator().calSeqNum();
    
    return SubarrayControlImpl::isActionCompleteHelper(
        seqNoMP,
        consecutiveErrorLimit, consecutiveErrorCount_,
        nextSequenceNo_, carmaAntNo_,
        "Calibrator", debug);
}

