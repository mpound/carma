#include "carma/control/FaultHandle.h"

#include "carma/fault/FaultControl.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/FaultSubsystem.h"
#include "carma/util/Time.h"


using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::fault;
using namespace carma::monitor;
using namespace carma::util;


FaultHandle::FaultHandle(MonitorSystem &monitorSys,
                         ControlSubsystemBase::Reachable & reachable)
    : FaultControlRemoteObjHandle(FAULT_CONTROL_NAME,
                                  &(reachable.fault()),
                                  &(monitorSys.fault()),
                                  &monitorSys,
                                  true,
                                  false)
{
}


FaultHandle::~FaultHandle( )
try {
} catch (...) {
    // Just stifle any exceptions
    return;
}


void
convertStringVecSeqString(
    const vector< string > & stringVec,
    fault::SeqString &       seqString)
{
    const int stringVecSize = stringVec.size();
    seqString.length(stringVecSize);

    for (int i = 0; i < stringVecSize; ++i)
        seqString[i] = CORBA::string_dup(stringVec[i].c_str());
}


void
FaultHandle::disableAlarms(vector< string > mpNames)
{
    fault::SeqString seqString;
    convertStringVecSeqString(mpNames, seqString);

    if (isObjReachable())  {
        const double sendTime = Time::MJD();

        try {
            remoteObj()->disableAlarms(seqString);
        } catch (...) {
            invalidateObjRefIfNeededForCaught();
            throw;
        }

        logSentCommandIfNeeded(("FaultControl::disableAlarms"), sendTime);
    }
}


void
FaultHandle::restoreAlarms(vector< string > mpNames)
{
    fault::SeqString seqString;
    convertStringVecSeqString(mpNames, seqString);

    if (isObjReachable())  {
        const double sendTime = Time::MJD();

        try {
            remoteObj()->restoreAlarms(seqString);
        } catch (...) {
            invalidateObjRefIfNeededForCaught();
            throw;
        }

        logSentCommandIfNeeded(("FaultControl::restoreAlarms"), sendTime);
    }
}

void
FaultHandle::setAlarmEnable( int subarrayNo, bool on )
{
    if (isObjReachable()) {
        const double sendTime = Time::MJD();

        try {
            remoteObj()->setAlarmEnable(subarrayNo, on);
        } catch (...) {
            invalidateObjRefIfNeededForCaught();
            throw;
        }

        logSentCommandIfNeeded(("FaultControl::setAlarmEnable"), sendTime);
    }
}

