#include "carma/control/LineLengthHandle.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/LineLengthSubsystem.h"


using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::monitor;


LineLengthHandle::LineLengthHandle(
    MonitorSystem &                   monitorSys,
    ControlSubsystemBase::Reachable & reachable ) :
LineLengthControlRemoteObjHandle( linelength::LINELENGTH_NAME,
                                  &(reachable.linelength()),
                                  &(monitorSys.lineLength()),
                                  &monitorSys,
                                  true,
                                  false )
{
    // nothing to do here?
}


LineLengthHandle::~LineLengthHandle( )
try {
    // nothing to do here?
} catch ( ... ) {
    // Just stifle any exceptions
    return;
}

void LineLengthHandle::setAntennaLORef(const unsigned short ant, const unsigned short synth)
{
    if (isObjReachable()) {
        std::ostringstream oss;
        oss << "LineLength::setAntennaLORef(" << ant << ", " << synth << ")";

        try {
            const double sendTime = carma::util::Time::MJD();
            remoteObj()->setAntennaLORef(ant, synth);
            logSentCommandIfNeeded(oss.str(), sendTime);
        } catch (const CORBA::Exception &ex) {
            processException(oss.str(), ex);
        }
    }
}

void LineLengthHandle::setLORefFreq(const unsigned short synth, const double freq_hz)
{
    if (isObjReachable()) {
        std::ostringstream oss;
        oss << "LineLength::setLORefFreq(" << synth << ", " << freq_hz << ")";

        try {
            const double sendTime = carma::util::Time::MJD();
            remoteObj()->setLORefFreq(synth, freq_hz);
            logSentCommandIfNeeded(oss.str(), sendTime);
        } catch (const CORBA::Exception &ex) {
            processException(oss.str(), ex);
        }
    }
}
