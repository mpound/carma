#include "carma/control/CorrDataRemapperHandle.h"

#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"

#include <iostream>
#include <sstream>

using namespace std;
using namespace carma::control;
using namespace carma::correlator;
using namespace carma::util;

#define CDR_FN(argList, tryBlock) \
  {\
    ostringstream os;						\
    os << "CorrDataRemapperHandle::" << __FUNCTION__;		\
    ScopedLogNdc ndc(os.str().c_str());				\
    if ( isObjReachable( ) ) {					\
      os.str("");						\
      os << "(" << argList << ")";				\
      try {							\
	const double sendTime = Time::MJD( );			\
	tryBlock;						\
	logSentCommandIfNeeded(os.str(), sendTime );		\
      }  catch (...) {						\
	logCaughtAsErrorAndRethrowAsUser();			\
      }								\
    } else {								\
      programLogInfoIfPossible("EML: CorrDataRemapper is not reachable");	\
    }\
  }

/**.......................................................................
 * Constructor.
 */
CorrDataRemapperHandle::CorrDataRemapperHandle(carma::monitor::MonitorSystem& carmaMonitor,
					       carma::monitor::ControlSubsystemBase::Reachable& reachable) :
  CorrDataRemapperRemoteObjHandle(CORRDATAREMAPPERCONTROL_NAME,
				  &( reachable.corrDataRemapper() ),
				  (carma::monitor::MonitorSubsystem*)&( carmaMonitor.signalPath() ),
				  &carmaMonitor,
				  true,
				  false)
{}

/**.......................................................................
 * Destructor.
 */
CorrDataRemapperHandle::~CorrDataRemapperHandle() {}

void CorrDataRemapperHandle::clearAstroBandInputMap(unsigned astroBandNo)
{
  CDR_FN("astroBandNo = " << astroBandNo,
	 remoteObj()->clearAstroBandInputMap(astroBandNo));
  
}

void CorrDataRemapperHandle::updateAstroBandInputMap(unsigned astroBandNo, std::vector<CorrDataRemapperControl::AstroBandInput>& abVec)
{
  CDR_FN("astroBandNo = " << astroBandNo,
	 CARMALOGINFO("EML inside updateAstroBandInputMap handle 0");
	 CorrDataRemapperControl::AstroBandInputSeq abSeq = convertVectorToSequence<CorrDataRemapperControl::AstroBandInputSeq>(abVec);
	 CARMALOGINFO("EML inside updateAstroBandInputMap handle 1");
	 remoteObj()->updateAstroBandInputMap(astroBandNo, abSeq));
	 CARMALOGINFO("EML inside updateAstroBandInputMap handle 2");
}

