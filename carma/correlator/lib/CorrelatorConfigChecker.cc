// $Id: CorrelatorConfigChecker.cc,v 1.50 2011/08/17 21:55:58 abeard Exp $

#include <iostream>
#include <cstdlib> // for atof()
#include <ostream>
#include <sstream>

#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/correlator/lib/CorrelatorListener.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedSharedLock.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::correlator;
using namespace carma::correlator::lib;


namespace {

// Initialize static variables
::pthread_rwlock_t gSingletonGuard = PTHREAD_RWLOCK_INITIALIZER;
CorrelatorConfigChecker * gSingleton = 0;

}  // namespace < anonymous >


/**.......................................................................
 * Constructor.
 */
CorrelatorConfigChecker::CorrelatorConfigChecker( const string & filename ) :
ConfigChecker( filename )
{
  defaultCorrelatorMonitorDelay_                           = 0.2;
  defaultCatchDataMonitorDelay_                            = 0.2;
  defaultNumberOfBands_                                    = 17;
  defaultCatchDataIPQfilename_                             = "catchDataIPQ";
  defaultCatchDataIPQnumberOfElements_                     = 10;
  defaultIntegratorIPQfilename_                            = "integratorIPQ";
  defaultIntegratorIPQnumberOfElements_                    = 10;
  defaultIPQmaxsize_                                       = 5000000;  // bytes
  defaultNumberOfRecordsToIntegrate_                       = -1;
  defaultIntegrationTime_                                  = 10.0;
  defaultCatchDataWaitForData_                             = 300;  // in msec
}

/**.......................................................................
 * Destructor.
 */
CorrelatorConfigChecker::~CorrelatorConfigChecker( )
try {
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


CorrelatorConfigChecker *
CorrelatorConfigChecker::getInstance( const string & filename )
{
    // Let's be optimistic and see if it's already allocated
    {
        const ScopedSharedLock< ::pthread_rwlock_t >
            readLock( gSingletonGuard );
    
        if ( gSingleton != 0 )
            return gSingleton;
    }
    
    // Okay, our optimism was not rewarded so we'll try again and allocate it
    // if still needed
    
    const ScopedExclusiveLock< ::pthread_rwlock_t >
        writeLock( gSingletonGuard );

    if ( gSingleton == 0 )
        gSingleton = new CorrelatorConfigChecker( filename );

    return gSingleton;
}


bool CorrelatorConfigChecker::isDebug(const string& ClassName) {
  //  if (pairs_["debug" + ClassName] == "1") {
  string name = "debug" + ClassName;
  if ( valueIsOneString( name ) )
    return true;
  else
    return false;
}

bool CorrelatorConfigChecker::isLogCmd(const string& ClassName) {
  //  if (pairs_["debug" + ClassName] == "1") {
  string name = "logCmd" + ClassName;
  if ( valueIsZeroString( name ) )
    return false;
  else
    return true;
}

bool
CorrelatorConfigChecker::isCatchDataNotifyCorrelatorListener(const CorrelatorListener* cl) {
  string cn = cl->getName();
  // if name is not in config file, then default is to return false except
  // for 2 special cases.
  //cerr << "CorrelatorConfigChecker::isCatchDataNotify... cn= " << cn << endl;
  if ( valueIsOneString( "catchDataNotify" + cn ) ) {
    //cerr << "CorrelatorConfigChecker::isCatchDataNotify... returning true"<< endl;
    return true;
  } else {
    // special default case for IpqWriter and DataWriter
    if ((cn == "CorrelatorIpqWriter" ||
         cn == "CorrelatorDataWriter") &&
         valueIsEmpty("catchDataNotify" + cn))
      return true;
    return false;
  }
}

bool CorrelatorConfigChecker::printTime() {
  if (valueIsZeroString( "printTime" ))
    return false;
  else
    return true;
}

bool CorrelatorConfigChecker::prettyFunction() {
  if (valueIsZeroString( "prettyFunction" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportCorrelatorData() {
  // if transportData doesn't exists, we still want to transport data
  //  if (pairs_["transportData"] == "0")
  if (valueIsZeroString( "transportCorrelatorData" ))
    return false;
  else
    return true;
}

bool CorrelatorConfigChecker::isTransportCorrelatorMonitorData() {
  // if transportCorrelatorMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportCorrelatorMonitorData" ))
    return false;
  else
    return true;
}
double CorrelatorConfigChecker::getCorrelatorMonitorDelay() {
  const string val = getValue("correlatorMonitorDelay");
  if ( val.empty() )
    return defaultCorrelatorMonitorDelay_;
  else
    return atof(val.c_str());
}

// ****************
// PIPELINE RELATED
// ****************
double CorrelatorConfigChecker::getCatchDataMonitorDelay() {
  const string val = getValue("catchDataMonitorDelay");
  if ( val.empty() )
    return defaultCatchDataMonitorDelay_;
  else
    return atof(val.c_str());
}
bool CorrelatorConfigChecker::isTransportDecimatorMonitorData() {
  // if transportDecimatorMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportDecimatorMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportPassBandMonitorData() {
  // if transportPassBandMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportPassBandMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportTsysMonitorData() {
  // if transportTsysMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportTsysMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportFluxMonitorData() {
  // if transportFluxMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportFluxMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportBlankFlagMonitorData() {
  // if transportBlankFlagMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportBlankFlagMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportLinelengthCorrectionMonitorData() {
  // if transportLinelengthCorrectionMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportLinelengthCorrectionMonitorData" ))
    return false;
  else
    return true;
}

bool CorrelatorConfigChecker::isTransportIFcorrectionMonitorData() {
  // if transportIFcorrectionMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportIFcorrectionMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportWvrMonitorData() {
  // if transportWvrMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportWvrMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportSelfCalMonitorData() {
  // By default transport self cal monitor data
  if (valueIsZeroString( "transportSelfCalMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportCorrelatorPublisherMonitorData() {
  // if transportCorrelatorPublisherMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportCorrelatorPublisherMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportIntegratorMonitorData() {
  // if transportIntegratorMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportIntegratorMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTransportVisBrickWriterMonitorData() {
  // if transportVisBrickWriterMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportVisBrickWriterMonitorData" ))
    return false;
  else
    return true;
}

bool CorrelatorConfigChecker::isTransportPipelineMonitorData() {
  // if transportPipelineMonitorData doesn't exists,
  // we still want to transport data
  if (valueIsZeroString( "transportPipelineMonitorData" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isCorrelatorIpqWriter() {
  // default will be true for this listener
  if (valueIsZeroString( "correlatorIpqWriter" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isCorrelatorVisBrickWriter() {
  // default will be false for this listener
  if (valueIsOneString( "correlatorVisBrickWriter" ))
    return true;
  else
    return false;
}
bool CorrelatorConfigChecker::isApplyDelays() {
  // default will be true for this listener
  if (valueIsZeroString( "applyDelays" ))
    return false;
  else
    return true;
}

BandManager* CorrelatorConfigChecker::getWbBandManager() {
  string val = getValue("maxNumberOfBands");
  int numBands;
  bool useDefaultNames = false;
  if ( val.empty() ) {
    numBands = defaultNumberOfBands_;
    useDefaultNames = true;
  } else
    numBands = atoi(val.c_str());
  if ( getConfigFileNotFound() ) { // use default file names
    for (int idx = 0; idx < numBands; ++idx) {
      ostringstream os;
      os << "carma.correlator.wbcorBand" << (idx+1);
      bandManager_.addBandName(os.str());
    }
  } else {
    for (int idx = 0; idx < numBands; ++idx) {
      ostringstream os;
      os << "band" << (idx + 1);
      val = getValue(os.str());
      if ( val.empty() == false ) {
        bandManager_.addBandName(val);
      }
    }
  }
  return &bandManager_;
}

BandManager* CorrelatorConfigChecker::getSlBandManager() {
  string val = getValue("maxNumberOfBands");
  int numBands;
  bool useDefaultNames = false;
  if ( val.empty() ) {
    numBands = defaultNumberOfBands_;
    useDefaultNames = true;
  } else
    numBands = atoi(val.c_str());
  if ( getConfigFileNotFound() ) { // use default file names
    for (int idx = 0; idx < numBands; ++idx) {
      ostringstream os;
      os << "carma.correlator.slcorBand" << (idx+1);
      bandManager_.addBandName(os.str());
    }
  } else {
    for (int idx = 0; idx < numBands; ++idx) {
      ostringstream os;
      os << "band" << (idx + 1);
      val = getValue(os.str());
      if ( val.empty() == false ) {
        bandManager_.addBandName(val);
      }
    }
  }
  return &bandManager_;
}

string CorrelatorConfigChecker::getCatchDataIPQfilename() {
  string val = getValue("catchDataIPQfilename");
  if ( val.empty() )
    val = defaultCatchDataIPQfilename_;
  return val;
}

int CorrelatorConfigChecker::getIPQmaxsize() {
  const string val = getValue("IPQmaxsize");
  int maxsize;
  if ( val.empty() )
    maxsize = defaultIPQmaxsize_;
  else
    maxsize = atoi(val.c_str());
  return maxsize;
}

string CorrelatorConfigChecker::getIntegratorIPQfilename() {
  string val = getValue("integratorIPQfilename");
  if ( val.empty() )
    val = defaultIntegratorIPQfilename_;
  return val;
}

int CorrelatorConfigChecker::getCatchDataIPQnumberOfElements() {
  const string val = getValue("catchDataIPQnumberOfElements");
  int noe;
  if ( val.empty() )
    noe = defaultCatchDataIPQnumberOfElements_;
  else
    noe = atoi(val.c_str());
  return noe;
}

int CorrelatorConfigChecker::getIntegratorIPQnumberOfElements() {
  const string val = getValue("integratorIPQnumberOfElements");
  int noe;
  if ( val.empty() )
    noe = defaultIntegratorIPQnumberOfElements_;
  else
    noe = atoi(val.c_str());
  return noe;
}
bool CorrelatorConfigChecker::isDecimator() {
  // default will be true for this listener
  if (valueIsZeroString( "decimator" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isPassBand() {
  // default will be true for this listener
  if (valueIsZeroString( "passBand" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isTsys() {
  // default will be true for this listener
  if (valueIsZeroString( "tsys" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isFlux() {
  // default will be true for this listener
  if (valueIsZeroString( "flux" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isBlankFlag() {
  // default will be true for this listener
  if (valueIsZeroString( "blankFlag" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isLinelengthCorrection() {
  // default will be true for this listener
  if (valueIsZeroString( "linelengthCorrection" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isIFcorrection() {
  // default will be true for this listener
  if (valueIsZeroString( "ifCorrection" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isWvr() {
  // default will be true for this listener
  if (valueIsZeroString( "wvr" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isSelfCal() {
  // default will be true for this listener
  if (valueIsZeroString( "selfCal" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isCorrelatorPublisher() {
  // default will be true for this listener
  if (valueIsZeroString( "correlatorPublisher" ))
    return false;
  else
    return true;
}
bool CorrelatorConfigChecker::isCorrelatorIntegrator() {
  // default will be true for this listener
  if (valueIsZeroString( "correlatorIntegrator" ))
    return false;
  else
    return true;
}

int CorrelatorConfigChecker::getNumberOfRecordsToIntegrate() {
  const string val = getValue("ciNumberOfRecords");
  int n2i; // number of Records to integrate
  if ( val.empty() )
    n2i = defaultNumberOfRecordsToIntegrate_;
  else
    n2i = atoi(val.c_str());
  return n2i;
}
int CorrelatorConfigChecker::getCatchDataWaitForData() {
  const string val = getValue("catchDataWaitForData");
  int wfd;
  if ( val.empty() )
    wfd = defaultCatchDataWaitForData_;
  else
    wfd = atoi(val.c_str());
  return wfd;
}

float CorrelatorConfigChecker::getIntegrationTime() {
  const string val = getValue("integrationTimeInSec");
  float it;
  if ( val.empty() )
    it = defaultIntegrationTime_;
  else
    it = atof(val.c_str());
  return it;
}
