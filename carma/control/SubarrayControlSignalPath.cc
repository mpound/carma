//$Id: SubarrayControlSignalPath.cc,v 1.4 2011/05/11 18:11:26 iws Exp $
#include "carma/control/SubarrayControlImpl.h"
#include "carma/control/SignalPathMapperHandle.h"
#include "carma/util/ExceptionUtils.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::util;

// These could be macro-ized since they all follow the same
// pattern except for queryConfiguration.

void
SubarrayControlImpl::initializeCableMap(const char * fileName)
try {
    signalPathMapper_->initializeCableMap( fileName );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser();
}

void
SubarrayControlImpl::loadConfiguration(const char * fileName,
                                       const char * confName,
                                       const char * astroBandConfName )
try {
    signalPathMapper_->loadConfiguration( fileName, confName, astroBandConfName );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser();
}

char *
SubarrayControlImpl::queryConfiguration()
try {
    string config = signalPathMapper_->queryConfiguration();
    return CORBA::string_dup(config.c_str());
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser();
    return CORBA::string_dup("");
}

void
SubarrayControlImpl::checkConfigurationSuccess(CORBA::Short astroBandNo)
try {
    signalPathMapper_->checkConfigurationSuccess( astroBandNo );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser();
}

void
SubarrayControlImpl::checkConfigurationValidity(CORBA::Short astroBandNo,
                             const char * confName )
try {
    signalPathMapper_->checkConfigurationValidity(
						  astroBandNo, confName, subarrayNo_, csCorrType() );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser();
}
