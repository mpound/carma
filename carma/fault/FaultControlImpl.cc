#include <string>

#include <carma/fault/FaultControlImpl.h>
#include <carma/fault/DagManager.h>
using namespace carma::fault;

#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
#include <carma/util/ScopedLogNdc.h>
using namespace carma::util;

static void checkEffectPreference(const enum carma::fault::EffectPreference pref)
{
    switch (pref) {
    case PREF_NONE:
    case PREF_BLANK:
    case PREF_FLAG:
        break;
    default:
        {
            std::ostringstream oss;
            oss << "unsupported preference value: " << pref;
            throw CARMA_ERROR(oss.str());
        }
        break;
    }
}

static std::string pref2str(const enum carma::fault::EffectPreference pref)
{
    switch (pref) {
    case PREF_NONE:
        return "PREF_NONE";
    case PREF_BLANK:
        return "PREF_BLANK";
    case PREF_FLAG:
        return "PREF_FLAG";
    default:
        return "UNKNOWN";
    }
}

static void logEffectPreference(const CORBA::UShort subarrayNumber,
                                const enum carma::fault::EffectPreference pref,
                                const std::string &type)
{
    std::ostringstream oss;
    oss << "Setting Subarray " << subarrayNumber
        << " " << type
        << " error preference to " << pref2str(pref);

    programLogInfoIfPossible(oss.str());
}

FaultControlImpl::FaultControlImpl(DagManager &manager)
    : manager_(manager)
{
    programLogInfoIfPossible("FaultControlImpl: ctor");
}

FaultControlImpl::~FaultControlImpl()
{
    programLogInfoIfPossible("FaultControlImpl: dtor");
}

void
FaultControlImpl::setNoiseState( const CORBA::UShort subarrayNumber, const CORBA::Boolean stateIsOn )
{
    try {
        const ScopedLogNdc ndc( "FaultControlImpl::setNoiseState" );
        std::ostringstream oss;

        oss << "Setting Subarray " << subarrayNumber << " corr noise ";
        oss << "state to " << (stateIsOn ? "ON" : "OFF");

        programLogInfoIfPossible(oss.str());

        /* call the actual DagManager routine */
        this->manager_.CORBA_setNoiseState(subarrayNumber, stateIsOn);
    } catch ( ... ) {
        logCaughtAsErrorAndRethrowAsUser(
            "Coming out of FaultControlImpl::setNoiseState"
            " on a UserException due to exception - " );
    }
}


void
FaultControlImpl::setDriveErrorPreference(
        const CORBA::UShort subarrayNumber,
        const enum carma::fault::EffectPreference pref )
try {

    const ScopedLogNdc ndc( "FaultControlImpl::setDriveErrorPreference" );

    checkEffectPreference(pref);
    logEffectPreference(subarrayNumber, pref, "drive");

    /* call the actual DagManager routine */
    this->manager_.CORBA_setDriveErrorPreference(subarrayNumber, pref);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser(
        "Coming out of FaultControlImpl::setDriveErrorPreference"
        " on a UserException due to exception - " );
}


void
FaultControlImpl::setMonitorErrorPreference(
        const CORBA::UShort subarrayNumber,
        const enum carma::fault::EffectPreference pref )
try {

    const ScopedLogNdc ndc( "FaultControlImpl::setMonitorErrorPreference" );

    checkEffectPreference(pref);
    logEffectPreference(subarrayNumber, pref, "monitor");

    /* call the actual DagManager routine */
    this->manager_.CORBA_setMonitorErrorPreference(subarrayNumber, pref);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser(
        "Coming out of FaultControlImpl::setMonitorErrorPreference"
        " on a UserException due to exception - " );
}


void
FaultControlImpl::setOfflineErrorPreference(
        const CORBA::UShort subarrayNumber,
        const enum carma::fault::EffectPreference pref )
try {

    const ScopedLogNdc ndc( "FaultControlImpl::setOfflineErrorPreference" );

    checkEffectPreference(pref);
    logEffectPreference(subarrayNumber, pref, "offline");

    /* call the actual DagManager routine */
    this->manager_.CORBA_setOfflineErrorPreference(subarrayNumber, pref);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser(
        "Coming out of FaultControlImpl::setOfflineErrorPreference"
        " on a UserException due to exception - " );
}


void
FaultControlImpl::setPhaselockErrorPreference(
        const CORBA::UShort subarrayNumber,
        const enum carma::fault::EffectPreference pref )
try {

    const ScopedLogNdc ndc( "FaultControlImpl::setPhaselockErrorPreference" );

    checkEffectPreference(pref);
    logEffectPreference(subarrayNumber, pref, "phaselock");

    /* call the actual DagManager routine */
    this->manager_.CORBA_setPhaselockErrorPreference(subarrayNumber, pref);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser(
        "Coming out of FaultControlImpl::setPhaselockErrorPreference"
        " on a UserException due to exception - " );
}


void
FaultControlImpl::disableAlarms( const carma::fault::SeqString & inMonitorPointNames )
try {
    const ScopedLogNdc ndc( "FaultControlImpl::disableAlarms" );
    const int numMpNames = inMonitorPointNames.length();
    std::list<std::string> mpNames;
    int i;

    /* convert the SeqString into a StringList */
    for (i = 0; i < numMpNames; i++)
        mpNames.push_back( static_cast< const char * >( inMonitorPointNames[i] ));

    /* check for nothing to disable */
    if (mpNames.empty()) {
        programLogInfoIfPossible( "monitor point name list is empty" );
        return;
    }

    if (mpNames.size() == 1) {
        programLogInfoIfPossible("Disabling alarms for " + *(mpNames.begin()));
    } else {
        std::ostringstream oss;

        oss << "Disabling alarms for " << mpNames.size() << " mps";
        programLogInfoIfPossible(oss.str());
    }

    /* call the actual DagManager routine */
    this->manager_.CORBA_disableAlarms(mpNames);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser(
        "Coming out of FaultControlImpl::disableAlarms"
        " on a UserException due to exception - " );
}


void
FaultControlImpl::restoreAlarms( const carma::fault::SeqString & inMonitorPointNames )
try {
    const ScopedLogNdc ndc( "FaultControlImpl::restoreAlarms" );
    const int numMpNames = inMonitorPointNames.length();
    std::list<std::string> mpNames;
    int i;

    for (i = 0; i < numMpNames; i++)
        mpNames.push_back( static_cast< const char * >( inMonitorPointNames[i] ));

    /* check for nothing to disable */
    if (mpNames.empty()) {
        programLogInfoIfPossible("monitor point name list is empty");
        return;
    }

    if (mpNames.size() == 1) {
        programLogInfoIfPossible("Restoring alarms for " + *(mpNames.begin()));
    } else {
        std::ostringstream oss;

        oss << "Restoring alarms for " << mpNames.size() << " mps";
        programLogInfoIfPossible( oss.str() );
    }

    /* call the actual DagManager routine */
    this->manager_.CORBA_restoreAlarms(mpNames);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser(
        "Coming out of FaultControlImpl::restoreAlarms"
        " on a UserException due to exception - " );
}


void
FaultControlImpl::setAlarmEnable( const CORBA::UShort subarrayNumber, const CORBA::Boolean stateIsOn )
{

    try {
        const ScopedLogNdc ndc( "FaultControlImpl::setAlarmEnable" );
        std::ostringstream oss;

        oss << "Setting Subarray " << subarrayNumber << " alarm enable state ";
        oss << "to " << (stateIsOn ? "ON" : "OFF");

        programLogInfoIfPossible(oss.str());

        /* call the actual DagManager routine */
        this->manager_.CORBA_setAlarmEnable(subarrayNumber, stateIsOn);
    } catch ( ... ) {
        logCaughtAsErrorAndRethrowAsUser(
            "Coming out of FaultControlImpl::setAlarmEnable"
            " on a UserException due to exception - " );
    }
}


void
FaultControlImpl::setAlarmDeadmanSecs( const CORBA::Short alarmDeadmanSecs )
try {
    const ScopedLogNdc ndc( "FaultControlImpl::setAlarmDeadmanSecs" );
    std::ostringstream oss;

    oss << "Setting alarm deadman to " << alarmDeadmanSecs << " seconds";
    programLogInfoIfPossible(oss.str());

    /* call the actual DagManager routine */
    this->manager_.CORBA_setAlarmDeadmanSecs(alarmDeadmanSecs);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser(
        "Coming out of FaultControlImpl::setAlarmDeadmanSecs"
        " on a UserException due to exception - " );
}

/* vim: set ts=4 sts=4 sw=4 et: */
