/**
 * @file
 * Definition for AlarmControlImpl class.
 *
 * @author Colby Gutierrez-Kraybill
 * $Revision: 1.19 $
 * $Date: 2012/01/20 17:04:50 $
 * $Id: AlarmControlImpl.cc,v 1.19 2012/01/20 17:04:50 iws Exp $
 */

#include <sstream>
using namespace std;

// Carma includes
#include <carma/alarm/Trace.h>
#include <carma/alarm/AlarmControlImpl.h>
#include <carma/alarm/AudioControlThread.h>
using namespace carma::alarm;

#include <carma/util/ExceptionUtils.h>
#include <carma/util/FrameAlignedTimer.h>
#include <carma/util/Program.h>
#include <carma/util/ScopedPthreadMutexLock.h>
#include <carma/util/ScopedPthreadMutexLockManager.h>
#include <carma/util/ScopedLogNdc.h>
#include <carma/util/StartPthread.h>
#include <carma/util/Time.h>
#include <carma/util/Trace.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

static const std::string NO_ALARM_TEXT = "(no current alarm)";

// -----------------------------------------------------------------------------
AlarmControlImpl::AlarmControlImpl(AudioControlThread2 &control)
	: monitor_()
	, threadGroup_()
	, control_(control)
{
	CPTRACE4("AlarmControlImpl constructor");
	struct AlarmState &state = this->state_;

	state.enable = false;
	state.repeat = true;
	state.deadManSec = ::time(0);
	state.alarmTime = 0.0;
	state.on = false;
	state.mp = NO_ALARM_TEXT;
	state.reason = NO_ALARM_TEXT;

	/*
	 * Start the monitor writer thread and audio control thread
	 */
	::pthread_t thread;
	thread = StartPthreadWithRef(AlarmControlImpl::monitorThreadEP, *this);
	this->threadGroup_.insert(thread);
	thread = StartPthreadWithRef(AudioControlThread2::audioThreadEP, control);
	this->threadGroup_.insert(thread);
}

// -----------------------------------------------------------------------------
AlarmControlImpl::~AlarmControlImpl()
{
	CPTRACE4("AlarmControlImpl destructor");
	this->control_.exitThread();
}

// -----------------------------------------------------------------------------
void AlarmControlImpl::soundTheAlarm(const std::string &alarmName, const bool repeat)
{
	AudioControlThread2 &control = this->control_;

	control.setSound(alarmName);
	control.setRepeat(repeat);
	control.setState(true);
}

// -----------------------------------------------------------------------------
void AlarmControlImpl::silenceTheAlarm()
{
	AudioControlThread2 &control = this->control_;

	control.setSound("");
	control.setRepeat(false);
	control.setState(false);
}

// -----------------------------------------------------------------------------
void AlarmControlImpl::turnOn(
	const char *alarmName,
	const char *mp,
	const char *reason,
	bool repeat)
try {
	const ScopedLogNdc ndc("AlarmControlImpl::turnOn");
	struct AlarmState &state = this->state_;
	std::ostringstream oss;

	oss << "alarmName=" << alarmName
		<< " mp=" << mp
		<< " reason=" << reason
		<< " repeat=" << std::boolalpha << repeat;

	CPTRACE4(oss.str());
	programLogInfoIfPossible(oss.str());

	ScopedPthreadMutexLock scopelock(state.mutex);

	// We ALWAYS want to record all alarm info, even when disabled
	state.alarmTime = Time::MJD();
	state.deadManSec = ::time(NULL);
	state.mp = mp;
	state.reason = reason;
	state.repeat = repeat;
	state.alarmName = alarmName;

	// if the alarm is enabled, start it immediately
	if (state.enable == true)
		soundTheAlarm(alarmName, repeat);

	state.on = true;
} catch (...) {
	logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void AlarmControlImpl::turnOff()
try {
	const ScopedLogNdc ndc("AlarmControlImpl::turnOff");
	struct AlarmState &state = this->state_;

	CPTRACE4("Entry");
	programLogInfoIfPossible("Entry");

	ScopedPthreadMutexLock scopelock(state.mutex);

	state.on = false;
	state.deadManSec = ::time(NULL);
	state.mp = NO_ALARM_TEXT;
	state.reason = NO_ALARM_TEXT;
	silenceTheAlarm();
} catch (...) {
	logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void AlarmControlImpl::updateDeadMan()
try {
	const ScopedLogNdc ndc("AlarmControlImpl::updateDeadMan");
	struct AlarmState &state = this->state_;

	// This is overly verbose for logging really...
	CPTRACE4("Entry");
	//programLogInfoIfPossible("Entry");

	ScopedPthreadMutexLock scopelock(state.mutex);

	state.deadManSec = ::time(NULL);
} catch (...) {
	logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void AlarmControlImpl::enableAlarm(bool enable)
try {
	const ScopedLogNdc ndc("AlarmControlImpl::enableAlarm");
	struct AlarmState &state = this->state_;

	std::ostringstream oss;
	oss << "enable=" << std::boolalpha << enable;
	CPTRACE4(oss.str());
	programLogInfoIfPossible(oss.str());

	ScopedPthreadMutexLock scopelock(state.mutex);

	// No state change, bail out
	if (state.enable == enable)
		return;

	state.enable = enable;

	// If there's an alarm turn it immediately on or off
	if (state.on) {
		if (enable) {
			soundTheAlarm(state.alarmName, state.repeat);
		} else {
			silenceTheAlarm();
		}
	}

} catch (...) {
	logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void AlarmControlImpl::monitorThreadEP(AlarmControlImpl &This)
{
	const ScopedLogNdc ndc("AlarmControlImpl::monitorThreadEP");
	carma::monitor::AlarmSubsystem &monitor = This.monitor_;
	struct AlarmState &state = This.state_;

	typedef monitor::AlarmSubsystem::AlarmEnableMonitorPointEnum AlarmEnableEnum;

	programLogInfoIfPossible("monitor thread starting");

	while (true) {
		try {
			ThreadQuitTestSelf();
			if (!monitor.autoWriterIsAlive())
				monitor.startAutoWriter(0.100);

			FrameAlignedTimer framer;
			framer.ResetNextFireTime();
			while (true) {
				ScopedPthreadMutexLockManager lockManager(state.mutex);

				ThreadQuitTestSelf();
				framer.WaitForNextFireTime();
				ThreadQuitTestSelf();

				lockManager.LockMutex();

				monitor.timestamp().setValue(Time::MJD());
				monitor.alarmOn().setValue(state.on);
				monitor.alarmTime().setValue(state.alarmTime);
				monitor.alarmMP().setValue(state.mp);
				monitor.alarmReason().setValue(state.reason);

				if (state.enable == true && state.on == false)
					monitor.alarmEnable().setValue(AlarmEnableEnum::ENABLED);
				else if (state.enable == true && state.on == true)
					monitor.alarmEnable().setValue(AlarmEnableEnum::TRIGGERED);
				else
					monitor.alarmEnable().setValue(AlarmEnableEnum::DISABLED);

				// Check if deadman is getting updated or not
				// If no update in the last 30 seconds, alarm...
				std::ostringstream oss;
				oss << "checking dead man: deadManSec=" << state.deadManSec;
				CPTRACE4(oss.str());

				if (state.deadManSec < (::time(NULL) - 60)) {
					lockManager.UnlockMutex();

					This.turnOn("alarm", "dead.man",
						"Did not receive dead man switch update in last 60 seconds!",
						true);
				}
			} // while true
		} catch (...) {
			// stop the autowriter (if it is running)
			monitor.stopAutoWriter();

			// thread quit requested: exit this thread
			if (CaughtExceptionIsThreadQuitRequestedError()) {
				programLogInfoIfPossible("monitor thread quit requested");
				return;
			}

			// number one reason for exception, FSP has not be started..
			std::ostringstream oss;

			oss << "exception while accessing the monitor system: " << getStringForCaught();
			programLogErrorIfPossible(oss.str());
			programLogInfoIfPossible("retrying in 5 seconds");

			sleep(5);
		}
	} // while true

	programLogErrorIfPossible("monitor thread exiting");
}

/* vim: set ts=4 sts=4 sw=4 noet: */
