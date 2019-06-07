/**
 * @file
 * AlarmImpl Corba interface implementation.
 *
 * @author Colby Gutierrez-Kraybill
 * $Revision: 1.10 $
 * $Date: 2012/01/20 17:04:50 $
 * $Id: AlarmControlImpl.h,v 1.10 2012/01/20 17:04:50 iws Exp $
 */
#ifndef CARMA_ALARMCONTROLIMPL_H
#define CARMA_ALARMCONTROLIMPL_H

#include <carma/monitor/AlarmSubsystem.h>

#include <carma/util/AutoPthreadQuitAndJoinGroup.h>
#include <carma/util/PthreadMutex.h>

namespace carma {
namespace alarm {

class AudioControlThread2;

/**
 * AlarmControl implementation class.
 */
class AlarmControlImpl
{
	public:

		// Constructor
		AlarmControlImpl(AudioControlThread2 &control);

		// Destructor
		virtual ~AlarmControlImpl();

		// Using the Tie template mechanism requires these methods to be public
		void turnOn(const char *alarmName,
					const char *mp,
					const char *reason,
					bool repeat);

		void turnOff();
		void updateDeadMan();
		void enableAlarm(bool enable);

		static void monitorThreadEP(AlarmControlImpl &This);

	private:

		// Separate the task of maintaining and updating state variables from
		// physically turning alarm on and off (these do the latter).
		void soundTheAlarm(const std::string &alarmName, bool repeat);
		void silenceTheAlarm();

		// Group together state information so we know what needs mutex protected
		struct AlarmState {
			carma::util::PthreadMutex mutex;
			bool enable;
			bool repeat;
			time_t deadManSec;
			double alarmTime;
			bool on;
			std::string mp;
			std::string reason;
			std::string alarmName;
		};

		::carma::monitor::AlarmSubsystem monitor_;
		struct AlarmState state_;

		// Threads
		carma::util::AutoPthreadQuitAndJoinGroup threadGroup_;

		// Audio Control
		AudioControlThread2 &control_;

}; // End class AlarmControlImpl

} // namespace alarm
} // namespace carma

#endif // CARMA_ALARMCONTROLIMPL_H

/* vim: set ts=4 sts=4 sw=4 noet: */
