#ifndef CARMA_ALARM_HANDLE_H
#define CARMA_ALARM_HANDLE_H

/**
 * @file
 *
 * Carma Alarm interface to the remote Alarm DO
 *
 * @author: Amar Amarnath
 * @author: Colby Gutierrez-Kraybill
 *
 * $Id: AlarmHandle.h,v 1.2 2006/10/10 21:12:57 scott Exp $
 * $CarmaCopyright$
 *
 */
 

#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/AlarmSubsystem.h"
#include "carma/alarm/AlarmControl.h"


namespace carma
{
  namespace control
  {

    typedef carma::control::RemoteObjHandleT< alarm::AlarmControl >
      AlarmControlRemoteObjHandle;

    //! @brief Manages acc Alarm reference to alarm DO 
    class AlarmHandle : public AlarmControlRemoteObjHandle
    {
      public:
	/**
	 * Constructor
	 *
	 * @param carmaMonitor carma::monitor::MonitorSystem&
	 *              monitor system, which allows Alarm handle to get a 
	 *              reference to its own monitor stream.
	 * @param const carma::monitor::ControlSubsystemBase:Reachable&
	 *        reachable
	 *              monitor system, which allows Alarm handle to set 
	 *              monitor points for the subarray within control monitor 
	 *              subsystem .
	 */
	AlarmHandle(
	    carma::monitor::MonitorSystem &                   carmaMonitor, 
	    carma::monitor::ControlSubsystemBase::Reachable & reachable );


	virtual ~AlarmHandle();

	/**
	 * turnOn the alarm, given a particular alarm name
	 *
	 * @param const char* alarmName Name of alarm to invoke (comes from watch.tab,
	 *  and matches an entry in sounds.tab)
	 * @param const char* mp MonitorPoint that lead to this alarm
	 * @param const char* reason Extra explanation of alarm
	 * @param bool repeat Repeat the sound file over and over (usually true)
	 * @return none.
	 */
	void turnOn(
	    const char *alarmName,
	    const char *mp,
	    const char *reason,
	    bool repeat );

	/**
	 * turnOff the alarm
	 *
	 * @return none.
	 */
	void turnOff();

	/**
	 * Let the alarm system know that the fault watching part is alive.
	 *
	 * @return none.
	 */
	void updateDeadMan();
	
	/**
	 * Enable/disable the alarm system
	 * @parameter state true=enable false=disable
	 *
	 * @return none.
	 */
	void enableAlarm(bool state);
    };


  }  // namespace alarm
}  // namespace carma


#endif
