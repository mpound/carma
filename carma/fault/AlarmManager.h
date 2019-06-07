/*
 * Alarm Output Manager
 *
 * This objects manages all of the alarm output functionality in the
 * fault system. This is used to play tones when things go wrong, so
 * that the observers can be woken up in the middle of the night for
 * my amusement.
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef ALARM_MANAGER_H
#define ALARM_MANAGER_H

#include <string>
#include <list>

#include <carma/monitor/MonitorSystem.h>
#include <carma/monitor/ControlSubsystemExt.h>
#include <carma/util/ScopedPthreadMutexLock.h>

#include <carma/fault/DagMLNode.h>
#include <carma/fault/Evaluators.h>
#include <carma/fault/FaultTransport.h>
#include <carma/fault/FaultSystemMonitorInfo.h>

typedef std::list<std::string> StringList;

class AlarmManager
{
	public:
		AlarmManager();

		/* attach to a given subarray */
		void attach_alarm_output(DagMLNodePtr output);

		/* attach all monitor points to the Carma Monitor System */
		void attach_to_monitor_system(carma::monitor::MonitorSystem *inputCms,
					      carma::monitor::MonitorSystem *outputCms);

		/* special transient support */
		void set_alarm_enable_state(const int saNo, const bool on);

		/* soft enable/disable of specific monitor point alarms */
		void disable_alarms(const StringList &names);
		void restore_alarms(const StringList &names);

		/* perform one mainloop cycle */
		void perform_cycle(int frame, FaultTransportWriter &transport);

		/* get the monitor info from the last iteration */
		const AlarmManagerMonitorInfo& getMonitorInfo() const;

	protected:
		/* the basic step of any normal processing cycle */
		void evaluate_alarm_output(int frame, FaultTransportWriter &transport);

		/* enable/disable a single monitor point alarm */
		void set_monitor_point_alarm(const std::string &name, bool disable);
		DagMLNodePtr check_monitor_point(const std::string &name);
		void automatic_alarm_reenable(const int frame);

		/* Maps of various node types for quick lookups and traversals */
		DagMLNodeMap monitor_map_;
		DagMLNodeMap gather_map_;
		DagMLNodeMap disabled_map_;

		/* Monitor Information */
		AlarmManagerMonitorInfo monitor_;

		/* The alarm output node */
		DagMLNodePtr alarm_output_;

		/* Fault Accumulator */
		AlarmEvaluator evaluator_;

		/*
		 * Mutex to lock all user-accessible (public) operations
		 *
		 * This has the mutable keyword so that it can still be locked
		 * during methods which are marked const
		 */
		mutable carma::util::PthreadMutex mutex_;
};

#endif /* ALARM_MANAGER_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
