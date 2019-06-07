/*
 * Manage All Alarm Functionality
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <cstdio>

#include <boost/foreach.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/local_time_adjustor.hpp>
#include <boost/date_time/c_local_time_adjustor.hpp>

#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>
#include <carma/util/Time.h>

#include <carma/fault/AlarmManager.h>
#include <carma/fault/Evaluators.h>
#include <carma/fault/FaultUtils.h>

AlarmManager::AlarmManager()
	: evaluator_()
	, mutex_()
{
	/* intentionally left empty */
}

void AlarmManager::attach_alarm_output(DagMLNodePtr output)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	enum DagMLNode::NodeTypes type;

	if (output->getType() != DagMLNode::TOP_NODE)
		throw CARMA_ERROR("tried to attach to a non-dag node");

	this->alarm_output_ = output;

	/* populate the monitor map */
	type = DagMLNode::MP_NODE;
	populateNodeMapChecked(output, type, this->monitor_map_);

	/* populate the gather map */
	type = DagMLNode::GATHER_NODE;
	populateNodeMapChecked(output, type, this->gather_map_);
}

void AlarmManager::attach_to_monitor_system(
		carma::monitor::MonitorSystem *inputCms,
		carma::monitor::MonitorSystem *outputCms)
{
	/* setup the evaluator */
	AlarmEvaluator &evaluator = this->evaluator_;
	evaluator.set_monitor_map(this->monitor_map_);
	evaluator.set_gather_map(this->gather_map_);
	evaluator.attach_to_monitor_system(inputCms, outputCms);

	/* setup the update information structure */
	DagMLNodePtr output = this->alarm_output_;
	VariableMap varmap;
	DagMLNodeUpdateInfo info(inputCms, outputCms,
							 varmap, true,
							 this->gather_map_, this->monitor_map_);

	/*
	 * Update and check the DAG
	 *
	 * This will pre-resolve anything which does not rely on a surrounding
	 * <varmap_scope> to get its value.
	 *
	 * Afterwards, it will verify that the entire tree is setup correctly,
	 * and all expected references can be found.
	 */
	updateAndCheckDag(output, info);
}

void AlarmManager::perform_cycle(int frame, FaultTransportWriter &transport)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	AlarmManagerMonitorInfo &monitor = this->monitor_;

	try {
		/* start the update timer */
		monitor.updateLatency.start();

		/* automatically re-enable alarms if it is the correct time */
		this->automatic_alarm_reenable(frame);

		/* evaluate the alarm output */
		this->evaluate_alarm_output(frame, transport);

		/* update the list of disabled alarms */
		DagMLNodeList list;
		BOOST_FOREACH(const DagMLNodeMap::value_type &t, this->disabled_map_)
			list.push_back(t.second);

		transport.setAlarmDisabled(list);

		/* stop the update timer */
		monitor.updateLatency.stop();
	} catch (...) {
		monitor.exceptionCount++;
		std::ostringstream oss;
		oss << "AlarmManager exception: " << carma::util::getStringForCaught();
		programLogErrorIfPossible(oss.str());
	}
}

void AlarmManager::evaluate_alarm_output(int frame, FaultTransportWriter &transport)
{
	DagMLNodePtr node = this->alarm_output_;
	AlarmManagerMonitorInfo &monitor = this->monitor_;
	AlarmEvaluator &evaluator = this->evaluator_;

	// evaluate this frame for alarms (adds faults to internal accumulator)
	struct AlarmEvalResult result;
	evaluator.evaluate(frame, node, result);

//#define PRINT_ALARM_FAULTS 1
#ifdef PRINT_ALARM_FAULTS
	std::cout << "Noisy Alarm On: " << std::boolalpha << result.noisyAlarm << std::endl;
	std::cout << "Silent Alarm On: " << std::boolalpha << result.silentAlarm << std::endl;

	BOOST_FOREACH(const DagMLNodePtr &fault, result.faults) {
		std::cout << "--> FAULT: " << *fault.get() << std::endl;
	}
#endif

	/* save the monitor status */
	monitor.noisyAlarmOn = result.noisyAlarm;
	monitor.silentAlarmOn = result.silentAlarm;
	monitor.faults = result.faults;
	monitor.faultCount = result.faults.size();

	/* add the faults to the transporter */
	transport.setAlarmFaults(result.faults);
	transport.setAlarmHistory(result.history);
}

/*
 * LOCKING: must hold this->mutex_
 */
void AlarmManager::automatic_alarm_reenable(const int frame)
{
	using namespace boost::posix_time;

	/*
	 * Every program that uses carma::util::Program::main() forces the
	 * local time to be UTC. This means we have to convert back to local
	 * time. Finding this out took a day of my time.
	 */

	// CARMA is UTC-8 (Pacific Time)
	typedef boost::date_time::local_adjustor<ptime, -8, us_dst> us_pacific;

	const ptime utc(from_time_t(carma::util::Time::gettime_t(frame)));
	const ptime loc = us_pacific::utc_to_local(utc);
	const time_duration td = loc.time_of_day();

	// we reset all alarms for 10 seconds between 14:00:00 (2pm)
	// and 14:00:10 (2pm + 10 seconds). This allows for dropped frames.
	if (!(td.hours() == 14 && td.minutes() == 0 && td.seconds() < 10))
		return;

	// copy the disabled map so we don't clobber the iterator
	DagMLNodeMap map = this->disabled_map_;

	// iterate through the copy, removing all alarms
	BOOST_FOREACH(const DagMLNodeMap::value_type &t, map) {
		const DagMLNodePtr node = t.second;
		this->set_monitor_point_alarm(node->getName(), false);

		std::ostringstream oss;
		oss << "automatic re-enable alarm for: " << node->getName();
		programLogInfoIfPossible(oss.str());
	}
}

/*
 * LOCKING: must hold this->mutex_
 */
DagMLNodePtr AlarmManager::check_monitor_point(const std::string &name)
{
	DagMLNodeMap &map = this->monitor_map_;
	DagMLNodeMap::const_iterator it;

	/* find the monitor point */
	it = map.find(name);
	if (it == map.end()) {
		std::ostringstream oss;

		oss << "Fault System DAG does not contain MP: " << name;
		throw CARMA_ERROR(oss.str());
	}

	/* nothing, it exists! */
	return it->second;
}

/*
 * LOCKING: must hold this->mutex_
 */
void AlarmManager::set_monitor_point_alarm(const std::string &name, bool disable)
{
	/* find the monitor point */
	DagMLNodePtr node = this->check_monitor_point(name);

	/* add/remove the MP from the map of disabled MP's */
	DagMLNodeMap &map = this->disabled_map_;
	if (disable) {
		map[name] = node;
	} else {
		map.erase(name);
	}

	/* actually enable/disable the monitor point */
	DagMPNode *mp = dynamic_cast<DagMPNode *>(node.get());
	mp->setDisableAlarm(disable);
}

void AlarmManager::disable_alarms(const StringList &names)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);

	/* check all of the monitor points */
	BOOST_FOREACH(const StringList::value_type &name, names)
		this->check_monitor_point(name);

	/* actually disable all of the monitor points */
	BOOST_FOREACH(const StringList::value_type &name, names)
		this->set_monitor_point_alarm(name, true);
}

void AlarmManager::restore_alarms(const StringList &names)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);

	/* check all of the monitor points */
	BOOST_FOREACH(const StringList::value_type &name, names)
		this->check_monitor_point(name);

	/* actually enable all of the monitor points */
	BOOST_FOREACH(const StringList::value_type &name, names)
		this->set_monitor_point_alarm(name, false);
}

void AlarmManager::set_alarm_enable_state(const int saNo, const bool on)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	const int index = saNo - 1;

	// update the monitor data and the evaluator state
	this->monitor_.alarmEnable.at(index) = on;
	this->evaluator_.set_alarm_enable_subarray(saNo, on);
}

const AlarmManagerMonitorInfo& AlarmManager::getMonitorInfo() const
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	return this->monitor_;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
