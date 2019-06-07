/*
 * Fault System Alarm Accumulator
 *
 * This code implements an accumulator which will take input from a
 * single-frame-only (instantaneous) alarm evaluator, and keep track of
 * which faults have been present long enough to ring the alarm.
 *
 * It also takes care of storing a configurable amount of history information
 * so that a simple historical log of faults can be presented to the user.
 *
 * Copyright (c) 2011 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef ALARM_FAULT_ACCUMULATOR_H
#define ALARM_FAULT_ACCUMULATOR_H

#include <string>
#include <list>
#include <map>

#include <stdint.h>

#include <boost/circular_buffer.hpp>
#include <boost/shared_ptr.hpp>

#include <carma/fault/DagMLNode.h>

/*----------------------------------------------------------------------------*/
/* Accumulator Information Storage                                            */
/*----------------------------------------------------------------------------*/

struct AccumulatorInfo
{
	DagMLNodePtr node;
	unsigned int first_frame_bad;
	unsigned int last_frame_bad;
	unsigned int frames_bad;
	bool seen_this_iteration;

	/* ctor */
	AccumulatorInfo(DagMLNodePtr node);
};

typedef boost::shared_ptr<struct AccumulatorInfo> AccumulatorInfoPtr;

typedef std::map <uint32_t, AccumulatorInfoPtr> AccumulatorMap;
typedef std::list<AccumulatorInfoPtr> AccumulatorList;
typedef boost::circular_buffer<AccumulatorInfoPtr> AccumulatorCircBuf;

/*----------------------------------------------------------------------------*/
/* AlarmFaultAccumulator Class                                                */
/*----------------------------------------------------------------------------*/

class AlarmFaultAccumulator
{
	public:
	AlarmFaultAccumulator(const unsigned int history_len);

	void traversal_setup(const unsigned int frame);
	void traversal_addFault(DagMLNodePtr &node);
	void traversal_cleanup();

	bool isAlarm(const DagMLNodePtr &node);

	void getAlarms(DagMLNodeList &alarms);
	void getAlarmHistory(AccumulatorList &list);

	// for debugging
	void dump();

	protected:
	bool isAlarm(const AccumulatorInfoPtr &info);

	private:
	AccumulatorMap map_;
	AccumulatorCircBuf circ_;
	unsigned int current_frame_;
};

#endif /* ALARM_FAULT_ACCUMULATOR_H */

/* vim: set ts=4 sts=4 sw=4 noet tw=92: */
