/*
 * Alarm Fault Accumulator Implementation
 *
 * Copyright (c) 2011 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <cstdio>

#include <boost/foreach.hpp>

#include <carma/fault/AlarmFaultAccumulator.h>

/*----------------------------------------------------------------------------*/
/* AccumulatorInfo Class                                                      */
/*----------------------------------------------------------------------------*/

AccumulatorInfo::AccumulatorInfo(DagMLNodePtr node)
	: node(node)
	, first_frame_bad(0)
	, last_frame_bad(0)
	, frames_bad(0)
	, seen_this_iteration(false)
{
	/* intentionally left empty */
}

/*----------------------------------------------------------------------------*/
/* AlarmFaultAccumulator Class                                                */
/*----------------------------------------------------------------------------*/

AlarmFaultAccumulator::AlarmFaultAccumulator(const unsigned int history_len)
	: map_()
	, circ_(history_len)
	, current_frame_(0)
{
	/* intentionally left empty */
}

void AlarmFaultAccumulator::traversal_setup(const unsigned int frame)
{
	AccumulatorMap &map = this->map_;

	// save the current frame
	this->current_frame_ = frame;

	// mark each accumulated monitor point as not seen yet
	BOOST_FOREACH(const AccumulatorMap::value_type &t, map) {
		const AccumulatorInfoPtr &info = t.second;
		info->seen_this_iteration = false;
	}
}

void AlarmFaultAccumulator::traversal_addFault(DagMLNodePtr &node)
{
	AccumulatorMap &map = this->map_;
	const unsigned int frame = this->current_frame_;
	const DagMPNode *mp = dynamic_cast<DagMPNode *>(node.get());
	const uint32_t tagid = mp->getTagID();
	AccumulatorMap::const_iterator it = map.find(tagid);

	if (it == map.end()) {
		AccumulatorInfoPtr info(new AccumulatorInfo(node));

		info->first_frame_bad = frame;
		info->last_frame_bad = frame;
		info->frames_bad = 1;
		info->seen_this_iteration = true;

		// insert it into the map
		map[tagid] = info;
	} else {
		AccumulatorInfoPtr info = it->second;

		info->last_frame_bad = frame;
		info->frames_bad++;
		info->seen_this_iteration = true;
	}
}

void AlarmFaultAccumulator::traversal_cleanup()
{
	/*
	 * Now we remove the information for each alarm that was not
	 * seen during this frame. This is a two-pass process:
	 * - find all of the faults which were not seen
	 * - delete each of them one at a time
	 *
	 * The information about each alarm which is going to be erased
	 * is saved into a circular buffer to allow us to provide a view
	 * of the alarm history.
	 */
	AccumulatorMap &map = this->map_;
	AccumulatorCircBuf &circ = this->circ_;
	std::list<AccumulatorMap::key_type> keyList;

	/* find each element which was not seen this iteration */
	BOOST_FOREACH(const AccumulatorMap::value_type &t, map) {
		const uint32_t key = t.first;
		const AccumulatorInfoPtr &info = t.second;
		if (!info->seen_this_iteration) {

			/* only add it to the history if it was an alarm */
			if (this->isAlarm(info))
				circ.push_back(info);

			keyList.push_back(key);
		}
	}

	/* remove each key from the map */
	BOOST_FOREACH(const uint32_t key, keyList) {
		map.erase(key);
	}
}

void AlarmFaultAccumulator::getAlarms(DagMLNodeList &list)
{
	const AccumulatorMap &map = this->map_;

	BOOST_FOREACH(const AccumulatorMap::value_type &t, map) {
		const AccumulatorInfoPtr &info = t.second;
		const DagMLNodePtr &node = info->node;

		if (this->isAlarm(info))
			list.push_back(node);
	}
}

void AlarmFaultAccumulator::getAlarmHistory(AccumulatorList &list)
{
	const AccumulatorCircBuf &circ = this->circ_;

	BOOST_FOREACH(const AccumulatorInfoPtr &info, circ)
		list.push_back(info);
}

void AlarmFaultAccumulator::dump()
{
	BOOST_FOREACH(const AccumulatorMap::value_type &t, this->map_) {
		const AccumulatorInfoPtr &info = t.second;
		const DagMLNodePtr &node = info->node;

		std::cout << "MP"
				  << " name=" << node->getName()
				  << " bad=" << info->frames_bad
				  << " first=" << info->first_frame_bad
				  << " last=" << info->last_frame_bad
				  << std::endl;
	}
}

bool AlarmFaultAccumulator::isAlarm(const AccumulatorInfoPtr &info)
{
	const DagMLNodePtr &node = info->node;
	const DagMPNode *mp = dynamic_cast<DagMPNode *>(node.get());

	if (info->frames_bad >= mp->getAlarmAfterFrames())
		return true;

	return false;
}

bool AlarmFaultAccumulator::isAlarm(const DagMLNodePtr &node)
{
	AccumulatorMap &map = this->map_;
	const DagMPNode *mp = dynamic_cast<DagMPNode *>(node.get());
	const uint32_t tagid = mp->getTagID();
	AccumulatorMap::const_iterator it = map.find(tagid);

	// definitely not an alarm: there was no fault!
	if (it == map.end())
		return false;

	// not an alarm: we have not exceeded the timeout
	const AccumulatorInfoPtr &info = it->second;
	return this->isAlarm(info);
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
