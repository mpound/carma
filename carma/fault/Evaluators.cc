/*
 * Fault System DAG Evaluation
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <cstdio>
#include <string>
#include <sstream>

#include <boost/foreach.hpp>

#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

#include <carma/fault/Constants.h>
#include <carma/fault/DagMLNode.h>
#include <carma/fault/Evaluators.h>

/*
 * A quick macro to evaluate all the children of a certain node without
 * the overhead of a function call.
 */
#define EVALUATE_ALL_CHILDREN(xxxnode, xxxinfo)				\
{									\
	const DagMLNodeList &children = (xxxnode)->getChildren();	\
									\
	/* evaluate all of the children */				\
	BOOST_FOREACH(DagMLNodePtr child, children)			\
		this->evaluate_node_recursive(child, (xxxinfo));	\
}

/*
 * evaluator for blank/flag output nodes
 *
 * This is the meat of the Fault System's blank/flag capabilities. It evaluates
 * a tree of nodes and determines whether to blank or flag.
 *
 * The key points to note are listed below:
 * - An MP node which is bad is a fault
 * - An MP node which is bad stops evaluation of its children. It is assumed
 *   that nodes will be listed such that higher level components being offline
 *   mean that their children will all be offline too. Like this:
 *
 *   <mp canon="Sldc.Band1.Input1.state">
 *     <mp canon="Sldc.Band1.Input1.ifOutPower" />
 *     <mp canon="Sldc.Band1.Input1.dataValid" />
 *   </mp>
 *
 *   If the module state is "OFFLINE", then it is not possible for any of
 *   its children to be valid. This paves the way for each subsystem (antenna,
 *   correlator crate) to have a state monitor point, just like the dcons
 *   used in the examples above.
 *
 * - When evaluating children, we must check all of the children, and return
 *   the union of their results. This makes sure that this case works:
 *
 *   <mp canon="Sldc.Band1.Input1.state">
 *     <mp canon="Sldc.Band1.Input1.ifOutPower" effect="flag" />
 *     <mp canon="Sldc.Band1.Input1.dataValid" effect="blank" />
 *   </mp>
 *
 *   Assume both the ifOutPower and dataValid nodes are bad, but the module
 *   state is "ONLINE", which means it is fine and reporting information as
 *   expected.
 *
 *   If we stop evaluation after seeing that the ifOutPower node is bad, then we
 *   would have missed the blank condition, and flagged instead. This is wrong.
 *
 *   Therefore, we must keep evaluating all children after the first failure,
 *   and report the union of their failures. The "most harsh" values should
 *   apply for blanking/flagging.
 */

BFEvaluator::BFEvaluator()
	: inputCms_(NULL)
	, outputCms_(NULL)
	, monitor_map_()
	, gather_map_()
	, correlatorNoiseOn_(NUM_SUBARRAYS, false)
	, faults_()
	, bitmask_(0)
{
	/* intentionally empty */
}

void BFEvaluator::attach_to_monitor_system(carma::monitor::MonitorSystem *inputCms,
					   carma::monitor::MonitorSystem *outputCms)
{
	this->inputCms_ = inputCms;
	this->outputCms_ = outputCms;
}

void BFEvaluator::set_monitor_map(const DagMLNodeMap &monitor_map)
{
	this->monitor_map_ = monitor_map;
}

void BFEvaluator::set_gather_map(const DagMLNodeMap &gather_map)
{
	this->gather_map_ = gather_map;
}

void BFEvaluator::set_correlator_noise(const int saNo, const bool on)
{
	const int index = saNo - 1;
	this->correlatorNoiseOn_.at(index) = on;
}

void BFEvaluator::evaluate(const int frame, DagMLNodePtr node, struct BFEvalResult &result)
{
	/* check the node type */
	if (node->getType() != DagMLNode::BF_OUTPUT_NODE)
		throw CARMA_ERROR("not a bfoutput node");

	/* clear the current list of faults */
	this->faults_.clear();
	this->bitmask_ = 0;

	/* create an empty variable map */
	VariableMap varmap;

	/* fill in the info structure */
	DagMLNodeUpdateInfo info(this->inputCms_, this->outputCms_,
				 varmap, false,
				 this->gather_map_, this->monitor_map_);

	/* start the evaluation */
	this->evaluate_node_recursive(node, info);

	/* copy the results into the result set */
	result.faults = this->faults_;
	result.bitmask = this->bitmask_;
}

void BFEvaluator::evaluate_mp_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagMPNode *mp = dynamic_cast<DagMPNode *>(node.get());

	/*
	 * If the MP is not valid, stop traversal and add it to the faults.
	 *
	 * We don't continue on to children to support masking. Only the MP
	 * node type supports masking of children
	 */
	if (!mp->isValid()) {
		/* TODO FIXME: clone the variable map as well */
		this->addErrorBit(mp->getErrorBit());
		this->faults_.push_back(node);
		return;
	}

	EVALUATE_ALL_CHILDREN(node, info);
}

void BFEvaluator::evaluate_mp_ref_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagMPRefNode *ref = dynamic_cast<DagMPRefNode *>(node.get());
	DagMLNodePtr mp = ref->getTarget();

	/* check the cached target */
	if (mp.get() == NULL) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		oss << "mp_ref: no cached target";
		programLogErrorIfPossible(oss.str());
#endif
		this->addErrorBit(DagMLNode::EFFECT_MONITORDATA);
		return;
	}

	/* evaluate the real node in our place */
	this->evaluate_node_recursive(mp, info);
}

void BFEvaluator::evaluate_gather_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	/* A gather is always true, it only has children to evaluate */
	EVALUATE_ALL_CHILDREN(node, info);
}

void BFEvaluator::evaluate_gather_ref_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagGatherRefNode *ref = dynamic_cast<DagGatherRefNode *>(node.get());
	DagMLNodePtr gather = ref->getTarget();

	/* check the cached target */
	if (gather.get() == NULL) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		oss << "gather_ref: no cached target";
		programLogErrorIfPossible(oss.str());
#endif
		this->addErrorBit(DagMLNode::EFFECT_MONITORDATA);
		return;
	}

	/* evaluate the real node in our place */
	this->evaluate_node_recursive(gather, info);
}

void BFEvaluator::evaluate_transient_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagTransientNode *transient = dynamic_cast<DagTransientNode *>(node.get());
	const int transient_subarray_num = transient->getSubarrayNumber();

	/* if this is an error value, we should pretend everything is ok */
	if (transient_subarray_num == -1) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		oss << "transient: invalid subarray number";
		programLogErrorIfPossible(oss.str());
#endif
		return;
	}

	bool enabled = false;
	const int index = transient_subarray_num - 1;
	const enum DagTransientNode::Conditions cond = transient->getCondition();
	switch (cond) {
	case DagTransientNode::CORRELATOR_NOISE_OFF:
		enabled = !this->correlatorNoiseOn_.at(index);
		break;
	default:
		std::ostringstream oss;
		oss << "BFEvaluator encountered unknown transient cond: " << cond;
		throw CARMA_ERROR(oss.str());
	}

	/* if the transient is not enabled, do not continue to the children */
	if (!enabled)
		return;

	/* the transient is enabled, evaluate all children */
	EVALUATE_ALL_CHILDREN(node, info);
}

void BFEvaluator::evaluate_bfoutput_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	/* A bf_output node only has children */
	EVALUATE_ALL_CHILDREN(node, info);
}

void BFEvaluator::evaluate_if_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagIfNode *ifnode = dynamic_cast<DagIfNode *>(node.get());

	/*
	 * If the underlying reference is not valid, we're dead in the water.
	 * The old <port> node would cause blanking in this case, and we will
	 * do the same thing.
	 */
	if (!ifnode->isValid())
		this->addErrorBit(DagMLNode::EFFECT_MONITORDATA);

	/*
	 * if we cannot pass through this node (the condition is not satisfied)
	 * then we pretend that everything below it does not exist, and cannot
	 * change the blank/flag status.
	 */
	if (!ifnode->allowPassThrough())
		return;

	/* the if is enabled, evaluate all children */
	EVALUATE_ALL_CHILDREN(node, info);
}

void BFEvaluator::evaluate_varmap_scope_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagVarmapScopeNode *scope = dynamic_cast<DagVarmapScopeNode *>(node.get());

	/*
	 * If the underlying reference is not valid, we're dead in the water.
	 * The old <port> node would cause blanking in this case, and we will
	 * do the same thing.
	 */
	if (!scope->isValid())
		this->addErrorBit(DagMLNode::EFFECT_MONITORDATA);

	/* add this varmap_scope to the variable map */
	VariableMap varmap(info.varmap);
	bool changed = false;
	scope->addToVariableMap(varmap, changed);

	/* create the new update info (new varmap) */
	DagMLNodeUpdateInfo new_info(info.inputCms, info.outputCms,
				     varmap, changed,
				     info.gather_map, info.monitor_map);

	/* now we continue evaluation for each child node */
	EVALUATE_ALL_CHILDREN(node, new_info);
}

void BFEvaluator::evaluate_bad_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	const DagBadNode *bad = dynamic_cast<DagBadNode *>(node.get());
	this->addErrorBit(bad->getErrorBit());
}

void BFEvaluator::evaluate_node_recursive(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	/* update the node based on the current variable map */
	node->update(info);

//#define DEBUG_BF_TRAVERSAL 1
#ifdef DEBUG_BF_TRAVERSAL
	std::cout << "BF TRAVERSAL: " << *node << std::endl;
#endif

	/* no cache, dispatch based on node type */
	switch (node->getType()) {
	case DagMLNode::MP_NODE:
		this->evaluate_mp_node(node, info);
		break;
	case DagMLNode::MP_REF_NODE:
		this->evaluate_mp_ref_node(node, info);
		break;
	case DagMLNode::GATHER_NODE:
		this->evaluate_gather_node(node, info);
		break;
	case DagMLNode::GATHER_REF_NODE:
		this->evaluate_gather_ref_node(node, info);
		break;
	case DagMLNode::TRANSIENT_NODE:
		this->evaluate_transient_node(node, info);
		break;
	case DagMLNode::BF_OUTPUT_NODE:
		this->evaluate_bfoutput_node(node, info);
		break;
	case DagMLNode::IF_NODE:
		this->evaluate_if_node(node, info);
		break;
	case DagMLNode::VARMAP_SCOPE_NODE:
		this->evaluate_varmap_scope_node(node, info);
		break;
	case DagMLNode::BAD_NODE:
		this->evaluate_bad_node(node, info);
		break;
	default:
		std::ostringstream oss;

		oss << "unsupported node: " << *node;
		throw CARMA_ERROR(oss.str());
	}
}

void BFEvaluator::addErrorBit(enum DagMLNode::EffectBits bit)
{
	this->bitmask_ |= bit;
}

/*
 * evaluator for alarm output nodes
 *
 * This is the meat of the Fault System's alarm output capabilities. It
 * evaluates a tree of nodes and determines whether the alarm should be
 * on or off.
 *
 * The key points to note are listed below:
 * - An MP node which is bad is a fault
 * - An MP node which is bad stops evaluation of its children. It is assumed
 *   that nodes will be listed such that higher level components being offline
 *   mean that their children will all be offline too. Like this:
 *
 *   <mp canon="Sldc.Band1.Input1.state">
 *     <mp canon="Sldc.Band1.Input1.ifOutPower" />
 *     <mp canon="Sldc.Band1.Input1.dataValid" />
 *   </mp>
 *
 *   If the module state is "OFFLINE", then it is not possible for any of
 *   its children to be valid. This paves the way for each subsystem (antenna,
 *   correlator crate) to have a state monitor point, just like the dcons
 *   used in the examples above.
 *
 * - When evaluating children, we must check all of the children, and return
 *   the union of their results. This makes sure that this case works:
 *
 *   <mp canon="Sldc.Band1.Input1.state">
 *     <mp canon="Sldc.Band1.Input1.ifOutPower" alarm="true" alarm_after="10.0" />
 *     <mp canon="Sldc.Band1.Input1.dataValid" alarm="true" alarm_after="5.0" />
 *   </mp>
 *
 *   Assume both the ifOutPower and dataValid nodes are bad, but the module
 *   state is "ONLINE", which means it is fine and reporting information as
 *   expected.
 *
 *   If we stop evaluation after seeing that the ifOutPower node is bad, then we
 *   would have missed the alarm_after=5.0 condition, and would have alarmed 5.0
 *   seconds too late. This is wrong.
 *
 *   Therefore, we must keep evaluating all children after the first failure,
 *   and report the union of their failures. The "most harsh" values should
 *   apply for turning the alarm on.
 *
 *   We must be sure that the alarm goes on after 5.0 seconds, even though we
 *   saw that the ifOutPower node was bad.
 *
 * - User controlled disable of alarms (hierarchical)
 *
 *   Users (observers) expect that they can disable an alarm and have it
 *   removed from the alarm output. This should also be hierarchical, so that
 *   you only need to disable the highest level alarm in the DAG, and all nodes
 *   below it will not alarm. This facilitates things such as taking antennas
 *   offline, where you do not want alarms, no matter what.
 *
 * - Alarms should wait their full timeout after coming back online
 *
 *   From discussions with Steve, alarms should be taken out of the evaluation
 *   hierarchy when they have been masked by higher level elements. Take the
 *   following as an example:
 *
 *   <mp canon="Sldc.Band1.Input1.state" alarm_after="20.0">
 *     <mp canon="Sldc.Band1.Input1.ifOutPower" alarm_after="10.0" />
 *     <mp canon="Sldc.Band1.Input1.dataValid" alarm_after="5.0" />
 *   </mp>
 *
 *   Assume that Sldc.Band1.Input1.state has been bad for >20.0 seconds. The
 *   alarm is on, and the ifOutPower and dataValid nodes are masked.
 *
 *   Now, the state changes to ONLINE (good), but ifOutPower and dataValid are
 *   still bad. After 5.0 seconds, the dataValid alarm will come on.
 *   Essentially, the masked child nodes have their timeouts reset when they
 *   become unmasked.
 */

AlarmEvaluator::AlarmEvaluator()
	: inputCms_(NULL)
	, outputCms_(NULL)
	, accumulator_(64)
	, monitor_map_()
	, gather_map_()
	, alarmEnableSubarray_(NUM_SUBARRAYS, false)
	, faults_()
{
	/* intentionally empty */
}

void AlarmEvaluator::attach_to_monitor_system(carma::monitor::MonitorSystem *inputCms,
					      carma::monitor::MonitorSystem *outputCms)
{
	this->inputCms_ = inputCms;
	this->outputCms_ = outputCms;
}

void AlarmEvaluator::set_monitor_map(const DagMLNodeMap &monitor_map)
{
	this->monitor_map_ = monitor_map;
}

void AlarmEvaluator::set_gather_map(const DagMLNodeMap &gather_map)
{
	this->gather_map_ = gather_map;
}

void AlarmEvaluator::set_alarm_enable_subarray(const int saNo, const bool on)
{
	const int index = saNo - 1;
	this->alarmEnableSubarray_.at(index) = on;
}

void AlarmEvaluator::evaluate(const int frame, DagMLNodePtr node, struct AlarmEvalResult &result)
{
	AlarmFaultAccumulator &accumulator = this->accumulator_;
	DagMLNodeList &faults = this->faults_;

	/* check the node type */
	if (node->getType() != DagMLNode::TOP_NODE)
		throw CARMA_ERROR("not a dag node");

	/* clear the current list of faults */
	faults.clear();

	/* create an empty variable map */
	VariableMap varmap;

	/* fill in the info structure */
	DagMLNodeUpdateInfo info(this->inputCms_, this->outputCms_,
				 varmap, false,
				 this->gather_map_, this->monitor_map_);

	/* setup the accumulator for this traversal */
	accumulator.traversal_setup(frame);

	/* start the evaluation */
	this->evaluate_node_recursive(node, info);

	/* finish the accumulator for this traversal */
	accumulator.traversal_cleanup();

	/* copy the results into the result set */
	result.noisyAlarm = false;
	result.silentAlarm = false;
	result.faults = faults;
	accumulator.getAlarmHistory(result.history);

	/*
	 * Now separate the alarms into noisy and silent alarms
	 * and set their statuses separately.
	 */
	BOOST_FOREACH(DagMLNodePtr fault, faults) {
		if (fault->getType() != DagMLNode::MP_NODE)
			continue;

		DagMPNode *mp = dynamic_cast<DagMPNode *>(fault.get());
		if (mp->getSilent()) {
			result.silentAlarm = true;
		} else {
			result.noisyAlarm = true;
		}
	}
}

void AlarmEvaluator::evaluate_mp_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagMPNode *mp = dynamic_cast<DagMPNode *>(node.get());
	AlarmFaultAccumulator &accumulator = this->accumulator_;

	/*
	 * If the MP is soft-disabled by the user, then we need to stop
	 * evaluation (not continue to the children). We are also not
	 * alarming, by definition.
	 */
	if (mp->getDisableAlarm()) {
		return;
	}

	/*
	 * If the MP is not valid (not written or outside the limits),
	 * then we should add it to the accumulator.
	 *
	 * Note that this is an instantaneous fault, not a long-term
	 * fault which has crossed the alarm threshold.
	 */
	if (!mp->isValid()) {
		accumulator.traversal_addFault(node);
	}

	/*
	 * Check with the accumulator whether this is an alarm yet
	 *
	 * If it is an alarm, then we should add it to the list of faults, and
	 * then return immediately. We should not visit the children: they
	 * are masked.
	 */
	if (accumulator.isAlarm(node)) {
		this->faults_.push_back(node);
		/* TODO FIXME: clone the variable map as well */
		return;
	}

	/*
	 * This MP is not ringing the alarm yet, so we must evaluate
	 * all of the children.
	 */
	EVALUATE_ALL_CHILDREN(node, info);
}

void AlarmEvaluator::evaluate_mp_ref_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagMPRefNode *ref = dynamic_cast<DagMPRefNode *>(node.get());
	DagMLNodePtr mp = ref->getTarget();

	/* check the cached target */
	if (mp.get() == NULL) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		oss << "mp_ref: no cached target";
		programLogErrorIfPossible(oss.str());
#endif
		return;
	}

	/* evaluate the real node in our place */
	this->evaluate_node_recursive(mp, info);
}

void AlarmEvaluator::evaluate_gather_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	/* A gather is always true, it only has children to evaluate */
	EVALUATE_ALL_CHILDREN(node, info);
}

void AlarmEvaluator::evaluate_gather_ref_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagGatherRefNode *ref = dynamic_cast<DagGatherRefNode *>(node.get());
	DagMLNodePtr gather = ref->getTarget();

	/* check the cached target */
	if (gather.get() == NULL) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		oss << "mp_ref: no cached target";
		programLogErrorIfPossible(oss.str());
#endif
		return;
	}

	/* evaluate the real node in our place */
	this->evaluate_node_recursive(gather, info);
}

void AlarmEvaluator::evaluate_top_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	/* A dag node only has children */
	EVALUATE_ALL_CHILDREN(node, info);
}

void AlarmEvaluator::evaluate_transient_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagTransientNode *transient = dynamic_cast<DagTransientNode *>(node.get());
	const int transient_subarray_num = transient->getSubarrayNumber();

	/* if this is an error value, we should pretend everything is ok */
	if (transient_subarray_num == -1) {
#ifdef ENABLE_VERBOSE_LOGGING
		std::ostringstream oss;
		oss << "transient: invalid subarray number";
		programLogErrorIfPossible(oss.str());
#endif
		return;
	}

	bool enabled = false;
	const int index = transient_subarray_num - 1;
	const enum DagTransientNode::Conditions cond = transient->getCondition();
	switch (cond) {
	case DagTransientNode::ALARM_ENABLE_SUBARRAY:
		enabled = this->alarmEnableSubarray_.at(index);
		break;
	default:
		std::ostringstream oss;
		oss << "AlarmEvaluator encountered unknown transient cond: " << cond;
		throw CARMA_ERROR(oss.str());
	}

	/* if the transient is not enabled, do not continue to children */
	if (!enabled)
		return;

	/* the transient is enabled, evaluate all children */
	EVALUATE_ALL_CHILDREN(node, info);
}

void AlarmEvaluator::evaluate_if_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagIfNode *ifnode = dynamic_cast<DagIfNode *>(node.get());

	/*
	 * If the underlying reference is not valid, we're dead in the water.
	 * In this case, we pretend that the node is just not allowing us to
	 * pass through, and ignore the alarm.
	 *
	 * TODO: re-evaluate if this is a problem in the real world.
	 * TODO: Steve and I think that with the few numbers of frames lost,
	 * TODO: it should not be an issue.
	 *
	 * If necessary, we can send the tagid along after some set number of
	 * frames being bad and raise the alarm. This wouldn't be very difficult
	 * to implement.
	 */
	if (!ifnode->isValid())
		return;

	/*
	 * if we cannot pass through this node (the condition is not satisfied)
	 * then we pretend that everything below it is not causing an alarm
	 */
	if (!ifnode->allowPassThrough())
		return;

	/* the if is enabled, evaluate all children */
	EVALUATE_ALL_CHILDREN(node, info);
}

void AlarmEvaluator::evaluate_varmap_scope_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	DagVarmapScopeNode *scope = dynamic_cast<DagVarmapScopeNode *>(node.get());

	/*
	 * If the underlying reference is not valid, we're dead in the water.
	 * In this case, we pretend that the node is just not allowing us to
	 * pass through, and ignore the alarm.
	 *
	 * TODO: re-evaluate if this is a problem in the real world.
	 * TODO: Steve and I think that with the few numbers of frames lost,
	 * TODO: it should not be an issue.
	 *
	 * If necessary, we can send the tagid along after some set number of
	 * frames being bad and raise the alarm. This wouldn't be very difficult
	 * to implement.
	 */
	if (!scope->isValid())
		return;

	/* add this varmap_scope to the variable map */
	VariableMap varmap(info.varmap);
	bool changed = false;
	scope->addToVariableMap(varmap, changed);

	/* create the new update info (new varmap) */
	DagMLNodeUpdateInfo new_info(info.inputCms, info.outputCms,
				     varmap, changed,
				     info.gather_map, info.monitor_map);

	/* now we continue evaluation for each child node */
	EVALUATE_ALL_CHILDREN(node, new_info);
}

void AlarmEvaluator::evaluate_node_recursive(DagMLNodePtr node, const DagMLNodeUpdateInfo &info)
{
	/* update the node based on the current variable map */
	node->update(info);

//#define DEBUG_ALARM_TRAVERSAL 1
#ifdef DEBUG_ALARM_TRAVERSAL
	std::cout << "ALARM TRAVERSAL: " << *node << std::endl;
#endif

	/* no cache, dispatch based on node type */
	switch (node->getType()) {
	case DagMLNode::MP_NODE:
		this->evaluate_mp_node(node, info);
		break;
	case DagMLNode::MP_REF_NODE:
		this->evaluate_mp_ref_node(node, info);
		break;
	case DagMLNode::GATHER_NODE:
		this->evaluate_gather_node(node, info);
		break;
	case DagMLNode::GATHER_REF_NODE:
		this->evaluate_gather_ref_node(node, info);
		break;
	case DagMLNode::TOP_NODE:
		this->evaluate_top_node(node, info);
		break;
	case DagMLNode::TRANSIENT_NODE:
		this->evaluate_transient_node(node, info);
		break;
	case DagMLNode::IF_NODE:
		this->evaluate_if_node(node, info);
		break;
	case DagMLNode::VARMAP_SCOPE_NODE:
		this->evaluate_varmap_scope_node(node, info);
		break;
	default:
		std::ostringstream oss;

		oss << "unsupported node: " << *node;
		throw CARMA_ERROR(oss.str());
	}
}

