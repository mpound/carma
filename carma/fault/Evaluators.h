/*
 * Fault System DAG Tree Evaluators
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef EVALUATORS_H
#define EVALUATORS_H

#include <carma/fault/AlarmFaultAccumulator.h>
#include <carma/fault/DagMLNode.h>

/*----------------------------------------------------------------------------*/
/* Blank/Flag Node Evaluator                                                  */
/*----------------------------------------------------------------------------*/

struct BFEvalResult {
	uint32_t bitmask;
	DagMLNodeList faults;
};

class BFEvaluator
{
	public:
		BFEvaluator();

		void attach_to_monitor_system(carma::monitor::MonitorSystem *inputCms,
					      carma::monitor::MonitorSystem *outputCms);
		void set_monitor_map(const DagMLNodeMap &monitor_map);
		void set_gather_map(const DagMLNodeMap &gather_map);

		// transient support
		void set_correlator_noise(const int saNo, const bool on);

		void evaluate(const int frame, DagMLNodePtr node, struct BFEvalResult &result);

	private:

		/*
		 * Evaluate the validity of a tree of nodes
		 *
		 * This is a dispatcher based on node type. All non-masked faulting
		 * nodes will be returned in the list of faults.
		 */
		void evaluate_node_recursive(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);

		/* Evaluators for each node type */
		void evaluate_mp_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_mp_ref_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_gather_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_gather_ref_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_transient_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_bfoutput_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_if_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_varmap_scope_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_bad_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);

		/*
		 * Update the bitmask as needed
		 */
		void addErrorBit(enum DagMLNode::EffectBits bit);

		carma::monitor::MonitorSystem *inputCms_;
		carma::monitor::MonitorSystem *outputCms_;

		DagMLNodeMap monitor_map_;
		DagMLNodeMap gather_map_;

		std::vector<bool> correlatorNoiseOn_;

		/* per-evaluation faults and effect */
		DagMLNodeList faults_;
		uint32_t bitmask_;
};

/*----------------------------------------------------------------------------*/
/* Alarm Output Node Evaluator                                                */
/*----------------------------------------------------------------------------*/

struct AlarmEvalResult {
	bool noisyAlarm;
	bool silentAlarm;
	DagMLNodeList faults;
	AccumulatorList history;
};

class AlarmEvaluator
{
	public:
		AlarmEvaluator();

		void attach_to_monitor_system(carma::monitor::MonitorSystem *inputCms,
					      carma::monitor::MonitorSystem *outputCms);
		void set_monitor_map(const DagMLNodeMap &monitor_map);
		void set_gather_map(const DagMLNodeMap &gather_map);

		// transient support
		void set_alarm_enable_subarray(const int saNo, const bool on);

		void evaluate(const int frame, DagMLNodePtr node, struct AlarmEvalResult &result);

	private:

		/*
		 * Evaluate the validity of a tree of nodes
		 *
		 * This is a dispatcher based on node type. All non-masked faulting
		 * nodes will be returned in the list of faults.
		 */
		void evaluate_node_recursive(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);

		/* Evaluators for each node type */
		void evaluate_mp_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_mp_ref_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_gather_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_gather_ref_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_top_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_transient_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_if_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);
		void evaluate_varmap_scope_node(DagMLNodePtr node, const DagMLNodeUpdateInfo &info);

		carma::monitor::MonitorSystem *inputCms_;
		carma::monitor::MonitorSystem *outputCms_;

		AlarmFaultAccumulator accumulator_;
		DagMLNodeMap monitor_map_;
		DagMLNodeMap gather_map_;
		std::vector<bool> alarmEnableSubarray_;

		/* per-evaluation faults */
		DagMLNodeList faults_;
};

#endif /* EVALUATORS_H */
