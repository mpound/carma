/*
 * Subarray Blank/Flag Manager
 *
 * This object provides support for generating all blanking/flagging
 * information for a given subarray. This covers most of the features
 * of the old fault system.
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef SUBARRAY_MANAGER_H
#define SUBARRAY_MANAGER_H

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

class BFManager
{
	public:
		BFManager();

		/* attach to the top-of-tree DAG node */
		void attach_dag_node(DagMLNodePtr dagnode);

		/* attach all monitor points to the Carma Monitor System */
		void attach_to_monitor_system(carma::monitor::MonitorSystem *inputCms,
					      carma::monitor::MonitorSystem *outputCms);

		/* special transients support */
		void set_noise_source_state(const int saNo, const bool on);

		/* effect preference support */
		void set_drive_error_preference(const int saNo, const enum carma::fault::EffectPreference pref);
		void set_monitor_error_preference(const int saNo, const enum carma::fault::EffectPreference pref);
		void set_offline_error_preference(const int saNo, const enum carma::fault::EffectPreference pref);
		void set_phaselock_error_preference(const int saNo, const enum carma::fault::EffectPreference pref);

		/* perform one mainloop cycle */
		void perform_cycle(const int frame, FaultTransportWriter &transport);

		/* get the monitor information from the last iteration */
		const BFManagerMonitorInfo& getMonitorInfo() const;

	protected:

		/* Convert Error Bitmask to External Representation */
		uint32_t convert_bitmask(DagBFOutputNode *node, const struct BFEvalResult &result);

		/* Process the status of a single input */
		void process_input(const int frame, DagMLNodePtr node, FaultTransportWriter &transport);

		/* Maps of various node types for quick lookups and traversals */
		DagMLNodeMap gather_map_;
		DagMLNodeMap monitor_map_;
		DagMLNodeMap bfoutput_map_;

		/* Monitor Information */
		BFManagerMonitorInfo monitor_;

		/* The original top-of-tree structure */
		DagMLNodePtr dagnode_;

		/* blank/flag evaluator */
		BFEvaluator evaluator_;

		/* Input Monitor System */
		carma::monitor::MonitorSystem *inputCms_;

		/*
		 * Mutex to lock all user-accessible (public) operations
		 *
		 * This has the mutable keyword so that it can still be locked
		 * during methods which are marked const
		 */
		mutable carma::util::PthreadMutex mutex_;
};

#endif /* SUBARRAY_MANAGER_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
