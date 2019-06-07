/*
 * Blank/Flag Manager Implementation
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <iomanip>
#include <cstdio>
#include <utility>
#include <sstream>

#include <boost/foreach.hpp>

#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

#include <carma/fault/DagMLExpr.h>
#include <carma/fault/Evaluators.h>
#include <carma/fault/FaultUtils.h>
#include <carma/fault/BFManager.h>
#include <carma/fault/BlankFlagConstants.h>

#include <carma/monitor/ControlSubsystemExt.h>
#include <carma/monitor/SignalPathSubsystem.h>

/*----------------------------------------------------------------------------*/
/* BFManager Class                                                            */
/*----------------------------------------------------------------------------*/

BFManager::BFManager()
	: evaluator_()
	, inputCms_()
	, mutex_()
{
	/* Intentionally Empty */
}

void BFManager::attach_dag_node(DagMLNodePtr dagnode)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	enum DagMLNode::NodeTypes type;

	/* check that this is actually a subarray node */
	if (dagnode->getType() != DagMLNode::TOP_NODE)
		throw CARMA_ERROR("tried to attach to a non-dag node");

	this->dagnode_ = dagnode;

	/* populate the monitor map */
	type = DagMLNode::MP_NODE;
	populateNodeMapChecked(dagnode, type, this->monitor_map_);

	/* populate the gather map */
	type = DagMLNode::GATHER_NODE;
	populateNodeMapChecked(dagnode, type, this->gather_map_);

	/* populate the bf_output map */
	type = DagMLNode::BF_OUTPUT_NODE;
	populateNodeMapChecked(dagnode, type, this->bfoutput_map_);
}

void BFManager::attach_to_monitor_system(
		carma::monitor::MonitorSystem *inputCms,
		carma::monitor::MonitorSystem *outputCms)
{
	BFEvaluator &evaluator = this->evaluator_;
	evaluator.set_monitor_map(this->monitor_map_);
	evaluator.set_gather_map(this->gather_map_);
	evaluator.attach_to_monitor_system(inputCms, outputCms);

	/* save input monitor system */
	this->inputCms_ = inputCms;

	/* setup the update information structure */
	DagMLNodePtr output = this->dagnode_;
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

void BFManager::set_noise_source_state(const int saNo, const bool on)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	const int index = saNo - 1;

	// update the monitor data and the evaluator state
	this->monitor_.correlatorNoiseOn.at(index) = on;
	this->evaluator_.set_correlator_noise(saNo, on);
}

void BFManager::set_drive_error_preference(const int saNo, const enum carma::fault::EffectPreference pref)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	const int index = saNo - 1;

	// update the monitor data and the evaluator state
	this->monitor_.driveErrorPreference.at(index) = pref;
}

void BFManager::set_monitor_error_preference(const int saNo, const enum carma::fault::EffectPreference pref)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	const int index = saNo - 1;

	// update the monitor data and the evaluator state
	this->monitor_.monitorErrorPreference.at(index) = pref;
}

void BFManager::set_offline_error_preference(const int saNo, const enum carma::fault::EffectPreference pref)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	const int index = saNo - 1;

	// update the monitor data and the evaluator state
	this->monitor_.offlineErrorPreference.at(index) = pref;
}

void BFManager::set_phaselock_error_preference(const int saNo, const enum carma::fault::EffectPreference pref)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	const int index = saNo - 1;

	// update the monitor data and the evaluator state
	this->monitor_.phaselockErrorPreference.at(index) = pref;
}

void BFManager::perform_cycle(const int frame, FaultTransportWriter &transport)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	BFManagerMonitorInfo &monitor = this->monitor_;

	try {
		/* start the update timer */
		monitor.updateLatency.start();

		/* Clear the total fault count */
		monitor.faultCount = 0;

		/*
		 * Evaluate all inputs
		 *
		 * We separately catch exceptions for each input in order to process as
		 * many inputs as possible in catastrophic situations, rather than
		 * just stopping after the first bad input.
		 *
		 * Unlikely, but it seems good to be defensive here.
		 */
		BOOST_FOREACH(DagMLNodeMap::value_type &t, this->bfoutput_map_) {
			try {
				this->process_input(frame, t.second, transport);
			} catch (...) {
				std::ostringstream oss;
				oss << "BFManager::process_input exception: " << carma::util::getStringForCaught();
				programLogErrorIfPossible(oss.str());
			}
		}

		/* stop the update timer */
		monitor.updateLatency.stop();
	} catch (...) {
		monitor.exceptionCount++;
		std::ostringstream oss;
		oss << "BFManager exception: " << carma::util::getStringForCaught();
		programLogErrorIfPossible(oss.str());
	}
}

const BFManagerMonitorInfo& BFManager::getMonitorInfo() const
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	return this->monitor_;
}

/*----------------------------------------------------------------------------*/
/* Miscellaneous Internal Use Functions                                       */
/*----------------------------------------------------------------------------*/

uint32_t BFManager::convert_bitmask(DagBFOutputNode *node, const struct BFEvalResult &result)
{
	const SignalPathSubsystem &sp = this->inputCms_->signalPath();
	const int abIdx = node->getAstroBandNumber() - 1;
	uint32_t output = 0;

	/* not enough info to determine astroband <-> subarray mapping */
	if (!sp.mapping().astroband(abIdx).subarrayNo().isValid()) {
		output |= carma::fault::BlankFlagBits::MONITORDATA;
		output |= carma::fault::BlankFlagBits::MONITORDATA_BLANK;
		return output;
	}

	/* double check the subarray number */
	{
		const int saNo = sp.mapping().astroband(abIdx).subarrayNo().getValue();
		if (saNo < SUBARRAY_NUM_START || saNo > SUBARRAY_NUM_END) {
			std::ostringstream oss;
			oss << "BFManager::convert_bitmask: SPM subarray number " << saNo
				<< " outside range [" << SUBARRAY_NUM_START
				<< "-" << SUBARRAY_NUM_END << "]";
			throw CARMA_ERROR(oss.str());
		}
	}

	const int saIdx = sp.mapping().astroband(abIdx).subarrayNo().getValue() - 1;
	const BFManagerMonitorInfo &monitor = this->monitor_;

	/*
	 * This is a macro to save some typing and ensure uniformity
	 * across all of the different bit types.
	 */
#define CONVERT_HELPER(ENUM, MONVEC)												\
	if (result.bitmask & DagMLNode::EFFECT_##ENUM) {								\
		switch (monitor.MONVEC.at(saIdx)) {											\
		case carma::fault::PREF_NONE:												\
			break;																	\
		case carma::fault::PREF_BLANK:												\
			output |= carma::fault::BlankFlagBits::ENUM;							\
			output |= carma::fault::BlankFlagBits::ENUM##_BLANK;					\
			break;																	\
		case carma::fault::PREF_FLAG:												\
			output |= carma::fault::BlankFlagBits::ENUM;							\
			break;																	\
		default:																	\
			throw CARMA_ERROR(#ENUM " BF preference unknown");						\
		}																			\
	}

	CONVERT_HELPER(DRIVE, driveErrorPreference);
	CONVERT_HELPER(MONITORDATA, monitorErrorPreference);
	CONVERT_HELPER(OFFLINE, offlineErrorPreference);
	CONVERT_HELPER(PHASELOCK, phaselockErrorPreference);
#undef CONVERT_HELPER

	return output;
}

void BFManager::process_input(const int frame, DagMLNodePtr node, FaultTransportWriter &transport)
{
	DagBFOutputNode *output = dynamic_cast<DagBFOutputNode *>(node.get());
	BFEvaluator &evaluator = this->evaluator_;
	struct BFEvalResult result;

	evaluator.evaluate(frame, node, result);

	/*
	 * Convert the internal result bitmask to the external result
	 * bitmask type. Publish it into the output monitor stream.
	 */
	{
		const uint32_t bitmask = this->convert_bitmask(output, result);
		output->setOutputValue(bitmask);
	}

	/* save the faults for later */
	int band = output->getAstroBandNumber();
	int input = output->getAstroInputNumber();
	transport.addInputFaults(band, input, result.faults);

	/* save the total number of faults */
	this->monitor_.faultCount += result.faults.size();

//#define PRINT_INPUT_FAULTS 1
#ifdef PRINT_INPUT_FAULTS
	/* check the validity -- if the node is fine, return immediately */
	if (result.bitmask == BlankFlagBits::NONE)
		return;

	/* we were bad, so print the faults */
	std::cout << "* Input " << std::left << std::setw(14) << node->getName()
		      << " effect: " << result.bitmask << std::endl;

	BOOST_FOREACH(const DagMLNodePtr &fault, result.faults) {
		std::cout << "--> FAULT: " << fault.get() << std::endl;
	}
#endif
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
