/*
 * Convenience class to hold fault system monitor information
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef FAULT_SYSTEM_MONITOR_INFO_H
#define FAULT_SYSTEM_MONITOR_INFO_H

#include <string>

#include <carma/fault/Constants.h>
#include <carma/fault/FaultControl.h>

#include <carma/util/TimedBenchmark.h>
using namespace carma::util;

/*
 * Monitor information for the blank/flag output
 */
class BFManagerMonitorInfo
{
	public:

		/* constructor */
		BFManagerMonitorInfo();

		/* Basic Information */
		TimedBenchmark updateLatency;
		int faultCount;
		int exceptionCount;

		/* Transients Monitor */
		std::vector<bool> correlatorNoiseOn;

		/* Effect Preferences */
		std::vector<enum carma::fault::EffectPreference> driveErrorPreference;
		std::vector<enum carma::fault::EffectPreference> monitorErrorPreference;
		std::vector<enum carma::fault::EffectPreference> offlineErrorPreference;
		std::vector<enum carma::fault::EffectPreference> phaselockErrorPreference;
};

inline BFManagerMonitorInfo::BFManagerMonitorInfo()
	: faultCount(0)
	, exceptionCount(0)
	, correlatorNoiseOn(NUM_SUBARRAYS, false)
	, driveErrorPreference(NUM_SUBARRAYS, carma::fault::PREF_BLANK)
	, monitorErrorPreference(NUM_SUBARRAYS, carma::fault::PREF_BLANK)
	, offlineErrorPreference(NUM_SUBARRAYS, carma::fault::PREF_BLANK)
	, phaselockErrorPreference(NUM_SUBARRAYS, carma::fault::PREF_BLANK)
{
	/* intentionally left empty */
}

/*
 * Monitor information for the alarm output
 */
class AlarmManagerMonitorInfo
{
	public:

		/* constructor */
		AlarmManagerMonitorInfo();

		/* Basic Information */
		TimedBenchmark updateLatency;
		int faultCount;
		int exceptionCount;

		/* Alarm Status */
		bool noisyAlarmOn;
		bool silentAlarmOn;

		/* Alarm Enable Status */
		std::vector<bool> alarmEnable;

		/* Faults from the last evaluation */
		DagMLNodeList faults;
};

inline AlarmManagerMonitorInfo::AlarmManagerMonitorInfo()
	: updateLatency()
	, faultCount(0)
	, exceptionCount(0)
	, noisyAlarmOn(false)
	, silentAlarmOn(false)
	, alarmEnable(NUM_SUBARRAYS, true)
{
	/* intentionally left empty */
}

/*
 * Monitor information for the entire fault system
 */
class FaultSystemMonitorInfo
{
	public:

		/* constructor */
		FaultSystemMonitorInfo();

		/* misc monitor information */
		int inputCmsFrameCount;
		int updateCycleNo;
		int droppedFrames;
		int exceptionCount;

		float monSysFrameSize;

		std::string alarmFile;
		std::string blankFlagFile;

		bool alarmConfigError;
		bool blankFlagConfigError;
		bool emailConfigError;

		/* various clock readings, in the order used */
		TimedBenchmark monitorSyncLatency;
		TimedBenchmark evaluationLatency;
		TimedBenchmark totalLatency;
};

inline FaultSystemMonitorInfo::FaultSystemMonitorInfo()
	: inputCmsFrameCount(0)
	, updateCycleNo(0)
	, droppedFrames(0)
	, exceptionCount(0)
	, monSysFrameSize(0)
	, alarmFile("unknown")
	, blankFlagFile("unknown")
	, alarmConfigError(false)
	, blankFlagConfigError(false)
	, emailConfigError(false)
{
	/* intentionally left empty */
}

#endif /* FAULT_SYSTEM_MONITOR_INFO_H */
