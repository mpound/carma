/*
 * Manages an entire fault system DAG
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <cstdio>
#include <string>

#include <boost/foreach.hpp>

#include <carma/fault/Constants.h>
#include <carma/fault/DagManager.h>
#include <carma/fault/FaultControl.h>

#include <carma/util/Time.h>
#include <carma/monitor/MonitorPointSet.h>

#include <carma/util/ThreadQuit.h>
#include <carma/util/ScopedLogNdc.h>
#include <carma/util/StartPthread.h>
#include <carma/util/TimedBenchmark.h>
#include <carma/util/programLogging.h>
#include <carma/util/ExceptionUtils.h>
using namespace carma::util;

#include <carma/monitor/AlarmSubsystem.h>
#include <carma/monitor/ControlSubsystemExt.h>
#include <carma/monitor/FaultSubsystem.h>
#include <carma/monitor/SignalPathSubsystem.h>
#include <carma/monitor/MonitorSystemSelector.h>
using namespace carma::monitor;

DagManager::DagManager()
	: threadGroup_()
	, alarmROH_()
	, lastAlarmState_(false)
	, alarmDeadmanSeconds_(15)
	, prevInputCmsFrame_(0)
{
	/* the last alarm call time was never */
	struct timespec &ts = this->lastAlarmCall_;
	ts.tv_sec = 0;
	ts.tv_nsec = 0;
}

DagManager::~DagManager()
{
	/* intentionally left empty */
}

void DagManager::load_alarm_xml_file(const std::string &name)
{
	FaultSystemMonitorInfo &monitor = this->monitor_;

	monitor.alarmFile = name;
	monitor.alarmConfigError = false;

	try {
		FaultSystemParser &parser = this->alarmParser_;
		TimedBenchmark bench;

		/* load the XML file */
		bench.start();
		parser.load_xml_file(name);
		bench.stop();

		std::ostringstream oss;
		oss << "Alarm XML load + process + DTD validate: " << bench.print();
		programLogInfoIfPossible(oss.str());
	} catch (...) {
		std::ostringstream oss;
		oss << "DAG error (alarm): " << carma::util::getStringForCaught();
		programLogErrorIfPossible(oss.str());

		monitor.alarmConfigError = true;
	}
}

void DagManager::load_bf_xml_file(const std::string &name)
{
	FaultSystemMonitorInfo &monitor = this->monitor_;

	monitor.blankFlagFile = name;
	monitor.blankFlagConfigError = false;

	try {
		FaultSystemParser &parser = this->bfParser_;
		TimedBenchmark bench;

		/* load the XML file */
		bench.start();
		parser.load_xml_file(name);
		bench.stop();

		std::ostringstream oss;
		oss << "BF XML load + process + DTD validate: " << bench.print();
		programLogInfoIfPossible(oss.str());
	} catch (...) {
		std::ostringstream oss;
		oss << "DAG error (bf): " << carma::util::getStringForCaught();
		programLogErrorIfPossible(oss.str());

		monitor.blankFlagConfigError = true;
	}
}

void DagManager::setInputCms(const std::string &name)
{
	const CmsSelector selector = convertStringToCmsSelector(name);
	std::string inputName;

	/* this is ugly as hell, but it works ... */
	CmsPtr inputCms(makeCms(selector, inputName).release());
	this->inputCms_ = inputCms;

	programLogInfoIfPossible("attached to input CMS: " + inputName);
}

void DagManager::setOutputCms(const std::string &name)
{
	const CmsSelector selector = convertStringToCmsSelector(name);
	std::string outputName;

	/* this is ugly as hell, but it works ... */
	CmsPtr outputCms(makeCms(selector, outputName).release());
	this->outputCms_ = outputCms;

	programLogInfoIfPossible("attached to output CMS: " + outputName);
}

CmsPtr DagManager::getInputCms() const
{
	return this->inputCms_;
}

CmsPtr DagManager::getOutputCms() const
{
	return this->outputCms_;
}

void DagManager::attach_to_alarm(CmsPtr inputCms, CmsPtr outputCms)
{
	FaultSystemMonitorInfo &monitor = this->monitor_;
	if (monitor.alarmConfigError)
		return;

	try {
		FaultSystemParser &parser = this->alarmParser_;
		DagMLNodePtr node = parser.make_dagmlnode_tree();

		/* create the new alarm manager */
		AlarmManagerPtr manager(new AlarmManager());

		/* attach to the alarm output and validate the structure */
		manager->attach_alarm_output(node);

		/* set default states to alarm enabled */
		for (int i = SUBARRAY_NUM_START; i <= SUBARRAY_NUM_END; i++) {
			manager->set_alarm_enable_state(i, true);
		}

		/*
		 * attach to the monitor systems
		 *
		 * The output CMS is not needed in the alarm system, so we never
		 * pass it through just to avoid mistakes.
		 */
		manager->attach_to_monitor_system(inputCms.get(), NULL);

		/* save the manager */
		this->alarm_manager_ = manager;
	} catch (...) {
		std::ostringstream oss;
		oss << "DAG attach (alarm): " << carma::util::getStringForCaught();
		programLogErrorIfPossible(oss.str());

		monitor.alarmConfigError = true;
	}
}

void DagManager::attach_to_blankflag(CmsPtr inputCms, CmsPtr outputCms)
{
	FaultSystemMonitorInfo &monitor = this->monitor_;
	if (monitor.blankFlagConfigError)
		return;

	try {
		FaultSystemParser &parser = this->bfParser_;
		DagMLNodePtr node = parser.make_dagmlnode_tree();

		/* create the new blank/flag manager */
		BFManagerPtr manager(new BFManager());

		/* attach to the top level node and validate the structure */
		manager->attach_dag_node(node);

		/* setup defaults */
		for (int i = SUBARRAY_NUM_START; i <= SUBARRAY_NUM_END; i++) {
			manager->set_noise_source_state(i, false);
			manager->set_drive_error_preference(i, carma::fault::PREF_BLANK);
			manager->set_monitor_error_preference(i, carma::fault::PREF_BLANK);
			manager->set_offline_error_preference(i, carma::fault::PREF_BLANK);
			manager->set_phaselock_error_preference(i, carma::fault::PREF_BLANK);
		}

		/* attach to the monitor systems */
		manager->attach_to_monitor_system(inputCms.get(), outputCms.get());

		/* save the manager */
		this->bf_manager_ = manager;
	} catch (...) {
		std::ostringstream oss;
		oss << "DAG attach (blankflag): " << carma::util::getStringForCaught();
		programLogErrorIfPossible(oss.str());

		monitor.blankFlagConfigError = true;
	}
}

void DagManager::start_email_thread(const std::string &emailTab, const int emailHoldoffSecs)
{
	try {
		/*
		 * Start the email thread
		 *
		 * The email thread will send emails in the background as the
		 * fault system notifies it about alarms that have occurred
		 */
		EmailManagerPtr manager(new EmailManager(emailTab, emailHoldoffSecs));

		::pthread_t thread = StartPthreadWithRef(manager->emailThreadEP, *manager, "emailThread");
		this->threadGroup_.insert(thread);
		this->email_manager_ = manager;
	} catch (...) {
		std::ostringstream oss;
		oss << "Email attach: " << carma::util::getStringForCaught();
		programLogErrorIfPossible(oss.str());

		this->monitor_.emailConfigError = true;
	}
}

void DagManager::start_update_thread()
{
	/*
	 * Start the update thread
	 *
	 * The update thread will block until the next frame. Afterwards,
	 * it will run one cycle each frame. This means that the actual fault
	 * system output will not start for ~500ms from now.
	 */
	::pthread_t thread = StartPthreadWithRef(this->updateThreadEP, *this, "updateThread");
	this->threadGroup_.insert(thread);
}

/*----------------------------------------------------------------------------*/
/* Private Members                                                            */
/*----------------------------------------------------------------------------*/

/*
 * Fault System Mainloop
 *
 * Some code and inspiration for this mainloop comes from the existing fault
 * system's DagManager class updateThreadEntryPoint() method.
 */
void DagManager::run_update_mainloop()
{
	CmsPtr inputCms = this->getInputCms();
	CmsPtr outputCms = this->getOutputCms();
	FaultSystemMonitorInfo &monitor = this->monitor_;
	FaultTransportWriter &transport = this->transport_;

	/* Set some monitor information */
	monitor.updateCycleNo++;
	monitor.monSysFrameSize = inputCms->getTotalMonitorSystemSize() / 1024.0;

	/*
	 * Blocking read from the input CMS
	 *
	 * This copies the shared memory CMS into the local copy of
	 * the CMS. You can now operate on the local CMS as much as
	 * you need to before writing it back to shared memory.
	 */
	inputCms->read();
	monitor.totalLatency.start();

	/*
	 * Set the time the input CMS was read by the fault system.
	 * This is used for internal monitor system bookkeeping only.
	 */
	inputCms->setRawReadTime();

	/*
	 * Check the current and last frame counters and report errors
	 */
	int count = inputCms->getFrameCount();
	monitor.inputCmsFrameCount = count;
	this->checkMonitorFrameNumber(count);

	/*
	 * Setup the fault transport writer and get ready to transport
	 * the next round of faults
	 */
	transport.clear();
	transport.setCmsFrameNumber(count);
	transport.setFaultCycleNumber(monitor.updateCycleNo);

	/*
	 * Syncronize the local input CMS with the local output CMS
	 *
	 * This only operates on the in-process copies. Nothing has
	 * been written out to the shared memory yet.
	 */
	monitor.monitorSyncLatency.start();
	outputCms->synchronize(*inputCms);
	monitor.monitorSyncLatency.stop();

	/*
	 * We have an error. We have already forced out the list of monitor
	 * points causing the error and written the output CMS inside the
	 * function. Don't both with the rest of the mainloop.
	 *
	 * The alarm will go on due to the deadman in the worst case.
	 */
	const bool have_error = this->writeFaultSystemErrorState();
	if (have_error) {
		transport.write();
		outputCms->write();
		return;
	}

	/* Start the DAG evaluation latency timer */
	monitor.evaluationLatency.start();

	/*
	 * Run the DAG evaluation for blanking/flagging
	 *
	 * When this finishes, all of the blank/flag monitor points have
	 * been set to their final values, and each BFManager's
	 * monitor information is ready for collection.
	 */
	BFManagerPtr &bfManager = this->bf_manager_;
	bfManager->perform_cycle(count, transport);

	/*
	 * Run the DAG evaluation to get the alarm output state
	 *
	 * When this finishes, all of the alarm monitor points have been
	 * evaluated, and the monitor information is ready for collection.
	 */
	AlarmManagerPtr &alarmManager = this->alarm_manager_;
	alarmManager->perform_cycle(count, transport);

	/* Stop the DAG evaluation latency timer */
	monitor.evaluationLatency.stop();

	/* Stop the totalLatency timer */
	monitor.totalLatency.stop();

	/*
	 * Write all of the monitor points for the fault system itself
	 *
	 * These monitor points cannot (by definition) be used in the DAG
	 * itself, since they are created by the evaluation of the DAG itself
	 */
	this->writeFaultSystemMonitorPoints();

	/*
	 * Write our local copy of the output CMS into shared memory
	 *
	 * Now the rest of the system can see it
	 */

	outputCms->write();

	/*
	 * Write out the fault transport writer's information to the
	 * underlying IPQ. This means that clients can now see the
	 * fault data that was produced.
	 */
	transport.write();

	/* Activate the alarm if necessary */
	this->updateAlarmInfo(count);
}

void DagManager::checkMonitorFrameNumber(int inputCmsFrame)
{
	FaultSystemMonitorInfo &monitor = this->monitor_;
	const int cycleNo = monitor.updateCycleNo;
	int &prevInputCmsFrame = this->prevInputCmsFrame_;

	/*
	 * Make sure we have a previous reading
	 *
	 * If there was no previous reading, save the current frame
	 * number and return immediately.
	 */
	if (prevInputCmsFrame == 0) {
		prevInputCmsFrame = inputCmsFrame;
		return;
	}

	/* check for duplicate frames */
	if (inputCmsFrame == prevInputCmsFrame) {
		std::ostringstream oss;

		oss << "Update cycle #" << cycleNo
			<< ": duplicate frames (" << prevInputCmsFrame
			<< " -> " << inputCmsFrame << ")";
		programLogErrorIfPossible(oss.str());
	}

	/* check for misordered frames */
	if (inputCmsFrame < prevInputCmsFrame) {
		std::ostringstream oss;

		oss << "Update cycle #" << cycleNo
			<< ": misordered frames (" << prevInputCmsFrame
			<< " -> " << inputCmsFrame << ")";
		programLogErrorIfPossible(oss.str());
	}

	/* check for skipped frames */
	const int skipped = (inputCmsFrame - (prevInputCmsFrame + 1));
	if (skipped > 0) {
		monitor.droppedFrames += skipped;
		std::ostringstream oss;

		oss << "Update cycle #" << cycleNo
			<< ": " << skipped << " frame skip detected ("
			<< prevInputCmsFrame << " -> " << inputCmsFrame
			<< ")";
		programLogWarnIfPossible(oss.str());
	}

	/* save the frame counter */
	prevInputCmsFrame = inputCmsFrame;
}

bool DagManager::writeFaultSystemErrorState()
{
	CmsPtr outputCms = this->getOutputCms();
	FaultSubsystem &fault = outputCms->fault();
	FaultSystemMonitorInfo &monitor = this->monitor_;
	FaultTransportWriter &transport = this->transport_;

	// always write these monitor points no matter what
	fault.alarmConfigError().setValue(monitor.alarmConfigError);
	fault.blankFlagConfigError().setValue(monitor.blankFlagConfigError);
	fault.emailConfigError().setValue(monitor.emailConfigError);

	std::list<carma::monitor::tagIDType> ids;

	if (monitor.alarmConfigError)
		ids.push_back(fault.alarmConfigError().getTagID());

	if (monitor.blankFlagConfigError)
		ids.push_back(fault.blankFlagConfigError().getTagID());

	if (monitor.emailConfigError)
		ids.push_back(fault.emailConfigError().getTagID());

	// if there is an error, write to to the transporter and
	// tell the mainloop to exit very early
	if (!ids.empty()) {
		transport.setConfigFaults(ids);
		return true;
	}

	// no error
	return false;
}

void DagManager::writeFaultSystemMonitorPoints()
{
	CmsPtr inputCms = this->getInputCms();
	CmsPtr outputCms = this->getOutputCms();
	FaultSubsystem &fault = outputCms->fault();
	FaultSystemMonitorInfo &monitor = this->monitor_;
	int num;

	/* Fault System timing information */
	fault.cmsSyncStepLatency().setValue(monitor.monitorSyncLatency.milliseconds());
	fault.dagUpdStepLatency().setValue(monitor.evaluationLatency.milliseconds());
	fault.totalLatency().setValue(monitor.totalLatency.milliseconds());

	/* Monitor System information */
	fault.inputCmsFrameCount().setValue(monitor.inputCmsFrameCount);

	/* Fault System internal diagnostics */
	num = static_cast<int>(monitor.updateCycleNo & 0x7fffffff);
	fault.updateCycleNo().setValue(num);
	fault.droppedFrames().setValue(monitor.droppedFrames);
	fault.exceptionCount().setValue(monitor.exceptionCount);
	fault.monSysFrameSize().setValue(monitor.monSysFrameSize);

	fault.alarmFile().setValue(monitor.alarmFile);
	fault.blankFlagFile().setValue(monitor.blankFlagFile);

	/* BlankFlag output monitor points */
	{
		const BFManagerPtr &manager = this->bf_manager_;
		const BFManagerMonitorInfo &info = manager->getMonitorInfo();
		FaultSubsystem::BlankFlag &blankFlag = fault.blankFlag();

		blankFlag.updateLatency().setValue(info.updateLatency.milliseconds());
		blankFlag.faultCount().setValue(info.faultCount);
		blankFlag.exceptionCount().setValue(info.exceptionCount);

		for (unsigned int i = 0; i < info.correlatorNoiseOn.size(); i++) {
			const bool on = info.correlatorNoiseOn.at(i);
			fault.subarray(i).correlatorNoiseOn().setValue(on);
		}

#define MP_ARRAY_HELPER(VEC, MPNAME)											\
		for (unsigned int i = 0; i < info.VEC.size(); i++) {					\
			typedef FaultSubsystem::ErrorPreferenceMonitorPointEnum ErrorMPE;	\
																				\
			const carma::fault::EffectPreference pref = info.VEC.at(i);			\
			ErrorMPE &mp = fault.subarray(i).MPNAME();							\
																				\
			/* convert types without casts */									\
			switch (pref) {														\
			case carma::fault::PREF_NONE:										\
				mp.setValue(ErrorMPE::NONE);									\
				break;															\
			case carma::fault::PREF_BLANK:										\
				mp.setValue(ErrorMPE::BLANK);									\
				break;															\
			case carma::fault::PREF_FLAG:										\
				mp.setValue(ErrorMPE::FLAG);									\
				break;															\
			default:															\
				mp.setAveValidity(carma::monitor::MonitorPoint::INVALID_NO_DATA);\
				break;															\
			}																	\
		}

		MP_ARRAY_HELPER(driveErrorPreference, driveErrorPreference);
		MP_ARRAY_HELPER(monitorErrorPreference, monitorErrorPreference);
		MP_ARRAY_HELPER(offlineErrorPreference, offlineErrorPreference);
		MP_ARRAY_HELPER(phaselockErrorPreference, phaselockErrorPreference);

#undef MP_ARRAY_HELPER
	}

	/* Alarm output monitor points */
	{
		const AlarmManagerPtr &manager = this->alarm_manager_;
		const AlarmManagerMonitorInfo &info = manager->getMonitorInfo();
		FaultSubsystem::Alarm &alarm = fault.alarm();

		alarm.updateLatency().setValue(info.updateLatency.milliseconds());
		alarm.noisyAlarmOn().setValue(info.noisyAlarmOn);
		alarm.silentAlarmOn().setValue(info.silentAlarmOn);
		alarm.faultCount().setValue(info.faultCount);
		alarm.exceptionCount().setValue(info.exceptionCount);

		fault.alarmOn().setValue(info.noisyAlarmOn);

		/*
		 * This is a massive hack to make the RTD composite page behave as it
		 * has in the past. Previously, it displayed the value of the alarm
		 * subsystem hardware switch.
		 *
		 * Now that the fault system controls the alarm in per-subarray mode,
		 * we have to display the per-subarray alarm enable status too.
		 *
		 * We do the following:
		 * - hardware switch disabled				HW_DISABLED
		 * - software switch disabled				SW_DISABLED
		 * - noisy alarm is on						TRIGGERED
		 * - silent alarm is on						SILENT
		 * - all other cases						ENABLED
		 *
		 * In addition to the other stuff above, it gets worse: we have to do
		 * the thresholding ourselves. The frameCollator normally runs the monitor
		 * point thresholding code, but we come after the frameCollator in the
		 * monitor data processing pipeline.
		 *
		 * If I did this the "right" way (using the SystemThresholdFrame class), it
		 * would add ~10% CPU usage to the fault system, for no reason. Instead,
		 * we just hard-code the thresholds right here.
		 */
		for (unsigned int i = 0; i < info.alarmEnable.size(); i++) {
			typedef FaultSubsystem::AlarmEnableMonitorPointEnum FaultEnum;
			typedef AlarmSubsystem::AlarmEnableMonitorPointEnum AlarmEnum;

			AlarmEnum &hwswitch = inputCms->alarm().alarmEnable();
			FaultEnum &rtdmp = fault.subarray(i).alarmEnable();

			if (hwswitch.getValue() == AlarmEnum::DISABLED) {
				rtdmp.setValue(FaultEnum::HW_DISABLED);
				rtdmp.setValidity(MonitorPoint::VALID_ERROR);
			} else if (!info.alarmEnable.at(i)) {
				rtdmp.setValue(FaultEnum::SW_DISABLED);
				rtdmp.setValidity(MonitorPoint::VALID_WARNING);
			} else if (info.noisyAlarmOn) {
				rtdmp.setValue(FaultEnum::TRIGGERED);
				rtdmp.setValidity(MonitorPoint::VALID_ERROR);
			} else if (info.silentAlarmOn) {
				rtdmp.setValue(FaultEnum::SILENT);
				rtdmp.setValidity(MonitorPoint::VALID_GOOD);
			} else {
				rtdmp.setValue(FaultEnum::ENABLED);
				rtdmp.setValidity(MonitorPoint::VALID_GOOD);
			}
		}
	}

	/*
	 * We must do the averaging for the fault subsystem ourselves
	 * because this process runs after the averaging has already
	 * been done for the rest of the monitor system
	 */
	fault.updateFrameAverage();
	/*
	 * Pretend we have a FrameScriberPublisher running so that we
	 * can set the correct write times, which should make
	 * MonitorSubsystem::isCurrent() return true. This is required
	 * for the SubarrayController FaultHandle stuff to work correctly.
	 */
	double t0 = Time::MJD();
	int fc = inputCms->getFrameCount();
	SubsystemFrame& ssf = fault.monitorPointSet().getSubsystemFrame();
	ssf.setFrameCount(fc);
	ssf.setLastWriteTime(t0);
	ssf.setScriberWriteTime(t0);
	ssf.setReceiveTime(t0);
	ssf.setPublishTime(t0);
	ssf.setLastWriterDelay(0.123);   // arbitrary
	ssf.setScriberWriteDelay(0.345); // arbitrary
	ssf.markFrameReceived();
	ssf.markFramePublished();

	/* We're done writing values, set the time of the last write */
	outputCms->setFinalWriteTime();
}

/*
 * Update the shared alarm info
 *
 * Note that this is only safe if run syncronized with the run_update_mainloop()
 * function. In our case, we run it directly, so it cannot conflict with
 * actually running the DAG evaluation.
 */
void DagManager::updateAlarmInfo(const int frame)
{
	std::string sound = "default";
	std::string mpName = "unknown monitor point";
	AlarmManagerPtr &alarmManager = this->alarm_manager_;
	EmailManagerPtr &emailManager = this->email_manager_;
	carma::fault::AlarmControlROH &alarmROH = this->alarmROH_;
	const ScopedLogNdc ndc("DagManager::updateAlarmInfo");

	const AlarmManagerMonitorInfo &info = alarmManager->getMonitorInfo();
	const bool isAlarmOn = info.noisyAlarmOn;

	/* Get the sound and monitor point name */
	if (info.noisyAlarmOn && info.faults.size() >= 1) {
		BOOST_FOREACH(DagMLNodePtr fault, info.faults) {
			DagMPNode *mp = dynamic_cast<DagMPNode *>(fault.get());

			/* skip silent alarms */
			if (mp->getSilent())
				continue;

			/* not a silent alarm, save the MP name and sound */
			mpName = mp->getName();
			sound = mp->getSound();

			/* exit the loop: only the first error MP is displayed */
			break;
		}
	}

	/* If the email manager is running, update it */
	if (emailManager.get())
		emailManager->addFaults(frame, info.faults);

	/*
	 * Get the current time from the monotonic clock
	 *
	 * This clock is guaranteed never to slew backwards. It is still adjusted
	 * by NTP, meaning that it may slew forwards, but that doesn't hurt the
	 * timing here.
	 */
	struct timespec now;
	int ret = clock_gettime(CLOCK_MONOTONIC, &now);
	if (ret < 0) {
		programLogErrorIfPossible("clock_gettime() failed");

		/* force us to call the alarm */
		alarmROH.setState(isAlarmOn, sound, mpName);
		return;
	}

	/* if the alarm state has changed, call the alarm immediately */
	if (this->lastAlarmState_ != isAlarmOn) {
		this->lastAlarmCall_ = now;
		this->lastAlarmState_ = isAlarmOn;
		alarmROH.setState(isAlarmOn, sound, mpName);
		return;
	}

	/*
	 * If it has been too long since we last called the alarm, then
	 * we should just go ahead and call it. This will keep it notified
	 * that we are still connected.
	 */
	const int seconds = now.tv_sec - this->lastAlarmCall_.tv_sec;
	if (seconds >= this->alarmDeadmanSeconds_) {
		this->lastAlarmCall_ = now;
		this->lastAlarmState_ = isAlarmOn;
		alarmROH.setState(isAlarmOn, sound, mpName);
		return;
	}
}

void DagManager::updateThreadEP(DagManager &manager)
{
	const ScopedLogNdc ndc("DagManager::updateThread");

	while (true) {
		/* run an iteration of the fault system mainloop */
		try {
			ThreadQuitTestSelf();
			manager.run_update_mainloop();
		} catch (...) {
			if (CaughtExceptionIsThreadQuitRequestedError()) {
				programLogInfoIfPossible("thread quit requested");
				return;
			}

			std::ostringstream oss;
			oss << "error in mainloop: " << carma::util::getStringForCaught();
			programLogErrorIfPossible(oss.str());
		}
	}
}

/*----------------------------------------------------------------------------*/
/* External CORBA Interface                                                   */
/*----------------------------------------------------------------------------*/

static void checkSubarrayNumber(const int num)
{
	if (num < SUBARRAY_NUM_START || num > SUBARRAY_NUM_END) {
		std::ostringstream oss;
		oss << "Subarray #" << num << "is invalid";
		throw CARMA_ERROR(oss.str());
	}
}

static void checkEffectPreference(const enum carma::fault::EffectPreference pref)
{
	switch (pref) {
	case carma::fault::PREF_NONE:
	case carma::fault::PREF_BLANK:
	case carma::fault::PREF_FLAG:
		break;
	default:
		{
			std::ostringstream oss;
			oss << "unsupported preference value: " << pref;
			throw CARMA_ERROR(oss.str());
		}
		break;
	}
}

void DagManager::CORBA_setNoiseState(int num, bool on)
{
	checkSubarrayNumber(num);

	try {
		BFManagerPtr &manager = this->bf_manager_;
		manager->set_noise_source_state(num, on);
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser();
	}
}

void DagManager::CORBA_setDriveErrorPreference(int num, enum carma::fault::EffectPreference pref)
{
	checkSubarrayNumber(num);
	checkEffectPreference(pref);

	try {
		BFManagerPtr &manager = this->bf_manager_;
		manager->set_drive_error_preference(num, pref);
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser();
	}
}

void DagManager::CORBA_setMonitorErrorPreference(int num, enum carma::fault::EffectPreference pref)
{
	checkSubarrayNumber(num);
	checkEffectPreference(pref);

	try {
		BFManagerPtr &manager = this->bf_manager_;
		manager->set_monitor_error_preference(num, pref);
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser();
	}
}

void DagManager::CORBA_setOfflineErrorPreference(int num, enum carma::fault::EffectPreference pref)
{
	checkSubarrayNumber(num);
	checkEffectPreference(pref);

	try {
		BFManagerPtr &manager = this->bf_manager_;
		manager->set_offline_error_preference(num, pref);
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser();
	}
}

void DagManager::CORBA_setPhaselockErrorPreference(int num, enum carma::fault::EffectPreference pref)
{
	checkSubarrayNumber(num);
	checkEffectPreference(pref);

	try {
		BFManagerPtr &manager = this->bf_manager_;
		manager->set_phaselock_error_preference(num, pref);
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser();
	}
}

void DagManager::CORBA_disableAlarms(const StringList &names)
{
	try {
		AlarmManagerPtr &manager = this->alarm_manager_;
		manager->disable_alarms(names);
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser();
	}
}

void DagManager::CORBA_restoreAlarms(const StringList &names)
{
	try {
		AlarmManagerPtr &manager = this->alarm_manager_;
		manager->restore_alarms(names);
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser();
	}
}

void DagManager::CORBA_setAlarmEnable(int num, bool on)
{
	try {
		AlarmManagerPtr &manager = this->alarm_manager_;
		manager->set_alarm_enable_state(num, on);
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser();
	}
}

void DagManager::CORBA_setAlarmDeadmanSecs(int seconds)
{
	/* Check for sanity */
	if (seconds < 5) {
		std::ostringstream oss;

		oss << "minimum alarm deadman timeout is 5 seconds";
		throw CARMA_ERROR(oss.str());
	}

	if (seconds > 600) {
		std::ostringstream oss;

		oss << "maximum alarm deadman timeout is 600 seconds";
		throw CARMA_ERROR(oss.str());
	}

	this->alarmDeadmanSeconds_ = seconds;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
