/*
 * Manage an entire DAG
 *
 * This is basically the meat of the fault system. It handles watching an
 * entire DAG, and turning on/off the alarm and blank/flag as necessary.
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef DAG_MANAGER_H
#define DAG_MANAGER_H

#include <map>
#include <string>

#include <boost/shared_ptr.hpp>

#include <carma/monitor/MonitorSystem.h>

#include <carma/util/AutoPthreadQuitAndJoinGroup.h>

#include <carma/fault/AlarmControlROH.h>
#include <carma/fault/FaultSystemParser.h>
#include <carma/fault/FaultSystemMonitorInfo.h>
#include <carma/fault/FaultTransport.h>
#include <carma/fault/AlarmManager.h>
#include <carma/fault/EmailManager.h>
#include <carma/fault/BFManager.h>

typedef boost::shared_ptr<carma::monitor::CarmaMonitorSystem> CmsPtr;
typedef boost::shared_ptr<BFManager> BFManagerPtr;
typedef boost::shared_ptr<AlarmManager> AlarmManagerPtr;
typedef boost::shared_ptr<EmailManager> EmailManagerPtr;

class DagManager
{
	public:
		DagManager();
		~DagManager();

		/* load an XML dag file */
		void load_alarm_xml_file(const std::string &name);
		void load_bf_xml_file(const std::string &name);

		void setInputCms(const std::string &name);
		void setOutputCms(const std::string &name);

		CmsPtr getInputCms() const;
		CmsPtr getOutputCms() const;

		/* attach to the alarm output */
		void attach_to_alarm(CmsPtr inputCms, CmsPtr outputCms);

		/* attach to the blankflag outputs */
		void attach_to_blankflag(CmsPtr inputCms, CmsPtr outputCms);

		/* run the email thread */
		void start_email_thread(const std::string &emailTab, const int emailHoldoffSecs);

		/* run the update thread */
		void start_update_thread();

		/* external CORBA calls */
		void CORBA_setNoiseState(int num, bool on);
		void CORBA_setDriveErrorPreference(int num, enum carma::fault::EffectPreference pref);
		void CORBA_setMonitorErrorPreference(int num, enum carma::fault::EffectPreference pref);
		void CORBA_setOfflineErrorPreference(int num, enum carma::fault::EffectPreference pref);
		void CORBA_setPhaselockErrorPreference(int num, enum carma::fault::EffectPreference pref);
		void CORBA_disableAlarms(const StringList &names);
		void CORBA_restoreAlarms(const StringList &names);
		void CORBA_setAlarmEnable(int num, bool on);
		void CORBA_setAlarmDeadmanSecs(int seconds);

	protected:

		/* run a single cycle of the update thread */
		void run_update_mainloop();
		void checkMonitorFrameNumber(int inputCmsFrame);
		bool writeFaultSystemErrorState();
		void writeFaultSystemMonitorPoints();
		void updateAlarmInfo(const int frame);

		/* update thread */
		AutoPthreadQuitAndJoinGroup threadGroup_;
		carma::fault::AlarmControlROH alarmROH_;
		struct timespec lastAlarmCall_;
		bool lastAlarmState_;
		int alarmDeadmanSeconds_;
		int prevInputCmsFrame_;
		static void updateThreadEP(DagManager &fs);

		CmsPtr inputCms_;
		CmsPtr outputCms_;

		FaultSystemParser bfParser_;
		FaultSystemParser alarmParser_;

		BFManagerPtr bf_manager_;
		AlarmManagerPtr alarm_manager_;
		EmailManagerPtr email_manager_;

		FaultSystemMonitorInfo monitor_;
		FaultTransportWriter transport_;
};

#endif /* DAG_MANAGER_H */

/* vim: set ts=4 sts=4 sw=4 noet: */
