//
// @version $Revision: 1.65 $
//
// @usage @autogen
//
// @description
//  Fault System
//
// @key alarmFile "fault/alarm.xml" string
//  The Fault System Alarm configuration file
//
// @key blankFlagFile "fault/blankflag.xml" string
//  The Fault System Blank/Flag configuration file
//
// @key inputCMS "raw" string
//  Monitor system to use for input.
//  One of { raw, final, intermediate }
//
// @key outputCMS "intermediate" string
//  Monitor system to use for output.
//  One of { raw, final, intermediate }
//
// @key runEmailService false bool
//  Enable the Fault System Email facility
//
// @key emailTab "fault/email.tab" string
//  The Fault System Email facility configuration file
//
// @key emailHoldoffSecs 300 int
//  The Fault System Email facility holdoff time
//
// @logger MONITOR_FACILITY carma.fault.faultSystem
//

/*
 * Main Fault System Program
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <carma/fault/DagManager.h>
#include <carma/fault/FaultControlImpl.h>

#include <carma/fault/FaultControl.h>
#include <carma/fault/FaultControl_skel.h>
#include <carma/fault/FaultControl_skel_tie.h>

#include <carma/corba/Server.h>

#include <carma/util/ScopedLogNdc.h>
#include <carma/util/programLogging.h>
#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
using namespace carma::util;

struct options {
	/* main fault system configuration */
	std::string alarm_file;
	std::string bf_file;
	std::string inputCms;
	std::string outputCms;

	/* fault system email configuration */
	bool runEmailService;
	std::string emailTab;
	int emailHoldoffSecs;
};

/*
 * This function only exists to guarantee that the Xerces-C library has
 * been initialized before any objects which may use it
 */
static void startFaultSystem(DagManager &manager, struct options &opts)
{
	const ScopedLogNdc ndc("startFaultSystem");

	/*
	 * setup the internal references to the input
	 * and output monitor systems
	 */
	programLogInfoIfPossible("setting requested CMS's");
	manager.setInputCms(opts.inputCms);
	manager.setOutputCms(opts.outputCms);

	CmsPtr inputCms = manager.getInputCms();
	CmsPtr outputCms = manager.getOutputCms();

	/* load the BF XML */
	programLogInfoIfPossible("loading the Blank/Flag configuration file");
	manager.load_bf_xml_file(opts.bf_file);

	/* load the Alarm XML */
	programLogInfoIfPossible("loading the Alarm configuration file");
	manager.load_alarm_xml_file(opts.alarm_file);

	/* update the CMS */
	programLogInfoIfPossible("updating the input CMS");
	inputCms->readNewestConditionalCopy();

	/* attach to the blankflag output */
	programLogInfoIfPossible("attaching to the blankflag output");
	manager.attach_to_blankflag(inputCms, outputCms);

	/* attach to the alarm output */
	programLogInfoIfPossible("attaching to the alarm output");
	manager.attach_to_alarm(inputCms, outputCms);

	/* start the emailer */
	if (opts.runEmailService) {
		manager.start_email_thread(opts.emailTab, opts.emailHoldoffSecs);
	}

	/* start the update thread */
	programLogInfoIfPossible("starting the update thread");
	manager.start_update_thread();

	/* finished! */
	programLogInfoIfPossible("fault system started!");
}

int carma::util::Program::main()
{
	struct options opts;

	/* main fault system configuration */
	opts.alarm_file = getConfFile(getStringParameter("alarmFile"));
	opts.bf_file = getConfFile(getStringParameter("blankFlagFile"));
	opts.inputCms = getStringParameter("inputCMS");
	opts.outputCms = getStringParameter("outputCMS");

	/* fault system email service */
	opts.runEmailService = getBoolParameter("runEmailService");
	opts.emailTab = getConfFile(getStringParameter("emailTab"));
	opts.emailHoldoffSecs = getIntParameter("emailHoldoffSecs");

	/* initialize the xerces-c library */
	xercesc::XMLPlatformUtils::Initialize();

	try {

		/* get the fault system ready to run */
		DagManager dagManager;
		startFaultSystem(dagManager, opts);

		/* get the FaultControl CORBA interface ready */
		carma::fault::FaultControlImpl servant(dagManager);
		carma::corba::Server &server = carma::util::Program::getCorbaServer();
		server.addServant<POA_carma::fault::FaultControl_tie>(servant, carma::fault::FAULT_CONTROL_NAME);

		/* run the CORBA ORB (blocks) */
		programLogInfoIfPossible("Calling corba::Server::run()");
		server.run(false);
		programLogInfoIfPossible("Finished corba::Server::run()");

		/*
		 * TODO FIXME HACK:
		 *
		 * Until I figure out how to shut down all of my threads cleanly,
		 * we'll force exit the process when the ORB stops blocking.
		 *
		 * The standard CARMA atexit handler will cause crashes due
		 * to the xercesc termination order.
		 */
		exit(EXIT_SUCCESS);
	} catch (...) {
		std::ostringstream oss;

		oss << "CRASH: " << getStringForCaught() << std::endl;
		std::cerr << oss.str() << std::endl;
		programLogErrorIfPossible(oss.str());
		exit(EXIT_FAILURE);
	}

	/*
	 * cleanup the xerces-c library
	 *
	 * Minimal testing has shown that keeping Xerces-C loaded throughout
	 * the entire runtime should not be a problem. The memory hit with a
	 * very large XML file is extremely low, a few megabyte at most.
	 */
	xercesc::XMLPlatformUtils::Terminate();

	return 0;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
