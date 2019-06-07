//
// @version $Revision: 1.6 $
//
// @usage @autogen
//
// @description
//  utility to dump or check fault system DAG file
//
// @key alarmFile "fault/alarm.xml" string
//  The Fault System Alarm configuration file
//
// @key blankFlagFile "fault/blankflag.xml" string
//  The Fault System Blank/Flag configuration file
//
// @key dtdValidate true bool
//  Validate the DAG file using the XML DTD
//
// @key monitorValidate true bool
//  Validate the DAG by attaching to the CARMA monitor system. This
//  performs extra checks on the DAG structure.
//
// @key inputCms "raw" string
//  The input CARMA monitor system to attach to. You should not need
//  to change the default.
//
// @key outputCms "raw" string
//  The output CARMA monitor system to attach to. You should not need
//  to change the default.
//
// @key dumpAlarm false bool
//  Dump the pre-processed alarmFile to stdout
//
// @key dumpBlankFlag false bool
//  Dump the pre-processed blankFlagFile to stdout
//
// @logger MONITOR_FACILITY carma.fault.dumpCheckDag
//

/*
 * Utility to dump or check a Fault System DAG file
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <cstdio>
#include <string>

#include <carma/fault/DagVerifier.h>

#include <carma/util/ExceptionUtils.h>
#include <carma/util/Program.h>
using namespace carma::util;

struct options {
	std::string alarm_name;		/* alarm filename */
	std::string bf_name;		/* blankflag filename */

	bool dtd_validate;			/* XML DTD validation */
	bool monitor_validate;		/* Monitor System validation */

	std::string inputCmsName;	/* Input Cms Name */
	std::string outputCmsName;	/* Output Cms Name */

	bool dump_alarm;			/* dump alarm file to stdout */
	bool dump_bf;				/* dump bf file to stdout */
};

static void real_main(const struct options &opts)
{
	DagVerifier verifier(opts.alarm_name, opts.bf_name);

	verifier.load_xml_files(opts.dtd_validate);

	if (opts.monitor_validate)
		verifier.validate_monitor_points(opts.inputCmsName, opts.outputCmsName);

	if (opts.dump_alarm)
		verifier.dump_alarm_to_stdout();

	if (opts.dump_bf)
		verifier.dump_bf_to_stdout();
}

int Program::main ()
{
	struct options opts;
	int ret = 0;

	/* initialize the xerces-c library */
	xercesc::XMLPlatformUtils::Initialize();

	/*
	 * Another main is used since we must have the Xerces-C library initialized
	 * before calling any function from it. The parser calls functions in it's
	 * constructor.
	 */
	opts.alarm_name = getConfFile(getStringParameter("alarmFile"));
	opts.bf_name = getConfFile(getStringParameter("blankFlagFile"));
	opts.dtd_validate = getBoolParameter("dtdValidate");
	opts.monitor_validate = getBoolParameter("monitorValidate");
	opts.inputCmsName = getStringParameter("inputCms");
	opts.outputCmsName = getStringParameter("outputCms");
	opts.dump_alarm = getBoolParameter("dumpAlarm");
	opts.dump_bf = getBoolParameter("dumpBlankFlag");

	try {
		real_main(opts);
	} catch (...) {
		std::cerr << "ERROR: " << getStringForCaught() << std::endl;
		ret = EXIT_FAILURE;
	}

	/*
	 * cleanup the xerces-c library
	 *
	 * Minimal testing has shown that keeping Xerces-C loaded throughout
	 * the entire runtime should not be a problem. The memory hit with a
	 * very large XML file is extremely low, a few megabyte at most.
	 */
	xercesc::XMLPlatformUtils::Terminate();
	return ret;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
