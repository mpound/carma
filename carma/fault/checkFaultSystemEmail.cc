//
// @version $Revision: 1.1 $
//
// @usage @autogen
//
// @description
//  utility to dump or check fault system email configuration file(s)
//
// @key emailTab "fault/email.tab" string
//  The Fault System Email configuration file (CARMA Table format)
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

#include <boost/shared_ptr.hpp>

#include <carma/fault/EmailManager.h>

#include <carma/util/ExceptionUtils.h>
#include <carma/util/Program.h>
using namespace carma::util;

typedef boost::shared_ptr<EmailManager> EmailManagerPtr;

int Program::main ()
{
	const std::string emailTab = getConfFile(getStringParameter("emailTab"));

	try {
		EmailManagerPtr manager(new EmailManager(emailTab, 600));
	} catch (...) {
		std::cerr << "ERROR: " << getStringForCaught() << std::endl;
		exit(EXIT_FAILURE);
	}

	return 0;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
