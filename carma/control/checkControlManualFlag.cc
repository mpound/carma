//
// @version $Revision: 1.1 $
//
// @usage @autogen
//
// @description
//  utility to check the control subsystem manual blank/flag/birdie
//  configuration file
//
// @key file "control/SpectralLineCorrelator.tab" string
//  The configuration file (CARMA Table format)
//
// @logger MONITOR_FACILITY carma.control.checkControlManualFlag
//

#include <iostream>
#include <iomanip>
#include <cstdio>
#include <string>

#include <boost/foreach.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include <carma/services/Table.h>

#include <carma/util/ExceptionUtils.h>
#include <carma/util/Program.h>
using namespace carma::util;

#include <carma/control/ManualFlag.h>
using namespace carma::control;

typedef carma::monitor::ManualFlagPreferenceMonitorPointEnum MFPrefMPE;

int Program::main ()
{
	try {
		const std::string filename = getConfFile(getStringParameter("file"));

		const std::vector<ManualFlagPtr> vec = parseManualFlagTable(filename, false);
		const boost::posix_time::ptime now = boost::posix_time::second_clock::local_time();

		BOOST_FOREACH(const ManualFlagPtr &mf, vec) {
			std::ostringstream oss;
			oss << mf->start << " "
				<< mf->end << " "
				<< std::setw(3) << mf->band << " "
				<< std::setw(3) << mf->input1 << " "
				<< std::setw(3) << mf->input2 << " "
				<< std::setw(6) << MFPrefMPE::valueToString(mf->preference);

			if (mf->start <= now && mf->end >= now) {
				oss << " --- PRESENT IN RTS";
			} else {
				oss << " --- SKIP IN RTS (MIRIAD ONLY)";
			}

			std::cout << oss.str() << std::endl;
		}
	} catch (...) {
		std::cerr << "ERROR: " << getStringForCaught() << std::endl;
		exit(EXIT_FAILURE);
	}

	return 0;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
