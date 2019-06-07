/*
 * Control Subsystem Manual Blank/Flag/Birdie support configuration file
 * parser.
 */

#ifndef CARMA_CONTROL_MANUALFLAG_H
#define CARMA_CONTROL_MANUALFLAG_H

#include <carma/monitor/ControlCorrelEnum.h>

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/shared_ptr.hpp>

#include <vector>

namespace carma {
namespace control {

struct ManualFlag
{
	typedef carma::monitor::ManualFlagPreferenceMonitorPointEnum MFPrefMPE;

	// constructor
	ManualFlag();

	boost::posix_time::ptime start;
	boost::posix_time::ptime end;
	int band;
	int input1;
	int input2;
	MFPrefMPE::MANUALFLAGPREFERENCE preference;
};

typedef boost::shared_ptr<ManualFlag> ManualFlagPtr;

std::vector<ManualFlagPtr> parseManualFlagTable(const std::string &filename, const bool currentOnly = true);

} // namespace carma::control
} // namespace carma

#endif /* CARMA_CONTROL_MANUALFLAG_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
