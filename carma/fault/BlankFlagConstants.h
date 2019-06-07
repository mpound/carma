/*
 * Constants for Blank/Flag bits
 */

#ifndef BLANKFLAGCONSTANTS_H
#define BLANKFLAGCONSTANTS_H

#include <iostream>
#include <iomanip>

namespace carma {
namespace fault {

struct BlankFlagBits {
	enum e {
		NONE				= 0,		// no bits set
		DRIVE				= (1 << 0),	// antenna drive state
		DRIVE_BLANK			= (1 << 1), // if set blank, else flag
		MONITORDATA			= (1 << 2),	// insufficient monitor data
		MONITORDATA_BLANK	= (1 << 3), // if set blank, else flag
		OFFLINE				= (1 << 4),	// antenna subarray differs from correlator subarray
		OFFLINE_BLANK		= (1 << 5), // if set blank, else flag
		PHASELOCK			= (1 << 6),	// antenna LO / YIG state
		PHASELOCK_BLANK		= (1 << 7), // if set blank, else flag
	};
};

};
};

#endif /* BLANKFLAGCONSTANTS_H */

/* vim: set ts=4 sts=4 sw=4 noet tw=92: */
