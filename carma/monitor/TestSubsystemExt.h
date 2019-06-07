#ifndef CARMA_MONITOR_TESTSUBSYSTEMEXT_H
#define CARMA_MONITOR_TESTSUBSYSTEMEXT_H

/**
 *
 * @file
 *
 * Semi hand-forged extentions to the auto-generated classes for the
 * Test subsystem. This file was originally created by
 * mpml2cpp-python, then modified manually by the author.
 *
 * @author: Ira W. Snyder
 *
 * $CarmaCopyright$
 *
 */

#include <carma/monitor/TestSubsystem.h>

namespace carma {
namespace monitor {

namespace CM = carma::monitor;

/**
 * The container for the Digitizer logical collection
 */
class TestSubsystem::Digitizer : public DigitizerBase {
public:

	/**
	 * Constructor
	 *
	 * This is an indexed device in which an array of 4
	 * instances will be created, each with an index in the range [1, 4]
	 * associated with it.
	 *
	 * @param digitizerNo the index to associate with this instance
	 * @param name the instance name of the container (for use by references)
	 */
	Digitizer(ushort digitizerNo, const std::string &name="Digitizer");

	/**
	 * Destructor
	 */
	virtual ~Digitizer();

	// add new or overriding method declarations here

};

} // namespace monitor
} // namespace carma

#endif // CARMA_MONITOR_TESTSUBSYSTEMEXT_H
