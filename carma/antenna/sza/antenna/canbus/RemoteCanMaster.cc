#include "carma/antenna/sza/antenna/canbus/RemoteCanMaster.h"

using namespace carma::canbus;
using namespace sza::antenna::canbus;
using namespace std;

// -----------------------------------------------------------------------------
RemoteCanMaster::RemoteCanMaster(std::string hostname) : remoteIo_(hostname), CanMaster()
{
	// Now setup your CAN network exactly the same as you normally would.
	// e.g. Create and add devices to the CAN.
} 
	
// -----------------------------------------------------------------------------
RemoteCanMaster::~RemoteCanMaster()
{
	// Nothing here.
}

// -----------------------------------------------------------------------------
void RemoteCanMaster::postMessage(
	const carma::canbus::Message& msg,
	carma::canbus::txPriorityType prio)
{
	// Dispatch this to our instance of InetCan and effectively circumvent
	// the normal postMessage routine.
	remoteIo_.postMessage(msg, prio);
}

// -----------------------------------------------------------------------------
carma::canbus::Message RemoteCanMaster::getMessage()
{
	// Same for getMessage.
	return remoteIo_.getMessage();
}

