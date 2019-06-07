#ifndef REMOTECANMASTER_H
#define REMOTECANMASTER_H

#include "carma/antenna/sza/antenna/canbus/CanMaster.h"
#include "carma/canbus/InetCan.h"

namespace sza {
namespace antenna {
namespace canbus {

class RemoteCanMaster : public sza::antenna::canbus::CanMaster {
public:

	/**
	 * Constructor.
	 * @param hostname Can-Over-Ip server hostname.
	 */
	RemoteCanMaster(std::string hostname);

	~RemoteCanMaster();

	//  Here's the trick: overload getMessage and postMessage to use
	//  InetCan (CanOverIp client) instead of CanDio but still use 
	//  CanDio in emulation mode!  This allows for the base master to 
	//  still act on CanDio when it needs to while stubbing out the 
	//  actual communication to occur over the network.

	/**
         * Post a message to the canbus.
	 * Note that the message priority will have no effect remotely.
	 */
	void postMessage(
        	const carma::canbus::Message& msg,
        	carma::canbus::txPriorityType prio = carma::canbus::NORMAL);
 
	/**
	 * Get a message.
	 */
	carma::canbus::Message getMessage();

private:

	carma::canbus::InetCan remoteIo_;
}; // End class RemoteCanMaster
}}} // End namespace sza::antenna::canbus
#endif
