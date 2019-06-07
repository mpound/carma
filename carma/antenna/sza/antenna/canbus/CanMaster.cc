#include "carma/antenna/sza/antenna/canbus/CanMaster.h"
#include "carma/antenna/sza/antenna/canbus/CanDevice.h"
#include "carma/szautil/Debug.h"

#include "carma/canbus/exceptions.h"
#include "carma/canbus/Types.h"
#include "carma/canbus/Utilities.h"

#include <signal.h>

using namespace sza::antenna::canbus;
using namespace sza::util;
using namespace carma::canbus;
using namespace std;

const nodeType CanMaster::DONGLELESS_NODE = 511;

/**.......................................................................
 * Use this constructor to simulate the CAN bus.
 *
 * All constructors simply call carma::canbus::Master's run method.
 */
CanMaster::CanMaster() : Master() 
{
  DBPRINT(true, Debug::DEBUG11, "Inside simulation constructor");
}

/**.......................................................................
 * Constructor with modbus number.
 *
 * All constructors simply call carma::canbus::Master's run method.
 */
CanMaster::CanMaster(int modBusNo) : Master(modBusNo) {}

/**.......................................................................
 * Constructor with modbus number and slot number.
 *
 * All constructors simply call carma::canbus::Master's run method.
 */
CanMaster::CanMaster(int modBusNo, int modSlotNo) : 
  Master(modBusNo, modSlotNo, true) {}

/**.......................................................................
 * Overload the virtual function in the base class so we can
 * isntantiate this class
 */
std::map<msgType, std::string> CanMaster::getControls() const 
{ 
  std::map<msgType, std::string> tmp; 

  return tmp; 
}

/**.......................................................................
 * Overload the virtual function in the base class so we can
 * instantiate this class.
 */
void CanMaster::updateStatus() 
{
    // No-op for now.
}

/**.......................................................................
 * Public method to add a device to our network.
 */
void CanMaster::addCanDevice(CanDevice* device)
{
  DBPRINT(true, Debug::DEBUG11, "Adding device");
  addDevice(device);
}

/**.......................................................................
 * Public method to remove a device from our network.
 */
void CanMaster::removeCanDevice(CanDevice* device)
{
  //  removeDevice(device->getKey());
  removeDevice(device->getApi(), device->getNode());
}

/**.......................................................................
 * Overwrite carma::canbus::Master::run()
 */
void CanMaster::run()
{
  // Stop the reset that may have been initiated by the Dio
  // constructor

  // EML commenting out reset() on startup 29 Oct 2013 since this
  // seems to screw up the canbus now on certain antennas (C21) and
  // the only fix is to power cycle the canmodules
  //
  //  reset();

  // And run the base class.

  carma::canbus::Master::run();
}

/**.......................................................................
 * Issue a hardware (Dio card) reset.  Resets all modules on
 * the canbus
 */
void CanMaster::issueHardwareReset()
{
  reset();
}

void CanMaster::runReadThread() 
{
  // Try blocking all signals

  sigset_t allSignals;
  sigfillset(&allSignals);
  pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

  Message msg;
  idType id;
  std::vector<byteType> data;
  busIdType busId;
  bool host;
  bool mode;    // Address mode 0 api/node, 1 bt/sn
  apiType api;
  nodeType node;
  keyType key;  // Key derived from CAN id.
  msgType mid;
  std::map<keyType, Device*>::iterator i;
  double rxTime; 		  // Receive time for a message.
  bool sim;             // Is message simulated?
  int oldState;
  
  while (true) { // Run forever. 
    
    // Block for message - this is the only cancellation point.
    msg = getMessage();
    
    if (pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &oldState) != 0)
      throw CARMA_EXCEPTION(carma::canbus::PthreadFailException, 
			    "Master::runTimerThread() - Error setting cancel state.");
    
    // If we got here, we have a message - lock the device mutex 
    // while we process it.
    pthread_mutex_lock(&deviceMutex_);	

    // Extract the canId, data, and busId from message.
    id = msg.getId();
    data = msg.getData();
    busId = msg.getBusId();
    rxTime = msg.getRxMjd(); 
    sim = msg.isSimulated();
    
    // Extract carma message id parameters from canId
    host = isToHost(id);
    mode = getMode(id);
    fromId(api, node, mid, id);
    
    key = Device::createKey(api, node); 
    
    if (node == DONGLELESS_NODE) {

      // Address contains the reserved node id.  Don't process.
      nDonglelessPackets_++;
      
      //        } else if (host && (mode == APPLICATION)) { 
      
      // The above line prevents me from getting blanking frame
      // packets from the PAM modules -- EML
    } else if (host) {
      
      // Message is addressed to the host and the addressing
      // mode is application (non-engineering) - process this message.
      
      // Find the device in the device map.
      i = devices_.find(key);
      
      //	   cout << "Got a device index for key" << key << endl;
      
      // Did we find it?
      if (i != devices_.end()) {
	
	// Ok, process the message.
	Device* dev = i->second;
	// Set the last Rx Time of device to msg RxMjd.
	dev->setLastRxTime(rxTime);
	
	//	       cout << "Found a device: " << dev->getApi() << endl;
	
	// Set the busId if it has changed
	if (dev->getBusId() != busId) {
	  dev->setBusId(busId);
	}
	
	// Try to process the message.
	// If an exception is thrown while processing the message
	// the device is unstable and can't be trusted.  Remove
	// the device from the devices_ map and continue.
	try {
	  dev->processMsg(mid, data, sim);
	} catch (carma::util::ErrorException &err) {
	  // First log the error.
	  std::ostringstream os;
	  os << err.what()
	     << "Caught in Master::runReadThread() while processing "
	     << "msg 0x" << hex << mid << dec 
	     << " from api " << dev->getApi() << " node " 
	     << dev->getNode() << endl 
	     << "Removing device and proceeding." << std::endl << std::ends;
	  carma::util::ErrorException newErr(os, __FILE__, __LINE__);
	  newErr.report();
	  newErr.log(log4cpp::Priority::ERROR);
	  
	  // Remove the device. (Don't call removeDevice as it too
	  // locks the device mutex resulting in deadlock.
	  devices_.erase(i);
	} catch (...) {
	  // First log the error.
	  ostringstream os;
	  os << "Master::runReadThread() - Unknown exception caught"
	     << " while processing msg 0x" << hex << mid << dec 
	     << " from api " << dev->getApi() << " node " 
	     << dev->getNode() << ": " << endl 
	     << "Removing device and proceeding." << endl << ends;
	  carma::util::ErrorException err(os, __FILE__, __LINE__);
	  err.report();
	  err.log(log4cpp::Priority::ERROR);
	  
	  // Remove the device. (Don't call removeDevice as it too
	  // locks the device mutex resulting in deadlock.
	  devices_.erase(i);
	}
	
      } else {
	// Device not found.
	nUnknownPackets_++;
      } // If device exists.
    } else {
      // Message was not addressed to the host or is an
      // engineering message - don't process.
      nUnknownPackets_++;
    }

    pthread_mutex_unlock(&deviceMutex_);
    if (pthread_setcancelstate(oldState, NULL) != 0)
      throw CARMA_EXCEPTION(carma::canbus::PthreadFailException, 
			    "Master::runTimerThread() - Error setting cancel state.");

  }  // End loop forever
}
