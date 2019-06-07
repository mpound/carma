#ifndef CANMASTER_H
#define CANMASTER_H

#include "carma/canbus/Master.h"

#define CAN_MODBUS_NO  0
#define CAN_MODSLOT_NO 0

/**
 * @file CanMaster.h
 * 
 * Started: Fri Nov 21 16:20:41 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace canbus {
      
      class CanDevice;
      
      class CanMaster : public carma::canbus::Master {
      public:
	
	/**
	 * Default constructor for emulation.  Use this constructor
	 * when there is not yet any Janz hardware on the target
	 * system.  The underlying Dio and CanIo classes both
	 * write to /dev/null when in emulation mode.
	 *
	 * @throws carma::util::ErrorException upon failure of any
	 * derived class constructors.  
	 *
	 * @see carma::canbus::Device::simulateMsg
	 */
	CanMaster();
	
	/**
	 * This constructor takes a modBusNo as an input.  This
	 * number is a hardwired id used to identify the Janz
	 * Modulbus carrier card on compact PCI bus.  It is located
	 * on a ROM chip on the Modulbus carrier board itself and is
	 * labeled with a clearly visible sticker "Mbusx".
	 *
	 * @param modulbusNo module bus number of Janz Modulbus
	 * carrier card.
	 *
	 * @throws carma::canbus::BadParameterException if modulbus
	 * number is invalid.
	 * 
	 * @throws carma::util::ErrorException upon failure of any
	 * derived class constructors.
	 */	  	 	
	CanMaster(int modBusNo);
	
	/**
	 * Constructor for single device.
	 * 
	 * This Master constructor takes a modBusNo and modBusSlot
	 * as an input.  It is intended for use with only a single
	 * CAN bus on the multi bus Carma CAN Dio card.  The
	 * modBusSlot must be either a 0 or 1 and represents the CAN
	 * bus to use.  The modBusNo number is a hardwired id used
	 * to identify the Janz Modulbus carrier card on compact PCI
	 * bus.  It is located on either a hex switch or a rom chip
	 * on the Janz Modulbus cPCI carrier board.  @param
	 * modulbusNo module bus number of Janz Modbulbus carrier
	 * card.  
	 * 
	 * @param modulbusSlot number of CAN card to use (0 or
	 * 1). 
	 * 
	 * @throws carma::canbus::BadParameterException if modulbus
	 * number is invalid.  
	 *
	 * @throws carma::util::ErrorException upon failure of any
	 * derived class constructors.
	 */
	CanMaster(int modBusNo, int modSlotNo);
	
	/**
	 * Public method to add a device to our network.
	 */
	void addCanDevice(CanDevice* device);
	
	/**
	 * Public method to remove a device from our network.
	 */
	void removeCanDevice(CanDevice* device);
	
	/**
	 * Overwrite carma::canbus::Master::run()
	 */
	void run();
	
	/**
	 * Issue a hardware (Dio card) reset.  Resets all modules on
	 * the canbus
	 */
	void issueHardwareReset();

	void runReadThread();

	static const carma::canbus::nodeType DONGLELESS_NODE;

      private:
	
	/**
	 * Get controls.
	 * Master controls correspond to CAN bus control commands
	 * that are accepted by all modules regardless of API 
	 * implementation or functionality.  They consist of 
	 * basic commands such as setTime, Software reset, 
	 * Hardware reset...  This routine, although virtual
	 * has a default implementation that should be called
	 * in any overridden version and prepended to any other
	 * user defined controls.
	 * @return map<msgType, std::string> map containing the message
	 * id as a key (msgType) and a std::string describing the control.
	 */
	std::map<carma::canbus::msgType, std::string> getControls() const;
	
	/**
	 * Update the status of the master.
	 * This routine is intended to be overloaded by a user
	 * implementing a master subclass.  It is included to allow 
	 * the user to retrieve information from the bus(ses) and 
	 * the Master via getBusStatus() and other Master get 
	 * methods and then place the information into the monitor 
	 * stream which will be implementation defined.  It is 
	 * automatically called once every half second frame.  
	 * If a user doesn't wish to place any bus specific or master 
	 * status info into the monitor stream, this routine must be 
	 * no-oped.
	 * @see CanIo::getBusStatus
	 */
	void updateStatus();
	
      }; // End class CanMaster
      
    }; // End namespace canbus
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 


