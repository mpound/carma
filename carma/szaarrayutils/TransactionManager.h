#ifndef SZA_ARRAY_TRANSACTION_H
#define SZA_ARRAY_TRANSACTION_H

/**
 * @file TransactionManager.h
 * 
 * Tagged: Wed Jun 16 22:25:23 UTC 2004
 * 
 * @author Erik Leitch
 */
#include <string>
#include <list>

#include "carma/szaarrayutils/input.h"

namespace sza {
  namespace array {
    
    /**
     * A class for managing transactions
     */
    class TransactionManager {
    public:
      
      static const unsigned DEV_NAME_MAX      = 100;
      static const unsigned SERIAL_NAME_MAX   = 20;
      static const unsigned LOCATION_NAME_MAX = 100;
      static const unsigned WHO_NAME_MAX      = 20;
      static const unsigned PREFIX_LEN        = 5;

      /**
       * Constructor.
       */
      TransactionManager();
      
      /**
       * Destructor.
       */
      virtual ~TransactionManager();
      
      /**
       * Read a transaction catalog
       */
      void readCatalog(std::string dir, std::string file);

      /**
       * Print known devices
       */
      void printDevices();

      /**
       * Print known locations
       */
      void printLocations();

      /**
       * Print known serialNumbers
       */
      void printSerialNumbers();

      /**
       * Print the tree of device->location/serialNumbers
       */
      void printAll();

      //------------------------------------------------------------
      // A location specifier
      //------------------------------------------------------------

      struct Location {
	std::string name_;

	Location(std::string name) : name_(name) {};
      };

      /**
       * A predicate for testing if a Location matches a
       * requested location
       */
      class Location_eq : public std::unary_function<Location, bool> {
	std::string name_;
      public:
	explicit Location_eq(std::string name) : name_(name) {}
	bool operator() (const Location& location) const {
	  return location.name_ == name_;
	}
      };

      //------------------------------------------------------------
      // A serial number specifier
      //------------------------------------------------------------

      struct SerialNumber {
	std::string name_;

	SerialNumber(std::string name) : name_(name) {};
      };

      /**
       * A predicate for testing if a SerialNumber matches a
       * requested sn
       */
      class SerialNumber_eq : public std::unary_function<SerialNumber, bool> {
	std::string name_;
      public:
	explicit SerialNumber_eq(std::string name) : name_(name) {}
	bool operator() (const SerialNumber& sn) const {
	  return sn.name_ == name_;
	}
      };

      //------------------------------------------------------------
      // A device specifier
      //------------------------------------------------------------

      struct Device {
	std::string name_;

	// A list of pointers to valid serial numbers for this device

	std::list<SerialNumber*> serialNumbers_;

	// A list of pointers to valid locations for this device

	std::list<Location*> locations_;

	Device(std::string name) : name_(name) {};
	void addLocation(Location* location);
	void addSerialNumber(SerialNumber* serialNumber);
	void printLocations();
	void printSerialNumbers();
	Location* findLocation(std::string name);
	SerialNumber* findSerialNumber(std::string name);
	bool isValidLocation(std::string name);
	bool isValidSerialNumber(std::string name);
      };

      /**
       * A predicate for testing if a Device matches a
       * requested device
       */
      class Device_eq : public std::unary_function<Device, bool> {
	std::string name_;
      public:
	explicit Device_eq(std::string name) : name_(name) {}
	bool operator() (const Device& device) const {
	  return device.name_ == name_;
	}
      };

      /**
       * Clear the catalog
       */
      void clearCatalog();

    private:

      // The known list of devices

      std::list<Device> devices_;
      std::list<Location> locations_;
      std::list<SerialNumber> serialNumbers_;

      // Read a location specifier from an input stream

      void readLocation(Device* device, InputStream* stream);

      // Read a serialNumber specifier from an input stream

      void readSerialNumber(Device* device, InputStream* stream);

      // Read a device specifier from an input stream

      Device* readDevice(InputStream* stream);

    public:

      // Find a device by name

      Device* findDevice(std::string name);

      // Return true if this is a valid device

      bool isValidDevice(std::string name);

      // Return true if this is a valid serial number/device pair

      bool isValidSerialNumber(std::string device, std::string serial);

      // Return true if this is a valid location/device pair

      bool isValidLocation(std::string device, std::string location);

    private:

      // Find a device by name

      Location* findLocation(std::string name);

      // Find a serial number by name

      SerialNumber* findSerialNumber(std::string name);

    public:

      /*
       * Return non-zero if a character isn't a separator character.
       */
      static int notSeparatorChar(int c);

      // Add a device by name

      Device* addDevice(std::string name);

      // Add a device by name

      Location* addLocation(std::string name);

      // Add a serial number by name

      SerialNumber* addSerialNumber(std::string name);

    }; // End class TransactionManager
    
  } // End namespace array
} // End namespace sza



#endif // End #ifndef SZA_ARRAY_TRANSACTION_H
