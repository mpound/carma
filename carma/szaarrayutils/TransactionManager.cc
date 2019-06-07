#include "carma/szaarrayutils/TransactionManager.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include <cstring>
#include <algorithm>

using namespace sza::array;
using namespace sza::util;
using namespace std;

const unsigned TransactionManager::DEV_NAME_MAX;
const unsigned TransactionManager::SERIAL_NAME_MAX;
const unsigned TransactionManager::LOCATION_NAME_MAX;
const unsigned TransactionManager::WHO_NAME_MAX;
const unsigned TransactionManager::PREFIX_LEN;

/**.......................................................................
 * Constructor.
 */
TransactionManager::TransactionManager() {}

/**.......................................................................
 * Destructor.
 */
TransactionManager::~TransactionManager() {}

/**.......................................................................
 * Read a location specifier from an input stream, and associate the
 * results with the passed device.
 */
void TransactionManager::readLocation(Device* device, InputStream* stream) 
{
  LogStream errStr;

  // Sanity check

  if(device==0) {
    errStr.appendMessage(true, "A location specifier was encountered "
			 "before a device\n");
    throw Error(errStr);
  }

  // Parse location names until end of line is encountered

  while(stream->nextc != '\n' && stream->nextc != EOF) {

    // Read the name of the location into stream->work.

    if(input_skip_space(stream, 1, 0)) {
      errStr.appendMessage(true, "Error in input_skip_space\n");
      throw Error(errStr);
    }

    // Read the name of the device as a literal string, including
    // whitespace

    if(input_literal(stream, 1, "", "", notSeparatorChar, "")) {
      errStr.appendMessage(true, "Error in input_word\n");
      throw Error(errStr);
    };

    // Advance to the next character.

    if(stream->nextc != '\n' && read_InputStream(stream, 1)) {
      errStr.appendMessage(true, "Error in read_InputStream\n");
      throw Error(errStr);
    };

    // Add the location to the list of known locations, and to the
    // list associated with this device

    Location* location = addLocation(stream->work);
    device->addLocation(location);
    
  };    

  // Skip any trailing whitespace, including newlines
  
  if(input_skip_white(stream, 1, 0)) {
    errStr.appendMessage(true, "Error in input_skip_white\n");
    throw Error(errStr);
  };
}

/**.......................................................................
 * Read a location specifier from an input stream, and associate the
 * results with the passed device.
 */
void TransactionManager::readSerialNumber(Device* device, InputStream* stream) 
{
  LogStream errStr;

  // Sanity check

  if(device==0) {
    errStr.appendMessage(true, "A serialNumber specifier was encountered "
			 "before a device\n");
    throw Error(errStr);
  }

  // Parse serialNumber names until end of line is encountered

  while(stream->nextc != '\n' && stream->nextc != EOF) {

    // Read the name of the serialNumber into stream->work.

    if(input_skip_space(stream, 1, 0)) {
      errStr.appendMessage(true, "Error in input_skip_space\n");
      throw Error(errStr);
    }

    // Read the next literal string
    
    if(input_literal(stream, 1, "", "", notSeparatorChar, "")) {
      errStr.appendMessage(true, "Error in input_literal\n");
      throw Error(errStr);
    };
    
    // Advance to the next character.

    if(stream->nextc != '\n' && read_InputStream(stream, 1)) {
      errStr.appendMessage(true, "Error in read_InputStream\n");
      throw Error(errStr);
    };
    
    // Add the serialNumber to the list of known serialNumbers, and to the
    // list associated with this device

    SerialNumber* serialNumber = addSerialNumber(stream->work);
    device->addSerialNumber(serialNumber);
  };

  // Skip any trailing whitespace, including newlines

  if(input_skip_white(stream, 1, 0)) {
    errStr.appendMessage(true, "Error in input_skip_white\n");
    throw Error(errStr);
  };
}

/**.......................................................................
 * Read a device specifier from an input stream.  This method is
 * called when the DEVICE token is encountered at the head of a line.
 * This must be followed by valid serial number and location
 * declarations.
 */
TransactionManager::Device* TransactionManager::readDevice(InputStream* stream)
{
  LogStream errStr;
  Device* device=NULL;

  // Read the name of the location into stream->work.

  if(input_skip_space(stream, 1, 0)) {
    errStr.appendMessage(true, "Error in input_skip_space\n");
    throw Error(errStr);
  }

  // Read the name of the device as a literal string, including
  // whitespace

  if(input_literal(stream, 1, "", "", notSeparatorChar, "")) {
    errStr.appendMessage(true, "Missing device name\n");
    throw Error(errStr);
  };

  // Add this device to the list of known devices if we don't already
  // know about it

  if((device=findDevice(stream->work)) == 0) 
    device=addDevice(stream->work);
  
  // Skip any trailing whitespace, including newlines

  if(input_skip_white(stream, 1, 0)) {
    errStr.appendMessage(true, "Error in input_skip_white\n");
    throw Error(errStr);
  };
  
  return device;
}

/**.......................................................................
 * Clear the transaction catalog
 */
void TransactionManager::clearCatalog()
{
  devices_.clear();
  locations_.clear();
  serialNumbers_.clear();
}

/**.......................................................................
 * Read a transaction catalog from the input stream
 */
void TransactionManager::readCatalog(std::string dir, std::string file)
{
  LogStream errStr;
  Device* device=NULL;
  InputStream* stream=0;

  // Create an input stream.

  stream = new_InputStream();

  if(stream==0) {
    errStr.appendMessage(true, "Unable to allocate a new input stream\n");
    throw Error(errStr);
  }
  
  // Connect the catalog file to an input stream and read scans from
  // it into the catalog.

  try {
    
    if(open_FileInputStream(stream, (char*)dir.c_str(), 
			    (char*)file.c_str())) {
      errStr.initMessage(true);
      errStr << "Unable to connect input stream to file: "
	     << dir << "/" << file << endl;
      throw Error(errStr);
    }
    
    // Locate the first entry.
    
    if(input_skip_white(stream, 0, 0)) {
      errStr.appendMessage(true, "Error in input_skip_white\n");
      throw Error(errStr);
    }
    
    // Read to the end of the stream or error.
    
    while(stream->nextc != EOF) {
      
      // Read the identifier
      
      if(input_word(stream, 0, 1)) {
	errStr.appendMessage(true, "Error in input_literal\n");
	throw Error(errStr);
      }
      
      // We have three possibilities
      
      if(strcmp(stream->work, "device")==0)
	device = readDevice(stream);
      else if(strcmp(stream->work, "serial")==0)
	readSerialNumber(device, stream);
      else if(strcmp(stream->work, "location")==0)
	readLocation(device, stream);
      else {
	errStr.initMessage(true);
	errStr << "Unrecognized identifier: " 
	       << stream->work << endl;
	throw Error(errStr);
      }
    }
  } catch(const Exception& err) {
    
    // Close the file.
    
    if(stream != 0)
      del_InputStream(stream);

    throw err;
  }

  // Close the file.

  if(stream != 0)
    del_InputStream(stream);
}

/**.......................................................................
 * Find a serial number in the list of known serialNumbers
 */
TransactionManager::SerialNumber* 
TransactionManager::findSerialNumber(std::string name)
{
  // Search for a match in the list of known serial numbers

  list<SerialNumber>::iterator ilist = find_if(serialNumbers_.begin(), 
					       serialNumbers_.end(),
					       SerialNumber_eq(name));
  if(ilist == serialNumbers_.end())
    return 0;
  else
    return &(*ilist);
}

/**.......................................................................
 * Find a location in the list of known locations
 */
TransactionManager::SerialNumber* 
TransactionManager::Device::findSerialNumber(std::string name)
{
  // Search for a match in the list of known serial numbers

  for(std::list<SerialNumber*>::iterator ilist = serialNumbers_.begin();
      ilist != serialNumbers_.end(); ilist++)
    if((*ilist) != 0 && (*ilist)->name_ == name)
      return *ilist;

  return 0;
}

/**.......................................................................
 * Add a serial number to the list of known serialNumbers
 */
TransactionManager::SerialNumber* 
TransactionManager::addSerialNumber(std::string name)
{
  SerialNumber* snPtr = findSerialNumber(name);

  if(snPtr == 0) {
    SerialNumber newSerialNumber(name);
    serialNumbers_.insert(serialNumbers_.begin(), newSerialNumber);
  }

  return findSerialNumber(name);
}

/**.......................................................................
 * Add a serial number to the list of known serial numbers for a device
 */
void 
TransactionManager::Device::addSerialNumber(SerialNumber* serialNumber)
{
  for(std::list<SerialNumber*>::iterator ilist = serialNumbers_.begin();
      ilist != serialNumbers_.end(); ilist++)
    if((*ilist) == serialNumber)
      return;

  // Else insert the serial number pointer

  serialNumbers_.insert(serialNumbers_.begin(), serialNumber);
}

/**.......................................................................
 * Find a location in the list of known locations
 */
TransactionManager::Location* TransactionManager::findLocation(std::string name)
{
  // Search for a match in the list of known locations

  list<Location>::iterator ilist = find_if(locations_.begin(), 
					   locations_.end(),
					   Location_eq(name));
  if(ilist == locations_.end())
    return 0;
  else
    return &(*ilist);
}

/**.......................................................................
 * Find a location in the list of known locations
 */
TransactionManager::Location* 
TransactionManager::Device::findLocation(std::string name)
{
  // Search for a match in the list of known locations

  for(std::list<Location*>::iterator ilist = locations_.begin();
      ilist != locations_.end(); ilist++)
    if((*ilist) != 0 && (*ilist)->name_ == name)
      return *ilist;

  return 0;
}

/**.......................................................................
 * Add a location to the list of known locations
 */
TransactionManager::Location* 
TransactionManager::addLocation(std::string name)
{
  Location* locationPtr = findLocation(name);

  if(locationPtr == 0) {
    Location newLocation(name);
    locations_.insert(locations_.begin(), newLocation);
  }

  return findLocation(name);
}

/**.......................................................................
 * Add a location to the list of known locations for a device
 */
void 
TransactionManager::Device::addLocation(TransactionManager::Location* location)
{
  for(std::list<Location*>::iterator ilist = locations_.begin();
      ilist != locations_.end(); ilist++)
    if(*ilist == location)
      return;
  
  // Else insert the location pointer
  
  locations_.insert(locations_.begin(), location);
}

/**.......................................................................
 * Find a device in the list of known devices
 */
TransactionManager::Device* TransactionManager::findDevice(std::string name)
{
  // Search for a match in the list of known devices

  list<Device>::iterator ilist = find_if(devices_.begin(), 
					 devices_.end(),
					 Device_eq(name));
  if(ilist == devices_.end())
    return 0;
  else
    return &(*ilist);
}

/**.......................................................................
 * Return true if this is a valid device
 */
bool TransactionManager::isValidDevice(std::string name)
{
  return findDevice(name) != 0;
}

/**.......................................................................
 * Return true if this is a valid serial number/device pair
 */
bool TransactionManager::isValidSerialNumber(std::string deviceName, 
					     std::string serialName)
{
  Device* device = findDevice(deviceName);
  if(device != 0)
    return device->isValidSerialNumber(serialName);
  return false;
}

/**.......................................................................
 * Return true if this is a valid location number/device pair
 */
bool TransactionManager::isValidLocation(std::string deviceName, 
					  std::string locationName)
{
  Device* device = findDevice(deviceName);
  if(device != 0)
    return device->isValidLocation(locationName);
  return false;
}

/**.......................................................................
 * Add a device to the list of known devices
 */
TransactionManager::Device* TransactionManager::addDevice(std::string name)
{
  Device* devicePtr = findDevice(name);

  if(devicePtr == 0) {
    Device newDevice(name);
    devices_.insert(devices_.begin(), newDevice);
  }

  // Note that on return from this function, a pointer to newDevice
  // would be invalidated.  The persistent copy resides in devices_,
  // so return its pointer instead

  return findDevice(name);
}

/**.......................................................................
 * Print known devices
 */
void TransactionManager::printDevices()
{
  for(std::list<Device>::iterator ilist = devices_.begin();
      ilist != devices_.end(); ilist++)
    std::cout << ilist->name_ << std::endl;
}

/**.......................................................................
 * Print known devices
 */
void TransactionManager::printAll()
{
  for(std::list<Device>::iterator ilist = devices_.begin();
      ilist != devices_.end(); ilist++) 
  {
    std::cout << ilist->name_ << ":" << std::endl;
    ilist->printSerialNumbers();
    ilist->printLocations();
  }
}

/**.......................................................................
 * Print known locations for a device
 */
void TransactionManager::printLocations()
{
  for(std::list<Location>::iterator ilist = locations_.begin();
      ilist != locations_.end(); ilist++)
    std::cout << ilist->name_ << std::endl;
}

/**.......................................................................
 * Print known locations
 */
void TransactionManager::Device::printLocations()
{
  for(std::list<Location*>::iterator ilist = locations_.begin();
      ilist != locations_.end(); ilist++)
    std::cout << (*ilist)->name_ << std::endl;
}

/**.......................................................................
 * Print known serial numbers
 */
void TransactionManager::printSerialNumbers()
{
  for(std::list<SerialNumber>::iterator ilist = serialNumbers_.begin();
      ilist != serialNumbers_.end(); ilist++)
    std::cout << ilist->name_ << std::endl;
}

/**.......................................................................
 * Print known serial numbers
 */
void TransactionManager::Device::printSerialNumbers()
{
  for(std::list<SerialNumber*>::iterator ilist = serialNumbers_.begin();
      ilist != serialNumbers_.end(); ilist++)
    std::cout << (*ilist)->name_ << std::endl;
}

/**.......................................................................
 * Return true if the passed location is a valid location for a device
 */
bool TransactionManager::Device::isValidLocation(std::string name)
{
  return findLocation(name) != 0;
}

/**.......................................................................
 * Return true if the passed serial number is valid for a device
 */
bool TransactionManager::Device::isValidSerialNumber(std::string name)
{
  return findSerialNumber(name) != 0;
}

/*.......................................................................
 * Return non-zero if a character isn't a separator character.
 */
int TransactionManager::notSeparatorChar(int c)
{
  return c!='\n' && c!=',' && c!=')' && c!='}';
}
