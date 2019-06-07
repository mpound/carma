#include <netinet/in.h> // Needed for htons()

#include "carma/antenna/sza/antenna/control/PmacCommand.h"

#include "carma/szautil/DataArray.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor initializes request type to invalid.
 */
PmacCommand::PmacCommand()
{
  cmd_.info_.request_ = INVALID;
}

/**.......................................................................
 * Constructor initializes request type to invalid.
 */
PmacCommand::~PmacCommand() {};

/**
 * Obtain the response after sending a control character.
 */
void PmacCommand::packCtrlResponseCmd() {};

/**
 * Permits writing to the PMAC host port for firmware download.
 */
void PmacCommand::packFwDownLoadCmd() {};

/**.......................................................................
 * Query the IP address.
 */
void PmacCommand::packPmacGetIpAddressCmd() 
{
  packPmacIpAddressCmd(false);
}

/**.......................................................................
 * Set the IP address.
 */
void PmacCommand::packPmacSetIpAddressCmd(unsigned int address) 
{
  packPmacIpAddressCmd(true, address);
}

/**.......................................................................
 * Set or query the IP address
 */
void PmacCommand::packPmacIpAddressCmd(bool setIp, unsigned int address) 
{
  unsigned short length = 4;

  cmd_.info_.requestType_ = setIp ? UPLOAD : DOWNLOAD;
  cmd_.info_.request_     = PMAC_IPADDRESS;
  cmd_.info_.wValue_      = 0;
  cmd_.info_.wIndex_      = 0;
  cmd_.info_.wLength_     = htons(length);

  // Convert the address to network-byte order

  address = htonl(address);

  // And insert it into the data array

  unsigned char* data = (unsigned char*)&address;
  for(unsigned idata=0; idata < length; idata++)
    cmd_.sendData_[idata] = data[idata];

  // Always set the length of this command to length, even if we don't
  // set the IP address

  cmdSize_ = sizeof(EthCmdInfo) + length;

  expectsResponse_ = true;
  responseLength_  = 4;
}

/**.......................................................................
 * Causes a ^X to be sent to the PMAC.
 */
void PmacCommand::packPmacFlushCmd() {};

/**.......................................................................
 * Cause the PMAC to return any string that may reside in the
 * PMAC.
 */
void PmacCommand::packPmacGetBufferCmd() {};

/**.......................................................................
 * Return any string that may be residing in the PMAC.
 */
void PmacCommand::packPmacGetLineCmd() {};

/**.......................................................................
 * Read from DPRAM shared memory.
 */
void PmacCommand::packPmacGetMemCmd(unsigned short offset, 
				    unsigned short length)
{
  if(length > PMAC_DATA_MAX_LEN)
    throw Error("PmacCommand::packGetMemCmd: "
		"Requested too much data for a single read.\n");

  cmd_.info_.requestType_ = UPLOAD;
  cmd_.info_.request_     = PMAC_GETMEM;
  cmd_.info_.wValue_      = htons(offset);
  cmd_.info_.wIndex_      = 0;
  cmd_.info_.wLength_     = htons(length);

  cmdSize_ = sizeof(EthCmdInfo);

  expectsResponse_ = true;
  responseLength_  = length;
}

/**.......................................................................
 * Write an array of unsigned longs to DPRAM shared memory.
 *
 * According to the specification, up to 1400 bytes may be written in
 * a single packet.  The wValue field contains the byte offset to
 * write the data to, while the wLength parameter indicates how many
 * bytes to write.  After sending the command, we must wait to receive
 * 1 byte, whose value is irrelevant, but is just to indicate that the
 * command was received.
 */
void PmacCommand::packReadRegCmd(RegMapBlock *blk, 
				 unsigned int first, unsigned int nreg)
{
  // Check that the requested operation won't exceed the PMAC command
  // buffer size.

  unsigned short byteLength = DataArray::byteLength(blk->addr_mode_, nreg);

  if(byteLength > PMAC_DATA_MAX_LEN)
    throw Error("PmacCommand::packReadRegCmd: "
		"Requested too much data for a single read.\n");

  // Throw an error if this register is not a readable PMAC register.

  if(!(blk->flags_ & REG_DPRAM && blk->flags_ & REG_R)) {
    ostringstream os;
    os << "PmacCommand::packReadRegCmd: "
       <<"Register " << blk->name_
       << " is not a readable pmac register." << endl << ends;
    throw Error(os.str());
  }

  // Set up for a GETMEM command

  cmd_.info_.requestType_ = UPLOAD;
  cmd_.info_.request_     = PMAC_GETMEM;
  cmd_.info_.wValue_      = htons((unsigned short)blk->location_);
  cmd_.info_.wIndex_      = 0;
  cmd_.info_.wLength_     = htons(byteLength);

  cmdSize_ = sizeof(EthCmdInfo);

  expectsResponse_ = true;
  responseLength_  = byteLength;
}

/**.......................................................................
 * Unpack a response to a readRegister command from the pmac, with no
 * externally supplied buffer.
 */
unsigned int* PmacCommand::readRegResponse(RegMapBlock *blk, 
					    unsigned int first, 
					    unsigned int nreg)
{
  // And unpack the data into a temporary buffer, using the
  // addressing mode appropriate to this register.

  DataArray::unpack(blk->addr_mode_, blk->flags_, 
		    (unsigned int*)(tmpBuffer_), 
		    (unsigned int*)(readData_),
		    first, nreg);

  return (unsigned int*)(tmpBuffer_);
}

/**.......................................................................
 * Unpack a response to a readRegister command from the pmac using an
 * externally supplied buffer.
 */
void PmacCommand::readRegResponse(RegMapBlock *blk, 
					   unsigned int first, 
					   unsigned int nreg,
					   unsigned int* response)
{
  // And unpack the data into a temporary buffer, using the
  // addressing mode appropriate to this register.

  DataArray::unpack(blk->addr_mode_, blk->flags_, 
		    response, 
		    (unsigned int*)(readData_),
		    first, nreg);
}

/**.......................................................................
 * Send a string to the pmac, causing it to return any
 * available striungs that may be residing in the PMAC.
 */
void PmacCommand::packPmacGetResponseCmd(string outString) {};

/**.......................................................................
 * Send a single character or control character to the pmac
 */
void PmacCommand::packPmacPortCmd(unsigned char port) {};

/**.......................................................................
 * Query the port.
 */
void PmacCommand::packPmacPortCmd() {};

/**.......................................................................
 * Determine if there is data ready to be read.
 */
void PmacCommand::packPmacReadReadyCmd() {};

/**.......................................................................
 * Send a single character or control character to the pmac
 */
void PmacCommand::packPmacSendCtrlCharCmd(char outch) {};

/**.......................................................................
 * Send a string to the PMAC
 */
void PmacCommand::packPmacSendLineCmd(string line) {};

/**.......................................................................
 * Set/clear a bit in a 32-bit word.
 */
void PmacCommand::packPmacSetBitCmd(unsigned short bitNo, bool on) {};

/**.......................................................................
 * Set bits in a 32-bit word to a new value.
 */
void PmacCommand::packPmacSetBitsCmd(unsigned int mask) {};

/**.......................................................................
 * Write an array of bytes to DPRAM shared memory.  
 *
 * According to the specification, up to 1400 bytes may be written in
 * a single packet.  The wValue field contains the byte offset to
 * write the data to, while the wLength parameter indicates how many
 * bytes to write.  After sending the command, we must wait to receive
 * 1 byte, whose value is irrelevant, but is just to indicate that the
 * command was received.
 */
void PmacCommand::packPmacSetMemCmd(unsigned short offset, 
				    unsigned short length,
				    unsigned char* data)
{
  if(length > PMAC_DATA_MAX_LEN)
    throw Error("PmacCommand::packSetMemCmd: Data array is too long.\n");

  cmd_.info_.requestType_ = UPLOAD;
  cmd_.info_.request_     = PMAC_SETMEM;
  cmd_.info_.wValue_      = htons(offset);
  cmd_.info_.wIndex_      = 0;
  cmd_.info_.wLength_     = htons(length);

  for(unsigned idata=0; idata < length; idata++)
    cmd_.sendData_[idata] = data[idata];

  cmdSize_ = sizeof(EthCmdInfo) + length;

  expectsResponse_ = true;
  responseLength_  = 1;
}

/**.......................................................................
 * Write an array of unsigned longs to DPRAM shared memory.
 *
 * According to the specification, up to 1400 bytes may be written in
 * a single packet.  The wValue field contains the byte offset to
 * write the data to, while the wLength parameter indicates how many
 * bytes to write.  After sending the command, we must wait to receive
 * 1 byte, whose value is irrelevant, but is just to indicate that the
 * command was received.
 */
void PmacCommand::packWriteRegCmd(RegMapBlock *blk, 
				  unsigned int first, unsigned int nreg, 
				  unsigned int *value)
{
  // Check that the requested operation won't exceed the PMAC command
  // buffer size.

  unsigned short byteLength = DataArray::byteLength(blk->addr_mode_, nreg);

  if(byteLength > PMAC_DATA_MAX_LEN)
    throw Error("PmacCommand::packWriteRegCmd: "
		"Data array is too long.\n");

  // Throw an error if this register is not a writeable register.

  if(!((blk->flags_ & REG_DPRAM) && (blk->flags_ & REG_W))) {
    ostringstream os;
    os << "PmacCommand::packWriteRegCmd: "
       <<"Register " << blk->name_
       << " is not a writable pmac register." << endl << ends;
    throw Error(os.str());
  }

  // Set up for a SETMEM command

  cmd_.info_.requestType_ = UPLOAD;
  cmd_.info_.request_     = PMAC_SETMEM;
  cmd_.info_.wValue_      = htons((unsigned short)blk->location_);
  cmd_.info_.wIndex_      = 0;
  cmd_.info_.wLength_     = htons(byteLength);

  // And pack the data into our send buffer

  DataArray::pack(blk->addr_mode_, blk->flags_, 
		  (unsigned int*)(cmd_.sendData_), 
		  value,
		  first, nreg);

  cmdSize_ = sizeof(EthCmdInfo) + byteLength;

  expectsResponse_ = true;
  responseLength_  = 1;
}

/**.......................................................................
 * Write multiple lines to the PMAC with one packet.
 */
void PmacCommand::packPmacWriteBufferCmd(unsigned int* data, unsigned int len) 
{};

/**.......................................................................
 * Return the request identifier.
 */
PmacCommand::Request PmacCommand::request()
{
  return static_cast<PmacCommand::Request>(cmd_.info_.request_);
}

/**.......................................................................
 * Return a pointer to our read data buffer as a void* suitable for
 * passing to read(2).
 */
void* PmacCommand::readData()
{
  return static_cast<void*>(readData_);
}

/**.......................................................................
 * Return a pointer to our send data buffer as a const void* suitable for
 * passing to read(2).
 */
const void* PmacCommand::sendData()
{
  return static_cast<void*>(cmd_.sendData_);
}

/**.......................................................................
 * Return our command container a void* suitable for passing to
 * write(2).
 */
const void* PmacCommand::cmd()
{
  return static_cast<const void*>(&cmd_);
}

/**.......................................................................
 * Return our size as a size_t suitable for passing to write(2) or
 * read(2).
 */
size_t PmacCommand::size()
{
  return static_cast<size_t>(cmdSize_);
}

/**.......................................................................
 * Return the response length.
 */
size_t PmacCommand::responseLength()
{
  return static_cast<size_t>(responseLength_);
}
