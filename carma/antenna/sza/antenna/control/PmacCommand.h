#ifndef PMACCOMMAND_H
#define PMACCOMMAND_H

/**
 * @file PmacCommand.h
 * 
 * Tagged: Thu Nov 13 16:53:43 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <string>

// Required C header files from the array control code

#include "carma/szaarrayutils/regmap.h" // RegAddrMode

// The hard limit -- pmac can't process more than this in a single
// message

#define PMAC_DATA_MAX_LEN 1400

namespace sza {
  namespace antenna {
    namespace control {
      
      
      class PmacComms;
      
      class PmacCommand {
      public:
	
	/**
	 * Enumerate the request type, as specified by Delta Tau
	 */
	enum RequestType {
	  UPLOAD   = 0xC0, // An input wrt the pmac
	  DOWNLOAD = 0x40  // An output command wrt to the pmac
	};
	
	/**
	 * Enumerate supported commands
	 */
	enum Request {
	  INVALID          = 0x0,  // Default which will be used to
	  // indicate that a command has not
	  // been initialized
	  CTRL_RESPONSE    = 0xC4,
	  FWDOWNLOAD       = 0xCB,
	  IPADDRESS        = 0xE0,
	  PMAC_FLUSH       = 0xB3,
	  PMAC_GETBUFFER   = 0xC5,
	  PMAC_GETLINE     = 0xB1,
	  PMAC_GETMEM      = 0xB4,
	  PMAC_GETRESPONSE = 0xBF,
	  PMAC_PORT        = 0xBE,
	  PMAC_READREADY   = 0xC2,
	  PMAC_SENDLINE    = 0xB0,
	  PMAC_SETMEM      = 0xB5,
	  PMAC_SETBIT      = 0xBA,
	  PMAC_SETBITS     = 0xBB,
	  PMAC_WRITEBUFFER = 0xC6,
	  PMAC_WRITEERROR  = 0xC7,
	  PMAC_IPADDRESS   = 0xE0
	};
	
	struct EthCmdInfo {
	  unsigned char  requestType_;
	  unsigned char  request_;
	  unsigned short wValue_;
	  unsigned short wIndex_;
	  unsigned short wLength_;
	};
	
	struct EthCmd {
	  EthCmdInfo info_;
	  unsigned char sendData_[PMAC_DATA_MAX_LEN];
	};
	
	/**
	 * Constructor.
	 */
	PmacCommand();
	
	/**
	 * Destructor.
	 */
	~PmacCommand();
	
	/**
	 * Obtain the response after sending a control character.
	 */
	void packCtrlResponseCmd();
	
	/**
	 * Permits writing to the PMAC host port for firmware download.
	 */
	void packFwDownLoadCmd();
	
	/**
	 * Query the IP address.
	 */
	void packPmacGetIpAddressCmd();
	
	/**
	 * Set the IP address.
	 */
	void packPmacSetIpAddressCmd(unsigned int address);
	
	/**
	 * Generic IP address command
	 */
	void packPmacIpAddressCmd(bool setIp=false, unsigned int address=0x0);

	/**
	 * Causes a ^X to be sent to the PMAC.
	 */
	void packPmacFlushCmd();
	
	/**
	 * Cause the PMAC to return any std::string that may reside in the
	 * PMAC.
	 */
	void packPmacGetBufferCmd();
	
	/**
	 * Return any std::string that may be residing in the PMAC.
	 */
	void packPmacGetLineCmd();
	
	/**
	 * Read from DPRAM shared memory.
	 */
	void packPmacGetMemCmd(unsigned short offset, 
			       unsigned short length);
	
	/**
	 * Send a std::string to the pmac, causing it to return any
	 * available striungs that may be residing in the PMAC.
	 */
	void packPmacGetResponseCmd(std::string outString);
	
	/**
	 * Send a single character or control character to the pmac
	 */
	void packPmacPortCmd(unsigned char port);
	
	/**
	 * Query the port.
	 */
	void packPmacPortCmd();
	
	/**
	 * Determine if there is data ready to be read.
	 */
	void packPmacReadReadyCmd();
	
	/**
	 * Send a single character or control character to the pmac
	 */
	void packPmacSendCtrlCharCmd(char outch);
	
	/**
	 * Send a std::string to the PMAC
	 */
	void packPmacSendLineCmd(std::string line);
	
	/**
	 * Set/clear a bit in a 32-bit word.
	 */
	void packPmacSetBitCmd(unsigned short bitNo, bool on);
	
	/**
	 * Set bits in a 32-bit word to a new value.
	 */
	void packPmacSetBitsCmd(unsigned int mask);
	
	/**
	 * Write an array of bytes to DPRAM shared memory.
	 */
	void packPmacSetMemCmd(unsigned short offset, 
			       unsigned short length,
			       unsigned char* data);
	
	/**
	 * Write multiple lines to the PMAC with one packet.
	 */
	void packPmacWriteBufferCmd(unsigned int* data, unsigned int len);
	
	//------------------------------------------------------------
	// Derived command types.  These are built on top of the
	// primitive command types supported by the pmac interface.
	//------------------------------------------------------------
	
	/**
	 * Write an array of different size types to DPRAM shared memory.
	 */
	void packWriteRegCmd(RegMapBlock* blk, 
			     unsigned int first, unsigned int nreg, 
			     unsigned int* value);
	
	/**
	 * Read an array of different size types from DPRAM shared memory.
	 */
	void packReadRegCmd(RegMapBlock* blk, 
			    unsigned int first, unsigned int nreg);
	
	//------------------------------------------------------------
	// Methods to do with reading responses back from the pmac.
	//------------------------------------------------------------
	
	/**
	 * Return a pointer to the internal data buffer.  We make this
	 * public so that others can access data read from the pmac.
	 */
	void* readData();
	
	/**
	 * Return the number of bytes read.
	 */
	size_t responseLength();
	
	/**
	 * Read register values from the pmac into an externally
	 * supplied buffer.
	 */
	void readRegResponse(RegMapBlock *blk, 
				      unsigned int first, unsigned int nreg,
				      unsigned int* response);

	/**
	 * Read register values from the pmac return a pointer to an
	 * internal buffer.
	 */
	unsigned int* readRegResponse(RegMapBlock *blk, 
				      unsigned int first, unsigned int nreg);
	
      private:
	
	friend class PmacComms;
	
	EthCmd cmd_;
	
	/**
	 * The size of the command to send.
	 * Can be less than sizeof(EthCmd)
	 */
	unsigned short cmdSize_; 
	
	/**
	 * True if we are expecting a response to a command
	 */
	bool expectsResponse_;    
	
	/**
	 * If we are expecting a response, how many bytes should be
	 * received?
	 */
	unsigned short responseLength_; 
	
	/**
	 * A buffer into which we can read data returned by the pmac.
	 */
	unsigned char readData_[PMAC_DATA_MAX_LEN];

	/**
	 * A pointer to this buffer may be handed back via calls to
	 * unsigned int* readRegResponse(), above
	 */
	unsigned char tmpBuffer_[PMAC_DATA_MAX_LEN];
	
	//------------------------------------------------------------
	// Methods for accessing private members.
	//------------------------------------------------------------
	
	/**
	 * Return the message type.
	 */
	Request request();
	
	/**
	 * A void pointer to the internal command buffer.
	 */
	const void* cmd();
	
	/**
	 * The size of the command to send.
	 */
	size_t size();
	
	/**
	 * Return a pointer to the internal send data buffer, suitable
	 * for passing to write(2).
	 */
	const void* sendData();
	
      }; // End class PmacCommand
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
