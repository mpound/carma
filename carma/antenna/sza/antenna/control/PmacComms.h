#ifndef PMACCOMMS_H
#define PMACCOMMS_H

/**
 * @file PmacComms.h
 * 
 * Tagged: Thu Nov 13 16:53:44 UTC 2003
 * 
 * @author Erik Leitch
 */
// Required C header files from the array control code

#include "carma/szaarrayutils/regmap.h" // RegMapBlock

#include "carma/antenna/sza/antenna/control/PmacCommand.h"

#define PMAC_HOST_ADDR "192.6.94.5"
#define PMAC_HOST_PORT 1025

/**
 * I have measured PMAC latency to be about 60 ms, or 60000 usec.
 * Quite slow, unfortunately.  We'll set the timeout to something
 * significantly larger than this.
 */
#define PMAC_TIMEOUT_USEC 600000

namespace sza {
  namespace antenna {
    namespace control {

      class SzaShare;
      
      class PmacComms {
	
      public:
	
	/**
	 * Constructor with pointer to shared resources.
	 */
	PmacComms(SzaShare* share);
	
	/**
	 * Constructor.  
	 */
	PmacComms();
	
	/**
	 * Destructor.
	 */
	~PmacComms();
	
	/**
	 * Connect to the pmac.  
	 *
	 * Returns true on success.
	 */
	bool connect();
	bool connect(std::string host);
	
	/**
	 * Disconnect from the pmac.  
	 */
	void disconnect();
	
	/**
	 * Return true if the pmac is connected.
	 */
	bool pmacIsConnected();
	
	/**
	 * Send a single command to the PMAC.
	 */
	void sendCommand(PmacCommand& command);
	
	/**
	 * Read a response from the PMAC.  Does not check that a
	 * response is available, and may block waiting for one,
	 * depending on how the socket was configured.
	 */
	void readResponse(PmacCommand& command);
	
	//------------------------------------------------------------
	// Methods which will be used to read and write register values.
	// These methods will send a the appropriate command to the pmac,
	// and time out waiting for a response.
	//------------------------------------------------------------
	
	/**
	 * Method to read the value of a named register in the pmac
	 * memory space.
	 */
	void readReg(RegMapBlock *blk, 
		     unsigned int first, unsigned int nreg, 
		     unsigned int *value);
	
	/**
	 * Method to write to a named register in the pmac.
	 */
	void writeReg(RegMapBlock *blk, 
		      unsigned int first, unsigned int nreg, 
		      unsigned int *value);
	
	/**
	 * Method to read a chunk of memory out of the pmac DPRAM.
	 */
	void* getMem(unsigned short offset, unsigned short length);

	/**
	 * Write a block of memory to the pmac dpram.
	 */
	void* setMem(unsigned short offset, unsigned short length,
		     unsigned char* data);
	
	/**
	 * Get the current IP address of the PMAC
	 */
	std::string getIpAddress();

	/**
	 * Set the IP address of the PMAC
	 */
	void setIpAddress(std::string ipAddress);

	unsigned int ipStringToInt(std::string addr);
	std::string  ipIntToString(unsigned int);

      private:
	
	/**
	 * Pointer to the shared resources of the antenna control system.
	 */
	SzaShare* share_;
	
	/**
	 * The file descriptor associated with the PMAC.
	 */
	int fd_;
	
	/**
	 * The set of file descriptors to be watched for readbility.
	 */
	fd_set read_fds_;
	
	/**
	 * The max fd in read_fds_, plus 1.
	 */
	int fd_set_size_;
	
	/**
	 * True when we have an ethernet connection to the PMAC.
	 */
	bool connected_;
	
	/**
	 * A private message container used for sending and receiving
	 * data from the PMAC.
	 */
	PmacCommand command_;
	
	/**
	 * True when we are expecting a response to a command.
	 */
	bool responsePending_;
	
	/**
	 * Service our message queue.
	 */
	void serviceMsgQ();
	
	/**
	 * Private method to zero the set of descriptors to be watched
	 * for readability.
	 */
	void zeroReadFds();
	
	/**
	 * Private method to register a file descriptor to be
	 * watched for input.
	 */
	void registerReadFd(int fd);
	
	/**
	 * Block, waiting for input from the pmac or a message on our
	 * message queue, or for a timeout to occur, if relevant.
	 */
	int waitForNextMessage();
	
	/**
	 * Block, waiting for input from the pmac.
	 */
	int waitForResponse();
	
	/**
	 * Private method to send a command to the pmac, and return
	 * its response.
	 *
	 * NB: This method can return NULL if the pmac was not
	 * connected.
	 */
	void* getResponse(PmacCommand& command);
	
      }; // End class PmacComms
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
