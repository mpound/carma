#ifndef PMACBOARD_H
#define PMACBOARD_H

/**
 * @file PmacBoard.h
 * 
 * Tagged: Thu Nov 13 16:53:43 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <vector>

#include "carma/antenna/sza/antenna/control/AxisPositions.h"
#include "carma/antenna/sza/antenna/control/Board.h"
#include "carma/antenna/sza/antenna/control/Model.h"
#include "carma/antenna/sza/antenna/control/PmacComms.h"
#include "carma/antenna/sza/antenna/control/PmacTarget.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"

#define DPRAM_TIMEOUT 100

#define PMAC_TEST

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * Encapsulate details about the registers of the pmac board.
       */
      class PmacBoard : public Board {
	
      public:
	
	/**
	 * Constructor function for this board
	 *
	 * @throws Exception
	 */
	PmacBoard(SzaShare* share, std::string name, bool simPmac=false);
	
	/**
	 * Destructor function for this board
	 */
	~PmacBoard();
	
	/**
	 * Returns true if the Pmac isn't ready to receive a new command
	 */
	bool isBusyOld(); // Version until 1 Oct 2008

	bool isBusy();    // No-op version for the new tracking mode,
			  // in which we don't synchronize with the
			  // pmac
	
	/**
	 * Return the current value of the PMAC position-fault flag.
	 *
	 * @throws Exception
	 */
	unsigned int readPositionFault();
	
	/**
	 * Tell the PMAC to read a new position
	 *
	 * @throws Exception
	 */
	void commandNewPosition(PmacTarget* pmac);
	
	/**
	 * Read the pmac monitor data to update our view of where the
	 * telescope axes are currently positioned. This function must
	 * not be called until lacking_ & (PTG_ZEROS | PTG_ENCODERS) 
	 * is zero.
	 *
	 * @param axes      AxisPositions *  The telescope position will 
	 *                                   be recorded in a container of 
	 *                                   this type.
	 *
	 * @throws Exception
	 */
	bool readPosition(AxisPositions* axes, Model* model);
	
	/**
	 * Public interface to PmacComms::connect() to connect to the
	 * pmac.
	 */
	bool connect();
	
	/**
	 * Public interface to PmacComms::disconnect() to connect to the
	 * pmac.
	 */
	void disconnect();
	
	/**
	 * Mirror the DPRAM to shared memory.
	 */
	void mirrorDpramToSharedMemory();
	
	/**
	 * Return true if the pmac is connected.
	 */
	bool pmacIsConnected();

	unsigned char driveStatusToBit(unsigned int driveStatus);

      private:
	
	/**
	 * A Boolean flag specifying whether or not we want to
	 * simulate the pmac
	 */
	bool simPmac_;

	/**
	 * An object use to communicate with the pmac.
	 */
	PmacComms* comms_;
	
	/**
	 * This register is set to 1 to signal the pmac to stop
	 * updating its DPRAM.
	 */
	RegMapBlock* hostRead_;    
	
	/**
	 * The pmac sets this to non-zero when it is writing to the
	 * dual-port-ram.
	 */
	RegMapBlock* pmacWrite_;    
	
	/**
	 * We set this flag register to inform the PMAC when we have
	 * placed a new position and mode in the following
	 * registers. The PMAC clears it when it is ready for a new
	 * position.
	 */
	RegMapBlock* newPosition_;  	

	/**
	 * PMAC sets this flag register
	 * when it loses sync and needs
	 * us to resync it
	 */
	RegMapBlock* positionFault_;

	/**
	 * The form of the next operation that the PMAC should do,
	 * from:
	 *
	 *   track - Be at the target az,el,dk
	 *           at the next 1-second tick 
	 *   slew  - Move to the target az,el,dk 
	 *           as fast as possible, then 
	 *           halt. 
	 *   halt  - Stop the telescope. 
	 *   sync  - Wait for a programmed pulse 
	 *           from the time-code-reader 
	 *           then enter track mode and 
	 *           aim to reach the target 
	 *           az,el,dk a second later 
	 */
	RegMapBlock* newMode_;      
	
	/**
	 * The target azimuth (encoder counts)
	 */
	RegMapBlock* newAz_;        
	
	/**
	 * The target elevation (encoder counts)
	 */
	RegMapBlock* newEl_;        
	
	/**
	 * The target deck angle (encoder counts).
	 */
	RegMapBlock* newDk_;        
	
	/**
	 * The target azimuth rate (milli-counts/sec)
	 */
	RegMapBlock* newAzRate_;   
	
	/**
	 * The target elevation rate (milli-counts/sec)
	 */
	RegMapBlock* newElRate_;   
	
	/**
	 * The target deck rate (milli-counts/sec)
	 */
	RegMapBlock* newDkRate_;   
	
	/**
	 * The readback azimuth (encoder counts)
	 */
	RegMapBlock* azPos_;        
	
	/**
	 * The readback elevation (encoder counts)
	 */
	RegMapBlock* elPos_;        
	
	/**
	 * The readback deck angle (encoder counts).
	 */
	RegMapBlock* dkPos_;        
	
	/**
	 * The pmac drive-status arranged as 2 32-bit words of status
	 * bits.
	 */
	RegMapBlock* driveStatus_;  
	RegMapBlock* statusMask_;  
	
	/**
	 * The size in bytes, of the DPRAM
	 */
	unsigned long dpramSize_;
	
	/**
	 * The base address of the first register block in DPRAM.
	 */
	unsigned long dpramBaseAddr_;
	
	/**
	 * The size in bytes, of the command portion of the DPRAM
	 */
	unsigned long dpramCmdSize_;
	
	/**
	 * The base address of the first register block in DPRAM.
	 */
	unsigned long dpramCmdBaseAddr_;
	
	/**
	 * A place to compose blocks of registers which we want to
	 * write to PMAC dpram.
	 */
	std::vector<unsigned char> dpramWork_;

	/**
	 * Calculate the size of the DPRAM
	 */
	void computeDpramStats();
	
	/**
	 * Read the DPRAM from shared memory.
	 */
	unsigned int* readoutDpram();
	
	/**
	 * Method to read a register from the pmac.
	 */
	void readReg(RegMapBlock* blk, unsigned int first, 
		     unsigned int nreg, unsigned int* value);
	
	/**
	 * Method to write a register to the pmac.
	 */
	void writeReg(RegMapBlock* blk, unsigned int first, 
		      unsigned int nreg, unsigned int* value);
	
	/**
	 * Write a register to the work array.
	 */
	void writeRegToWork(RegMapBlock *blk, 
			    unsigned int first, unsigned int nreg, 
			    unsigned int *value);

	/**
	 * Write the work array to dpram.
	 */
	void writeWork();

	/**
	 * method to initialize the pmac
	 */
	void initializePmac();

      }; // End class PmacBoard
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
