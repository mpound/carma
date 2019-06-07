#ifndef SCANNER_H
#define SCANNER_H

/**
 * @file Scanner.h
 * 
 * Tagged: Thu Nov 13 16:53:51 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <list>

#include "carma/szautil/AntNum.h"
#include "carma/szautil/DataFrameManager.h"

#include "carma/szautil/AntennaFrameBuffer.h"

#include "carma/antenna/sza/antenna/control/FrameBoard.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * A class to encapsulate the transmission of monitor frames
       * from the AC to the ACC.
       */
      class Scanner {
	
      public:
	
	/**
	 * Constructor with antenna specification.
	 *
	 * @throws Exception
	 */
	Scanner(SzaShare* share, sza::util::AntNum* ant);
	
	/**
	 * Destructor.
	 */
	~Scanner();
	
	/**
	 * If there is room in the circular frame buffer, record
	 * another data frame.
	 */
	void packNextFrame();

	/**
	 * Method to return the next frame from our ring buffer.
	 */
	sza::util::DataFrameManager* dispatchNextFrame();

	/**
	 * Return the number of data frames waiting to be dispatched.
	 */
	unsigned int getNframesInQueue();

	/**
	 * Return the list of boards controlled by this task.
	 */
	std::list<Board*> listBoards();
	
      private:
	
	/**
	 * Pointer to the shared antenna resources.
	 */
	SzaShare* share_;

	/**
	 * An object to encapsulate the circular data buffer we will
	 * use to transmit data back to the ACC
	 */
	sza::util::AntennaFrameBuffer fb_;
	
	/**
	 * A board to encapsulate the features register
	 */
	FrameBoard* frame_; 
	
	//------------------------------------------------------------
	// Internal record-keeping structs
	//------------------------------------------------------------
	
	/**
	 * A structure to manage details of feature bits received from
	 * the control program.  
	 */
	struct Features {
	  unsigned seq_;  // Sequence number of the last feature
	  // marker command received from the control
	  // program
	  unsigned transient_;  // Bit-mask union of persistent
	  // feature markers received from the
	  // control program since the last
	  // frame.
	  unsigned persistent_; // The bit mask union of persistent
	  // feature markers previously received
	  // from the control program but not
	  // cancelled.
	  void reset();
	} features_;
	/*
	 * A structure of the following type is used to store the
	 * current bit mask of receiver walsh states, and the bit mask
	 * of walsh states which was in effect during the frame that
	 * we are currently sending to the archiver.
	 */
	struct Walsh {
	  bool request_on_; // If walshing is requested, this will be
	  // set to true. If requested, slow
	  // walshing will commence on the next
	  // invocation of the tgen interrupt
	  // handler. If walshing is currently on,
	  // walshing will cease at the end of the
	  // current walsh cycle.
	  bool current_on_; // True if walshing was on during the
	  // last integration.  at the end of the
	  // current walsh cycle.
	  int counter_;     // The walsh iteration counter.  This
	  // tells the scanner where in the walsh
	  // cycle we currently are. 
	  unsigned current_state_; // The current bit mask of walsh states 
	  unsigned last_state_;    // The bit mask for the frame we
	  // are currently sending
	  void reset();
	} walsh_;
	
	
	/**
	 * Reset non-pointer members of the Scanner object.
	 */
	void initialize();
	
	/**
	 * Method to record the features bitmask in the register
	 * database
	 *
	 * @throws Exception
	 */
	void recordFeatures();
	
	/**
	 * Method to record the time in the register database.
	 *
	 * @throws Exception
	 */
	void setTime();
	
	/**
	 * Method to record the time in the register database.
	 *
	 * @throws Exception
	 */
	void recordTime();
	
	/**
	 * Archive the record number of the current frame in the
	 * register database.
	 *
	 * @throws Exception
	 */
	void recordRecordNumber(unsigned nrecord);
	
	/**
	 * Archive the current walsh state in the register database.
	 *
	 * @throws Exception
	 */
	void recordWalshState();
	
	/**
	 * If true, skip the next frame
	 */
	bool skipOne_; 
	
	/**
	 * An internal record number that is incremented whenever a
	 * frame is recorded
	 */
	unsigned recordNumber_; 
	
      }; // End class Scanner
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
