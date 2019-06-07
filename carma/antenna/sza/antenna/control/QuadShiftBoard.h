#ifndef QUADSHIFTBOARD_H
#define QUADSHIFTBOARD_H

/**
 * @file QuadShiftBoard.h
 * 
 * Tagged: Thu Nov 13 16:53:50 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"

#include "carma/antenna/sza/antenna/control/Board.h"
#include "carma/antenna/sza/antenna/control/PolarEncoderPos.h"
#include "carma/antenna/sza/antenna/control/SlowWalsh.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Incomplete class specification lets us declare FrameBuffer a
       * friend (below) without defining it
       */
      class FrameBuffer;
      
      /**
       * The following struct manages pointers to the lo_quad register
       * for a receiver card, and members where we will store the
       * quadrature phase shift for a receiver during the last
       * integration.
       */
      class QuadShiftBoard : public Board {
	
      public:
	
	/**
	 * Constructor with name.
	 *
	 * @throws (via Board::findReg) Exception
	 */
	QuadShiftBoard(SzaShare* share, std::string name);
	
	/**
	 * Constructor with antenna enumerator.
	 *
	 * @throws  (via Board::findReg) Exception
	 */
	QuadShiftBoard(SzaShare* share, sza::util::AntNum ant);
	
      private:
	
	/**
	 * FrameBuffer will pass private members of QuadShiftBoard to
	 * SzaShare when archiving data.
	 */
	friend class FrameBuffer; 
	
	/**
	 * Keep a copy of the Antenna enumerator we were passed.
	 */
	sza::util::AntNum antNum_; 
	
	/**
	 * Pointer to the local user-requested lo_quad register for
	 * each receiver.
	 */
	RegMapBlock* loQuadRequested_; 
	
	/**
	 * Pointer to the control lo_quad register for each receiver
	 */
	RegMapBlock* loQuadActual_;
	
	/**
	 * The current of the requested lo_quad for this receiver.
	 */
	unsigned quadRequested_; 
	
	/**
	 * The value of the last requested lo_quad for this receiver
	 * during the last integration.
	 */
	unsigned quadLast_; 
	
	/**
	 * The last polarization state of this receiver.
	 */
	unsigned polarization_; 
	
	/**
	 * Pointer to the walsh function for this receiver.
	 */
	SlowWalsh walsh_; 
	
	/**
	 * A container which encapsulates encoder positions which
	 * correspond to known polarization states for this receiver.
	 */
	PolarEncoderPos polar_;      
	
      }; // End class QuadShiftBoard
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef
