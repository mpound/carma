#ifndef CAMERABOARD_H
#define CAMERABOARD_H

/**
 * @file CameraBoard.h
 * 
 * Tagged: Thu Nov 13 16:53:35 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/Board.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * The registers of the virtual tracker board. This contains
       * details of the tracking computations performed by this
       * task.
       */
      class CameraBoard : public Board {
	
      public:
	
	/**
	 * Constructor function for the CameraBoard class
	 *
	 * @throws (via Board::findReg) Exception
	 */
	CameraBoard(SzaShare* share, std::string name);
	
	/**
	 * Record a new value of the zero_angle in the register
	 * database
	 *
	 * @throws (via SzaShare::writeReg) Exception
	 */
	void recordAngle(long angle);
	
      private:
	
	/**
	 * The deck angle at which the TV image is upright
	 */
	RegMapBlock *angle_;   
	
      }; // End class CameraBoard
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
