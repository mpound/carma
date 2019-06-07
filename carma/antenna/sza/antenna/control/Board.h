#ifndef BOARD_H
#define BOARD_H

/**
 * @file Board.h
 * 
 * Tagged: Thu Nov 13 16:53:34 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szaarrayutils/regmap.h"
#include "carma/szautil/AntNum.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      class SzaShare;

      /**
       * A class which encapsulates resources of a board of the shared
       * register map.
       */     
      class Board {
	
      public:
	
	/**
	 * Constructor looks up a board by name and stores a pointer
	 * to it in private member board_ (below).
	 *
	 * @throws Exception
	 */
	Board(SzaShare* share, std::string name);
	
	/**
	 * Constructor looks up the rx board corresponding to the
	 * requested antenna and stores a pointer to it in private
	 * member board_ (below)
	 *
	 * @throws Exception
	 */
	Board(SzaShare* share, sza::util::AntNum ant);
	
	/**
	 * Constructor for a virtual board
	 *
	 * @throws Exception
	 */
	Board(SzaShare* share);
	
	/**
	 * Declaration of destructor as pure virtual prevents
	 * instantiation of this base class
	 */
	//	virtual ~Board() = 0;
	virtual ~Board();
	
	/**
	 * Return a pointer to a register of the board managed by this
	 * object.
	 *
	 * @throws Exception
	 */
	RegMapBlock* findReg(char* name);
	
	/**
	 * Verify that this board is reachable.
	 *
	 * @throws (indirectly) Exception
	 */
	bool isReachable();
	
	/**
	 * Function to reset private members of a board-management
	 * object.  Should be overwritten by classes which inherit
	 * from Board
	 *
	 * @throws Exception
	 */
	virtual void reset() {};
	
	/**
	 * Public function to return the index of this board in the
	 * register database
	 */
	int getIndex();
	
	/**
	 * Method to read a register from this board.
	 */
	virtual void readReg(RegMapBlock* blk, unsigned int first, 
			     unsigned int nreg, unsigned int* value);
	
	/**
	 * Methods to write to a register of this board.
	 */
	virtual void writeReg(RegMapBlock* blk, unsigned int first, 
			      unsigned int nreg, unsigned int* value);
	
	/**
	 * Methods to write to a register of this board.
	 */
	virtual void writeReg(RegMapBlock* blk, unsigned int first, 
			      unsigned int nreg, bool* value);

      protected:
	
	/**
	 * The resource object of the shared memory database
	 */
	SzaShare* share_;
	
	/**
	 * A pointer to the board this object refers to.
	 */
	RegMapBoard* board_;
	
	/**
	 * True if this Board has a real board
	 * corresponding to it.  
	 *
	 * @todo Will we ever not have one??
	 */
	bool hasBoard_;  
	
      }; // End class Board
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif
