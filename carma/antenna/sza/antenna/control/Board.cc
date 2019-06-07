#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/antenna/sza/antenna/control/Board.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor function
 */
Board::Board(SzaShare* share, string name)
{
  // Initialize the private elements to something safe
  
  board_    = 0;
  share_    = 0;
  hasBoard_ = true;
  
  // Sanity check arguments
  
  if(share == 0)
    throw Error("Board::Board: Received NULL SzaShare descriptor.\n");
  
  share_ = share;
  
  board_ = share_->findRegMapBoard(name);

  if(board_==0) {
    LogStream errStr;
    errStr.initMessage(true);
    errStr << "Lookup of board: " << name << " failed" << endl;
    throw Error(errStr);
  }
    

}

/**.......................................................................
 * Constructor function
 */
Board::Board(SzaShare* share, AntNum ant)
{
  // Initialize the private elements to something safe
  
  board_    = 0;
  share_    = 0;
  hasBoard_ = true;
  
  // Sanity check arguments
  
  if(share == 0)
    throw Error("Board::Board: Received NULL SzaShare descriptor.\n");
  
  // Check that this antenna descriptor refers to a valid single
  // Antenna (AntNum descriptors can also be unions of several
  // antennas)
  
  if(!ant.isValidSingleAnt())
    throw Error("Board::Board: Received invalid receiver designation.\n");
  
  share_ = share;
  
  board_ = share_->findRegMapBoard((char*)(ant.getAntennaName().data()));
  
  if(board_ == 0) {
    ostringstream os;
    os << "Board::Board: Lookup of board \"" << ant.getAntennaName().data()
       << "\" failed." << ends;
    throw Error(os.str());
  }
}

/**.......................................................................
 * Constructor function for a virtual board
 */
Board::Board(SzaShare* share)
{
  // Initialize the private elements to something safe
  
  board_    = 0;
  share_    = 0;
  hasBoard_ = false;
  
  if(share == 0)
    throw Error("Board::Board: Received NULL SzaShare descriptor.\n");
  
  share_ = share;
}

/**.......................................................................
 * Empty function body for the virtual destructor
 */
Board::~Board() {};

/**.......................................................................
 * Look up a register of this board
 */
RegMapBlock* Board::findReg(char* name)
{
  RegMapBlock* block = 0;
  
  block = find_RegMapBoard_Block(board_, name);
  
  // find_RegMapBoard_Block() returns NULL on error
  
  if(block == 0) {
    ostringstream os;
    os << "Board::findReg: Lookup of register \"" << name
       << "\" on board: " << board_->name
       << " failed." << ends;
    throw Error(os.str());
  }
  
  return block;
}

/**.......................................................................
 * A public function to verify that this board is reachable
 */
bool Board::isReachable()
{
  // SzaShare::verifyBoard() returns true if an error occurred trying
  // to reach the board, so a false value signifies the board is
  // reachable
  
  return !share_->verifyBoard(board_->number);
}

/**.......................................................................
 * Return the index of this board in the register database
 */
int Board::getIndex()
{
  return board_->number;
}

/**.......................................................................
 * Method to read a register from this board.
 */
void Board::readReg(RegMapBlock* blk, unsigned int first, 
		    unsigned int nreg, unsigned int* value)
{
  CoordRange range(first, first+nreg-1);
  share_->readReg(blk, value, &range);
}

/**.......................................................................
 * Method to write to a register of this board.
 */
void Board::writeReg(RegMapBlock* blk, unsigned int first, 
		     unsigned int nreg, unsigned int* value)
{
  CoordRange range(first, first+nreg-1);
  share_->writeReg(blk, value, &range);
}

/**.......................................................................
 * Method to write to a register of this board.
 */
void Board::writeReg(RegMapBlock* blk, unsigned int first, 
		     unsigned int nreg, bool* value)
{
  CoordRange range(first, first+nreg-1);
  unsigned ival = (unsigned)(*value);
  share_->writeReg(blk, &ival, &range);
}
