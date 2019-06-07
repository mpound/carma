#include "carma/antenna/sza/antenna/control/PmacBoard.h"

#include "carma/szautil/DataArray.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/PmacFlags.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::control;

#define DEVELOPMENT
#define TEST_VERS

/**.......................................................................
 * Constructor for PMAC board class.  This calls the base-class
 * constructor with the board as the first argument
 *
 * @throw Exception
 */
PmacBoard::PmacBoard(SzaShare* share, string name, bool simPmac) :  
  Board(share, name), simPmac_(simPmac)
{
  // Initialize pointers to NULL, so we can check for failure

  hostRead_      = 0;
  pmacWrite_     = 0;
  newPosition_   = 0;
  positionFault_ = 0;
  newMode_       = 0;
  newAz_         = 0;
  newEl_         = 0;
  newDk_         = 0;
  newAzRate_     = 0;
  newElRate_     = 0;
  newDkRate_     = 0;
  azPos_         = 0;
  elPos_         = 0;
  dkPos_         = 0;
  driveStatus_   = 0;
  statusMask_    = 0;

  dpramSize_     = 0;
  dpramBaseAddr_ = 0x0;
  comms_         = 0;

  // Look up relevant registers of the board

  hostRead_       = findReg("host_read");
  pmacWrite_      = findReg("pmac_write");
  newPosition_    = findReg("new_position");
  positionFault_  = findReg("position_fault");
  newMode_        = findReg("new_mode");
  newAz_          = findReg("new_az");
  newEl_          = findReg("new_el");
  newDk_          = findReg("new_dk");
  newAzRate_      = findReg("new_az_rate");
  newElRate_      = findReg("new_el_rate");
  newDkRate_      = findReg("new_dk_rate");
  azPos_          = findReg("az_pos");
  elPos_          = findReg("el_pos");
  dkPos_          = findReg("dk_pos");
  driveStatus_    = findReg("drive_status");
  statusMask_     = findReg("statusMask");

  // Calculate the size of the DPRAM

  computeDpramStats();

  // Initialize the PMAC communications container.

  comms_ = new PmacComms(share);

  // Ensure that if commandNewPosition() is called before any position
  // was written to the pmac, the shared memory register will be
  // initialized to something valid

  unsigned newPosition = 0;
  unsigned hostRead = 0;
  share_->writeReg(newPosition_, &newPosition);
  share_->writeReg(hostRead_,    &hostRead);

  // And initialize

  reset();
}

/**.......................................................................
 * Destructor.
 */
PmacBoard::~PmacBoard()
{
  if(comms_ != 0)
    delete comms_;
}

/**.......................................................................
 * Return 1 if the pmac is either unreachable or its newPosition flag
 * is still clear. This indicates that the pmac isn't ready to receive
 * a new command.
 */
bool PmacBoard::isBusyOld()
{
  unsigned newPosition=0;
  bool isbusy;

  // If simulating, lie.

  if(simPmac_)
    return false;

#if 0
  // Return busy if the board cannot be reached

  if(!share_->verifyBoard(board_->number))
    return true;

  // ...or if there was an error reading the board

  try {
    readReg(newPosition_, 0, 1, &newPosition);
  } catch(...) {
    return true;
  }

  DBPRINT(false, Debug::DEBUG5, "newPosition is: " << newPosition << endl);

#else
  return false;
#endif

  // ...or if the board's newPosition flag hasn't been cleared

  return (newPosition==1);
}

/**.......................................................................
 * Return 1 if the pmac is either unreachable or its newPosition flag
 * is still clear. This indicates that the pmac isn't ready to receive
 * a new command.
 */
bool PmacBoard::isBusy()
{
  return false;
}

/**.......................................................................
 * Return the current value of the PMAC position-fault flag.
 */
unsigned int PmacBoard::readPositionFault()
{
  // If simulating, lie.

  if(simPmac_)
    return 0;

  // Else read the value for real

  unsigned positionFault;

  // Read the last value of the position fault register that was
  // mirrored to shared memory.

  share_->readReg(positionFault_, &positionFault);

  return positionFault;
}

/**.......................................................................
 * Tell the PMAC to read a new position
 */
void PmacBoard::commandNewPosition(PmacTarget* pmac)
{
  if(simPmac_)
    return;

  // When the new command has been written to the dual-port-ram, this
  // value will be assigned to the PMAC newPosition flag.  The PMAC is
  // responsible for setting this back to 0, to indicate that the new
  // position has been acknowledged.

  unsigned newPosition=1;
  unsigned prevNewPosition;
  unsigned val;
  LogStream errStr;

  // Check that the previous position was acknowledged.  The value in
  // shared memory at this point will be the value that was read on
  // the last strobe that mirrored the dpram to memory.

#ifndef TEST_VERS
  share_->readReg(newPosition_, &prevNewPosition);

  if(prevNewPosition != 0) 
    ThrowError("Last position wasn't acknowledged by the pmac.");
#endif
	       
  // Write the new positions.

  val = static_cast<unsigned>(pmac->getMode());
  writeRegToWork(newMode_,    0, 1, &val);

  val = static_cast<unsigned>(pmac->PmacAxis(Axis::AZ)->getCount());
  writeRegToWork(newAz_,      0, 1, &val);

  val = static_cast<unsigned>(pmac->PmacAxis(Axis::EL)->getCount());
  writeRegToWork(newEl_,      0, 1, &val);

  val = static_cast<unsigned>(pmac->PmacAxis(Axis::AZ)->getRate());
  writeRegToWork(newAzRate_,  0, 1, &val);

  val = static_cast<unsigned>(pmac->PmacAxis(Axis::EL)->getRate());
  writeRegToWork(newElRate_,  0, 1, &val);

  // Occasionally the pmac receives a new positioning command, but
  // appears to ignore it.  We now believe that this is because the
  // pmac may read the new position flag before the new position
  // values have been completely written to dpram, and instead get the
  // old positions, which causes the pmac to report that it's done
  // without actually moving to the requested position.
  //
  // To get around this, we will write the new position in one go, but
  // leave the newPosition flag set to 0, then separately change the
  // newPosition flag to 1 after we have written the data

  newPosition = 0;
  writeRegToWork(newPosition_,0, 1, &newPosition);

  // And write the work buffer to the pmac all in one go.

  writeWork();

  // Finally, set the newPosition flag to 1 to tell the pmac it's ok
  // to read the position

  newPosition = 1;
  writeReg(newPosition_, 0, 1, &newPosition);
}

/*.......................................................................
 * Read the pmac monitor data to update our view of where the telescope
 * axes are currently positioned. This function must not be called
 * until trk->lacking & (PTG_ZEROS | PTG_ENCODERS) is zero.
 *
 * Input/Output:
 *
 *  axes      AxisPositions *  The telescope position will be recorded
 */
bool PmacBoard::readPosition(AxisPositions* axes, Model* model)
{
  if(simPmac_)
    return true;

  unsigned busy = 1;     // The value read from pmac.pmacWrite 
  unsigned pmac_status;  // The 32-bit word that records the pmac 
                         // drive status. 
  int waserr = 0;        // True after an error 
  signed pmac_az;        // The azimuth encoder position 
  signed pmac_el;        // The elevation encoder position 
  signed pmac_dk;        // The deck encoder position 
  bool tracking = false;

  // Can we read the required axis positions?  The positions in shared
  // memory at this point will be the values that were read on the
  // last strobe that mirrored the dpram to memory.
    
  share_->readReg(azPos_,       &pmac_az);
  share_->readReg(elPos_,       &pmac_el);
  share_->readReg(driveStatus_, &pmac_status);
  
  // Record the encoder positions. For DASI the encoder units that
  // we used to command the pmac to position the elevation axis were
  // the same as those that are returned for monitoring in the DPRAM
  // elevation.
  
  axes->az_.count_ = pmac_az;
  axes->el_.count_ = pmac_el;
  
  // Convert the encoder counts to topocentric coordinates.
  
  axes->az_.topo_ = model->Encoder(Axis::AZ)->
    convertCountsToSky(axes->az_.count_);
  
  axes->el_.topo_ = model->Encoder(Axis::EL)->
    convertCountsToSky(axes->el_.count_);
  
  // Is the pmac tracking ok?
  
  tracking = pmac_status & (1U<<2U);
  
  return tracking;
}

/**.......................................................................
 *  A public method to connect to the pmac.
 */
bool PmacBoard::connect()
{
  if(simPmac_)
    return true;

  bool connected = comms_->connect();

  // If we successfully connected to the pmac, call its initialization
  // method

  if(connected) {

    // The UDP socket seems to get allocated without complaint even if
    // the pmac is not present.  This will cause an exception to be
    // thrown when we actually write to the pmac, so we will trap this
    // on the initialization step, disconnect and set connected to
    // false.

    try {
      initializePmac();
    } catch(...) {
      COUT("Caught an error initializing -- disconnecting");
      disconnect();
      connected = false;
    }
  }

  return connected;
}

/**.......................................................................
 *  A public method to disconnect from the pmac.
 */
void PmacBoard::disconnect()
{
  comms_->disconnect();
}

/**.......................................................................
 * Determine the size of the used portion of the DPRAM from the
 * register map for the pmac board. Also store the address of the
 * first register in DPRAM.
 */
void PmacBoard::computeDpramStats()
{
  RegMapBlock *blkmin=0, *blkmax=0, *blk=0;
  bool first=true;

  // Iterate through all registers of the pmac board to calculate the
  // size of the DPRAM map.

  for(unsigned iblk=0; iblk < board_->nblock; iblk++) {
    blk = board_->blocks[iblk];
    
    // Check only DPRAM registers.

    if(blk->flags_ & REG_DPRAM) {
      
      if(first) {
	blkmin = blk;
	blkmax = blk;
	first = false;
      } else {

	if(blk->location_ < blkmin->location_)
	  blkmin = blk;

	if(blk->location_ > blkmax->location_)
	  blkmax = blk;
      }
    }
  }

  // If no registers are in DPRAM, return 0.

  if(blkmin==0 || blkmax==0)
    dpramSize_ = 0;
  else
  
    // Else we have the min and max address of blocks in the DPRAM.
    // The following assumes that all blocks are contiguous, and that
    // all are 32-bit registers (hence the *4)
    
    dpramSize_ = (blkmax->location_ - blkmin->location_) + blkmax->nreg_ * 4;
  
  // And store the base address of the first dpram register block.
  
  dpramBaseAddr_ = blkmin->location_;

  // And store the size of just the command portion of the dpram.

  dpramCmdSize_ = (newElRate_->location_ - newPosition_->location_) + 
    newElRate_->nreg_ * 4;

  // And the base address of the first block of the command portion of
  // the dpram.

  dpramCmdBaseAddr_ = newPosition_->location_;

  // And resize the work vector to encompass the command block size.

  dpramWork_.resize(dpramCmdSize_);
}

/**.......................................................................
 * Mirror the DPRAM image to shared memory.
 *
 * We may want to use SzaShare::writeRawReg() for speed, but for now,
 * just use the normal interface for simplicity.
 */
void PmacBoard::mirrorDpramToSharedMemory()
{
  if(simPmac_)
    return;

  unsigned int sentRead=1, rcvdRead;

  // Do nothing if the pmac is not connected.

  if(!comms_->pmacIsConnected())
    return;

  // Write to the hostRead register to tell the pmac that we want
  // monitor data.  Note that here we actually want to write to the
  // pmac, and not to shared memory, so we use PmacBoard::writeReg()

  writeReg(hostRead_, 0, 1, &sentRead);
  
  // Read out the DPRAM from the pmac.
  
  unsigned int* dpram = readoutDpram();

  // Loop through all registers of our board, writing the values
  // passed in dpram to shared memory.
  
  RegMapBlock* blk;
  
  for(unsigned iblk=0; iblk < board_->nblock; iblk++) {
    
    blk = board_->blocks[iblk];
    
    // The pmac board can contain a mix of registers which are local
    // and which are DPRAM registers.  Here of course we only want to
    // write to DPRAM registers.
    
    if(blk->flags_ & REG_DPRAM) {
      
      // blk->location contains the offset in bytes of this register
      // in the DPRAM map.  Divide by sizeof(unsigned int) to get the
      // address to pass to SzaShare::writeReg()
      
      unsigned* uPtr = (dpram + blk->location_ / 4);
      signed* sPtr = (signed*)(dpram + blk->location_ / 4);

      if(blk->isUint())
	share_->writeReg(blk, uPtr);
      else
	share_->writeReg(blk, sPtr);
    }
  }

  // Read the drive_status register and convert to an orthogonal
  // bitmask

  {
    unsigned driveStatus;
    share_->readReg(driveStatus_, &driveStatus);
    share_->writeReg(statusMask_, driveStatusToBit(driveStatus));
  }

  // Check that the hostRead register was set back to 0 by the pmac
  // before we read out the monitor data.

  share_->readReg(hostRead_, &rcvdRead);

#ifndef DEVELOPMENT
  if(rcvdRead != 0) {
    LogStream errStr;
    errStr.appendMessage(true, "Pmac didn't finish writing monitor data"
			 "before it was read.");
    throw Error(errStr);
  }
#endif
}

/**.......................................................................
 * Read out the entire used portion of the DPRAM and return an
 * unsigned int pointer to it, suitable for passing to
 * SzaShare::writeReg()
 */
unsigned int* PmacBoard::readoutDpram() 
{
  unsigned int* dpram=0;

  // Readout the DPRAM memory block.
  
  dpram = (unsigned int*)comms_->getMem(dpramBaseAddr_, dpramSize_);

  return dpram;
}

/**.......................................................................
 * Private interface to PmacComms::readReg().  
 *
 * Overrides Board::readReg().
 */
void PmacBoard::readReg(RegMapBlock *blk, 
			unsigned int first, unsigned int nreg, 
			unsigned int *value)
{
  comms_->readReg(blk, first, nreg, value);
}

/**.......................................................................
 * Private interface to PmacComms::writeReg().  
 * 
 * Overrides Board::writeReg().
 */
void PmacBoard::writeReg(RegMapBlock *blk, 
			 unsigned int first, unsigned int nreg, 
			 unsigned int *value)
{
  comms_->writeReg(blk, first, nreg, value);
}

/**.......................................................................
 * Write a register to the work array.
 * 
 * Overrides Board::writeReg().
 */
void PmacBoard::writeRegToWork(RegMapBlock *blk, 
			       unsigned int first, unsigned int nreg, 
			       unsigned int *value)
{
  // Block locations for DPRAM registers are in bytes in dpram space.
  // Calcualte the start address of this block in the byte array.

  unsigned int startAddr = (blk->location_ - dpramCmdBaseAddr_) + first * 4;

  // And pack the data into the work array at the appropriate location.

  DataArray::pack(blk->addr_mode_, blk->flags_, 
		  (unsigned int*)(&dpramWork_[0] + startAddr), 
		  value, first, nreg);
}

/**.......................................................................
 * Write the work array to dpram.
 */
void PmacBoard::writeWork()
{
  comms_->setMem(dpramCmdBaseAddr_, dpramCmdSize_, &dpramWork_[0]);
}

/**.......................................................................
 * Return true if the pmac is connected.
 */
bool PmacBoard::pmacIsConnected() 
{
  if(simPmac_)
    return false;

  return comms_->pmacIsConnected();
}

/**.......................................................................
 * On connection, we will call this method to initialize the pmac.
 */
void PmacBoard::initializePmac()
{
  unsigned newPosition = 0;
  DBPRINT(false, Debug::DEBUG6, "About to initialize the pmac");
  COUT("About to initialize the pmac");

  writeReg(newPosition_, 0, 1, &newPosition);
}


unsigned char PmacBoard::driveStatusToBit(unsigned int driveStatus)
{
  unsigned char mask = 0x0;

  // Bit 0 -- high means program running, low -- stopped

  if(driveStatus & 0x1) 
    mask |= PmacFlags::RUNNING;
  else
    mask |= PmacFlags::STOPPED;

  // Bit 1 -- high means error, low -- ok

  if(driveStatus & 0x2) 
    mask |= PmacFlags::PROGRAM_ERROR;
  else
    mask |= PmacFlags::PROGRAM_OK;
  
  // Bit 2 -- high means acquired, low -- not acquired

  if(driveStatus & 0x4) 
    mask |= PmacFlags::SOURCE_ACQUIRED;
  else
    mask |= PmacFlags::SOURCE_NOT_ACQUIRED;

  // Bit 3 -- high means bad, low -- ok

  if(driveStatus & 0x8) 
    mask |= PmacFlags::UNSTABLE;
  else
    mask |= PmacFlags::STABLE;
  
  //  bit 0: stopped, running (program)
  //  bit 1: ok, err (program error)
  //  bit 2: not acquired, acquired (source)
  //  bit 3: ok, bad (stable)

  return mask;
}
