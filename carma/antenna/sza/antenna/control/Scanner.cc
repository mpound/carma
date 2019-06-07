#include "carma/szautil/Debug.h"
#include "carma/antenna/sza/antenna/control/Scanner.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor with Antenna number
 */
Scanner::Scanner(SzaShare* share, AntNum* ant) : 
  fb_(ant), share_(share)
{
  unsigned short iant;

  // Initialize the board pointers to NULL

  frame_     = 0;

  // Now allocate the new descriptors

  frame_     = new FrameBoard(share, "frame");

  // Finally, perform various initialization tasks

  initialize();
}

/**.......................................................................
 * Destructor
 */
Scanner::~Scanner()
{
  if(frame_ != 0)
    delete frame_;
}

/**.......................................................................
 * Reset non-pointer members of the Scanner object
 */
void Scanner::initialize()
{
  skipOne_            = false;
  recordNumber_       = 0;

  // Reset members of bookkeeping structs

  features_.reset();

  // And record the walsh state of the FrameBoard register

  recordWalshState();
}

/**.......................................................................
 * Reset members of the features struct
 */
void Scanner::Features::reset()
{
  seq_        = 0;
  transient_  = 0;
  persistent_ = 0;
}

/**.......................................................................
 * Reset members of the walsh struct
 */
void Scanner::Walsh::reset()
{
  // Default to no slow walshing and set the current and last walsh
  // states to 0.

  current_on_    = false;
  request_on_    = false;
  counter_       = 0;
  current_state_ = 0;
  last_state_    = 0;
}

/**.......................................................................
 * Update the record number internally, and in the register database
 */
void Scanner::recordRecordNumber(unsigned record)
{
  frame_->archiveRecordNumber(record);
  recordNumber_ = record;
}

/**.......................................................................
 * Record the features mask in the database
 */
void Scanner::recordFeatures()
{
  unsigned features = features_.transient_ | features_.persistent_;
  frame_->archiveFeatures(features, features_.seq_);
}

/**.......................................................................
 * Record the walshstate bitmask in the database
 */
void Scanner::recordWalshState()
{
}

/**.......................................................................
 * Record the features mask in the database
 */
void Scanner::recordTime()
{
  frame_->archiveTime();
}

/**.......................................................................
 * Record the features mask in the database
 */
void Scanner::setTime()
{
  frame_->setTime();
}

/**.......................................................................
 * If there is room in the circular frame buffer, record another
 * data frame and push it onto the event channel.
 */
void Scanner::packNextFrame()
{
  // Set the current time in the database.

  setTime();

  // Increment the record number.

  recordRecordNumber(recordNumber_+1);

  // Record the time.

  recordTime();

  // Record the bit-mask of feature markers received since the last
  // record was successfully dispatched to the archiver, plus any
  // persistent features that haven't been cancelled yet.

  recordFeatures();

  // Record the bit-mask of receiver walsh states in effect during the
  // last frame.

  recordWalshState();

  // Copy the shared-memory object into the next frame in the frame
  // buffer

  share_->packFrame(fb_.getNextFrame());

#if 0
  unsigned short received = 0;
  share_->writeReg("bias",      "received", &received);
  share_->writeReg("caltert",   "received", &received);
  share_->writeReg("ifmod",     "received", &received);
  share_->writeReg("intmod",    "received", &received);
  share_->writeReg("rx",        "received", &received);
  share_->writeReg("thermal",   "received", &received);
  share_->writeReg("tiltmeter", "received", &received);
  share_->writeReg("varactor",  "received", &received);
  share_->writeReg("yig",       "received", &received);
#endif

  // Finally, clear the transient feature mask.

  features_.transient_ = 0;
}

/**.......................................................................
 * If there is room in the circular frame buffer, record another
 * data frame and push it onto the event channel.
 */
DataFrameManager* Scanner::dispatchNextFrame()
{
  // Get the next frame (if any) to be dispatched

  return fb_.dispatchNextFrame();
}

/**.......................................................................
 * Return the number of frames waiting to be dispatched.
 */
unsigned int Scanner::getNframesInQueue()
{
  return fb_.getNframesInQueue();
}

