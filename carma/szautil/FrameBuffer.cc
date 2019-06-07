#include "carma/szautil/DataFrameManager.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/FrameBuffer.h"
#include "carma/szautil/LogStream.h"

using namespace std;
using namespace sza::util;

FrameBuffer::FrameBuffer(unsigned int nSlot)
{
  nSlot_  = nSlot;
  nUsed_  = 0;
  
  // Initialize our index pointers to NULL
  
  nextFreeSlot_ = 0;
  nextSendSlot_ = 0;
  lastSentSlot_ = 0;

  // Check passed arguments

  if(nSlot_ < 1)
    throw Error("FrameBuffer::FrameBuffer: nSlot < 1.\n");
  
  // Initialize each DataFrameManager to have a buffer large enough to
  // accomodate a frame
  
  slots_.resize(nSlot_);
  
  // Turn the vector into a circular linked list for convenience of
  // traversal.
  
  for(unsigned islot=0; islot < nSlot_-1; islot++)
    slots_[islot].next_ = &slots_[islot+1];
  slots_[nSlot_-1].next_ = &slots_[0];

  // And set both index pointers pointing to the first element.
  // lastSentSlot we leave NULL, so we can tell when no frames have
  // been sent.

  nextFreeSlot_ = &slots_[0];
  nextSendSlot_ = &slots_[0];
}

/**.......................................................................
 * Destructor.
 */
FrameBuffer::~FrameBuffer() {}

/**.......................................................................
 * Return a pointer to the next slot to be dispatched to the outside
 * world.
 */
DataFrameManager* FrameBuffer::dispatchNextFrame()
{
  // If there are no slots waiting to be dispatched, return NULL

  DBPRINT(false, Debug::DEBUG8, "Entering: nextSendSlot_ = " << nextSendSlot_);
  DBPRINT(false, Debug::DEBUG8, "Entering: lastSentSlot_ = " << lastSentSlot_);
  DBPRINT(false, Debug::DEBUG8, "Entering: nextFreeSlot_ = " << nextFreeSlot_);

  if(nUsed_ == 0)
    return 0;

  // Else set the return pointer pointing to the next frame to be sent
  
  if(Debug::debugging(Debug::DEBUG7)) {
    DBPRINT(false, Debug::DEBUG7, "Next send slot is: " 
	    << nextSendSlot_->id_);
    DBPRINT(false, Debug::DEBUG7, "Last sent slot is: " 
	    << ((lastSentSlot_ == 0) ? 0 : lastSentSlot_->id_));
  }

  DataFrameManager* dfm = nextSendSlot_->frame_;

  // Remove this slot from the map of known slots.  While it is being
  // sent, we don't want anyone else to find it as a valid slot.

  frameMap_.erase(nextSendSlot_->id_);

  DBPRINT(true, Debug::DEBUG14, "Dispatching slot: " << nextSendSlot_->id_);
    
  // Lock the frame.  This protects against writers trying to modify
  // the frame while the calling thread is attempting to send it.

  dfm->lock();

  // Only decrement the count of frames waiting to be sent if this
  // call means we just finished sending a frame.
  
  if(lastSentSlot_ != 0) {
    DBPRINT(false, Debug::DEBUG8, "Clearing slot: " << lastSentSlot_);
    clearSlot(lastSentSlot_);
    nUsed_--;
  }

  // Set the last sent frame pointing to the one we are currently dispatching

  lastSentSlot_ = nextSendSlot_;

  // And increment the next send frame pointer

  nextSendSlot_ = nextSendSlot_->next_;

  DBPRINT(false, Debug::DEBUG8, "Leaving: nextSendSlot_ = " << nextSendSlot_);
  DBPRINT(false, Debug::DEBUG8, "Leaving: lastSentSlot_ = " << lastSentSlot_);
  DBPRINT(false, Debug::DEBUG8, "Leaving: nextFreeSlot_ = " << nextFreeSlot_);

  return dfm;
}

/**.......................................................................
 * Return a pointer to the next free slot in the buffer, or NULL
 * if there are none.
 */
struct FrameBuffer::FrameBufferSlot* FrameBuffer::getNextSlot()
{
  struct FrameBufferSlot* slot = nextFreeSlot_;
  
  // If we have come full circle in the buffer, start eating our tail

  if(nUsed_ == nSlot_) {
    clearSlot(nextSendSlot_);
    nextSendSlot_ = nextSendSlot_->next_;
  } else // Only increment nUsed if the buffer is not full
    nUsed_++;
    
  // Set the index pointer pointing to the next free slot

  nextFreeSlot_ = nextFreeSlot_->next_;
  
  return slot;
}

/**.......................................................................
 * Return a pointer to a data frame in the buffer.  If a slot with
 * the passed id has already been installed, that slot will be
 * returned.  If no slots matching the passed id were found, the
 * next free slot will be initialized with the passed id, and its
 * pointer returned.
 */
DataFrameManager* FrameBuffer::getNextFrame()
{
  DBPRINT(false, Debug::DEBUG8, "Entering: nextSendSlot_ = " << nextSendSlot_);
  DBPRINT(false, Debug::DEBUG8, "Entering: lastSentSlot_ = " << lastSentSlot_);
  DBPRINT(false, Debug::DEBUG8, "Entering: nextFreeSlot_ = " << nextFreeSlot_);

  struct FrameBufferSlot* slot = getNextSlot();

  DBPRINT(false, Debug::DEBUG8, "Leaving: nextSendSlot_ = " << nextSendSlot_);
  DBPRINT(false, Debug::DEBUG8, "Leaving: lastSentSlot_ = " << lastSentSlot_);
  DBPRINT(false, Debug::DEBUG8, "Leaving: nextFreeSlot_ = " << nextFreeSlot_);

  DBPRINT(false, Debug::DEBUG8, "Returning slot: " << slot);

  if(slot != 0)
    return slot->frame_;

  return 0;
}

/**.......................................................................
 * Return a pointer to a data frame in the buffer.  If a slot with the
 * passed id has already been installed, that slot will be returned.
 * If no slots matching the passed id were found, and create == true,
 * the next free slot will be initialized with the passed id, and its
 * pointer returned.  Else NULL will be returned if no slot with the
 * passed id was found, and create == false.
 */
DataFrameManager* FrameBuffer::getFrame(unsigned int id, bool create)
{
  DataFrameManager* frame=0;
  LogStream logStr;

  // Lock the frame buffer.  We do this so that no other call to
  // getFrame() can modify the slot map while we are searching it.
  // 
  // Even with interlocking, we can still run into trouble if multiple
  // threads are simultaneously creating slots with create == true,
  // since the returned frame may refer to a slot whose id has since
  // been modified by another call to this method, but this is not my
  // usage model.  What I'm concerned about is a single thread
  // creating frames with create == true, and multiple threads looking
  // them up with create == false.  In this case, as long as we use
  // interlocking, we are guaranteed that one thread will not search
  // the map while another thread is in the process of modifying it,
  // and vice versa..

  DBPRINT(true, Debug::DEBUG10, "About to lock the frame");

  guard_.lock();

  try {

    // Search the map for a match
    
    struct FrameBufferSlot* slot = findSlot(id);
    
    // If a slot with that id was found, return it.  If no slot was
    // found, and we are not creating slots with this call, return
    // NULL.
    
    if(slot != 0 || !create) {
      if(slot == 0)
	frame = 0;
      else 
	frame = (slot == 0) ? 0 : slot->frame_;
    } else {

      // Else create the slot

      slot = getNextSlot();
      
      if(Debug::debugging(Debug::DEBUG7)) {
	if(slot==0)
	  cout << "No more free slots" << endl;
      }

      // If no free slots were found, slot will be NULL.  
      
      if(slot!=0) {
      
	// If a free slot was found, insert it into the map of tagged frames
	
	DBPRINT(false, Debug::DEBUG7, "Adding a slot with id: " << id);
	
	frameMap_[id] = slot;
	
	// And set the id of this slot
	
	slot->id_ = id;
	
	// Set the return value

	frame = slot->frame_;
      }
    }
  } catch (...) {
    logStr.appendMessage(true, "Caught exception");
  }

  // Release the frame buffer guard mutex

  guard_.unlock();

  // If an error occurred, throw it here.

  if(logStr.isError())
    throw Error(logStr);

  // Else return the frame

  return frame;
} 

/**.......................................................................
 * Find a slot by id number
 */
struct FrameBuffer::FrameBufferSlot* FrameBuffer::findSlot(unsigned int id)
{
  std::map<unsigned int, struct FrameBufferSlot*>::iterator slot;
  
  slot = frameMap_.find(id);
  
  if(slot != frameMap_.end())
    return slot->second;
  
  return 0;
}

/**.......................................................................
 * Public method to query how many frames are waiting in the queue.
 */
unsigned int FrameBuffer::getNframesInQueue()
{
  return nUsed_;
}

/**.......................................................................
 * Clear a previously used slot in the frame buffer
 */
void FrameBuffer::clearSlot(FrameBufferSlot* slot)
{
  // Delete this slot's entry in the map

  frameMap_.erase(slot->id_); 

  // Make sure this frame is unlocked

  slot->frame_->unlock();     

  // And reinitialize its contents.

  slot->frame_->reinitialize();
}

