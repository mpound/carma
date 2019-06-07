#ifndef SZA_UTIL_FRAMEBUFFER_H
#define SZA_UTIL_FRAMEBUFFER_H

/**
 * @file FrameBuffer.h
 * 
 * Tagged: Sun Mar 21 22:28:53 UTC 2004
 * 
 * @author Erik Leitch
 */
#include <vector>
#include <map>
#include "carma/szautil/Mutex.h"

namespace sza {
  namespace util {
    
    class DataFrameManager;
    
    class FrameBuffer {
    public:
      
      /**
       * Constructor.
       */
      FrameBuffer(unsigned nFrame);
      
      /**
       * Destructor.
       */
      virtual ~FrameBuffer();
      
      /**
       * Return a pointer to the frame with the passed id.  If no
       * frame with that id exists, and create == true, this call will
       * create it.  If no frame with that id exists, and create ==
       * false, this call returns NULL.
       */
      DataFrameManager* getFrame(unsigned int id, bool create);
      
      /**
       * Return a pointer to the next free data frame in the buffer.
       * Creates a new frame on every call.
       */
      DataFrameManager* getNextFrame();
      
      /**
       * Return a pointer to the next slot to be dispatched to the
       * outside world.  Using this method guarantees that the
       * returned pointer will not be modified until the next call to
       * dispatchNextFrame(), since the DataFrameManager object
       * returned is locked until dispatchNextFrame() is called again.
       */
      DataFrameManager* dispatchNextFrame();
      
      /**
       * Public method to query how many frames are waiting in the queue.
       */
      unsigned int getNframesInQueue();

    protected:
      
      /**
       * Create a struct that we will use as an internal buffer.
       */
      struct FrameBufferSlot {
	unsigned int id_;
	DataFrameManager* frame_;
	struct FrameBufferSlot* next_;
      };
      
      /**
       * A vector of slots
       */
      std::vector<struct FrameBufferSlot> slots_;

      /**
       * The number of frames in our buffer
       */
      unsigned long nSlot_;           
      
    private:
      
      /**
       * A guard mutex for this frame buffer.
       */
      Mutex guard_;

      /**
       * A map to help with searching
       */
      std::map<unsigned int, struct FrameBufferSlot*> frameMap_;
      
      /**
       * Pointer to the next free slot
       */
      struct FrameBufferSlot* nextFreeSlot_;    
      
      /**
       * Pointer to the next slot to be dispatched to the outside
       * world.
       */
      struct FrameBufferSlot* nextSendSlot_;    
      
      /**
       * Pointer to the last slot dispatched to the outside world.
       */
      struct FrameBufferSlot* lastSentSlot_;    
      
      /**
       * The number of slots currently in use.
       */
      unsigned long nUsed_;            
      
      /**
       * Find a slot by id number
       */
      struct FrameBufferSlot* findSlot(unsigned int id);

      /**
       * Get the next free slot
       */
      struct FrameBufferSlot* getNextSlot();
      
      /**
       * Clear a previously used slot in the frame buffer
       */
      void clearSlot(FrameBufferSlot* slot);

    }; // End class FrameBuffer
    
  } // End namespace util
} // End namespace sza

#endif // End #ifndef SZA_UTIL_FRAMEBUFFER_H
