#ifndef SZA_TRANSLATOR_ARRAYFRAMEBUFFER_H
#define SZA_TRANSLATOR_ARRAYFRAMEBUFFER_H

/**
 * @file ArrayFrameBuffer.h
 * 
 * Tagged: Tue Mar 23 19:01:49 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/FrameBuffer.h"
#include "carma/szautil/TimeVal.h"

#include "carma/szaarrayutils/scanner.h"

namespace sza {
  namespace util {
    
    class ArrayFrameBuffer : public sza::util::FrameBuffer {
    public:
      
      /**
       * Constructor.
       */
      ArrayFrameBuffer(unsigned int nFrame = SCAN_MAX_FRAME, 
		       bool archivedOnly=false, ArrayMap* arrayMap=NULL);
      
      /**
       * Destructor.
       */
      virtual ~ArrayFrameBuffer();

    private:

      sza::util::TimeVal timeVal;

    }; // End class ArrayFrameBuffer
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_TRANSLATOR_ARRAYFRAMEBUFFER_H
