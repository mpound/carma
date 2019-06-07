#ifndef SZA_UTIL_ANTENNAFRAMEBUFFER_H
#define SZA_UTIL_ANTENNAFRAMEBUFFER_H

/**
 * @file AntennaFrameBuffer.h
 * 
 * Tagged: Mon Mar 22 03:56:39 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"
#include "carma/szautil/FrameBuffer.h"

namespace sza {
  namespace util {
    
    class AntennaFrameBuffer : public sza::util::FrameBuffer {
    public:
      
      /**
       * Constructor.
       */
      AntennaFrameBuffer(const sza::util::AntNum& antNum,
			 unsigned int nFrame, 
			 bool archivedOnly=false);
      
      /**
       * Constructor.
       */
      AntennaFrameBuffer(const sza::util::AntNum& antNum, 
			 bool archivedOnly=false);
      
      /**
       * Constructor.
       */
      AntennaFrameBuffer(sza::util::AntNum* antNum, 
			 bool archivedOnly=false);
      
      /**
       * Destructor.
       */
      virtual ~AntennaFrameBuffer();
      
    private:
    }; // End class AntennaFrameBuffer
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ANTENNAFRAMEBUFFER_H
