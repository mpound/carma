#ifndef LOBAND_H
#define LOBAND_H

/**
 * @file LoBand.h
 * 
 * Started: Wed Dec 10 23:10:39 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"
#include "carma/szautil/Attenuation.h"

namespace sza {
  namespace util {
    
    /**
     * Class to enumerate recognized LO bands.
     */
    class LoBand {
    public:
      
      enum Band {
	LO_NONE   = 0,
	LO_XBAND  = 1,
	LO_KABAND = 2,
	LO_WBAND  = 4,
	LO_1MM    = 8,
	LO_ALL    = LO_XBAND|LO_KABAND|LO_WBAND|LO_1MM
      };
      
      static void setLoTermAttenuation(AntNum::Id antId, Attenuation atten);
      static Attenuation getLoTermAttenuation(AntNum::Id antId);

    private:

      static Attenuation loTermAttens_[AntNum::NANT];

    }; // End class LoBand
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 


