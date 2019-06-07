#ifndef SZA_UTIL_LOOSC_H
#define SZA_UTIL_LOOSC_H

/**
 * @file LoOsc.h
 * 
 * Tagged: Wed Aug 25 21:23:12 PDT 2004
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class LoOsc {
    public:

      enum Osc {
	YIG      = 0x1,
	VARACTOR = 0x2,
	GUNN     = 0x4,
	ALL      = YIG | VARACTOR | GUNN
      };

    }; // End class LoOsc
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_LOOSC_H
