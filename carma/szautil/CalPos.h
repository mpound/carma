#ifndef SZA_UTIL_CALPOS_H
#define SZA_UTIL_CALPOS_H

/**
 * @file CalPos.h
 * 
 * Tagged: Thu Jun 17 21:05:13 UTC 2004
 * 
 * @author 
 */
namespace sza {
  namespace util {
    
    /**
     * Class to enumerate recognized calibrator positions
     */
    class CalPos {
    public:
      
      enum Pos {
	NONE    = 0x0,
	SKY     = 0x1,
	AMBIENT = 0x2,
	HOTLOAD = 0x4,
	ALL     = SKY | AMBIENT | HOTLOAD
      };

    }; // End class CalPos
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CALPOS_H
