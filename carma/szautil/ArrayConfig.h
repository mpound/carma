
// $Id: ArrayConfig.h,v 1.1 2010/12/13 21:06:28 eml Exp $

#ifndef SZA_UTIL_ARRAYCONFIG_H
#define SZA_UTIL_ARRAYCONFIG_H

/**
 * @file ArrayConfig.h
 * 
 * Tagged: Wed Oct 15 14:14:12 PDT 2008
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:28 $
 * 
 * @author username: Command not found.
 */
namespace sza {
  namespace util {

    class ArrayConfig {
    public:

      /**
       * Constructor.
       */
      ArrayConfig();

      /**
       * Destructor.
       */
      virtual ~ArrayConfig();

      // I screwed this up -- started as orthogonal bits, then
      // realized after I'd exceeded 8 bits that I've used it in the
      // data stream as an 8-bit type.  For consistency sake, I'll
      // keep this as an 8-bit type, and new entries will simply have
      // to not be orthogonal bits

      enum Type {
	UNKNOWN = 0x0,
	NOCARMA = 0x1,       // No CARMA array
	A       = 0x2,       // CARMA A-array
	B       = 0x4,       // CARMA B-array
	BO      = B+1,
	C       = 0x8,       // CARMA C-array
	CO      = C+1,
	D       = 0x10,      // CARMA D-array
	DO      = D+1,
	E       = 0x20,      // CARMA E-array
	I       = 0x40,      // SZA   I-array
	L       = I+1,       // SZA   lowdec-array
	H       = I+2,       // SZA   highec-array
	BP      = I+3,       // SZA   B-array paired-antenna configuration
	AP      = I+4,       // SZA   A-array paired-antenna configuration
      };

    private:
    }; // End class ArrayConfig

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ARRAYCONFIG_H
