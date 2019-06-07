// $Id: AntennaType.h,v 1.1 2010/12/13 21:06:28 eml Exp $

#ifndef SZA_UTIL_ANTENNATYPE_H
#define SZA_UTIL_ANTENNATYPE_H

/**
 * @file AntennaType.h
 * 
 * Tagged: Thu Oct  9 22:48:42 PDT 2008
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:28 $
 * 
 * @author tcsh: username: Command not found.
 */
namespace sza {
  namespace util {

    class AntennaType {
    public:

      /**
       * Constructor.
       */
      AntennaType();

      /**
       * Destructor.
       */
      virtual ~AntennaType();

      enum Type {
	UNKNOWN = 0x0,
	SZA     = 0x2,
	BIMA    = 0x4,
	OVRO    = 0x8,
      };

    private:
    }; // End class AntennaType

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ANTENNATYPE_H
