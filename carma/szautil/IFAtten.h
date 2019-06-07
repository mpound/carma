#ifndef SZA_UTIL_IFATTEN_H
#define SZA_UTIL_IFATTEN_H

/**
 * @file IFAtten.h
 * 
 * Tagged: Wed Aug 18 06:20:23 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"
#include "carma/szautil/Attenuation.h"

namespace sza {
  namespace util {
    
    // A class for handling antenna IF attenuation settings

    class IFAtten {
    public:
      
      enum Type {
	ATTEN_NONE   = 0x0,
	ATTEN_INPUT  = 0x2,
	ATTEN_OUTPUT = 0x4,
	ATTEN_TOTAL  = 0x8,
      };

      /**
       * The max total attenuation the IF module can achieve
       */
      static const double attenMax_;

      /**
       * Return true if the passed attenuation is a valid setting
       */
      static bool isValidAtten(double atten)
	{
	  return !(atten < 0 || atten > attenMax_);
	}

      static void setDefaultInputAttenuation(AntNum::Id antId, float atten);
      static void setDefaultOutputAttenuation(AntNum::Id antId, float atten);
      static void setDefaultTotalAttenuation(AntNum::Id antId, float atten);

      static float getDefaultInputAttenuation(AntNum::Id antId);
      static float getDefaultOutputAttenuation(AntNum::Id antId);
      static float getDefaultTotalAttenuation(AntNum::Id antId);

    private:

      static float inputAttens_[AntNum::NANT];
      static float outputAttens_[AntNum::NANT];
      static float totalAttens_[AntNum::NANT];

    }; // End class IFAtten
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_IFATTEN_H
