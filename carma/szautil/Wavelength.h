#ifndef SZA_UTIL_WAVELENGTH_H
#define SZA_UTIL_WAVELENGTH_H

/**
 * @file Wavelength.h
 * 
 * Tagged: Wed Dec  1 11:58:54 PST 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Length.h"
#include "carma/szautil/Speed.h"

namespace sza {
  namespace util {
    
    class Frequency;

    class Wavelength : public Length {
    public:
      
      class Angstroms {};

      static const double cmPerAngstrom_;
      static Speed lightSpeed_;

      /**
       * Constructor.
       */
      Wavelength();
      Wavelength(const Frequency& frequency);
      Wavelength(const Length::Centimeters& units, double cm);
      Wavelength(const Microns& units, double microns);
      
      /**
       * Destructor.
       */
      virtual ~Wavelength();
      
      void setFrequency(Frequency& freq);
      void setFrequency(const Frequency& freq);

      void setMicrons(double microns);
      
      void setAngstroms(double angstroms);
    
      double microns();

      double angstroms();

      void initialize();

    }; // End class Wavelength
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_WAVELENGTH_H
