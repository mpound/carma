#ifndef SZA_UTIL_FREQUENCY_H
#define SZA_UTIL_FREQUENCY_H

/**
 * @file Frequency.h
 * 
 * Tagged: Fri Aug 20 12:44:45 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <iostream>

#include "carma/szautil/ConformableQuantity.h"
#include "carma/szautil/Speed.h"

namespace sza {
  namespace util {
    
    class Rx;
    class Wavelength;

    class Frequency : public ConformableQuantity {
    public:
      
      // A few useful conversions

      static const double HzPerGHz_;
      static const double HzPerMHz_;
      static Speed lightSpeed_;

      class MegaHz {};
      class GigaHz {};

      /**
       * Constructor.
       */
      Frequency();
      Frequency(const MegaHz& units, double MHz);
      Frequency(const GigaHz& units, double GHz);
      Frequency(Wavelength& wavelength);
      
      /**
       * Destructor.
       */
      virtual ~Frequency();
      
      virtual void setVal(double val, std::string units);

      // Set the frequency, in GHz

      void setGHz(double GHz);

      // Set the frequency, in MHz

      void setMHz(double MHz);

      // Set the frequency, in MHz

      void setHz(double Hz);

      // Return the frequency, in GHz

      inline double GHz() {
	return Hz_ / HzPerGHz_;
      }

      // Return the frequency, in MHz

      inline double MHz() {
	return Hz_ / HzPerMHz_;
      }

      double microns();
      double centimeters();
      double meters();

      Wavelength wavelength();

      inline unsigned short yigUnits() {
	return (unsigned short)MHz();
      }

      inline unsigned short gunnUnits() {
	return (unsigned short)(GHz() * 100);
      }

      // Return the frequency, in Hz

      inline double Hz() const {
	return Hz_;
      }

      /**
       * Allows cout << Length
       */
      friend std::ostream& operator<<(std::ostream& os, Frequency& frequency);

      Frequency operator-(Frequency& frequency);
      Frequency operator+(Frequency& frequency);
      bool operator<(Frequency& frequency);
      bool operator>(Frequency& frequency);

      void initialize();

    private:

      friend class Rx;
      friend class CorrelatorBand;

      // Constructor -- only Rx can call this constructor

      Frequency(double Hz);

      // The actual frequency, in Hz

      double Hz_;

    }; // End class Frequency
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_FREQUENCY_H
