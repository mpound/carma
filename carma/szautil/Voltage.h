#ifndef SZA_UTIL_VOLTAGE_H
#define SZA_UTIL_VOLTAGE_H

/**
 * @file Voltage.h
 * 
 * Tagged: Tue Mar 30 08:59:30 PST 2004
 * 
 * @author Erik Leitch
 */
#include <iostream>

#include "carma/szautil/ConformableQuantity.h"

namespace sza {
  namespace util {
    
    class Voltage : public ConformableQuantity {
    public:
      
      enum Id {
	DRAIN,
	GATE
      };

      static const double centiVoltsPerVolt_;
      static const double milliVoltsPerVolt_;

      class Volts {};
      class CentiVolts {};
      class MilliVolts {};

      // Constructors

      Voltage();
      Voltage(const Volts& units, double volts);
      Voltage(const CentiVolts& units, double centiVolts);
      Voltage(const MilliVolts& units, double milliVolts);

      // Destructor

      virtual ~Voltage();

      // Set methods

      void setVolts(double volts);
      void setCentiVolts(double centiVolts);
      void setMilliVolts(double milliVolts);

      // Return the frequency, in GHz

      inline double volts() {
	return volts_;
      }

      inline double centiVolts() {
	return volts_ * centiVoltsPerVolt_;;
      }

      inline double milliVolts() {
	return volts_ * milliVoltsPerVolt_;;
      }

      inline unsigned short gunnUnits() {
	return (unsigned short)centiVolts();
      }

      friend std::ostream& operator<<(std::ostream& os, Voltage& voltage);

    private:

      double volts_;

      void initialize();

    }; // End class Voltage
    
    std::ostream& operator<<(std::ostream& os, Voltage& voltage);

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_VOLTAGE_H
