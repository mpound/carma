// $Id: Temperature.h,v 1.5 2014/04/02 23:11:12 iws Exp $

/**
 * @file 
 * carma/services/Temperature.h
 * Representation of Temperature
 *
 * @author Peter Teuben
 * @version $Revision: 1.5 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_TEMPERATURE_H
#define CARMA_SERVICES_TEMPERATURE_H

#include "carma/services/ConformableQuantity.h"
#include "carma/services/Units.h"
#include <string>

namespace carma  {
  namespace services {
    /**
     * The Temperature class represents a temperature in any unit.
     * It uses the Units class internally to handle conversion
     * of any temperature unit to any other temperature unit. For example,
     * <br>
     * <tt>
     * Temperature t1(32,"F")<br>
     * Temperature t2(0,"C")<br>
     * </tt>
     * both <tt>t1</tt> and <tt>t2</tt> 
     * represent the same physical quantity.
     * Binary operations + and - are supported, as is the stream
     * operation <<.
     *
     * @see carma::services::ConformableQuantity
     */
      class Temperature : public ConformableQuantity {
	public:

	    /**
	     * Construct a Temperature given a value and units.
	     * @param value The value of this temperature
	     * @param units The units of the value.
	     * @throw carma::util::IllegalArgumentException if the
	     * value is converts to less than 0 on the Kelvin scale.
	     */
	    Temperature(double value, const std::string& units);

	    /** Destructor */
	    virtual ~Temperature();
	    
	    /**
	     * Override parent method
	     * @param value The value of this temperature
	     * @param units The units of the value.
	     * @throw carma::util::IllegalArgumentException if the
	     * value is converts to less than 0 on the Kelvin scale.
	     */
	    void reset(double value, const std::string& units);

	    /**
	     * Convenience method that returns the value of this
	     * temperature in Celsius
	     * @return temperature value in Celsius
	     */
	    double celsius() const;

	    /**
	     * Convenience method that returns the value of this
	     * temperature in fahrenheit
	     * @return temperature value in fahrenheit
	     */
	    double fahrenheit() const;

	    /**
	     * Convenience method that returns the value of this
	     * temperature in Kelvin.
	     * @return temperature value in Kelvin
	     */
	    double kelvin() const;

	    /**
	     * Add two Temperatures
	     * @return a temperature with Kelvin units that is
	     * @throws ConformabilityException
	     */
	    const Temperature operator+(const Temperature& t) const;

	    /**
	     * Subtract two temperatures
	     * @return a temperature with kelvin units
	     * @throws ConformabilityException
	     */
	    const Temperature operator-(const Temperature& t) const;

	    /**
	     * Compare two temperatures
	     * @return boolean saying whether lhs temperature is less
	     * than rhs temperature 
	     * @throws ConformabilityException
	     */
	    bool operator<(const Temperature &t) const;

	    /**
	     * Compare two temperature 
	     * @return boolean saying whether lhs temperature is greater
	     * than rhs temperature 
	     * @throws ConformabilityException
	     */
	    bool operator>(const Temperature &t) const;

	    /**
	     * Increment temperature 
	     * @return incremented temperature 
	     */
	    Temperature &operator+=(const Temperature &t);

	    /**
	     * Decrement temperature 
	     * @return decremented temperature 
	     */
	    Temperature &operator-=(const Temperature &t);

      };
/**
 *  Define the << operator to allow, e.g. cout << Temperature
 */
std::ostream& operator<<(std::ostream& os, 
			 const carma::services::Temperature& t);

  }
}


#endif //CARMA_SERVICES_TEMPERATURE_H
