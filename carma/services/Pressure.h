// $Id: Pressure.h,v 1.2 2014/04/02 23:11:12 iws Exp $

/**
 * @file 
 * Representation of Pressure.
 *
 * @author Marc Pound
 * @version $Revision: 1.2 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_PRESSURE_H
#define CARMA_SERVICES_PRESSURE_H

#include "carma/services/ConformableQuantity.h"
#include "carma/services/Units.h"
#include <string>

namespace carma  {
  namespace services {
    /**
     * The Pressure class can represent any pressure in any units.
     * It uses the Units class internally to handle conversion
     * of any pressure unit to any other pressure unit. For example,
     * <br>
     * <tt>
     * Pressure pressure1(100.0,"mbar")<br>
     * Pressure pressure2(1.23,"atmospheres")<br>
     * </tt>
     * both <tt>pressure1</tt> and <tt>pressure2</tt> 
     * represent the same physical quantity.
     * Binary operations + and - are supported, as is the stream
     * operation <<.
     *
     * @see carma::services::ConformableQuantity
     */
      class Pressure : public ConformableQuantity {
	public:

	    /**
	     * Construct an Pressure given a value and units.
	     * @param value The value of this pressure
	     * @param units The units of the value.
	     */
	    Pressure(double value, const std::string& units);

	    /** Destructor */
	    virtual ~Pressure();
	    
	    /**
	     * Convenience method that returns the value of this
	     * pressure in millibars 
	     * @return pressure value in millibar
	     */
	    double millibar() const;

	    /**
	     * Convenience method that returns the value of this
	     * pressure in atmospheres
	     * @return pressure value in units of the standard atmosphere
	     */
	    double atmosphere() const;

	    /**
	     * Add two Pressures. 
	     * @return an Pressure with millibar units 
	     * @throws ConformabilityException
	     */
	    const Pressure operator+(const Pressure& pressure) const;

	    /**
	     * Subtract two Pressures. 
	     * @return an Pressure with millibar units 
	     * @throws ConformabilityException
	     */
	    const Pressure operator-(const Pressure& pressure) const;

	    /**
	     * Compare two pressures 
	     * @return boolean saying whether lhs pressure is less
	     * than rhs pressure
	     * @throws ConformabilityException
	     */
	    bool operator<(const Pressure &pressure) const;

	    /**
	     * Compare two pressures
	     * @return boolean saying whether lhs pressure is greater
	     * than rhs pressure
	     * @throws ConformabilityException
	     */
	    bool operator>(const Pressure &pressure) const;

	    /**
	     * Increment Pressure
	     * @return incremented Pressure
	     */
	    Pressure &operator+=(const Pressure &pressure);

	    /**
	     * Decrement Pressure
	     * @return decremented Pressure
	     */
	    Pressure &operator-=(const Pressure &pressure);

      };
/**
 *  Define the << operator to allow, e.g. cout << Pressure
 */
std::ostream& operator<<(std::ostream& os, 
			 const carma::services::Pressure& pressure);

  }
}

#endif //CARMA_SERVICES_PRESSURE_H
