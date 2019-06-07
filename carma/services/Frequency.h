// $Id: Frequency.h,v 1.8 2014/04/02 23:11:12 iws Exp $

/**
 * @file 
 * carma/services/Frequency.h
 * Representation of Frequency.
 *
 * @author Marc Pound
 * @version $Revision: 1.8 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_FREQUENCY_H
#define CARMA_SERVICES_FREQUENCY_H

#include "carma/services/ConformableQuantity.h"
#include "carma/services/Units.h"
#include <string>

namespace carma  {
  namespace services {
    /**
     * The Frequency class can represent any frequency in any units.
     * It uses the Units class internally to handle conversion
     * of any frequency unit to any other frequency unit. For example,
     * <br>
     * <tt>
     * Frequency frequency1(100.0,"GHz")<br>
     * Frequency frequency2(1E9,"Hz")<br>
     * </tt>
     * both <tt>frequency1</tt> and <tt>frequency2</tt> 
     * represent the same physical quantity.
     * Binary operations + and - are supported, as is the stream
     * operation <<.
     *
     * @see carma::services::ConformableQuantity
     */
      class Frequency : public ConformableQuantity {
	public:

	    /**
	     * Construct an Frequency given a value and units.
	     * @param value The value of this frequency
	     * @param units The units of the value.
	     */
	    Frequency(double value, const std::string& units);

	    /** Destructor */
	    virtual ~Frequency();
	    
	    /**
	     * Convenience method that returns the value of this
	     * frequency in Hertz
	     * @return frequency value in Hertz
	     */
	    double hertz() const;

	    /**
	     * Convenience method that returns the value of this
	     * frequency in GHz
	     * @return frequency value in Gigahertz
	     */
	    double gigahertz() const;

	    /**
	     * Convenience method that returns the value of this
	     * frequency in millihertz. This method may be of
	     * use to the loberotator, which passes phase rate
	     * to the XAC in Millihertz  
	     * @return frequency value in Millihertz
	     */
	    double millihertz() const;

	    /**
	     * Convenience method that returns the value of this
	     * frequency in millihertz. This method may be of
	     * use to the loberotator, which passes phase rate
	     * to the XAC in Megahertz
	     * @return frequency value in Megahertz
	     */

	    double megahertz() const;

	    /**
	     * Convenience method that returns the value of this
	     * the wavelength equivalent of this frequency.
	     *
	     * @param units the desired wavelength units
	     * @return wavelength in desired units
	     */
	    double wavelength(const std::string& units) const;

	    /**
	     * Add two Frequencys. 
	     * @return an Frequency with Hz units 
	     * @throws ConformabilityException
	     */
	    const Frequency operator+(const Frequency& frequency) const;

	    /**
	     * Subtract two Frequencys. 
	     * @return an Frequency with Hz units 
	     * @throws ConformabilityException
	     */
	    const Frequency operator-(const Frequency& frequency) const;

	    /**
	     * Compare two frequencies
	     * @return boolean saying whether lhs frequency is less
	     * than rhs frequency
	     * @throws ConformabilityException
	     */
	    bool operator<(const Frequency &frequency) const;

	    /**
	     * Compare two frequencies
	     * @return boolean saying whether lhs frequency is greater
	     * than rhs frequency
	     * @throws ConformabilityException
	     */
	    bool operator>(const Frequency &frequency) const;

	    /**
	     * Increment Frequency
	     * @return incremented Frequency
	     */
	    Frequency &operator+=(const Frequency &frequency);

	    /**
	     * Decrement Frequency
	     * @return decremented Frequency
	     */
	    Frequency &operator-=(const Frequency &frequency);

      };
/**
 *  Define the << operator to allow, e.g. cout << Frequency
 */
std::ostream& operator<<(std::ostream& os, 
			 const carma::services::Frequency& frequency);

  }
}


#endif //CARMA_SERVICES_FREQUENCY_H
