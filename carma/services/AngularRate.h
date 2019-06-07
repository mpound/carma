// $Id: AngularRate.h,v 1.3 2005/06/06 20:37:52 mpound Exp $

/**
 * @file 
 * Representation of AngularRate in any units.
 *
 * @author Marc Pound
 * @version $Revision: 1.3 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_ANGULARRATE_H
#define CARMA_SERVICES_ANGULARRATE_H

#include "carma/services/ConformableQuantity.h"
#include <iostream>
#include <string>

namespace carma  {
  namespace services {

    /**
     * The AngularRate class can represent an angular rate in any units.
     * It uses the Units class internally to handle conversion
     * of any angular rate unit to any other angular rate unit. For example,
     * <br>
     * <tt>
     * AngularRate ar1(1.0,"degrees/s")<br>
     * AngularRate ar2(1.0227,"radians/s")<br>
     * </tt>
     * both <tt>ar1</tt> and <tt>ar2</tt> 
     * represent the (approximately!) the same physical quantity.
     * Binary operations + and - are supported, as is the stream
     * operation <<.
     */
      class AngularRate : public ConformableQuantity {
	public:

	    /**
	     * Construct an AngularRate given a value and units.
	     * @param value The value of this angular rate 
	     * @param units The units of the value.
	     */
	    AngularRate(double value, 
                    const std::string& units);


	    /** Destructor */
	    virtual ~AngularRate();
	    
	    /**
	     * Add two Angular rates. 
	     * @return a AngularRate with rad/s units that is
	     * the sum of the two Angular rates.
	     * @throws ConformabilityException
	     */
	   const AngularRate operator+(const AngularRate& ar) const;

	    /**
	     * Subtract two Angular rates. 
	     * @return an AngularRate with rad/s units that is 
	     * the difference of the two Angular rates
	     * @throws ConformabilityException
	     */
	   const AngularRate operator-(const AngularRate& ar) const;

	    /**
	     * Increment AngularRate
	     * @return incremented AngularRate
	     */
	    AngularRate &operator+=(const AngularRate &ar);

	    /**
	     * Decrement AngularRate
	     * @return decremented AngularRate
	     */
	    AngularRate &operator-=(const AngularRate &ar);

      };

/**
 *  Define the << operator to allow, e.g. cout << AngularRate
 */
std::ostream& operator<<(std::ostream& os, 
			 const carma::services::AngularRate& ar);
  }
}


#endif //CARMA_SERVICES_ANGULARRATE_H
