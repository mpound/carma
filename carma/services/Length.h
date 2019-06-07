// $Id: Length.h,v 1.6 2006/01/20 16:46:40 mpound Exp $

/**
 * @file 
 * Representation of Length in any units.
 *
 * @author Marc Pound
 * @version $Revision: 1.6 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_LENGTH_H
#define CARMA_SERVICES_LENGTH_H

#include "carma/services/ConformableQuantity.h"
#include <iostream>
#include <string>

namespace carma  {
  namespace services {
    /**
     * The Length class can represent a length in any units.
     * It uses the Units class internally to handle conversion
     * of any length unit to any other length unit. For example,
     * <br>
     * <tt>
     * Length length1(1000.0,"m")<br>
     * Length length2(1.0,"km")<br>
     * </tt>
     * both <tt>length1</tt> and <tt>length2</tt> 
     * represent the same physical quantity.
     * Binary operations + and - are supported, as is the stream
     * operation <<.
     */
      class Length : public ConformableQuantity {
	public:

	    /**
	     * Construct an Length given a value and units.
	     * @param value The value of this length
	     * @param units The units of the value.
	     */
	    Length(double value, const std::string& units);

	    /** Destructor */
	    virtual ~Length();
	    
	    /**
	     * Convenience method to return value in meters
	     */
	    double meters() const {
		return convert("meter");
	    }

	    /**
	     * Convenience method to return value in millimeters
	     */
	    double millimeters() const {
		return convert("millimeter");
	    }

	    /**
	     * Add two Lengths. 
	     * @return a Length with kilometer units that is
	     * the sum of the two Lengths.
	     * @throws ConformabilityException
	     */
	    const Length operator+(const Length& length) const;

	    /**
	     * Subtract two Lengths. 
	     * @return an Length with kilometer units that is 
	     * the difference of the two lengths
	     * @throws ConformabilityException
	     */
	    const Length operator-(const Length& length) const;

	    /**
	     * Increment Length
	     * @return incremented Length
	     */
	    Length &operator+=(const Length &frequency);

	    /**
	     * Decrement Length
	     * @return decremented Length
	     */
	    Length &operator-=(const Length &frequency);
      };
/**
 *  Define the << operator to allow, e.g. cout << Length
 */
      std::ostream& operator<<(std::ostream& os, 
			       const carma::services::Length& length);

  }
}


#endif //CARMA_SERVICES_LENGTH_H
