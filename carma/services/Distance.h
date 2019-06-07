// $Id: Distance.h,v 1.7 2004/10/03 19:46:28 mpound Exp $

/**
 * @file 
 * Representation of Distance in any units.
 *
 * @author Marc Pound
 * @version $Revision: 1.7 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_DISTANCE_H
#define CARMA_SERVICES_DISTANCE_H

#include "carma/services/ConformableQuantity.h"
#include "carma/services/Length.h"
#include "carma/services/Angle.h"
#include <iostream>
#include <string>

namespace carma  {
  namespace services {
    /**
     * The Distance class can represent an distance in any units.
     * Distance subclasses Length--the main difference is that
     * distances are not allowed to be negative.
     * As with other Conformable Quantities, Distance 
     * uses the Units class internally to handle conversion
     * of any distance unit to any other distance unit. For example,
     * <br>
     * <tt>
     * Distance distance1(149598000.0,"km")<br>
     * Distance distance2(1.0,"au")<br>
     * </tt>
     * both <tt>distance1</tt> and <tt>distance2</tt> 
     * represent the same physical quantity.
     * Binary operations + and - are supported, as is the stream
     * operation <<.
     * @see carma::services::ConformableQuantity
     * @see carma::services::Length
     */
      class Distance : public Length {
	public:

	    /**
	     * Construct an Distance given a value and units.
	     * @param value The non-negative value of this distance
	     * @param units The units of the value.
	     * @throw IllegalArgumentException if value < 0.
	     */
	    Distance(double value, const std::string& units);

	    /** Destructor */
	    virtual ~Distance();
	    

	    /**
	     * Get the equivalent trigonometric parallax for this distance.
	     * It is returned in the Angle container, with
	     * default units "arcsec".<br>
	     * Parallax(arcsec) = 1 / Distance[pc]
	     * If isInfinite() is true, a zero parallax is guaranteed.
	     *
	     * @see Distance::isInfinite()
	     */
	    const Angle getParallax() const; 

	    /**
	     * Given an parallax angle, return the equivalent distance.
	     * @param parallax An Angle representing the parallax
	     * @return A Distance object containing the distance to the
	     * source in parsec. 
	     */
	    static Distance getDistance(const Angle& parallax);

	    /*
	     * @return True if this represents an ''infinite'' distance.
	     * That is, it is far away enough that there is no
	     * parallax.  The CARMA convention is that (distance == 0) 
	     * means infinite.
	     */
	    bool isInfinite() const;

	    /**
	     * Add two Distances. 
	     * @return a Distance with kilometer units that is
	     * the sum of the two Distances.
	     * @throws ConformabilityException
	     */
	    const Distance operator+(const Distance& distance) const;

	    /**
	     * Subtract two Distances. 
	     * @return an Distance with kilometer units that is 
	     * the difference of the two distances
	     * @throws ConformabilityException
	     */
	    const Distance operator-(const Distance& distance) const;

	    /**
	     * Increment Distance
	     * @return incremented Distance
	     */
	    Distance &operator+=(const Distance &distance);

	    /**
	     * Decrement Distance
	     * @return decremented Distance
	     */
	    Distance &operator-=(const Distance &distance);

      };
/**
 *  Define the << operator to allow, e.g. cout << Distance
 */
      std::ostream& operator<<(std::ostream& os, 
			       const carma::services::Distance& distance);

  }
}


#endif //CARMA_SERVICES_DISTANCE_H
