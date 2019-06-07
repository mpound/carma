// $Id: ConformableQuantity.h,v 1.7 2005/06/06 20:37:52 mpound Exp $

/**
 * @file carma/services/ConformableQuantity.h
 * Parent class for representations of quantities that
 * can have conformable units, e.g. Angle and Distance.
 *
 * @author Marc Pound
 * @version $Revision: 1.7 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_CONFORMABLEQUANTITY_H
#define CARMA_SERVICES_CONFORMABLEQUANTITY_H

#include "carma/services/Units.h"
#include <iostream>
#include <string>

namespace carma  {
  namespace services {
    /**
     * The ConformableQuantity class can represent any quantity
     * that can be expressed in many units.  It is meant to be
     * subclassed to represent specific physical quantities
     * like angle and distance. 
     * It uses the carma::services::Units class internally to handle conversion
     * of any angular unit to any other angular unit. 
     * Binary operations + and - are supported, as is the stream
     * operation <<.
     * @todo (low priority) overload other operators
     */
      class ConformableQuantity {
	public:

	    /**
	     * Construct an ConformableQuantity given a value and units.
	     * @param value The value of this quantity
	     * @param units The units of the value.
	     */
	    explicit ConformableQuantity(double value, 
		                         const std::string& units);

	    /** Destructor */
	    virtual ~ConformableQuantity();
	    
	    /**
	     * Converts this quantity to any conformable units.
	     * This method is virtual in case subclasses need to
	     * munge the input variable for conformability.
	     * (e.g. HourAngle)
	     *
	     * @param convertTo A string representing the units to
	     * convert to.
	     * @return the value of this ConformableQuantity converted to
	     * the desired units
	     * @throw ConformabilityException
	     */
	    virtual double convert(const std::string& convertTo) const;

	    /**
	     * @return the value of this ConformableQuantity in the units
	     * when was instantiated.
	     */
	    double getValue() const 
	    {
		return value_;
	    } 

	    /**
	     * @return the units of this ConformableQuantity when 
	     * it was instantiated.
	     */
	    virtual std::string getUnits() const;

	    /**
	     * Add two ConformableQuantities. 
	     * @return an ConformableQuantity with radian units that is
	     * the sum of the two ConformableQuantities modulo 2PI.
	     * @throws ConformabilityException
	     */
	    virtual const ConformableQuantity 
		operator+(const ConformableQuantity& quantity) const;

	    /**
	     * Subtract two ConformableQuantities
	     * @return an ConformableQuantity with radian units that is 
	     * the sum of the two quantitys modulo 2PI.
	     * @throws ConformabilityException
	     */
	    virtual const ConformableQuantity 
		operator-(const ConformableQuantity& quantity) const;

        /**
         *  Add ConformableQuantity to itself
	     *  @return a ConformableQuantity in same units
	     *  @throws ConformabilityException
	     */
	    virtual ConformableQuantity& 
		operator+=(const ConformableQuantity& quantity);

        /**
         *  Subtract ConformableQuantity from itself
	     *  @return a ConformableQuantity in same units
	     *  @throws ConformabilityException
	     */
	    virtual ConformableQuantity& 
		operator-=(const ConformableQuantity& quantity);

	    /**
	     * Reset method to change value and/or units.
	     * This method is needed meant so that HourAngle
	     * can reassign "hours" to be "circle (hours/day)"
	     * so that Units will work transparently.
	     * note: no longer protected since lack of default
	     * constructor and unit specification insures conformability
	     *
	     * @param value The value of this quantity
	     * @param units The units of the value.
	     */
	    virtual void reset(double value, const std::string& units);

	protected:
	    /**
	     * Used for converting between units
	     */
	    Units u_;  

	private:

	    /**
	     * The base value of this ConformableQuantity
	     */
	    double value_; 

	    /**
	     *
	     * The base units of this ConformableQuantity
	     */
	    std::string units_;

      };

/**
 *  Define the << operator to allow, e.g. cout << ConformableQuantity.
 */
std::ostream& operator<<(std::ostream& os, 
			 ConformableQuantity& quantity);

  } // namespace services
} // namespace carma

#endif //CARMA_SERVICES_CONFORMABLEQUANTITY_H
