// $Id: HourAngle.h,v 1.16 2006/01/13 14:36:04 cgwon Exp $

/**
 * @file 
 * Representation of an hour angle (i.e. of a source), 
 * between -12 hours and 12 hours.
 *
 * @author Marc Pound
 * @version $Revision: 1.16 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_HOURANGLE_H
#define CARMA_SERVICES_HOURANGLE_H

#include "carma/services/Angle.h"
#include <iostream>
#include <string>

namespace carma  {
  namespace services {
    /**
     * The HourAngle class extends carma::services::Angle 
     * to specifically support hour angles.  
     * <br>
     * <tt>
     * HourAngle angle1(M_PI,"radians")<br>
     * HourAngle angle2(180.0,"degrees")<br>
     * </tt>
     * both <tt>angle1</tt> and <tt>angle2</tt> 
     * represent the same physical quantity.
     * Binary operations + and - are supported, as is the stream
     * operation <<.
     */
      class HourAngle : public Angle {
	public:

	    /**
	     * Construct an HourAngle given a value and units.
	     * @param value The value of this angle
	     * @param units The units of the value.
	     */
	    HourAngle(double value, 
		      const std::string& units);

	    /** Destructor */
	    virtual ~HourAngle();
	    
            double convert(const std::string& convertTo) const;

	    std::string getUnits() const;

	    /**
	     * Convenience method that returns the value of this
	     * angle in hours. Provides some backwards compatibility
	     * with sza::util::HourAngle. 
	     * @return angular value in hours
	     */
	    double hours() const;

	    /**
	     * Reset method to change value and/or units.
	     * This method is needed meant so that HourAngle
	     * can reassign "hours" to be "circle (hours/day)"
	     * so that Units will work transparently.
	     *
	     * @param value The value of this quantity
	     * @param units The units of the value.
	     */
	    void reset(double value, const std::string& units);

	    /**
	     * Add two HourAngles. 
	     * @return an HourAngle with radian units that is
	     * the sum of the two HourAngles modulo PI (i.e.
	     * between -12h and 12h).
	     * @throws ConformabilityException
	     */
	    const HourAngle operator+(const HourAngle& angle) const;

	    /**
	     * Subtract two HourAngles. 
	     * @return an HourAngle with radian units that is 
	     * the sum of the two angles modulo PI (i.e.
	     * between -12h and 12h).
	     * @throws ConformabilityException
	     */
	    const HourAngle operator-(const HourAngle& angle) const;

	    /**
	     * Increment HourAngle
	     * @return incremented HourAngle
	     */
	    HourAngle &operator+=(const HourAngle &frequency);

	    /**
	     * Decrement HourAngle
	     * @return decremented HourAngle
	     */
	    HourAngle &operator-=(const HourAngle &frequency);

	    /**
	     *   Return the HourAngle value as a string in +/-hh:mm:ss.s format
             *   @param precision    number of digits after the decimal point
	     */
	    std::string getString(int precision=0);

	    /**
	     *   Return LST for HourAngle value in +/-hh:mm:ss.s format
             *   @param precision    number of digits after the decimal point
	     */
	    std::string getLstString(int precision=0);

	    /**
	     *  return angle in a +/-hh:mm:ss.s format
	     * @param angle angle in _radians_
	     * @param precision number of digits after decimal point for secondsx
	     */
	    static inline std::string getAngleString(double angle, int precision=0) {
	      return Angle::getAngleString(angle, precision, false);
	    }

	    static std::string hms(double decimalHours, int precision);

	private:

	    /**
	     * Will return the value as an HourAngle between -12h and 12h.
	     * @return an HourAngle modulo 12 hours
	     */
	    HourAngle moduloPi(double value) const;
	    double moduloPiDouble(double value) const;
	    /**
	     * We need this because GNU units reserves "hours"
	     * for time.  It has "seclongitude" = "circle (seconds/day)"
	     * for right ascension, which is of less utility than
	     * "circle (hours/day)".   If the user specifies "hours"
	     * in the constructor we will subsititute hourUnits
	     
	     */
	    inline static std::string hourUnits()
                { 
		    return "circle (hours/day)"; 
		}

	    /**
	     * Check if a string means hours (one of: hours, hour, hrs, hr)
	     * @param units the string to check
	     * @return true if the units mean hours
	     */
	    bool isHours(const std::string& units) const;

	    std::string HOURS_;

      };

/**
 *  Define the << operator to allow, e.g. cout << HourAngle
 */
std::ostream& operator<<(std::ostream& os, 
			 const carma::services::HourAngle& hourangle);

  }
}



#endif //CARMA_SERVICES_HOURANGLE_H
