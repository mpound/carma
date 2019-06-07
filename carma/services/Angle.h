// $Id: Angle.h,v 1.26 2007/07/06 14:26:39 abeard Exp $

/**
 * @file 
 * Representation of an angle, return values are always modulo 2PI radians
 *
 * @author Marc Pound
 * @version $Revision: 1.26 $
 *
 * $CarmaCopyright$
 */


#ifndef CARMA_SERVICES_ANGLE_H
#define CARMA_SERVICES_ANGLE_H

#include "carma/services/ConformableQuantity.h"
#include <string>
#include <cmath>

namespace carma  {
  namespace services {
    /**
     * The Angle class can represent any angle in any units.
     * It uses the Units class internally to handle conversion
     * of any angular unit to any other angular unit. For example,
     * <br>
     * <tt>
     * Angle angle1(M_PI,"radians")<br>
     * Angle angle2(180.0,"degrees")<br>
     * </tt>
     * both <tt>angle1</tt> and <tt>angle2</tt> 
     * represent the same physical quantity.
     * Binary operations + and - are supported, as is the stream
     * operation <<.
     * This class was inspired by Erik Leitch's sza::util::Angle.
     */
    class Angle : public ConformableQuantity {
    public:

      /**
       * Default constructor.
       * Constructs an angle of value 0.0.
       */
      explicit Angle( );

      /**
       * Construct an Angle given a value and units.
       * @param value The value of this angle
       * @param units The units of the value.
       */
      explicit Angle(double value, const std::string& units);

      /** Destructor */
      virtual ~Angle();
      
      /**
       * Convenience method that returns the value of this
       * angle in radians. 
       * @param modulo Whether or not to return the value 
       * modulo 2PI or not; <tt>false</tt> if you do 
       * want modulo 2PI; default is true;
       * @return angular value in radians, modulo 2PI if requested.
       */
      double radians(bool modulo = true) const;
      
      /**
       * Convenience method that returns the value of this
       * angle in hours.
       * @param modulo Whether or not to return the value 
       * modulo 24 hours or not; <tt>false</tt> if you do 
       * want modulo 24 hours; default is true;
       * @return angular value in hours (0..24 if requested)
       */
      double hours(bool modulo = true) const;
      
      /**
       * Convenience method that returns the value of this
       * angle in degrees
       * @param modulo Whether or not to return the value 
       * modulo 360 degrees or not; <tt>false</tt> if you do 
       * want modulo 360 degrees; default is FALSE;
       * @return angular value in hours (0..360 if requested)
       */
      double degrees(bool modulo = false) const;
      
      /**
       * Convenience method that returns the value of this
       * angle in arcminutes
       * @param modulo Whether or not to return the value 
       * modulo 21600 arcminutes (=360 degrees) or not; 
       * <tt>false</tt> if you do 
       * want modulo 21600 arcminutes ; default is FALSe;
       * @return angular value in arcminute
       */
      double arcMinutes(bool modulo = false) const;
      
      /**
       * Convenience method that returns the value of this
       * angle in arcseconds
       * @param modulo Whether or not to return the value 
       * modulo 1296000 arcseconds (=360 degrees) or not; 
       * <tt>false</tt> if you do 
       * want modulo 1296000 arcseconds; default is FALSE;
       * @return angular value in arcseconds
       */
      double arcSeconds(bool modulo = false) const;
      
      /**
       * @param modulo Whether or not to return the value 
       * modulo 360 degrees or not; <tt>false</tt> if you do 
       * want modulo 360 degrees; default is true;
       * @param precision digits to the right of the decimal for seconds
       *
       * @return This angle in degrees in sexagesimal format, +/-DD:MM:SS[.ss]
       */
      std::string dms(bool modulo = true, int precision = 0) const;

      /**
       * Set the value of this angle in radians.
       * The units will be reset to "radians".
       * @param radians the new value of the Angle in radians
       */
      inline void setRadians(double radians) 
      {
	reset(radians,"radians");
      }
      /**
       * Add two Angles. 
       * @return an Angle with radian units that is
       * the sum of the two Angles modulo 2PI.
       * @throws ConformabilityException
       */
      const Angle operator+(const Angle& angle) const;
      
      /**
       * Subtract two Angles. 
       * @return an Angle with radian units that is 
       * the difference of the two angles modulo 2PI.
       * @throws ConformabilityException
       */
      const Angle operator-(const Angle& angle) const;
      
      /**
       * Multiply an angle by a scalar.
       */
      friend const Angle operator*(double left, const Angle & right );
      friend const Angle operator*(const Angle & left, double right );

      /**
       * Divide an angle by a scalar.
       */
      const Angle operator/(double scalar) const;

      /**
       *  Add Angle
       *  @return an Angle with radian units that is 
       *  the sum of the two angles modulo 2PI.
       *  @throws ConformabilityException
       */
      Angle& operator+=(const Angle& angle);
      
      /**
       *  Subtract Angle
       *  @return an Angle with radian units that is 
       *  the difference of the two angles modulo 2PI.
       *  @throws ConformabilityException
       */
      Angle& operator-=(const Angle& angle);

      /**
       * Multiply an angle by a scalar
       */
      Angle & operator*=(double scalar);

      /**
       * Divide by a scalar
       */
      Angle & operator/=(double scalar);
      
      /**
       * Less than comparison.
       */
      bool operator<( const Angle & right ) const;
      
      /**
       * Greater than comparison.
       */
      bool operator>( const Angle & right ) const;

      /**
       * Less than or equal comparison.
       */
      bool operator<=( const Angle & right ) const;

      /**
       * Greater than or equal comparison.
       */
      bool operator>=( const Angle & right ) const;

      /**
       *   Return a zero filled string +/-ddd:mm:ss.s 
       *    @param angle        angle in radians
       *    @param precision    number of digits after the decimal point
       */
      static inline std::string getString(double angle, int precision=0) 
      {
	return getAngleString(angle, precision, true, true);
      }

    protected:
      /**
       * Two pi. Used for modulus function in this class and
       * subclasses.
       */
      static const double twoPi_     = 2 * M_PIl;
      
      /*
       * helper for Angle and HourAngle
       * @return (+/-) hh:mm:ss or dd:mm:ss, value will be leading zero-filled.
       * @param angle in radians
       * @param precision    number of digits after the decimal point
       * @param dms  true of dms, false is hms
       * @param use the leading sign if positive
       *
       */
      static std::string getAngleString(double a, 
	                                int precision=0, 
					bool dms=true, 
					bool useSign = false);
      
    public:

      /**
       * A const string that may be used for unit conversion.
       * "radians"
       */ 
      static const std::string RADIANS_STR;

      /**
       * A const string that may be used for unit conversion.
       * "degrees"
       */ 
      static const std::string DEGREES_STR;

      /**
       * A const string that may be used for unit conversion.
       * "hours"
       */ 
      static const std::string HOURS_STR;

      /**
       * A const string that may be used for unit conversion.
       * "arcminutes"
       */ 
      static const std::string ARCMIN_STR;

      /**
       * A const string that may be used for unit conversion.
       * "arcseconds"
       */ 
      static const std::string ARCSEC_STR;

    private:
      
      /**
       * @return an Angle modulo 2PI radians
       */
      const Angle moduloTwoPi(double value) const;
      
      /**
       * @return a double modulo 2PI radians
       */
      double moduloTwoPiDouble(double value) const;

    };

    /**
     *  Define the << operator to allow, e.g. cout << Angle
     */
    std::ostream& operator<<(std::ostream& os, 
			     const carma::services::Angle& angle);
    
      
    const Angle operator*(double left, const Angle & right );
    const Angle operator*(const Angle & left, double right );
  }
}


#endif //CARMA_SERVICES_ANGLE_H
