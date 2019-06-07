#ifndef CARMA_SERVICES_DECANGLE_H
#define CARMA_SERVICES_DECANGLE_H

/**
 * @file 
 *
 * Representation of Declination Angle. This differs from
 * and Angle in that values greater 90 degrees and less than
 * -90 degrees are not allowed.  If any operation results
 * in a DecAngle outside the valid range, a ConformabilityException
 * is thrown.
 * 
 * @author Marc Pound
 * @version $Revision $
 */
#include "carma/services/ConformableQuantity.h"
#include "carma/services/Angle.h"

namespace carma {
  namespace services {
    
    class DecAngle : public Angle {
    public:
      
      /**
       * Constructor.
       */
      explicit DecAngle(double value, const std::string& units);
      
      /**
       * Destructor.
       */
      virtual ~DecAngle();

      std::string dms(int precision = 0) const ;

      /**
       * Add two DecAngles. 
       * @return a DecAngle with radian units 
       * @throws ConformabilityException if operation would result
       * in a DecAngle greater than 90 degrees or less than -90 degrees.
       */
      const DecAngle operator+(const DecAngle& angle) const;
      
      /**
       * Subtract two DecAngles. 
       * @return a DecAngle with radian units 
       * @throws ConformabilityException if operation would result
       * in a DecAngle greater than 90 degrees or less than -90 degrees.
       */
      const DecAngle operator-(const DecAngle& angle) const;
      
      /**
       * Add DecAngle
       * @return an DecAngle with radian units.
       * @throws ConformabilityException if operation would result
       * in a DecAngle greater than 90 degrees or less than -90 degrees.
       */
      DecAngle& operator+=(const DecAngle& angle);
      
      /**
       * Subtract DecAngle
       * @return an DecAngle with radian units.
       * @throws ConformabilityException if operation would result
       * in a DecAngle greater than 90 degrees or less than -90 degrees.
       * @throws ConformabilityException
       */
      DecAngle& operator-=(const DecAngle& angle);
      
 
    private:

      /** 
       * check that the value is in the valid declination range
       * of -90 to 90 degrees
       * @param value The value of the angle in radians
       * @return the value bounded by -PI/2 to PI/2 radians
       * or throw exception if impossible
       */
      double checkValue(double value ) const;

    };
    
    /**
     *  Define the << operator to allow, e.g. cout << DecAngle
     */
    std::ostream& operator<<(std::ostream& os, 
			     const carma::services::DecAngle& angle);
  } // End namespace services

} // End namespace carma



#endif // End #ifndef CARMA_SERVICES_DECANGLE_H
