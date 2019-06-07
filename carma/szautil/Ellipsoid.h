#ifndef SZA_UTIL_ELLIPSOID_H
#define SZA_UTIL_ELLIPSOID_H

/**
 * @file Ellipsoid.h
 * 
 * Tagged: Wed Aug 25 02:50:06 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Angle.h"
#include "carma/szautil/Length.h"

namespace sza {
  namespace util {
    
    class Ellipsoid {
    public:
      
      /**
       * Constructors.
       */
      Ellipsoid();
      Ellipsoid(Length majorAxis, Length minorAxis);
      Ellipsoid(Length majorAxis, double flattening);
      
      /**
       * Destructor.
       */
      virtual ~Ellipsoid();
      
      void setMajorAxisAndFlattening(Length majorAxis, double flattening);
      void setMajorMinorAxis(Length majorAxis, Length minorAxis);

      /**
       * Return the length of the vector from the center of the
       * ellipsoid to the surface at a given ellipsoidal latitude
       */
      virtual double flattening();
      Length majorAxis();
      Length minorAxis();

      /**
       * Return the eccentricity e, and e^2
       */
      virtual double firstEccentricity();
      virtual double firstEccentricitySquared();
      virtual double secondEccentricity();
      virtual double secondEccentricitySquared();

      /**
       * Return the length of the radius vector at a given ellipsoidal
       * latitude
       */
      Length radius(Angle latitude);
      
    private:

      bool initialized_;

      // The semi-major and semi-minor axes of this ellipsoid

      Length a_;
      Length b_;

      void checkInitialization();

    }; // End class Ellipsoid
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ELLIPSOID_H
