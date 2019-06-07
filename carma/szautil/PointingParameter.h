#ifndef SZA_UTIL_POINTINGPARAMETER_H
#define SZA_UTIL_POINTINGPARAMETER_H

/**
 * @file PointingParameter.h
 * 
 * Started: Wed Dec 17 20:12:42 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class PointingParameter {
      
    public:
      
      enum Parameter {
	NONE        = 0,
	
	// The longitude,latitude and altitude of the site 

	SITE        = 1,
	
	// The atmospheric refraction parameters 

	ATMOSPHERE  = 2,
	
	// The value of UT1-UTC 

	UT1UTC      = 4, 
	
	// The value of the equation of the equinoxes 

	EQNEQX      = 8,
	
	// The number of counts per turn of the encoders 

	ENCODERS    = 16,
	
	// Axis tilts 

	TILTS       = 32,
	
	// Collimation terms 

	COLLIMATION = 64,
	
	// Encoder limits 

	LIMITS      = 128,
	
	// The gravitational flexure of the mount 

	FLEXURE     = 256,
	
	// The zero points of the encoders 

	ZEROS       = 512,
	
	// The antenna-specific offsets

	LOCATION    = 1024,
	
	// The following should be the bitwise union of all of the above

	ALL         = 2047
      };

    }; // End class PointingParameter
      
  }; // End namespace util
}; // End namespace sza
  
#endif // End #ifndef 
