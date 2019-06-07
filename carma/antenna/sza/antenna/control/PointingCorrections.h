#ifndef POINTINGCORRECTIONS_H
#define POINTINGCORRECTIONS_H

/**
 * @file PointingCorrections.h
 * 
 * Tagged: Thu Nov 13 16:53:47 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * While computing pointing corrections, an object of the
       * following type is used to communicate accumulated azimuth
       * and elevation corrections along with their sin() and cos(),
       * between the functions that implement the various correction
       * stages.
       */
      class PointingCorrections {
	
      public:
	
	/**
	 * Constructor.
	 */
	PointingCorrections();
	
	/**
	 * The apparent azimuth of the source
	 */
	double az;       
	
	/**
	 * The apparent elevation of the source
	 */
	double el;       
	
	/**
	 * The apparent parallactic angle of the source
	 */
	double pa;       
	
	/**
	 * The corrected latitude of the source
	 */
	double lat;      
	
	/**
	 * sin(az)
	 */
	double sin_az;   
	
	/**
	 * cos(az)
	 */
	double cos_az;   
	
	/**
	 * sin(el)
	 */
	double sin_el;   
	
	/**
	 * cos(el)
	 */
	double cos_el;   
	
	/**
	 * sin(lat)
	 */
	double sin_lat;  
	
	/**
	 * cos(lat)
	 */
	double cos_lat;  
	
      }; // End class PointingCorrections
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
