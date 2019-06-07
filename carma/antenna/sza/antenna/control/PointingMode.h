#ifndef POINTINGMODE_H
#define POINTINGMODE_H

/**
 * @file PointingMode.h
 * 
 * Tagged: Thu Nov 13 16:53:47 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Define a class to specify a pointing mode
       */
      class PointingMode {
	
      public:
	
	/**
	 * Enumerate possible pointing modes.
	 */
	enum Type {
	  OPTICAL,
	  RADIO
	};
	
      }; // End class PointingMode
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
