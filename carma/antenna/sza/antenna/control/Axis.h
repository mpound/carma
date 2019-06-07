#ifndef AXIS_H
#define AXIS_H

/**
 * @file Axis.h
 * 
 * Tagged: Thu Nov 13 16:53:32 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * The following class just enumerates valid axes
       */
      class Axis {
	
      public:
	
	/**
	 * An enumerator to identify a valid axis.
	 */
	enum Type {
	  NONE = 0x0, 
	  AZ   = 0x1,// Make these orthogonal bits, so that they can
	  // be OR'd together
	  EL   = 0x2,
	  PA   = 0x4,
	  ALL  = AZ|EL|PA
	} type_;
	
	/**
	 * Constructor.
	 */
	Axis(Type type);
	
	/**
	 * Return true if this represents a single telescope axis
	 */
	bool isValidSingleAxis();
	
      }; // End class Axis
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
