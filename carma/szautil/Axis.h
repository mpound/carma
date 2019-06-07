#ifndef SZA_UTIL_AXIS_H
#define SZA_UTIL_AXIS_H

/**
 * @file Axis.h
 * 
 * Tagged: Thu Nov 13 16:53:32 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
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
	AZ   = 0x1,// Make these orthogonal bits, so that they can be
		   // OR'd together
	EL   = 0x2,
	PA   = 0x4,
	BOTH = AZ|EL, // Some telescope don't have a PA axis
	ALL  = AZ|EL|PA
      } type_;
      
      /**
       * Constructor.
       */
      inline Axis(Type type) 
	{
	  type_ = type;
	};
      
      /**
       * Return true if this represents a single telescope axis
       */
      inline bool isValidSingleAxis()
	{
	  if(type_ == AZ || type_ == EL || type_ == PA)
	    return true;
	  return false;
	};
      
      /**
       * Return true if a requested axis is set in the mask
       */
      inline bool isSet(Type type)
	{
	  return (unsigned) type_ & (unsigned) type;
	};

    }; // End class Axis
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 
