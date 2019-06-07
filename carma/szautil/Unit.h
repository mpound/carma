// $Id: Unit.h,v 1.1 2010/12/13 21:06:33 eml Exp $

#ifndef SZA_UTIL_UNIT_H
#define SZA_UTIL_UNIT_H

/**
 * @file Unit.h
 * 
 * Tagged: Sun Oct 22 19:22:27 PDT 2006
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:33 $
 * 
 * @author username: Command not found.
 */
#include <string>
#include <vector>

namespace sza {
  namespace util {

    // This class is intended as a base-class for all unit classes
    // defined in inheritors of ConformableQuantity class

    class Unit {
    public:

      /**
       * Constructor.
       */
      Unit();

      /**
       * Destructor.
       */
      virtual ~Unit();

      // Return true if the passed name is a recognized name for this
      // unit

      bool isThisUnit(std::string unitName);

    protected:

      // A collection of valid name for this unit

      std::vector<std::string> names_;
	
      // Add a name to this list

      void addName(std::string name);

      virtual void addNames();

    }; // End class Unit

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_UNIT_H
