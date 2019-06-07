// $Id: FunctionName.h,v 1.1 2010/12/13 21:06:30 eml Exp $

#ifndef SZA_UTIL_FUNCTIONNAME_H
#define SZA_UTIL_FUNCTIONNAME_H

/**
 * @file FunctionName.h
 * 
 * Tagged: Sat Jan 20 22:15:41 NZDT 2007
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:30 $
 * 
 * @author Erik Leitch
 */

#include <iostream>

namespace sza {
  namespace util {

    class FunctionName {
    public:

      /**
       * Constructor.
       */
      FunctionName(const char* prettyFunction);

      /**
       * Destructor.
       */
      virtual ~FunctionName();

      std::string prettyFunction();
      std::string noArgs();

      static std::string prettyFunction(const char* prettyFunction);
      static std::string noArgs(const char* prettyFunction);

    private:

      std::string prettyFunction_;

    }; // End class FunctionName

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_FUNCTIONNAME_H
