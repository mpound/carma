// $Id: Sort.h,v 1.2 2013/08/20 21:56:51 eml Exp $

#ifndef SZA_UTIL_SORT_H
#define SZA_UTIL_SORT_H

/**
 * @file Sort.h
 * 
 * Tagged: Wed Feb 14 09:35:58 NZDT 2007
 * 
 * @version: $Revision: 1.2 $, $Date: 2013/08/20 21:56:51 $
 * 
 * @author Erik Leitch
 */

#include <iostream>
#include <vector>
#include <string>

namespace sza {
  namespace util {

    class Sort {
    public:

      /**
       * Constructor.
       */
      Sort();

      /**
       * Destructor.
       */
      virtual ~Sort();

      static std::vector<std::string> sort(std::vector<std::string>& entries);
      static std::vector<std::string> sortNumeric(std::vector<std::string>& entries);

    }; // End class Sort

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SORT_H
