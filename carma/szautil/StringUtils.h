#ifndef SZA_UTIL_STRINGUTILS_H
#define SZA_UTIL_STRINGUTILS_H

/**
 * @file StringUtils.h
 * 
 * Tagged: Wed May 12 07:32:59 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <string>

namespace sza {
  namespace util {
    
    void strip(std::string& targetStr, std::string& stripStr);

    void strip(std::string& targetStr, char stripChar);

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_STRINGUTILS_H
