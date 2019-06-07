#ifndef SZA_UTIL_COUT_H
#define SZA_UTIL_COUT_H

/**
 * @file Cout.h
 * 
 * Tagged: Fri May 14 11:16:46 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <iostream>

namespace sza {
  namespace util {
    
    class Cout : public std::ostream {
    public:
      
      /**
       * Constructor.
       */
      Cout(std::ostream& os);
      
      /**
       * Destructor.
       */
      virtual ~Cout();
      
    private:
    }; // End class Cout
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_COUT_H
