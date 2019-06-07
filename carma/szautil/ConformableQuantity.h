#ifndef SZA_UTIL_CONFORMABLEQUANTITY_H
#define SZA_UTIL_CONFORMABLEQUANTITY_H

/**
 * @file ConformableQuantity.h
 * 
 * Tagged: Wed Dec  1 11:44:54 PST 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Exception.h"

namespace sza {
  namespace util {
    
    // A pure interface for unit'ed quantities

    class ConformableQuantity {
    public:
      
      /**
       * Constructor.
       */
      ConformableQuantity() {};
      
      /**
       * Destructor.
       */
      virtual ~ConformableQuantity() {};
      
      virtual void initialize() {};

      bool isFinite() {
	return finite_;
      }

    protected:
      
      virtual void setVal(double val, std::string units) {
	ThrowError("Undefined call to base-class setVal method");
      }

      void setFinite(bool finite) {
	finite_ = finite;
      }

      bool finite_;

    }; // End class ConformableQuantity
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CONFORMABLEQUANTITY_H
