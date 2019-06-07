#ifndef SZA_UTIL_ARRAYMAPBASE_H
#define SZA_UTIL_ARRAYMAPBASE_H

/**
 * @file ArrayMapBase.h
 * 
 * Tagged: Wed Oct  6 11:04:54 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szaarrayutils/szaregs.h"

namespace sza {
  namespace util {
    
    // An object which any class needing a copy of the SZA Array map
    // should instantiate.  This class manages a single static copy of
    // the SZA array map, which is allocated on the first construction
    // of this class, and deleted only when the last reference to any
    // such object is deleted.

    class ArrayMapBase {
    public:
      
      /**
       * Constructor.
       */
      ArrayMapBase();
      
      /**
       * Copy constructor
       */
      ArrayMapBase(ArrayMapBase& arrayMap);

      /**
       * Destructor.
       */
      virtual ~ArrayMapBase();
      
      /**
       * Get a reference to the array map
       */
      inline SzaArrayMap* arrayMap() {
	return arrayMap_;
      }

    private:

      // The underlying array map object

      static SzaArrayMap* arrayMap_;

      // A reference count

      static unsigned refCount_;

    }; // End class ArrayMapBase
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ARRAYMAPBASE_H
