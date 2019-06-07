#ifndef SZA_UTIL_REGISTERSET_H
#define SZA_UTIL_REGISTERSET_H

/**
 * @file RegisterSet.h
 * 
 * Tagged: Wed Oct  6 11:00:37 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/ArrayMapBase.h"

#include "carma/szaarrayutils/regset.h"

namespace sza {
  namespace util {
    
    // Wrapper class around sza::array::RegSet for managing sets of
    // registers

    class RegisterSet {
    public:
      
      /**
       * Constructor.
       */
      RegisterSet(ArrayMap* arrayMap=NULL, bool archivedOnly_=false);
      
      /**
       * Destructor.
       */
      virtual ~RegisterSet();
      
      /**
       * Add a register dscription to a register set
       */
      void addRegister(RegDescription& desc);
      void addRegister(RegDescription* desc);

      /**
       * Add a vector of register descriptions to a register set
       */
      void addRegisters(std::vector<RegDescription>& regs);

      /**
       * Return a pointer to the underlying register set
       */
      inline RegSet* regSet() {
	return regSet_;
      }

      /**
       * Boolean equivalence operator for a register set
       */
      bool operator==(RegisterSet& regSet);

      /**
       * Assignment operator
       */
      void operator=(RegisterSet const& registerSet);

      /**
       * Reset this object
       */
      void reset();

      inline bool archivedOnly() {
	return archivedOnly_;
      }

    private:

      // An array map
      
      ArrayMapBase arrayMap_;
      
      RegSet* regSet_;

      bool archivedOnly_;

    }; // End class RegisterSet
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REGISTERSET_H
