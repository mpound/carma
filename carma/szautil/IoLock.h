#ifndef SZA_UTIL_IOLOCK_H
#define SZA_UTIL_IOLOCK_H

/**
 * @file IoLock.h
 * 
 * Tagged: Sat May  8 08:22:36 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Mutex.h"

#define IOCOUT(statement) \
{\
    sza::util::IoLock::lockCout(); \
    std::cout << statement << std::endl; \
    sza::util::IoLock::unlockCout(); \
}

#define IOCTOUT(statement) \
{\
    sza::util::TimeVal timeVal;\
    timeVal.setToCurrentTime();\
    sza::util::IoLock::lockCout(); \
    std::cout << timeVal << ": " << statement << std::endl; \
    sza::util::IoLock::unlockCout(); \
}

#define IOCERR(statement) \
{\
    sza::util::IoLock::lockCerr(); \
    std::cerr << statement << std::endl; \
    sza::util::IoLock::unlockCerr(); \
}

namespace sza {
  namespace util {
    
    class IoLock {
    public:
      
      /**
       * Destructor.
       */
      virtual ~IoLock();
      
      static void lockCout();
      static void unlockCout();
      static void lockCerr();
      static void unlockCerr();

    private:

      static Mutex coutMutex_;
      static Mutex cerrMutex_;

      /**
       * Private constructor prevents instantiation
       */
      IoLock();

    }; // End class IoLock
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_IOLOCK_H
