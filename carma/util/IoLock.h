#ifndef CARMA_UTIL_IOLOCK_H
#define CARMA_UTIL_IOLOCK_H

/**
 * @file IoLock.h
 * 
 * Tagged: Sat May  8 08:22:36 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/util/PthreadMutex.h"

namespace carma {
namespace util {


class IoLock {
    public:
        class ScopedCerrLock;
        
    private:
        static carma::util::PthreadMutex cerrMutex_;
        
        IoLock( );
        
        /* virtual */ ~IoLock( );
}; // End class IoLock


} // End namespace util
} // End namespace carma


class carma::util::IoLock::ScopedCerrLock {
    public:
        ScopedCerrLock( );
        
        /* virtual */ ~ScopedCerrLock( );
};


inline
carma::util::IoLock::ScopedCerrLock::ScopedCerrLock( )
{
    cerrMutex_.Lock();
}


inline
carma::util::IoLock::ScopedCerrLock::~ScopedCerrLock( )
try {
    cerrMutex_.Unlock();
} catch ( ... ) {
    // Just stifle any exception
}


#endif
