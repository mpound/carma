#ifndef CARMA_UTIL_ONESHOTBARRIER_H
#define CARMA_UTIL_ONESHOTBARRIER_H

#include "carma/util/PthreadCond.h"
#include "carma/util/PthreadMutex.h"


namespace carma {
namespace util {


class OneShotBarrier {
    public:
        explicit OneShotBarrier( size_t satisfyCount );
        
        virtual ~OneShotBarrier( );
        
        void wait( );
        
    private:
        // No copying
        OneShotBarrier( const OneShotBarrier & rhs );
        OneShotBarrier & operator=( const OneShotBarrier & rhs );

        const size_t satisfyCount_;
        PthreadMutex guard_;
        PthreadCond  satisfiedCond_;
        size_t       waitCount_;
};


}  // namespace carma::util
}  // namespace carma


#endif
