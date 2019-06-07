#ifndef CARMA_UTIL_MULTISHOTBARRIER_H
#define CARMA_UTIL_MULTISHOTBARRIER_H

#include "carma/util/PthreadCond.h"
#include "carma/util/PthreadMutex.h"

namespace carma {
namespace util {

/**
 * MultiShotBarrier class. 
 * Variant of OneShotBarrier modified to support an arbitrary number of 
 * shots with a per-shot satisfy count.
 */
class MultiShotBarrier {
    public:

        /** 
         * Constructor 
         * @param initialSatisfyCount Satisfy count for first wait.
         * @throw carma::util::IllegalArgumentException if satisfyCount < 1.
         * @see reset
         */
        explicit MultiShotBarrier( size_t initialSatisfyCount );
        
        /**
         * Default constructor.
         * If this constructor is used, the user must call reset with a 
         * satisfy count prior to waiting.
         */
        explicit MultiShotBarrier( );

        
        /**
         * Destructor
         */
        virtual ~MultiShotBarrier( );
        
        /**
         * Reset the satisyCount to reuse barrier.
         * @param satisfyCount New satisfy count.
         * @throw carma::util::ErrorException if there are active waits.
         * @throw carma::util::IllegalArgumentException if satisfyCount < 1.
         */
        void reset( size_t satisfyCount );

        /**
         * Wait until all other waiters have reached this barrier.
         */
        void wait( );
        
    private:

        // No copying
        MultiShotBarrier( const MultiShotBarrier & rhs );
        MultiShotBarrier & operator=( const MultiShotBarrier & rhs );

        size_t       satisfyCount_;
        PthreadMutex guard_;
        PthreadCond  satisfiedCond_;
        size_t       waitCount_;

        size_t       activeWaiters_;
        PthreadMutex activeWaitersGuard_;

};


}  // namespace carma::util
}  // namespace carma

#endif
