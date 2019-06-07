#ifndef CARMA_UTIL_SCOPEDQILOCKMANAGER_H
#define CARMA_UTIL_SCOPEDQILOCKMANAGER_H


namespace carma {
namespace util {


class QuadraticInterpolator;


class ScopedQILockManager {
    public:
        explicit ScopedQILockManager(
            QuadraticInterpolator & qi,
            bool                    logIfLeftLocked = false );
        
        ~ScopedQILockManager( );
        
        void lockQI( );
        void unlockQI( );
        
    private:
        ScopedQILockManager( const ScopedQILockManager & rhs );
        ScopedQILockManager & operator=( const ScopedQILockManager & rhs );
        
        QuadraticInterpolator & qi_;
        const bool              logIfLeftLocked_;
        bool                    locked_;
};


} // namespace carma::util
} // namespace carma


#endif
