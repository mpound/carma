#ifndef CARMA_UTIL_SCOPEDFLOCKMANAGER_H
#define CARMA_UTIL_SCOPEDFLOCKMANAGER_H

namespace carma {
namespace util {

class ScopedFlockManager {
public:
    explicit ScopedFlockManager( bool logIfLeftLocked );

    /* virtual */ ~ScopedFlockManager( );

    void lockRead( int fd );

    void lockWrite( int fd );

    void unlock( int fd );

private:
    ScopedFlockManager( const ScopedFlockManager & rhs );
    ScopedFlockManager & operator=( const ScopedFlockManager & rhs );

    const bool logIfLeftLocked_;
    bool       locked_;
    int        fd_;
};

}} // namespace carma::util
#endif
