#ifndef CARMA_UTIL_SCOPED_PTHREAD_CANCEL_DISABLE_H
#define CARMA_UTIL_SCOPED_PTHREAD_CANCEL_DISABLE_H

namespace carma {
namespace util {


// A simple wrapper class that makes using
// pthread_setcancelstate easier in a C++ world

class ScopedPthreadCancelDisable {
    public:
        explicit ScopedPthreadCancelDisable( );

        /* virtual */ ~ScopedPthreadCancelDisable( );

    private:
        // no copying
        ScopedPthreadCancelDisable( const ScopedPthreadCancelDisable & );
        ScopedPthreadCancelDisable & operator=( const ScopedPthreadCancelDisable & );

        int oldState_;
};


}  // namespace carma::util
}  // namespace carma


#endif
