#ifndef CARMA_UTIL_PTHREAD_RW_LOCK_ATTR_H
#define CARMA_UTIL_PTHREAD_RW_LOCK_ATTR_H

#include <pthread.h>


namespace carma {
namespace util {


//! @brief A simple wrapper class that makes use of ::pthread_rwlockattr_t
//!        easier in a C++ world

class PthreadRWLockAttr {
    public:
        
        //! @brief Construct read/write lock attributes with the CARMA defaults
        //!
        //! Any internal errors will throw an exception.
        
        explicit PthreadRWLockAttr( );
        
        
        //! @brief Destruct read/write lock attributes
        //!
        //! No exceptions will be thrown and hence internal errors will be
        //! ignored as far as clients are concerned (though they may be
        //! logged).
        
        virtual ~PthreadRWLockAttr( );
        
        
        //! @brief Obtain a reference to the internal ::pthread_rwlockattr_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_rwlockattr_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          read/write lock attributes then think twice before using
        //!          this method.
        //!
        //! @return a reference to the internal ::pthread_rwlockattr_t
        
        const ::pthread_rwlockattr_t & InternalPthreadRWLockAttr( ) const;


        //! @brief Obtain a reference to the internal ::pthread_rwlockattr_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_rwlockattr_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          read/write lock attributes then think twice before using
        //!          this method.
        //!
        //! @return a reference to the internal ::pthread_rwlockattr_t
        
        ::pthread_rwlockattr_t & InternalPthreadRWLockAttr( );


    private:
        // no copying
        PthreadRWLockAttr( const PthreadRWLockAttr & rhs );
        PthreadRWLockAttr & operator=( const PthreadRWLockAttr & rhs );

        ::pthread_rwlockattr_t rwlockattr_;
};


}  // namespace carma::util
}  // namespace carma


inline const ::pthread_rwlockattr_t &
carma::util::PthreadRWLockAttr::InternalPthreadRWLockAttr( ) const
{
    return rwlockattr_;
}


inline ::pthread_rwlockattr_t &
carma::util::PthreadRWLockAttr::InternalPthreadRWLockAttr( )
{
    return rwlockattr_;
}


#endif
