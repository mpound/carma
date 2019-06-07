#ifndef CARMA_UTIL_PTHREAD_MUTEX_ATTR_H
#define CARMA_UTIL_PTHREAD_MUTEX_ATTR_H

#include <pthread.h>


namespace carma {
namespace util {


//! @brief A simple wrapper class that makes use of ::pthread_mutexattr_t
//!        easier in a C++ world

class PthreadMutexAttr {
    public:
        
        //! @brief Construct mutex attributes with the CARMA defaults
        //!
        //! Any internal errors will throw an exception.
        
        explicit PthreadMutexAttr( );
        
        
        //! @breief Construct mutex attributes with the given mutex type
        //!
        //! Any internal errors will throw an exception.
        
        explicit PthreadMutexAttr( int type );
        
        
        //! @brief Destruct mutex attributes
        //!
        //! No exceptions will be thrown and hence internal errors will be
        //! ignored as far as clients are concerned (though they may be
        //! logged).
        
        virtual ~PthreadMutexAttr( );
        
        
        //! @brief Obtain a reference to the internal ::pthread_mutexattr_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_mutexattr_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          mutex attributes then think twice before using this
        //!          method.
        //!
        //! @return a reference to the internal ::pthread_mutexattr_t
        
        const ::pthread_mutexattr_t & InternalPthreadMutexAttr( ) const;


        //! @brief Obtain a reference to the internal ::pthread_mutexattr_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_mutexattr_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          mutex attributes then think twice before using this
        //!          method.
        //!
        //! @return a reference to the internal ::pthread_mutexattr_t
        
        ::pthread_mutexattr_t & InternalPthreadMutexAttr( );


    private:
        // no copying
        PthreadMutexAttr( const PthreadMutexAttr & rhs );
        PthreadMutexAttr & operator=( const PthreadMutexAttr & rhs );

        ::pthread_mutexattr_t mutexAttr_;
};


}  // namespace carma::util
}  // namespace carma


inline const ::pthread_mutexattr_t &
carma::util::PthreadMutexAttr::InternalPthreadMutexAttr( ) const
{
    return mutexAttr_;
}


inline ::pthread_mutexattr_t &
carma::util::PthreadMutexAttr::InternalPthreadMutexAttr( )
{
    return mutexAttr_;
}


#endif
