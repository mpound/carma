#ifndef CARMA_UTIL_PTHREAD_COND_ATTR_H
#define CARMA_UTIL_PTHREAD_COND_ATTR_H

#include <pthread.h>


namespace carma {
namespace util {


//! @brief A simple wrapper class that makes use of ::pthread_condattr_t
//!        easier in a C++ world

class PthreadCondAttr {
    public:
        
        //! @brief Construct cond attributes with the CARMA defaults
        //!
        //! Any internal errors will throw an exception.
        
        explicit PthreadCondAttr( );
        
        
        //! @brief Destruct cond attributes
        //!
        //! No exceptions will be thrown and hence internal errors will be
        //! ignored as far as clients are concerned (though they may be
        //! logged).
        
        virtual ~PthreadCondAttr( );
        
        
        //! @brief Obtain a reference to the internal ::pthread_condattr_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_condattr_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          cond attributes then think twice before using this
        //!          method.
        //!
        //! @return a reference to the internal ::pthread_condattr_t
        
        const ::pthread_condattr_t & InternalPthreadCondAttr( ) const;


        //! @brief Obtain a reference to the internal ::pthread_condattr_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_condattr_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          cond attributes then think twice before using this
        //!          method.
        //!
        //! @return a reference to the internal ::pthread_condattr_t
        
        ::pthread_condattr_t & InternalPthreadCondAttr( );


    private:
        // no copying
        PthreadCondAttr( const PthreadCondAttr & rhs );
        PthreadCondAttr & operator=( const PthreadCondAttr & rhs );

        ::pthread_condattr_t condAttr_;
};


}  // namespace carma::util
}  // namespace carma


inline const ::pthread_condattr_t &
carma::util::PthreadCondAttr::InternalPthreadCondAttr( ) const
{
    return condAttr_;
}


inline ::pthread_condattr_t &
carma::util::PthreadCondAttr::InternalPthreadCondAttr( )
{
    return condAttr_;
}


#endif
