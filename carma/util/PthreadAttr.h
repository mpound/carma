#ifndef CARMA_UTIL_PTHREAD_ATTR_H
#define CARMA_UTIL_PTHREAD_ATTR_H

#include <pthread.h>


namespace carma {
namespace util {


//! @brief A simple wrapper class that makes use of ::pthread_attr_t
//!        easier in a C++ world

class PthreadAttr {
    public:

        //! @brief Construct pthread attributes with the CARMA defaults
        //!
        //! Any internal errors will throw an exception.

        explicit PthreadAttr( );


        //! @breief Construct pthread attributes with the given detached state
        //!
        //! Any internal errors will throw an exception.

        explicit PthreadAttr( int detachState );


        //! @brief Destruct pthread attributes
        //!
        //! No exceptions will be thrown and hence internal errors will be
        //! ignored as far as clients are concerned (though they may be
        //! logged).

        virtual ~PthreadAttr( );


        int getDetachState( ) const;

        void setDetachState( int detachState );


        ::size_t getStackSize( ) const;

        void setStackSize( ::size_t stackSize );


        ::size_t getGuardSize( ) const;

        void setGuardSize( ::size_t guardSize );


        //! @brief Obtain a reference to the internal ::pthread_attr_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_attr_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          pthread attributes then think twice before using this
        //!          method.
        //!
        //! @return a reference to the internal ::pthread_attr_t

        const ::pthread_attr_t & InternalPthreadAttr( ) const;


        //! @brief Obtain a reference to the internal ::pthread_attr_t
        //!
        //! @warning Use this with care. Certainly do not go off and call
        //!          something like ::pthread_attr_destroy on the return
        //!          value. If you don't know what you are doing with POSIX
        //!          pthread attributes then think twice before using this
        //!          method.
        //!
        //! @return a reference to the internal ::pthread_attr_t

        ::pthread_attr_t & InternalPthreadAttr( );


    private:
        // no copying
        PthreadAttr( const PthreadAttr & rhs );
        PthreadAttr & operator=( const PthreadAttr & rhs );

        ::pthread_attr_t attr_;
};


}  // namespace carma::util
}  // namespace carma


inline const ::pthread_attr_t &
carma::util::PthreadAttr::InternalPthreadAttr( ) const
{
    return attr_;
}


inline ::pthread_attr_t &
carma::util::PthreadAttr::InternalPthreadAttr( )
{
    return attr_;
}


#endif
