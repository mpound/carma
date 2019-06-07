#ifndef CARMA_UTIL_AUTO_FREED_MALLOC_PTR_H
#define CARMA_UTIL_AUTO_FREED_MALLOC_PTR_H

#include <cstdlib>

#include "carma/util/ErrorException.h"


namespace carma {
namespace util {


//! AutoFreedMallocPtr< T > works like ::std::auto_ptr< T > except that
//! the pointer is ::free'ed (if it is not 0) instead of delete'ed.

template < typename T >
class AutoFreedMallocPtr {
    public:
        explicit AutoFreedMallocPtr( );

        explicit AutoFreedMallocPtr( T * tPtr );

        AutoFreedMallocPtr( AutoFreedMallocPtr & rhs );

        ~AutoFreedMallocPtr( );

        template < typename S >
        AutoFreedMallocPtr( AutoFreedMallocPtr< S > & rhs ) :
        tPtr_( rhs.release() )
        {
        }

        AutoFreedMallocPtr & operator=( AutoFreedMallocPtr & rhs );

        template < typename S >
        AutoFreedMallocPtr &
        operator=( AutoFreedMallocPtr< S > & rhs )
        {
            reset( rhs.release() );
            return *this;
        }

        T & operator*( ) const;

        T * operator->( ) const;

        T * get( ) const;

        T * release( );

        void reset( T * tPtr = 0 );

        template < typename S >
        operator AutoFreedMallocPtr< S >( )
        {
            return AutoFreedMallocPtr< S >( this->release() );
        }

    private:
        T * tPtr_;
};


}  // namespace carma::util
}  // namespace carma


// ******* Below here is simply implementation *******

template < typename T >
carma::util::AutoFreedMallocPtr< T >::AutoFreedMallocPtr( ) :
tPtr_( 0 )
{
}


template < typename T >
carma::util::AutoFreedMallocPtr< T >::AutoFreedMallocPtr( T * const tPtr ) :
tPtr_( tPtr )
{
}


template < typename T >
carma::util::AutoFreedMallocPtr< T >::AutoFreedMallocPtr( AutoFreedMallocPtr & rhs ) :
tPtr_( rhs.release() )
{
}


template < typename T >
carma::util::AutoFreedMallocPtr< T >::~AutoFreedMallocPtr( )
try {
    if ( tPtr_ != 0 )
        ::free( tPtr_ );
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


template < typename T >
carma::util::AutoFreedMallocPtr< T > &
carma::util::AutoFreedMallocPtr< T >::operator=( AutoFreedMallocPtr & rhs )
{
    reset( rhs.release() );

    return *this;
}


template < typename T >
T &
carma::util::AutoFreedMallocPtr< T >::operator*( ) const
{
    if ( tPtr_ == 0 )
        throw CARMA_ERROR( "AutoFreedMallocPtr pointer is NULL" );
    
    return *tPtr_; 
}


template < typename T >
T *
carma::util::AutoFreedMallocPtr< T >::operator->( ) const
{
    if ( tPtr_ == 0 )
        throw CARMA_ERROR( "AutoFreedMallocPtr pointer is NULL" );

    return tPtr_; 
}


template < typename T >
T *
carma::util::AutoFreedMallocPtr< T >::get( ) const
{
    return tPtr_;
}


template < typename T >
T *
carma::util::AutoFreedMallocPtr< T >::release( )
{
    T * result = tPtr_;
    tPtr_ = 0;

    return result;
}


template < typename T >
void
carma::util::AutoFreedMallocPtr< T >::reset( T * const tPtr )
{
    if ( tPtr != tPtr_ ) {
        if ( tPtr_ != 0 )
            ::free( tPtr_ );
    
        tPtr_ = tPtr;
    }
}


#endif
