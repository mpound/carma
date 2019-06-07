#ifndef CARMA_UTIL_MP_ALLOCATOR_H
#define CARMA_UTIL_MP_ALLOCATOR_H

#include <limits>
#include <cstdlib>


namespace carma {
namespace util {


class MpRawAllocator {
    public:
        static void * alloc( ::std::size_t bytes );
        static void dealloc( void * p, ::std::size_t bytes );
};


template < typename T >
class MpAllocator {
    public:
        // type definitions
        typedef T                value_type;
        typedef T *              pointer;
        typedef const T *        const_pointer;
        typedef T&               reference;
        typedef const T &        const_reference;
        typedef ::std::size_t    size_type;
        typedef ::std::ptrdiff_t difference_type;

        // rebind allocator to type U
        template < typename U >
        struct rebind {
            typedef MpAllocator< U > other;
        };

        // return address of values
        pointer address( reference value ) const {
            return &value;
        }

        const_pointer address( const_reference value ) const {
            return &value;
        }

        /* constructors and destructor
        * - nothing to do because the allocator has no state
        */
        MpAllocator( ) throw( ) { }
        
        MpAllocator( const MpAllocator & rhs ) throw( ) { }

        template < typename U >
        MpAllocator( const MpAllocator< U > & rhs ) throw( ) { }
        
        ~MpAllocator( ) throw( ) { }

        // return maximum number of elements that can be allocated
        size_type max_size ( ) const throw( ) {
            return (::std::numeric_limits< ::std::size_t >::max( ) / sizeof( T ));
        }

        // allocate but don't initialize num elements of type T
        pointer allocate( size_type    num,
                          const void * dummy = 0 ) {
            return static_cast< pointer >( MpRawAllocator::alloc( num * sizeof( T ) ) );
        }

        // initialize elements of allocated storage p with value value
        void construct( pointer   p,
                        const T & value ) {
            // initialize memory with placement new
            new (static_cast< void * >( p )) T( value );
        }

        // destroy elements of initialized storage p
        void destroy( pointer p ) {
            // destroy objects by calling their destructor
            p->~T( );
        }

        // deallocate storage p of deleted elements
        void deallocate( pointer   p,
                         size_type num ) {
            MpRawAllocator::dealloc( static_cast< void * >( p ), (num * sizeof( T )) );
        }
};


// return that all specializations of this allocator are interchangeable

template < typename T1, typename T2 >
bool
operator==( const MpAllocator< T1 > & lhs, const MpAllocator< T2 > & rhs ) throw( ) {
   return true;
}


template < typename T1, typename T2 >
bool
operator!=( const MpAllocator< T1 > & lhs, const MpAllocator< T2 > & rhs ) throw( ) {
   return false;
}


}  // namespace carma::util
}  // namespace carma

#endif
