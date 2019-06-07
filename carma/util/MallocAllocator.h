#ifndef CARMA_UTIL_MALLOC_ALLOCATOR_H
#define CARMA_UTIL_MALLOC_ALLOCATOR_H

#include <limits>
#include <cstdlib>


namespace carma {
namespace util {


template < typename T >
class MallocAllocator {
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
            typedef MallocAllocator< U > other;
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
        MallocAllocator( ) throw( ) { }
        
        MallocAllocator( const MallocAllocator & rhs ) throw( ) { }

        template < typename U >
        MallocAllocator( const MallocAllocator< U > & rhs ) throw( ) { }
        
        ~MallocAllocator( ) throw( ) { }

        // return maximum number of elements that can be allocated
        size_type max_size ( ) const throw( ) {
            return (::std::numeric_limits< ::std::size_t >::max( ) / sizeof( T ));
        }

        // allocate but don't initialize num elements of type T
        pointer allocate( size_type    num,
                          const void * dummy = 0 ) {
            return static_cast< pointer >( ::malloc( num * sizeof( T ) ) );
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
            if ( p != 0 )
                ::free( static_cast< void * >( p ) );
        }
};


// return that all specializations of this allocator are interchangeable

template < typename T1, typename T2 >
bool
operator==( const MallocAllocator< T1 > & lhs, const MallocAllocator< T2 > & rhs ) throw( ) {
   return true;
}


template < typename T1, typename T2 >
bool
operator!=( const MallocAllocator< T1 > & lhs, const MallocAllocator< T2 > & rhs ) throw( ) {
   return false;
}


}  // namespace carma::util
}  // namespace carma

#endif
