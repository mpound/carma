/**
 * @file
 * Template definition for FftwAllocator class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2006/12/08 21:55:24 $
 * $Id: FftwAllocator.h,v 1.2 2006/12/08 21:55:24 abeard Exp $
 */
#ifndef CARMA_UTIL_FFTWALLOCATOR_H
#define CARMA_UTIL_FFTWALLOCATOR_H

#include <fftw3.h>

#include <cstddef>
#include <limits>

namespace carma {

namespace util {

    /**
     * Allocator for memory for use by FFTW.
     * In order for fftw to take advantage of specific processor features 
     * like SIMD instruction sets, allocated memory must be properly 
     * alligned.  FFTW provides the fftw_malloc and fftw_free functions
     * for this purpose which this allocator uses.
     */
    template <class T>
    class FftwAllocator { 
    public:

        // Josuttis is my homeboy (i.e. this is copied from section 15.4)

        // type definitions
        typedef size_t    size_type;
        typedef ptrdiff_t difference_type;
        typedef T *       pointer;
        typedef const T * const_pointer;
        typedef T &       reference;
        typedef const T & const_reference;
        typedef T         value_type;

        // rebind allocator to type U
        template <class U>
        struct rebind {
            typedef FftwAllocator<U> other;
        };

        // return address of values
        pointer address ( reference value ) const {
            return &value;
        }

        const_pointer address( const_reference value ) const {
            return &value;
        }

        /** 
         * Constructors and destructor
         * - nothing to do because the allocator has no state
         */
        FftwAllocator( ) throw ( ) {
        }

        FftwAllocator( const FftwAllocator &) throw ( ) {
        }

        template <class U>
        FftwAllocator( const FftwAllocator<U> & ) throw ( ) {
        }

        ~FftwAllocator( ) throw ( ) {
        }

        // return maximum number of elements that can be allocated
        size_type max_size( ) const throw ( ) {
            return std::numeric_limits<size_t>::max( ) / sizeof( T );
        }

        // allocate but don't initialize num elements of type T
        pointer allocate( size_type num,
                          const void * hint = 0 ) {
            // allocate memory with fftw_alloc
            return static_cast<pointer>( ::fftw_malloc( num * sizeof( T ) ) );
        }

        // initialize elements of allocated storage p with value value
        void construct( pointer p, const T & value ) {
            // initialized memory with placement new
            new ( static_cast<void *>( p ) ) T( value );
        }

        // destroy elements of initialized storage p
        void destroy( pointer p ) {
            // destroy objects by calling their destructor
            p->~T( );
        }

        // deallocate storage p of deleted elements
        void deallocate( pointer p, size_type num ) {
            // deallocate memory with fftw_free
            fftw_free( static_cast<void *>( p ) );
        }

    }; // class FftwAllocator

    // return that all specializations of this allocator are interchangeable
    template <class T1, class T2>
    bool operator==( const FftwAllocator<T1> &,
                     const FftwAllocator<T2> & ) throw ( ) {
        return true;
    }

    template <class T1, class T2>
    bool operator!=( const FftwAllocator<T1> &,
                     const FftwAllocator<T2> & ) throw ( ) {
        return false;
    }

} // namespace util
} // namespace carma
#endif
