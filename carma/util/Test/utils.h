#ifndef CARMA_UTIL_TEST_UTILS_H
#define CARMA_UTIL_TEST_UTILS_H

#include "carma/util/compileTimeCheck.h"


namespace carma {
namespace util {
namespace test {


template < typename T >
T mankyConvertFromVoidPtr( void * const p );


template < typename T >
void * mankyConvertToVoidPtr( const T & t );


}  // namespace carma::util::test
}  // namespace carma::util
}  // namespace carma


template < typename T >
inline T
carma::util::test::mankyConvertFromVoidPtr( void * const p ) {
    union {
        T      t;
        void * p;
    } sp;
    
    compileTimeCheck< sizeof( T ) == sizeof( void * ) >( );

    sp.p = p;
    
    return sp.t;
}


template < typename T >
inline void *
carma::util::test::mankyConvertToVoidPtr( const T & t ) {
    union {
        T      t;
        void * p;
    } sp;
    
    compileTimeCheck< sizeof( T ) == sizeof( void * ) >( );

    sp.t = t;
    
    return sp.p;
}


#endif
