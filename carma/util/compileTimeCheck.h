#ifndef CARMA_UTIL_COMPILE_TIME_CHECK_H
#define CARMA_UTIL_COMPILE_TIME_CHECK_H


namespace carma {
namespace util {


template < bool assertion >
void compileTimeCheck( );


template < >
inline void compileTimeCheck< true >( ) {  }


template < bool assertion >
inline void compileTimeCheck( ) {
    struct your_compile_time_assertion_failed;
    
    if ( sizeof( your_compile_time_assertion_failed ) != 3 )
        throw 11;
}


}  // namespace carma::util
}  // namespace carma


#endif
