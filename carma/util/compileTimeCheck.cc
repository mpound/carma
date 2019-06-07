#include "carma/util/compileTimeCheck.h"


namespace {


void
verifyItWorks( ) {
    using namespace carma::util;

    compileTimeCheck< true >( );
    // compileTimeCheck< false >( );
    
    compileTimeCheck< 1 >( );
    compileTimeCheck< -1 >( );
    
    compileTimeCheck< (1 + 1 + 3) == 5 >( );
    compileTimeCheck< (3 * 7 - 5) != 17 >( );

    compileTimeCheck< sizeof( char ) == 1 >( );
    compileTimeCheck< sizeof( unsigned char ) == 1 >( );

    compileTimeCheck< sizeof( short ) >= 2 >( );
    compileTimeCheck< sizeof( unsigned short ) >= 2 >( );

    compileTimeCheck< sizeof( int ) >= 4 >( );
    compileTimeCheck< sizeof( unsigned int ) >= 4 >( );

    compileTimeCheck< sizeof( long ) >= 4 >( );
    compileTimeCheck< sizeof( unsigned long ) >= 4 >( );

    compileTimeCheck< sizeof( long long ) >= 8 >( );
    compileTimeCheck< sizeof( unsigned long long ) >= 8 >( );

    compileTimeCheck< sizeof( float ) == 4 >( );
    compileTimeCheck< sizeof( double ) == 8 >( );
    compileTimeCheck< sizeof( long double ) >= 8 >( );
}


}  // namespace < anonymous >
