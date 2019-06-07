#include "carma/util/FloatStringifier.h"

#include <cmath>

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


unsigned long
getExponentBits( const float v )
{
    union {
        float         d;
        unsigned long ul;
    } u;

    u.d = v;

    return ((u.ul >> 23) & 0x00FFUL);
}


}  // anonymous namespace


FloatStringifier::FloatStringifier( )
{
    for ( unsigned short i = 0; i < 0x00FF; ++i ) {
        const int e = i - 127;

        const double base10Exponent = floor( log10( pow( 2.0, e ) ) );

        firstScale_[ i ] = pow( 10.0, 5.0 - base10Exponent );
        secondScale_[ i ] = pow( 10.0, 4.0 - base10Exponent );

        firstBiasedBase10Exponent_[ i ] =
            static_cast< unsigned short >( base10Exponent + 310.0 );
    }

    for ( unsigned short i = 0; i < 100; ++i ) {
        leadChunks_[ i ][ 0 ] = static_cast< char >( '0' + ((i / 10) % 10) );
        leadChunks_[ i ][ 1 ] = '.';
        leadChunks_[ i ][ 2 ] = static_cast< char >( '0' + ((i / 1) % 10) );
    }

    for ( unsigned short i = 0; i < 10000; ++i ) {
        otherChunks_[ i ][ 0 ] = static_cast< char >( '0' + ((i / 1000) % 10) );
        otherChunks_[ i ][ 1 ] = static_cast< char >( '0' + ((i / 100) % 10) );
        otherChunks_[ i ][ 2 ] = static_cast< char >( '0' + ((i / 10) % 10) );
        otherChunks_[ i ][ 3 ] = static_cast< char >( '0' + ((i / 1) % 10) );
    }

    for ( unsigned short i = 0; i < 621; ++i ) {
        int e;

        if ( i < 310 ) {
            e = 310 - i;
            exponents_[ i ][ 0 ] = '-';
        } else {
            e = i - 310;
            exponents_[ i ][ 0 ] = '+';
        }

        exponents_[ i ][ 1 ] = static_cast< char >( '0' + ((e / 100) % 10) );
        exponents_[ i ][ 2 ] = static_cast< char >( '0' + ((e / 10) % 10) );
        exponents_[ i ][ 3 ] = static_cast< char >( '0' + ((e / 1) % 10) );
    }
}


FloatStringifier::~FloatStringifier( )
try {
} catch ( ... ) {
    // Just stifle any exception

    return;
}


void
FloatStringifier::stringifyFloat( float        v,
                                  char * const buffer )
{
    // First, we handle the sign of the value
    if ( v < 0.0 ) {
        buffer[ 0 ] = '-';
        v = -v;
    } else {
        buffer[ 0 ] = '+';
    }


    const unsigned long exponentBits = getExponentBits( v );

    if ( exponentBits == 0 ) {
        // Either a true "0.0" or a "denorm", all of which we emit as "0.0"

        buffer[ 1  ] = '0';
        buffer[ 2  ] = '.';
        buffer[ 3  ] = '0';

        buffer[ 4  ] = '0';
        buffer[ 5  ] = '0';
        buffer[ 6  ] = '0';
        buffer[ 7  ] = '0';

        buffer[ 8  ] = 'e';
        buffer[ 9  ] = '+';
        buffer[ 10 ] = '0';
        buffer[ 11 ] = '0';
        buffer[ 12 ] = '0';
    } else if ( exponentBits == 0x00FF ) {
        // Either an "inf" or a "NaN", all of which we emit as "1.0e+310"

        buffer[ 1  ] = '1';
        buffer[ 2  ] = '.';
        buffer[ 3  ] = '0';

        buffer[ 4  ] = '0';
        buffer[ 5  ] = '0';
        buffer[ 6  ] = '0';
        buffer[ 7  ] = '0';

        buffer[ 8  ] = 'e';
        buffer[ 9  ] = '+';
        buffer[ 10 ] = '3';
        buffer[ 11 ] = '1';
        buffer[ 12 ] = '0';
    } else {
        unsigned short vBiasedBase10Exponent =
            firstBiasedBase10Exponent_[ exponentBits ];

        double vScaled = round( v * firstScale_[ exponentBits ] );

        if ( (vScaled >= 1e+6) && (vScaled <= 1e+7) ) {
            vBiasedBase10Exponent += 1;
            vScaled = round( v * secondScale_[ exponentBits ] );
        }

        unsigned long scaled =
            static_cast< unsigned long >( vScaled );

        const unsigned long chunkFactor = 10000;

        unsigned long leadChunk = scaled / chunkFactor;

        const unsigned long secondChunk =
            scaled - leadChunk * chunkFactor;

        buffer[ 1  ] = leadChunks_[ leadChunk ][ 0 ];
        buffer[ 2  ] = leadChunks_[ leadChunk ][ 1 ];
        buffer[ 3  ] = leadChunks_[ leadChunk ][ 2 ];

        buffer[ 4  ] = otherChunks_[ secondChunk ][ 0 ];
        buffer[ 5  ] = otherChunks_[ secondChunk ][ 1 ];
        buffer[ 6  ] = otherChunks_[ secondChunk ][ 2 ];
        buffer[ 7  ] = otherChunks_[ secondChunk ][ 3 ];

        buffer[ 8  ] = 'e';
        buffer[ 9  ] = exponents_[ vBiasedBase10Exponent ][ 0 ];
        buffer[ 10 ] = exponents_[ vBiasedBase10Exponent ][ 1 ];
        buffer[ 11 ] = exponents_[ vBiasedBase10Exponent ][ 2 ];
        buffer[ 12 ] = exponents_[ vBiasedBase10Exponent ][ 3 ];
    }
}
