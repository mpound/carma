#include "carma/util/DoubleStringifier.h"

#include <cmath>

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


unsigned long
getExponentBits( const double v )
{
    union {
        double             d;
        unsigned long long ull;
    } u;

    u.d = v;

    const unsigned long hiHalf = static_cast< unsigned long >( u.ull >> 32 );

    return ((hiHalf >> 20) & 0x07FFUL);
}


}  // namespace < anonymous >


DoubleStringifier::DoubleStringifier( )
{
    for ( unsigned short i = 0; i < 100; ++i ) {
        leadChunks_[ i ][ 0 ] = static_cast< char >( '0' + ((i / 1) % 10) );
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


    for ( unsigned short i = 0; i < 0x0800; ++i ) {
        const int e = i - 1023;

        const double base2Value = pow( 2.0, e );
        const double base10Log = log10( base2Value );
        const double base10Exponent = floor( base10Log );

        if ( base10Exponent <= (12 - 308) ) {
            firstScale_[ i ] = 1e+308;
            secondScale_[ i ] = 1e+307;

            firstBiasedBase10Exponent_[ i ] = (12 - 308) + 310;
        } else {
            firstScale_[ i ] = pow( 10.0, 12.0 - base10Exponent );
            secondScale_[ i ] = pow( 10.0, 11.0 - base10Exponent );

            firstBiasedBase10Exponent_[ i ] =
                static_cast< unsigned short >( base10Exponent + 310.0 );
        }
    }
}


DoubleStringifier::~DoubleStringifier( )
try {
} catch ( ... ) {
    // Just stifle any exceptions

    return;
}


void
DoubleStringifier::stringifyDouble( double       v,
                                    char * const buffer )
{
    // First, we handle the sign of the value
    if ( v < 0.0 ) {
        buffer[ 0 ] = '-';
        v = ::std::fabs( v );
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
        buffer[ 8  ] = '0';
        buffer[ 9  ] = '0';
        buffer[ 10 ] = '0';

        buffer[ 11 ] = '0';
        buffer[ 12 ] = '0';
        buffer[ 13 ] = '0';
        buffer[ 14 ] = '0';

        buffer[ 15 ] = 'e';
        buffer[ 16 ] = '+';
        buffer[ 17 ] = '0';
        buffer[ 18 ] = '0';
        buffer[ 19 ] = '0';
    } else if ( exponentBits == 0x007FF ) {
        // Either an "inf" or a "NaN", all of which we emit as "1.0e+310"

        buffer[ 1  ] = '1';
        buffer[ 2  ] = '.';

        buffer[ 3  ] = '0';
        buffer[ 4  ] = '0';
        buffer[ 5  ] = '0';
        buffer[ 6  ] = '0';

        buffer[ 7  ] = '0';
        buffer[ 8  ] = '0';
        buffer[ 9  ] = '0';
        buffer[ 10 ] = '0';

        buffer[ 11 ] = '0';
        buffer[ 12 ] = '0';
        buffer[ 13 ] = '0';
        buffer[ 14 ] = '0';

        buffer[ 15 ] = 'e';
        buffer[ 16 ] = '+';
        buffer[ 17 ] = '3';
        buffer[ 18 ] = '1';
        buffer[ 19 ] = '0';
    } else {
        // A "normalized" value

        unsigned short vBiasedBase10Exponent = firstBiasedBase10Exponent_[ exponentBits ];

        double vScaled = round( v * firstScale_[ exponentBits ] );

        if ( vScaled >= 1e+13 ) {
            vBiasedBase10Exponent += 1;
            vScaled = round( v * secondScale_[ exponentBits ] );
        }

        unsigned long long scaled =
            static_cast< unsigned long long >( vScaled );

        const unsigned long chunkFactor = 10000;
        const unsigned long twoChunkFactor = chunkFactor * chunkFactor;

        unsigned long lead2Chunks = scaled / twoChunkFactor;

        unsigned long leadChunk = lead2Chunks / chunkFactor;

        const unsigned long secondChunk =
            lead2Chunks - leadChunk * chunkFactor;

        const unsigned long tail2Chunks =
            scaled - (lead2Chunks * twoChunkFactor);

        const unsigned long thirdChunk =
            tail2Chunks / chunkFactor;

        const unsigned long fourthChunk =
            tail2Chunks - thirdChunk * chunkFactor;

        buffer[ 1  ] = leadChunks_[ leadChunk ][ 0 ];
        buffer[ 2  ] = '.';

        buffer[ 3  ] = otherChunks_[ secondChunk ][ 0 ];
        buffer[ 4  ] = otherChunks_[ secondChunk ][ 1 ];
        buffer[ 5  ] = otherChunks_[ secondChunk ][ 2 ];
        buffer[ 6  ] = otherChunks_[ secondChunk ][ 3 ];

        buffer[ 7  ] = otherChunks_[ thirdChunk ][ 0 ];
        buffer[ 8  ] = otherChunks_[ thirdChunk ][ 1 ];
        buffer[ 9  ] = otherChunks_[ thirdChunk ][ 2 ];
        buffer[ 10 ] = otherChunks_[ thirdChunk ][ 3 ];

        buffer[ 11 ] = otherChunks_[ fourthChunk ][ 0 ];
        buffer[ 12 ] = otherChunks_[ fourthChunk ][ 1 ];
        buffer[ 13 ] = otherChunks_[ fourthChunk ][ 2 ];
        buffer[ 14 ] = otherChunks_[ fourthChunk ][ 3 ];

        buffer[ 15 ] = 'e';
        buffer[ 16 ] = exponents_[ vBiasedBase10Exponent ][ 0 ];
        buffer[ 17 ] = exponents_[ vBiasedBase10Exponent ][ 1 ];
        buffer[ 18 ] = exponents_[ vBiasedBase10Exponent ][ 2 ];
        buffer[ 19 ] = exponents_[ vBiasedBase10Exponent ][ 3 ];
    }
}
