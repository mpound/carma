#include "carma/util/bitTwiddling.h"

using namespace carma;
using namespace carma::util;


namespace {


void
checkExtractBitfieldCompiles( const unsigned int x )
{
    // extractBitfield< 5, 1 >( '7' );
    // extractBitfield< 5, 1 >( short( 7 ) );
    // extractBitfield< 5, 1 >( 7 );
    // extractBitfield< 5, 1 >( 7L );
    // extractBitfield< 5, 1 >( 7LL );

    // extractBitfield< 5, 1 >( 3.14F );
    // extractBitfield< 5, 1 >( 3.14 );
    // extractBitfield< 5, 1 >( 3.14L );

    extractBitfield< 5, 1 >( (unsigned char)( 7 ) );
    extractBitfield< 5, 1 >( (unsigned short)( 7 ) );
    extractBitfield< 5, 1 >( 7U );
    extractBitfield< 5, 1 >( 7UL );
    extractBitfield< 5, 1 >( 7ULL );

    extractBitfield< 31,  0 >( x );
    extractBitfield< 27,  0 >( x );
    extractBitfield< 24,  0 >( x );
    extractBitfield< 17,  0 >( x );
    extractBitfield< 16,  0 >( x );
    extractBitfield< 15,  0 >( x );
    extractBitfield<  9,  0 >( x );
    extractBitfield<  8,  0 >( x );
    extractBitfield<  5,  0 >( x );
    extractBitfield<  1,  0 >( x );
    extractBitfield<  0,  0 >( x );

    extractBitfield< 31,  1 >( x );
    extractBitfield< 27,  1 >( x );
    extractBitfield< 24,  1 >( x );
    extractBitfield< 17,  1 >( x );
    extractBitfield< 16,  1 >( x );
    extractBitfield< 15,  1 >( x );
    extractBitfield<  9,  1 >( x );
    extractBitfield<  8,  1 >( x );
    extractBitfield<  5,  1 >( x );
    extractBitfield<  1,  1 >( x );

    extractBitfield< 31,  7 >( x );
    extractBitfield< 27,  7 >( x );
    extractBitfield< 24,  7 >( x );
    extractBitfield< 17,  7 >( x );
    extractBitfield< 16,  7 >( x );
    extractBitfield< 15,  7 >( x );
    extractBitfield<  9,  7 >( x );
    extractBitfield<  8,  7 >( x );

    extractBitfield< 31,  8 >( x );
    extractBitfield< 27,  8 >( x );
    extractBitfield< 24,  8 >( x );
    extractBitfield< 17,  8 >( x );
    extractBitfield< 16,  8 >( x );
    extractBitfield< 15,  8 >( x );
    extractBitfield<  9,  8 >( x );
    extractBitfield<  8,  8 >( x );

    extractBitfield< 31,  9 >( x );
    extractBitfield< 27,  9 >( x );
    extractBitfield< 24,  9 >( x );
    extractBitfield< 17,  9 >( x );
    extractBitfield< 16,  9 >( x );
    extractBitfield< 15,  9 >( x );
    extractBitfield<  9,  9 >( x );

    extractBitfield< 31, 15 >( x );
    extractBitfield< 27, 15 >( x );
    extractBitfield< 24, 15 >( x );
    extractBitfield< 17, 15 >( x );
    extractBitfield< 16, 15 >( x );

    extractBitfield< 31, 16 >( x );
    extractBitfield< 27, 16 >( x );
    extractBitfield< 24, 16 >( x );
    extractBitfield< 17, 16 >( x );
    extractBitfield< 16, 16 >( x );

    extractBitfield< 31, 17 >( x );
    extractBitfield< 27, 17 >( x );
    extractBitfield< 24, 17 >( x );
    extractBitfield< 17, 17 >( x );

    extractBitfield< 31, 19 >( x );
    extractBitfield< 27, 19 >( x );
    extractBitfield< 24, 19 >( x );

    extractBitfield< 31, 24 >( x );
    extractBitfield< 27, 24 >( x );
    extractBitfield< 24, 24 >( x );

    extractBitfield< 31, 27 >( x );
    extractBitfield< 27, 27 >( x );

    extractBitfield< 31, 31 >( x );
}


}  // namespace < anonymous >


#if 0


unsigned int
myIntExtractBitfield( const unsigned int x )
{
    return extractBitfield< 13, 5 >( x );
}


unsigned short
myShortExtractBitfield( const unsigned short x )
{
    return extractBitfield< 13, 5 >( x );
}


#endif
