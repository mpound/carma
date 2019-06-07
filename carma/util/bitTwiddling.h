#ifndef CARMA_UTIL_BITTWIDDLING_H
#define CARMA_UTIL_BITTWIDDLING_H

//! @file
//! Interface file for various utlities for twiddling bits

#include "carma/util/compileTimeCheck.h"


namespace carma {
namespace util {


template < int highestBit, int lowestBit >
unsigned char extractBitfield( unsigned char x );

template < int highestBit, int lowestBit >
unsigned short extractBitfield( unsigned short x );

template < int highestBit, int lowestBit >
unsigned int extractBitfield( unsigned int x );

template < int highestBit, int lowestBit >
unsigned long extractBitfield( unsigned long x );

template < int highestBit, int lowestBit >
unsigned long long extractBitfield( unsigned long long x );


// For now these ones are declared but the implementation should give a
// compile error on use because I am unclear on whether or not they should
// do sign extension

template < int highestBit, int lowestBit >
char extractBitfield( char x );

template < int highestBit, int lowestBit >
short extractBitfield( short x );

template < int highestBit, int lowestBit >
int extractBitfield( int x );

template < int highestBit, int lowestBit >
long extractBitfield( long x );

template < int highestBit, int lowestBit >
long long extractBitfield( long long x );


// These are just wacky and I'm disallowing them at compile time

template < int highestBit, int lowestBit >
float extractBitfield( float x );

template < int highestBit, int lowestBit >
double extractBitfield( double x );

template < int highestBit, int lowestBit >
long double extractBitfield( long double x );


namespace detail {


template < int highestBit, int lowestBit, typename T >
T extractUnsignedBitfieldImpl( T x );


}  // namespace carma::util::detail


}  // namespace carma::util
}  // namespace carma


template < int      highestBit,
           int      lowestBit,
           typename T >
inline T
carma::util::detail::extractUnsignedBitfieldImpl( const T x )
{
    compileTimeCheck< (highestBit >=  0) >();
    compileTimeCheck< (highestBit < (8 * sizeof( T ))) >();

    compileTimeCheck< (lowestBit >=  0) >();
    compileTimeCheck< (lowestBit < (8 * sizeof( T ))) >();

    compileTimeCheck< (highestBit >= lowestBit) >();

    const T hiMask = ~((~T(1)) << highestBit);
    const T result = ((x & hiMask) >> lowestBit);

    return result;
}


template < int highestBit, int lowestBit >
inline unsigned char
carma::util::extractBitfield( const unsigned char x )
{
    return detail::extractUnsignedBitfieldImpl< highestBit, lowestBit >( x );
}


template < int highestBit, int lowestBit >
inline unsigned short
carma::util::extractBitfield( const unsigned short x )
{
    return detail::extractUnsignedBitfieldImpl< highestBit, lowestBit >( x );
}


template < int highestBit, int lowestBit >
inline unsigned int
carma::util::extractBitfield( const unsigned int x )
{
    return detail::extractUnsignedBitfieldImpl< highestBit, lowestBit >( x );
}


template < int highestBit, int lowestBit >
inline unsigned long
carma::util::extractBitfield( const unsigned long x )
{
    return detail::extractUnsignedBitfieldImpl< highestBit, lowestBit >( x );
}


template < int highestBit, int lowestBit >
inline unsigned long long
carma::util::extractBitfield( const unsigned long long x )
{
    return detail::extractUnsignedBitfieldImpl< highestBit, lowestBit >( x );
}


template < int highestBit, int lowestBit >
inline char
carma::util::extractBitfield( const char x )
{
    compileTimeCheck< false >();
    
    return 0;
}


template < int highestBit, int lowestBit >
inline short
carma::util::extractBitfield( const short x )
{
    compileTimeCheck< false >();
    
    return 0;
}


template < int highestBit, int lowestBit >
inline int
carma::util::extractBitfield( const int x )
{
    compileTimeCheck< false >();
    
    return 0;
}


template < int highestBit, int lowestBit >
inline long
carma::util::extractBitfield( const long x )
{
    compileTimeCheck< false >();
    
    return 0;
}


template < int highestBit, int lowestBit >
inline long long
carma::util::extractBitfield( const long long x )
{
    compileTimeCheck< false >();
    
    return 0;
}


template < int highestBit, int lowestBit >
inline float
carma::util::extractBitfield( const float x )
{
    compileTimeCheck< false >();
    
    return 0;
}


template < int highestBit, int lowestBit >
inline double
carma::util::extractBitfield( const double x )
{
    compileTimeCheck< false >();
    
    return 0;
}


template < int highestBit, int lowestBit >
inline long double
carma::util::extractBitfield( const long double x )
{
    compileTimeCheck< false >();
    
    return 0;
}


#endif
