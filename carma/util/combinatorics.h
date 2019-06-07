#ifndef CARMA_UTIL_COMBINATORICS_H
#define CARMA_UTIL_COMBINATORICS_H


namespace carma {
namespace util {


int sumOf1ToN( int n );


}  // namespace carma::util
}  // namespace carma


inline int
carma::util::sumOf1ToN( const int n )
{
    // NOTE: I assume I don't overflow the size of an int with the product
    return ((n * (n + 1)) / 2);
}


#endif
