#ifndef CARMA_UTIL_DOUBLE_STRINGIFIER_H
#define CARMA_UTIL_DOUBLE_STRINGIFIER_H

#include "carma/util/ErrorException.h"


namespace carma {
namespace util {


class DoubleStringifier {
    public:

        enum {
            MAX_CHARS_PER_VALUE = 20
        };

        explicit DoubleStringifier( );

        virtual ~DoubleStringifier( );

        ::size_t stringifyDouble( double   v,
                                  char *   buffer,
                                  ::size_t bufferMaxCount );

    private:

        DoubleStringifier( const DoubleStringifier & );  // no copying
        DoubleStringifier & operator=( const DoubleStringifier & );  // no copying

        void stringifyDouble( double v, char * buffer );

        unsigned short firstBiasedBase10Exponent_[ 0x0800 + 1 ];

        double firstScale_[ 0x0800 ];
        double secondScale_[ 0x0800 ];

        char leadChunks_[ 100 ][ 1 ];
        char otherChunks_[ 10000 ][ 4 ];

        char exponents_[ 621 ][ 4 ];
};


}  // namespace carma::util
}  // namespace carma


inline ::size_t
carma::util::DoubleStringifier::stringifyDouble( const double   v,
                                                 char * const   buffer,
                                                 const ::size_t bufferMaxCount )
{
    if ( bufferMaxCount < MAX_CHARS_PER_VALUE )
        throw CARMA_ERROR( "Buffer too small" );

    stringifyDouble( v, buffer );

    return MAX_CHARS_PER_VALUE;
}


#endif
