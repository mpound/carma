#ifndef CARMA_UTIL_FLOAT_STRINGIFIER_H
#define CARMA_UTIL_FLOAT_STRINGIFIER_H

#include "carma/util/ErrorException.h"


namespace carma {
namespace util {


class FloatStringifier {
    public:

        enum {
            MAX_CHARS_PER_VALUE = 13
        };

        explicit FloatStringifier( );
        
        virtual ~FloatStringifier( );

        ::size_t stringifyFloat( float    v,
                                 char *   buffer,
                                 ::size_t bufferMaxCount );

    private:
    
        FloatStringifier( const FloatStringifier & );  // no copying
        FloatStringifier & operator=( const FloatStringifier & );  // no copying
        
        void stringifyFloat( float v, char * buffer );

        double firstScale_[ 0x0100 ];
        double secondScale_[ 0x0100 ];

        unsigned short firstBiasedBase10Exponent_[ 0x0100 + 1 ];

        char leadChunks_[ 100 ][ 3 ];
        char otherChunks_[ 10000 ][ 4 ];

        char exponents_[ 621 ][ 4 ];
};


}  // namespace carma::util
}  // namespace carma


inline ::size_t
carma::util::FloatStringifier::stringifyFloat( const float    v,
                                               char * const   buffer,
                                               const ::size_t bufferMaxCount )
{
    if ( bufferMaxCount < MAX_CHARS_PER_VALUE )
        throw CARMA_ERROR( "Buffer too small" );

    stringifyFloat( v, buffer );

    return MAX_CHARS_PER_VALUE;
}


#endif
