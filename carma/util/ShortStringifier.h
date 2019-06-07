#ifndef CARMA_UTIL_SHORT_STRINGIFIER_H
#define CARMA_UTIL_SHORT_STRINGIFIER_H

#include "carma/util/ErrorException.h"


namespace carma {
namespace util {


class ShortStringifier {
    public:

        enum {
            MAX_CHARS_PER_VALUE = 6
        };

        explicit ShortStringifier( );
        
        virtual ~ShortStringifier( );

        ::size_t stringifyShort( short    v,
                                 char *   buffer,
                                 ::size_t bufferMaxCount ) const;

    private:
        // no copying
        ShortStringifier( const ShortStringifier & rhs );
        ShortStringifier & operator=( const ShortStringifier & rhs );
        
        static const char * makeTable( );
        
        enum {
            BIAS = 32768
        };
    
        const char * const table_;
};


}  // namespace carma::util
}  // namespace carma


inline ::size_t
carma::util::ShortStringifier::stringifyShort(
    const short    v,
    char * const   buffer,
    const ::size_t bufferMaxCount ) const
{
    if ( (bufferMaxCount < MAX_CHARS_PER_VALUE) || (MAX_CHARS_PER_VALUE < 6) )
        throw CARMA_ERROR( "Buffer too small" );
    
    const unsigned int i =
        static_cast< unsigned int >( static_cast< int >( v ) + BIAS ) * 8U;
    
    buffer[ 0 ] = table_[ i + 0 ];
    buffer[ 1 ] = table_[ i + 1 ];
    buffer[ 2 ] = table_[ i + 2 ];
    buffer[ 3 ] = table_[ i + 3 ];
    buffer[ 4 ] = table_[ i + 4 ];
    buffer[ 5 ] = table_[ i + 5 ];
    
    return 6;
}


#endif
