#ifndef CARMA_UTIL_BYTE_STRINGIFIER_H
#define CARMA_UTIL_BYTE_STRINGIFIER_H


#include "carma/util/ErrorException.h"


namespace carma {
namespace util {


class ByteStringifier {
    public:

        enum {
            MAX_CHARS_PER_VALUE = 3
        };

        explicit ByteStringifier( );
        
        virtual ~ByteStringifier( );

        ::size_t stringifyByte( unsigned char v,
                                char *        buffer,
                                ::size_t      bufferMaxCount ) const;

    private:
        // No copying
        ByteStringifier( const ByteStringifier & rhs );  
        ByteStringifier & operator=( const ByteStringifier & rhs );
        
        const char * const table_;
};


}  // namespace carma::util
}  // namespace carma


inline ::size_t
carma::util::ByteStringifier::stringifyByte(
    const unsigned char v,
    char * const        buffer,
    const ::size_t      bufferMaxCount ) const
{
    if ( (bufferMaxCount < MAX_CHARS_PER_VALUE) || (MAX_CHARS_PER_VALUE < 3) )
        throw CARMA_ERROR( "Buffer too small" );
    
    const unsigned int i = static_cast< unsigned int >( v ) * 4U;

    buffer[ 0 ] = table_[ i + 0 ];
    buffer[ 1 ] = table_[ i + 1 ];
    buffer[ 2 ] = table_[ i + 2 ];
    
    return 3;
}


#endif
