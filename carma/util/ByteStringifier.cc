#include "carma/util/ByteStringifier.h"

using namespace carma;
using namespace carma::util;

namespace {


const char *
makeTable( )
{
    char * const table = new char[ 256UL * 4UL ];
    
    for ( unsigned int v = 0; v < 256; ++v ) {
        const unsigned int i = v * 4U;
        
        table[ i + 2 ] = static_cast< char >( '0' + ((v / 1) % 10) );
        
        if ( v < 10 )
            table[ i + 1 ] = ' ';
        else
            table[ i + 1 ] = static_cast< char >( '0' + ((v / 10) % 10) );

        if ( v < 100 )
            table[ i + 0 ] = ' ';
        else
            table[ i + 0 ] = static_cast< char >( '0' + ((v / 100) % 10) );
    }
    
    return table;
}


}  // namespace < anonymous >


ByteStringifier::ByteStringifier( ) :
table_( makeTable() )
{
}


ByteStringifier::~ByteStringifier( )
try {
    delete [ ] table_;
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}
