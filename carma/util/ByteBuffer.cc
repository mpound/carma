#include "carma/util/ByteBuffer.h"

#include <cstdlib>

#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


void
ByteBuffer::internalFree( )
{
    ::free( ptr_ );
}


void
ByteBuffer::internalDestructiveReserve( const size_t count )
{
    char * const p = static_cast< char * >( ::malloc( count ) );
    
    if ( p == 0 )
        throw CARMA_ERROR( "malloc failure" );
        
    if ( ptr_ != 0 )
        ::free( ptr_ );
        
    ptr_ = p;
    allocSize_ = count;
}
