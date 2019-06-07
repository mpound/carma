#include "carma/util/memoryUtils.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


bool
carma::util::valueIsMultiple( const size_t value,
                              const size_t alignment ) {
    return ((value % alignment) == 0);
}


size_t
carma::util::roundUpToMultiple( const size_t value,
                                const size_t alignment ) {
    size_t result;
    
    if ( (value % alignment) == 0 )
        result = value;
    else
        result = ((value / alignment) + 1) * alignment;
    
    return result;
}


bool
carma::util::pointerIsAligned( const void * const ptr,
                               const size_t       alignment ) {
    const size_t ptrAsScalar = reinterpret_cast< size_t >( ptr );
    
    return ((ptrAsScalar % alignment) == 0);
}


namespace {

const size_t kVmPageBytes = 4096;

}  // namespace < anonymous >


bool
carma::util::valueIsVmPageMultiple( const size_t value ) {
    return valueIsMultiple( value, kVmPageBytes );
}


size_t
carma::util::roundUpToVmPageMultiple( const size_t value ) {
    return roundUpToMultiple( value, kVmPageBytes );
}


bool
carma::util::pointerIsVmPageAligned( const void * const ptr ) {
    return pointerIsAligned( ptr, kVmPageBytes );
}


const void *
carma::util::byteOffsetPointer( const void * const ptr,
                                const size_t       byteOffset ) {
    const char * const result =
        static_cast< const char * >( ptr ) + byteOffset;
    
    return result;
}


void *
carma::util::byteOffsetPointer( void * const ptr,
                                const size_t byteOffset ) {
    char * const result =
        static_cast< char * >( ptr ) + byteOffset;
    
    return result;
}


size_t
carma::util::getDefaultVmMemoryCopyMinWinBytes( ) {
    return 65536;
}
