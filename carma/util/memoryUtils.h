#ifndef CARMA_UTIL_MEMORYUTILS_H
#define CARMA_UTIL_MEMORYUTILS_H


#include <cstdlib>


namespace carma {
namespace util {


bool valueIsMultiple( size_t value, size_t alignment );
size_t roundUpToMultiple( size_t value, size_t alignment );
bool pointerIsAligned( const void * ptr, size_t alignment );


bool valueIsVmPageMultiple( size_t value );
size_t roundUpToVmPageMultiple( size_t value );
bool pointerIsVmPageAligned( const void * ptr );


const void * byteOffsetPointer( const void * ptr, size_t byteOffset );
void * byteOffsetPointer( void * ptr, size_t byteOffset );


size_t getDefaultVmMemoryCopyMinWinBytes( );


}  // namespace carma::util
}  // namespace carma


#endif
