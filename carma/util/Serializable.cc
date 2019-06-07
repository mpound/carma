#include "carma/util/Serializable.h"

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"

#include <sstream>

using namespace ::std;
using namespace carma;
using namespace carma::util;

int32_t Serializable::version_;

int32_t Serializable::getVersion(){
  return Serializable::version_;
}

void
Serializable::throwByteArrayTooSmallError( const int neededBytes,
                                           const int availBytes )
{
    ostringstream oss;
    
    oss << "Available buffer of " << availBytes
        << " bytes is smaller than needed size of " << neededBytes
        << " bytes";
        
    throw CARMA_ERROR( oss.str() );
}

void
Serializable::throwByteArrayOverrunError( const int bufferSize,
                                          const int finalOffset )
{
    ostringstream oss;
    
    oss << "Byte array overrun detected on buffer of size " << bufferSize 
        << " with a final overrun offset position of " << finalOffset << ".";
        
    throw CARMA_ERROR( oss.str() );
}
