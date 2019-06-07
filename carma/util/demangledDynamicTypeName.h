#ifndef CARMA_UTIL_DEMANGLED_DYNAMIC_TYPE_NAME
#define CARMA_UTIL_DEMANGLED_DYNAMIC_TYPE_NAME

//! @file
//! Interface file for the demangledDynamicTypeName function template.

#include <string>
#include <typeinfo>

#include "carma/util/demangle.h"


namespace carma {
namespace util {

//! Returns a human readable name for the type of the passed in parameter.
//! @param t Parameter of type T.
//! @return Human readable name for the type T.
template < typename T >
::std::string
demangledDynamicTypeName( T & t ) {
	return demangleTypeName( typeid( t ) );
}

}  // namespace carma::util
}  // namespace carma


#endif

