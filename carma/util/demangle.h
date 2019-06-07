#ifndef CARMA_UTIL_DEMANGLE_H
#define CARMA_UTIL_DEMANGLE_H

//! @file
//! Interface file for the demangling routines.

#include <string>


namespace std {
	
class type_info;
	
}  // namespace std


namespace carma {
namespace util {

//! Returns a human readable name for the type whose compiler mangled
//! name is passed in.
//! @param mangledName Compiler mangled name for the type.
//! @return Human readable name for the type.
::std::string demangleTypeName( const char * mangledName );
::std::string demangleTypeName( const ::std::string & mangledName );

//! Returns a human readable name for the type whose ::std::type_info
//! is passed in.
//! @param typeInfo ::std::type_info for the type.
//! @return Human readable name for the type.
::std::string demangleTypeName( const ::std::type_info & typeInfo );


//! Returns a human readable name for the entry point whose compiler mangled
//! name is passed in.
//! @param mangledName Compiler mangled name for the entry point.
//! @return Human readable name for the entry point.
::std::string demangleEntryPointName( const char * mangledName );
::std::string demangleEntryPointName( const ::std::string & mangledName );


}  // namespace carma::util
}  // namespace carma


#endif
