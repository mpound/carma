#include "carma/util/demangle.h"

#include <typeinfo>

#include <cxxabi.h>

#include "carma/util/AutoFreedMallocPtr.h"


using namespace ::std;
using namespace ::abi;
using namespace carma;
using namespace carma::util;


namespace {


string
demangleSymbol( const char * const symbol )
{
    string result;

    if ( symbol != 0 ) {
        ::size_t length = 0;
        int status;
        
        const AutoFreedMallocPtr< char >
            demangledSymbol( __cxa_demangle( symbol, 0, &length, &status ) );
        
        if ( demangledSymbol.get() == 0 )
            result = symbol;
        else
            result = demangledSymbol.get();
    }

    return result;
}


}  // namespace < anonymous >


string
carma::util::demangleEntryPointName( const char * const x )
{
    string result;
    
    if ( (x == 0) || (x[ 0 ] != '_') || (x[ 1 ] != 'Z') )
        result = x;
    else
        result = demangleSymbol( x );
        
    return result;
}


string
carma::util::demangleEntryPointName( const string & x )
{
    return demangleEntryPointName( x.c_str( ) );
}


string
carma::util::demangleTypeName( const char * const x )
{
    return demangleSymbol( x );
}


string
carma::util::demangleTypeName( const string & x )
{
    return demangleTypeName( x.c_str( ) );
}


string
carma::util::demangleTypeName( const type_info & x )
{
    return demangleTypeName( x.name( ) );
}
