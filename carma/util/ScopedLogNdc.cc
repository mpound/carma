#include "carma/util/ScopedLogNdc.h"

#include <log4cpp/NDC.hh>

using namespace ::std;
using namespace log4cpp;
using namespace carma;
using namespace carma::util;


ScopedLogNdc::ScopedLogNdc( const char * const s )
{
    const string ss = s;

    NDC::push( '[' + ss + ']' );
}


ScopedLogNdc::ScopedLogNdc( const string & s )
{
    NDC::push( '[' + s + ']' );
}


ScopedLogNdc::ScopedLogNdc( const char * const s,
                            const char * const prefix,
                            const char * const suffix )
{
    const string ss = s;
    
    NDC::push( prefix + ss + suffix );
}


ScopedLogNdc::ScopedLogNdc( const string & s,
                            const string & prefix,
                            const string & suffix )
{
    NDC::push( prefix + s + suffix );
}


ScopedLogNdc::~ScopedLogNdc( )
try {
    NDC::pop();
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}
