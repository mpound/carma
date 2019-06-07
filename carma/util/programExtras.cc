#include "carma/util/programExtras.h"

#include <stdexcept>

#include "carma/util/Program.h"

using namespace ::std;
using namespace log4cpp;
using namespace carma::util;


Category &
carma::util::getProgramLogger( ) {
    return ProgramBase::getLogger( );
}


Category *
carma::util::getProgramLoggerIfAvailable( ) {
    return ProgramBase::getLoggerIfAvailable( );
}


Trace &
carma::util::getProgramTraceObject( ) {
    Trace * t = ProgramBase::getTraceObject( );
    
    if ( t == 0 )
        throw runtime_error( "ProgramBase::getTraceObject returned NULL" );
        
    return *t;
}


Trace *
carma::util::getProgramTraceObjectIfAvailable( ) {
    return ProgramBase::getTraceObjectIfAvailable( );
}
