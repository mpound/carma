#include "carma/services/trigFunctions.h"

#include "carma/services/Angle.h"
#include "carma/services/stringConstants.h"
#include "carma/util/IllegalArgumentException.h"

#include <cerrno>
#include <fenv.h>

using namespace carma;
using namespace carma::util;
using namespace carma::services;

namespace {
    
    const int USE_EXCEPTIONS = false; // Errno set for all but underflow

    void clearErrorStates( ) 
    {
        if ( USE_EXCEPTIONS ) {
            if ( !feclearexcept( FE_ALL_EXCEPT ) ) {
                throw CARMA_EXCEPTION( ErrorException, 
                        "feclearexcept failed!" );
            }
        }

        errno = 0;
    };

    void checkForError( const int localErrno )
    {
        if ( USE_EXCEPTIONS ) {
            const int except = fetestexcept( FE_INVALID | FE_DIVBYZERO | 
                                             FE_OVERFLOW | FE_UNDERFLOW );
            if ( except ) {

                if ( except & FE_INVALID ) { 
                    throw CARMA_EXCEPTION( 
                        ErrorException,  "Invalid operation." );
                } else if ( except & FE_DIVBYZERO ) {
                    throw CARMA_EXCEPTION( 
                        ErrorException, "Divide by zero." );
                } else if ( except & FE_OVERFLOW ) {
                    throw CARMA_EXCEPTION( 
                        ErrorException, 
                        "Result not representable due to overflow." );
                } else if ( except & FE_UNDERFLOW ) {
                    throw CARMA_EXCEPTION( 
                        ErrorException, 
                        "Result not representable due to underflow." );
                } else {
                    throw CARMA_EXCEPTION( ErrorException, "Unknown." );
                }
            }
        }
        
        if ( localErrno ) {
            throw CARMA_EXCEPTION( IllegalArgumentException, 
                    strerror( localErrno ) );
        }
    };

} // namespace <unnamed>

double 
services::sin( const Angle & angle ) 
{
    clearErrorStates( );

    const double answer = ::std::sin( angle.radians( false ) );

    checkForError( errno );

    return answer;
}

double
services::cos( const Angle & angle )
{
    clearErrorStates( );

    const double answer = ::std::cos( angle.radians( false ) );

    checkForError( errno );

    return answer;
}

double
services::tan( const Angle & angle )
{
    clearErrorStates( );

    const double answer = ::std::tan( angle.radians( false ) );

    checkForError( errno );

    return answer;
}

services::Angle
services::asin( const double x )
{
    clearErrorStates( );

    const double answer = ::std::asin( x );

    checkForError( errno );

    return Angle( answer, RADIANS );
}

services::Angle
services::acos( const double x ) 
{
    clearErrorStates( );

    const double answer = ::std::acos( x );

    checkForError( errno );

    return Angle( answer, RADIANS );
}

services::Angle
services::atan( const double x )
{
    clearErrorStates( );

    const double answer = std::atan( x );

    checkForError( errno );

    return Angle( answer, RADIANS ); 
}

services::Angle
services::atan2( const double x, const double y )
{
    clearErrorStates( );

    const double answer = std::atan2( x, y );

    checkForError( errno );

    return Angle( answer, RADIANS );
}
