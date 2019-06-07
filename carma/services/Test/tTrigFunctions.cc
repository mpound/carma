
#include "carma/services/trigFunctions.h"

#include "carma/services/Angle.h"
#include "carma/util/Program.h"

#include <cassert>
#include <errno.h>
#include <iomanip>
#include <iostream>
#include <limits>

using namespace carma;
using namespace carma::util;
using namespace std;

namespace {
    
    bool
    equalEnough( const double testVal, const double canonicalVal )
    {
        // Test to make sure that testVal is equal to within next highest
        // and lowest representable numbers.  Strict equality fails with
        // below tests most likely because extra bits are used.
        const double max = numeric_limits< double >::max( );
        const double hi = nextafter( canonicalVal, max );
        const double lo = nextafter( canonicalVal, -1.0 * max );
    
        return ( testVal >= lo && testVal <= hi );
    };

} // namespace < unnamed >

/**
 * @version $Id: tTrigFunctions.cc,v 1.4 2007/06/28 04:56:50 abeard Exp $
 *
 * @usage tTrigUtils
 *
 * @description A test program for trigFunctions 
 *
 * @noKeys
 *
 *@logger DEFAULT_FACILITY carma.services.Test.tTrigUtils
 */
int Program::main( )
{
    const double ANGLE_IN_RADIANS = 1.2;
    const services::Angle a( ANGLE_IN_RADIANS, services::Angle::RADIANS_STR );

    assert( equalEnough( services::cos( a ), cos( ANGLE_IN_RADIANS ) ) );
    assert( equalEnough( services::sin( a ), sin( ANGLE_IN_RADIANS ) ) );
    assert( equalEnough( services::tan( a ), tan( ANGLE_IN_RADIANS ) ) );

    const double x = -3.0 / 5.0;
    const double y = -4.0 / 5.0;
    assert( equalEnough( services::acos( y ).radians( false ), acos( y ) ) );
    assert( equalEnough( services::atan( y ).radians( false ), atan( y ) ) );
    assert( equalEnough( services::atan2( x, y ).radians( false ), 
                         atan2( x, y ) ) );
    assert( equalEnough( services::asin( x ).radians( false ), asin( x ) ) );

    try {
        const double outOfRange = 2.0;
        services::asin( outOfRange );
        assert( false );
    } catch ( ... ) {
        assert( true );
    }

    // test multiplication and division by a scalar
    services::Angle d( M_PI, services::Angle::RADIANS_STR );
    d = d * 0.5;
    assert( d.degrees( ) == 90.0 );

    d *= 2.0;
    assert( d.degrees( ) == 180.0 );

    d /= 2.0;
    assert( d.degrees( ) == 90.0 );

    services::Angle e = d / 2.0;
    assert( e.degrees( ) == 45.0 );

    // Test comparisons
    const services::Angle Pi( M_PI, services::Angle::RADIANS_STR );
    const services::Angle Zero;
    assert( Pi < ( 2.0 * Pi ) );
    assert( Pi <= ( 2.0 * Pi ) );
    assert( Pi < ( 2.25 * Pi ) ); // Verify no modulo 2 pi
    assert( Pi <= ( 2.25 * Pi ) ); // Verify no modulo 2 pi
    assert( Pi > Zero );
    assert( Pi >= Zero );
    assert( Pi > ( -2.0 * Pi ) );
    assert( Pi >= ( -2.0 * Pi ) );
    
    assert( Pi >= Pi );
    assert( Pi <= Pi );

    return 0;
}
