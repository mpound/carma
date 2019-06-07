// $Id: trigFunctions.h,v 1.1 2007/06/14 18:38:10 abeard Exp $
#ifndef CARMA_SERVICES_TRIGFUNCTIONS_H
#define CARMA_SERVICES_TRIGFUNCTIONS_H

namespace carma {
namespace services {

    class Angle;

    /**
     * Convenient trigonometric functions utilizing the Angle class.
     * All throw ErrorException or IllegalArgumentException on failure.
     */

    double sin( const Angle & angle );
    double cos( const Angle & angle );
    double tan( const Angle & angle );

    Angle asin( double x );
    Angle acos( double x );
    Angle atan( double x );
    Angle atan2( double x, double y );

} // namespace services
} // namespace carma

#endif
