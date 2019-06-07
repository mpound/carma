/**
 * @file 
 * carma::services::AzElPair class definition.
 *
 * </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.1 $
 * $Date: 2009/08/28 21:30:30 $
 * $Id: AzElPair.cc,v 1.1 2009/08/28 21:30:30 abeard Exp $
 */

#include "carma/services/AzElPair.h"

using namespace carma::services;

AzElPair::AzElPair( ) :
    azimuth( 0.0, Angle::RADIANS_STR ), 
    elevation( 0.0, Angle::RADIANS_STR )
{
    // Nothing
}

AzElPair::AzElPair( const Angle & az, const Angle & el ) :
    azimuth( az ),
    elevation( el ) 
{ 
    // Nothing
}

AzElPair::AzElPair( const AzElPair & copyFrom ) :
    azimuth( copyFrom.azimuth ),
    elevation( copyFrom.elevation )
{ 
    // Nothing
}

AzElPair & 
AzElPair::operator=( const AzElPair & rhs ) 
{
    if ( this != &rhs ) {
        azimuth = rhs.azimuth;
        elevation = rhs.elevation;
    }
    return *this;
}
