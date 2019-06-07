/**
 * @file 
 * carma::services::AzElPair class declaration.
 *
 * </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.1 $
 * $Date: 2009/08/28 21:30:30 $
 * $Id: AzElPair.h,v 1.1 2009/08/28 21:30:30 abeard Exp $
 */
#ifndef CARMA_SERVICES_AZELPAIR_H
#define CARMA_SERVICES_AZELPAIR_H

#include "carma/services/Angle.h"

namespace carma {
namespace services {

/**
 * Simple class to represent azimuth and elevation pair.
 */
class AzElPair {
public:

    carma::services::Angle azimuth;
    carma::services::Angle elevation;

    AzElPair( );
    
    AzElPair( const carma::services::Angle & az, 
              const carma::services::Angle & el );

    AzElPair( const AzElPair & copyFrom );

    AzElPair & operator=( const AzElPair & rhs );

protected:
    // Nothing
private:
    // Nothing
}; // class AzElPair

}}  // namespace carma::services
#endif
