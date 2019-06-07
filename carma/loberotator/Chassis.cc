
/**@file
 * Class definition for Lobe Rotator Master for Carma.
 * Written with heavy kibitzing from wbdcMaster.
 *
 * @author Steve Scott
 * $Id: Chassis.cc,v 1.2 2005/07/16 00:16:01 scott Exp $
 */

#include <sstream>

// Carma includes
#include "carma/util/ErrorException.h"
#include "carma/loberotator/Chassis.h"

using namespace std;
using namespace carma;
using namespace carma::loberotator;
using namespace carma::util;

// *********************************************************************
Chassis::Chassis() : nextChan(0) 
{
}

// *********************************************************************
Chassis::~Chassis() 
{        
}

// *********************************************************************
void Chassis::insert(Loberotator* lr)
{
    if ((nextChan < 0) || (nextChan >= N_CHAN)) {
        std::ostringstream o;
        o << "Channel number (" << nextChan << ") must be in range [0"
          << "-" << N_CHAN-1 << "]" ;
        throw CARMA_ERROR(o); 
    }
    loberotator_[nextChan++] = lr;
}

// *********************************************************************
loberotator::Loberotator& Chassis::loberotator(int chanIndex) const
{
    if ((chanIndex < 0) || (chanIndex >= nextChan)) {
        ostringstream o;
        o << "Channel number (" << chanIndex << ") must be in range [0"
          << "-" << nextChan-1 << "]" ;
        throw CARMA_ERROR(o); 
    }
    return *loberotator_[chanIndex];
}

