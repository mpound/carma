#include "carma/correlator/lib/CorrelatorPolarization.h"
#include "carma/util/IllegalArgumentException.h"

carma::monitor::PolarizationMonitorPointEnum::POLARIZATION 
carma::correlator::lib::corrPolToMonPol( 
    const carma::correlator::lib::Polarization corrPol )
{
    switch ( corrPol ) {
        case NONE_POL: 
            return carma::monitor::PolarizationMonitorPointEnum::UNKNOWN;
        case LEFT_POL:
            return carma::monitor::PolarizationMonitorPointEnum::L;
        case RIGHT_POL:
            return carma::monitor::PolarizationMonitorPointEnum::R;
        case HORIZONTAL_POL:
            return carma::monitor::PolarizationMonitorPointEnum::H;
        case VERTICAL_POL:
            return carma::monitor::PolarizationMonitorPointEnum::V;
        default:
            throw CARMA_EXCEPTION( carma::util::IllegalArgumentException,
                    "Invalid polarization." );
    }
}

carma::correlator::lib::Polarization
carma::correlator::lib::monPolToCorrPol(
    const carma::monitor::PolarizationMonitorPointEnum::POLARIZATION monPol )
{
    switch ( monPol ) {
        case carma::monitor::PolarizationMonitorPointEnum::UNKNOWN:
            return NONE_POL;
        case carma::monitor::PolarizationMonitorPointEnum::L:
            return LEFT_POL;
        case carma::monitor::PolarizationMonitorPointEnum::R:
            return RIGHT_POL;
        case carma::monitor::PolarizationMonitorPointEnum::H:
            return HORIZONTAL_POL;
        case carma::monitor::PolarizationMonitorPointEnum::V:
            return VERTICAL_POL;
        default:
            throw CARMA_EXCEPTION( carma::util::IllegalArgumentException,
                    "Invalid polarization." );
    }
}
