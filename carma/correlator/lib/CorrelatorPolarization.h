#ifndef CORRELATORPOLARIZATION_H
#define CORRELATORPOLARIZATION_H

#include "carma/monitor/SignalPathCommonMonitorPoints.h"

namespace carma {
  namespace correlator {
    namespace lib {

	typedef enum {
	  NONE_POL,
	  LEFT_POL,
	  RIGHT_POL,
	  HORIZONTAL_POL,
	  VERTICAL_POL
	} Polarization;

    typedef std::pair< int, Polarization > AntNoPolPair;

    carma::monitor::PolarizationMonitorPointEnum::POLARIZATION 
    corrPolToMonPol( Polarization corrPol ); 

    Polarization
    monPolToCorrPol(carma::monitor::PolarizationMonitorPointEnum::POLARIZATION);


    }
  }
};

#endif
