#ifndef CARMA_SERVICES_WATERS90GHZ_H
#define CARMA_SERVICES_WATERS90GHZ_H

#include "carma/services/Atmosphere.h"
#include "carma/services/OpacityModel.h"

namespace carma {
  namespace services {

    /**
     * Simple coeficient based opacity model at 90 GHz from Waters as 
     * described in TMS table 13.1.
     */
    class Waters90GHz : public OpacityModel {
    public:

        explicit Waters90GHz( );

        ~Waters90GHz( );

        double calculateOpacityAtZenith( double frequency,
                                         double temperature,
                                         double dewpoint,
                                         double pressure,
                                         double humidity ) const;

    private:
        
        const carma::services::Atmosphere atmosphere_; 

    }; // End class Waters90GHz
  } // End services 
} // End carma 
#endif
