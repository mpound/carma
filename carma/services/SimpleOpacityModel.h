#ifndef CARMA_SERVICES_SIMPLEOPACITYMODEL_H
#define CARMA_SERVICES_SIMPLEOPACITYMODEL_H

#include "carma/services/OpacityModel.h"
#include "carma/services/Waters90GHz.h"
#include "carma/services/Woody119GHzO2Line.h"
#include "carma/services/FreqDependent225GHz.h"

namespace carma {
  namespace services {

    /**
     * Simple first cut placeholder for the CARMA opacity model.
     * This will not be in service very long but should serve our 
     * needs temporarily until the powers that be decide on a more permanent
     * model.  This model is the superposition of the zenith opacity produced 
     * by Woody119GHzO2 and the FreqDependent225GHz model.
     */
    class SimpleOpacityModel : public OpacityModel {
    public:

        explicit SimpleOpacityModel( );

        ~SimpleOpacityModel( );

        double calculateOpacityAtZenith( double frequencyInGHz,
                                         double temperatureInK,
                                         double dewpointInK,
                                         double pressureInMbar,
                                         double humidityInPercent ) const;

    private:

        FreqDependent225GHz freq1mm_;
        Woody119GHzO2Line woodyModel_;
        Waters90GHz watersModel_;

    }; // End class SimpleOpacityModel
  } // End namespace services 
} // End namespace carma
#endif
