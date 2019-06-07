#ifndef CARMA_SERVICES_FREQDEPENDENT225GHZ_H
#define CARMA_SERVICES_FREQDEPENDENT225GHZ_H

#include "carma/services/Atmosphere.h"
#include "carma/services/OpacityModel.h"

namespace carma {
  namespace services {

    /**
     * Simple frequency dependent opacity model for 1mm observations.
     */
    class FreqDependent225GHz : public OpacityModel {
    public:

        explicit FreqDependent225GHz( );

        ~FreqDependent225GHz( );
        
        double calculateOpacityAtZenith( double frequency,
                                         double temperature,
                                         double dewpoint,
                                         double pressure,
                                         double humidity ) const;

    private:
        
        const carma::services::Atmosphere atmosphere_;

    }; // class FreqDependent225GHz

  } // End namespace services 
} // End namespace carma
#endif
