#ifndef CARMA_SERVICES_WOODY119GHZO2LINE_H
#define CARMA_SERVICES_WOODY119GHZO2LINE_H

#include "carma/services/OpacityModel.h"

namespace carma {
  namespace services {
    
    /**
     * Opacity model based on a simple model entitled "119 GHz Atmpospheric 
     * Oxygen Line" by Dave Woody dated 4 Jan 1989.
     */
    class Woody119GHzO2Line : public OpacityModel {
    public:

        Woody119GHzO2Line( );
        
        ~Woody119GHzO2Line( );

        double calculateOpacityAtZenith( double frequency,
                                         double temperature,
                                         double dewpoint,
                                         double pressure,
                                         double humidity ) const;

        static double calculateOpacityAtZenith( double frequency,
                                                double temperature,
                                                double pressure );

    }; // End class Woody119GHzO2Line
  } // End namespace services 
} // End carma
#endif
