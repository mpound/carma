#ifndef CARMA_SERVICES_OPACITY_MODEL_H
#define CARMA_SERVICES_OPACITY_MODEL_H

namespace carma {
  namespace services {

      /**
       * An atmospheric model is used to estimate atmospheric opacity given
       * local environment variables ( temperature, pressure, etc ) and 
       * observing variables ( frequency ).  Since no one model has been
       * established yet, this base class exists to allow models to be
       * easily interchanged while establishing the basic interface that
       * should satisfy any needed model.
       */
      class OpacityModel {
      public:

          explicit OpacityModel( );

          virtual ~OpacityModel( );

          /**
           * Calculate atmospheric opacity at the zenith (Tau0) in Nepers.
           * Note that not all models will use all input parameters. Note 
           * also that the input parameters include only measured quantities,
           * other parameters, such as water vapor density, can be calculated
           * from these using the carma::services::Atmosphere class. 
           * @param frequency Frequency in GHz.
           * @param temperature Ambient temperature in Kelvin.
           * @param dewpoint Dewpoint temperature in Kelvin.
           * @param pressure Barometric pressure in millibar.
           * @param humidity Relative humidity in percent.
           */
          virtual double 
          calculateOpacityAtZenith( double frequencyInGHz,
                                    double temperatureInK,
                                    double dewpointInK,
                                    double pressureInMbar,
                                    double humidityInPercent ) const = 0;

      }; // End class AtmosphericOpacityModel
  } // End namespace services 
} // End namespace carma
#endif
