#ifndef CARMA_SERVICES_PLANET_TEMPERATURE_H
#define CARMA_SERVICES_PLANET_TEMPERATURE_H

#include <vector>
#include <boost/shared_ptr.hpp>

#include "carma/services/Astro.h"
#include "carma/services/Frequency.h"
#include "carma/services/Interpolator.h"
#include "carma/services/Table.h"
#include "carma/services/Temperature.h"


typedef boost::shared_ptr<carma::services::Interpolator> interp_ptr;

namespace carma {
  namespace services {

  class PlanetTemperature {
      public:

      /**
       * @param name The planet represented
       * as an Astro::planetType There must be a
       * data table conf/data/<planetname>tb.tab,
       * where <planetname> is the lower case planet
       * name.
       * @see carma::services::constants::Astro
       * @see carma::services::Planet
       */
      PlanetTemperature( const constants::Astro::planetType & ptype );

      virtual ~PlanetTemperature( );

      /**
       * @return the brightness temperature of a
       * planet at the given mjd and frequency.
       */
      const Temperature brightnessTemperature( const double mjd,
                                           const Frequency & freq);

      /** 
       * @return underlying Table, mostly for debugging
       */
      const Table getTable() const {return table_;}

      private:
      void initialize( const constants::Astro::planetType & ptype );
      // Table containing Tb data
      Table table_;
      // Interpolators for each column of Tb data, as
      // a function of time.
      ::std::vector<interp_ptr> timeInterp_;
      // vector containing frequencies of input Tb data
      ::std::vector<double> tbfreqs_;

  };

  } // services
} // carma


#endif //CARMA_SERVICES_PLANET_TEMPERATURE_H
