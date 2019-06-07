/**
 *
 * @file  carma/services/Global.h
 * Common parameters used CARMA-wide, both typedefs and constants
 * such as number of antennas.
 *
 * @author: Marc Pound
 *
 * $Id: Global.h,v 1.8 2011/01/28 23:54:29 abeard Exp $
 * $CarmaCopyright$
 *
 */

#include <string>

#ifndef CARMA_SERVICES_GLOBAL_H
#define CARMA_SERVICES_GLOBAL_H

namespace carma {
  namespace services {


      /**
       * The Global class contains methods that return "fixed" CARMA
       * values, such as the numbers of each type of antenna.
       * The rationale for using accessors instead of static constants
       * is to allow us change the implementation of where we get these 
       * constants (for instance, read them from a file or a DBMS).
       */
      class Global {
      public:

	  /**
	   * @return The maximum number of antennas in CARMA
	   */
	  // we follow the "Rauch rule" here: "no mutator, no get prefix"
  	  static unsigned short maxAntennas() { return MAX_ANT; }

	  /**
	   * @return The total number of SZA antennas 
	   */
  	  static unsigned short nSzaAntennas() { return N_SZA_ANT; }

	  /**
	   * @return The total number of OVRO antennas 
	   */
  	  static unsigned short nOvroAntennas() { return N_OVRO_ANT; }

	  /**
	   * @return The total number of BIMA antennas 
	   */
  	  static unsigned short nBimaAntennas() { return N_BIMA_ANT; }

      /**
       * @return The total number of Wideband system bands
       */
      static unsigned short nWidebandBands() { return N_WB_BANDS; }

      /**
       * @return The total number Spectral-line system bands
       */
      static unsigned short nSpectralLineBands() { return N_SL_BANDS; } 

      /**
       * @return The total number of wideband correlator stations.
       */
      static unsigned short nWidebandStations() { return N_WB_STATIONS; }

      /**
       * @return The total number of spectral-line correlator stations
       */
      static unsigned short nSpectralStations() { return N_SL_STATIONS; }

      /**
       * @return The total number of astro bands
       */
      static unsigned short nAstroBands() { return 24; }

      /**
       * @return The total number of astro bands
       */
      static unsigned short nAstroInputs() { return 32; }

      private:
	  /** maximum number of antennas in CARMA */
	  static const unsigned short MAX_ANT    = 23;

	  /** maximum number of SZA antennas */
	  // prefix N corresponds to Style Rule 3-21.
	  static const unsigned short N_SZA_ANT  =  8;

	  /** maximum number of OVRO antennas */
	  static const unsigned short N_OVRO_ANT =  6;

	  /** maximum number of BIMA antennas */
	  static const unsigned short N_BIMA_ANT =  9;

      /** maximum number of Wideband bands */
      static const unsigned short N_WB_BANDS = 16;

      /** maximum number of Spectral-line bands */
      static const unsigned short N_SL_BANDS = 8;

      /** maximum number of wideband correlator stations */
      static const unsigned short N_WB_STATIONS = 8;

      /** maximum number of spectral line correlator stations */
      static const unsigned short N_SL_STATIONS = 15;

	  /** Default constructor */
	  Global();

	  /** Destructor */
	  ~Global();

    };

  } 
}  // End namespace carma::services  

#endif  // CARMA_SERVICES_GLOBAL_H
