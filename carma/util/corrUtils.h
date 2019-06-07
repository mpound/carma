#ifndef CARMA_UTIL_LIB_CORRUTILS_H
#define CARMA_UTIL_LIB_CORRUTILS_H
/**
 * Utility methods for the correlator.
 * @author Marc Pound
 */
#include "carma/util/CorrelatorType.h"

#include <string>
#include <map>

namespace carma {

  namespace util {

    static const unsigned NUM_SLC_BANDS = 8;
    static const unsigned NUM_WBC_BANDS = 16;
    static const unsigned NUM_C3G_BANDS = 8;

  class AstroBand;

      /**
       * @typedef hardwareType
       * The type of board hardware in a given band.
       * These match cobra::hardwareType.
       */
      typedef enum hardwareTypeEnum {
          HARDWARE_TYPE_UNKNOWN, ///< Unknown hardware
          HARDWARE_TYPE_COBRA,   ///< COBRA board hardware
          HARDWARE_TYPE_CARMA,   ///< CARMA board hardware
          HARDWARE_TYPE_C3G      ///< CARMA 3rd generation correlator hardware
      } hardwareType;

      /**
       * @return The hardware type of a given astro band.  These are
       * currently hardcoded such that bands 1-3 are cobra,
       * bands 4-8 are CARMA, rest are UNKNOWN.
       * @param  bandNo The input band number (one-based).
       */
      hardwareType hwType( unsigned int astroBandNo );
      hardwareType hwType(carma::util::AstroBand& band);

      /**
       * @return string representing hardware type
       */
      const ::std::string getStringForHardwareType( const hardwareType hwType );


      /**
       * This method returns the true bandwidth of a correlator band,
       * given CARMA's "rounded" description of a bandwidth.
       * @param bwstring A fiducial string representing the bandwidth
       * description, e.g. "500MHz", "31MHz", etc.
       * @return the actual correlator bandwidth in MHz given the 
       * fiducial string
       *
       * Yes, I would have like to have an enum instead of a string, 
       * but control and correlator namespaces both define CORR_BW enums,
       * and trying to mix and match them might cause dependency issues.
       */
      float actualBandwidth( const ::std::string & bwstring );

       /**
        * @return the correlator bandwidth enum for a given float value of 
        * the bandwidth. 
        * @param bw the bandwidth in GHz
        */
      carma::util::CorrelatorBandWidthType 
            getBandWidthTypeForBandWidth( const float bw );



      /**
       * @return The number of expected correlator bands for an astroband 
       * with the given configuration, or zero for unrecognized astroband
       * configuration.
       * @param abConf The astroband configuration, as from the signalpath
       * @param corrType The correlator designation type, e.g. 
       *                 spectral line or wideband.  This parameter is needed
       *                 because some astroband configurations have differing 
       *                 numbers of channels depending on the correlator type.
       *                 Default: CORR_SPECTRAL
       *
       */
      unsigned numExpectedCorrBands( 
              const ::std::string & abConf, 
              const CorrelatorType corrType = CORR_SPECTRAL);

      /**
       * @return the number of expected channels in an astroband given
       * that astroband's setup info, or zero for incompatible 
       * info.   NOTE: Number of channels returned assumes
       * end half channels are dropped.
       * @param corrType The correlator designation type, e.g. 
       *                 spectral line or wideband
       * @param bwType  The bandwidth designation, e.g. 500 MHz
       *    Note for corrType = CORR_WIDEBAND, this  parameter will be 
       *    ignored since only 500MHz mode is available therein.
       * @param bitType  The bitmode designation, e.g. CORR_3BIT.  
       *    Note for corrType = CORR_WIDEBAND, this parameter will be 
       *    ignored since only 2BIT mode is available therein.
       */
      unsigned numExpectedAstroChans(
              const CorrelatorType corrType, 
              const CorrelatorBandWidthType bwType,
              const CorrelatorBitType bitType,
              const CorrelatorFpgaModeType corrFpgaMode );

      /**
       * @return the correlator efficiency due to the 
       * the Van Vleck correction applied to this astroband.
       * @param bitType  The bitmode designation, e.g. CORR_3BIT.  
       */
      float correlatorEfficiency(const CorrelatorBitType bitType);

      carma::util::CorrelatorType getCorrTypeForSaNo( const int saNo );
      carma::util::CorrelatorType getCorrTypeForAstroBandNo( const int abNo );
      ::std::string getStringForCorrType(const carma::util::CorrelatorType corr);
      /**
       * @return the first astroband number for a given correlator type
       * @param corrType The correlator designation type
      unsigned firstAstroBandNo(const CorrelatorType type);
      unsigned lastAstroBandNo(const CorrelatorType type);
       */
  }
}
#endif //CARMA_UTIL_LIB_CORRUTILS_H
