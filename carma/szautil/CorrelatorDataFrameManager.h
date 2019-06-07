#ifndef SZA_UTIL_CORRELATORDATAFRAMEMANAGER_H
#define SZA_UTIL_CORRELATORDATAFRAMEMANAGER_H

/**
 * @file CorrelatorDataFrameManager.h
 * 
 * Tagged: Wed Mar 31 09:59:54 PST 2004
 * 
 * @author Erik Leitch
 */
#include <map>

#include "carma/szautil/Directives.h"

#if DIR_USE_CORR
#include "carma/correlator/lib/CorrelatorData.h"
#endif

// C header files from the array control code

#include "carma/szaarrayutils/szaregs.h"

#include "carma/szautil/CorrelatorBand.h"
#include "carma/szautil/RegMapDataFrameManager.h"
#include "carma/szautil/TimeVal.h"


#include <complex>

namespace cobra {
  class CorrelatorBand;
  class CorrelatorData;
  class AutoSpectra;
  class CrossSpectra;
}

namespace sza {
  namespace util {
    
    class DataFrame;

    class CorrelatorDataFrameManager : public RegMapDataFrameManager {
    public:
      
      /**
       * Copy constructor
       */
      CorrelatorDataFrameManager(CorrelatorDataFrameManager& fm);

      /**
       * Constructor with initialization from a DataFrame object.
       */
      CorrelatorDataFrameManager(bool archivedOnly=false, DataFrame* frame=0);

#if DIR_USE_CORR
      /**
       * Constructor from a CorrelatorData object
       */
      CorrelatorDataFrameManager(bool archivedOnly, 
				 carma::correlator::lib::
				 CorrelatorData& corrData);

      /**
       * Constructor from a CorrelatorData object
       */
      void setTo(carma::correlator::lib::CorrelatorData& corrData);

      /**
       * Constructor from a cobra::CorrelatorBand object
       */
      void setTo(cobra::CorrelatorBand& band, unsigned char antMask=0xff);

#endif

      /**
       * Destructor.
       */
      virtual ~CorrelatorDataFrameManager();
      
      /**
       * Initialize this object.
       */
      void initialize(bool archivedOnly=false, sza::util::DataFrame* frame = 0);

      /**
       * Reinitialize this frame before re-use.
       */
      void reinitialize();
      
      /**
       * A map of antenna number <--> baseline indices for
       * cross-correlations
       */
      std::map<unsigned int, std::map<unsigned int, unsigned int> > 
	crossBaselineIndex_;

      void setAntennaBasedGain(Complex<float> gain, 
			       unsigned iAnt, unsigned iBand, unsigned iChan);

      void printAntennaBasedGains();

      /**
       * Public method to set the received status for a set of bands
       */
      void setBandReceived(CorrelatorBand::Id id, bool received);

      /**
       * Public method to set the received status for a band
       */
      void setBandReceived(CorrelatorBand& iBand, bool received);

      /**
       * Method to mark a cross-spectrum baseline as received
       */
      void setCrossBaselineReceived(unsigned int iBase, bool received);

      /**
       * Initialize the variables which will keep track of what
       * sources were input to the correlator
       */
      void initializeSources();

      /**
       * Return true if all bands reported they were observing the
       * noise source
       */
      bool isNoise();

      /**
       * Return true if all bands reported they were observing RF
       */
      bool isRf();

    private:      

      // Statistics about correlator sources

      bool isRf_;
      bool isNoise_;

      // Arrays in which visibilities will be stored until ready to write

      std::vector<Complex<float>::Data> usbAvg_;
      std::vector<Complex<float>::Data> lsbAvg_;
      std::vector<Complex<float>::Data> usbVar_;
      std::vector<Complex<float>::Data> lsbVar_;

      std::vector<float> usbAmplitude_;
      std::vector<float> lsbAmplitude_;

      std::vector<float> usbAvgAmplitude_;
      std::vector<float> lsbAvgAmplitude_;

      std::vector<Complex<float>::Data> usb_;
      std::vector<Complex<float>::Data> lsb_;

      std::vector<float> autoAvg_;
      std::vector<float> autoVar_;
      std::vector<float> auto_;

      std::vector<unsigned char> baselineReceived_;

      // A static array of antennaBasedComplexGains

      static std::vector<std::vector<std::vector<Complex<float> > > > 
	antennaBasedGains_;

      static bool gainsAreInitialized_;

#if DIR_USE_CORR
      /**
       * Pack correlator data into our buffer
       */
      void packCorrData(carma::correlator::lib::CorrelatorData& corrData);

      /**
       * Pack a single band.
       */
      void packBand(carma::correlator::lib::CorrelatorBand& corrBand);

      /**
       * Pack a single baseline.
       */
      void packBaseline(carma::correlator::lib::CorrelatorBaseline& baseline);
      
      /**
       * Pack a sideband.
       */
      void packSideBand(carma::correlator::lib::CorrelatorSideband* sideband,
			unsigned int iAnt1, unsigned int iAnt2);

      /**
       * Return the name associated with this band
       */
      std::string bandName(carma::correlator::lib::CorrelatorBand& corrBand);

#endif

#if DIR_USE_CORR

      /**
       * Pack a single band.
       */
      void packCorrData(cobra::CorrelatorBand& corrBand, unsigned char antMask=0xff);

      /**
       * Pack a single spectrum for this band
       */
      void packSpectrum(cobra::CorrelatorData* spectrum, bool reverseFreqOrder=false, unsigned char antMask=0xff);

      /**
       * Pack a single spectrum for this band
       */
      void packAutoSpectrum(cobra::AutoSpectra* spectrum, bool reverseFreqOrder=false);

      /**
       * Pack a cross-spectrum
       */
      void packCrossSpectrum(cobra::CrossSpectra* spectrum, bool reverseFreqOrder=false, unsigned char antMask=0xff);

      /**
       * Pack the autocorrelationd for a single antenna
       */
      void packAuto(unsigned iAnt, float* data, bool reverseFreqOrder=false);

      /**
       * Pack a single baseline of a LSB cross spectrum
       */
      void packLsb(unsigned iBase, std::complex<float>* data, bool reverseFreqOrder=false);

      /**
       * Pack a single baseline of a USB cross spectrum
       */
      void packUsb(unsigned iBase, std::complex<float>* data, bool reverseFreqOrder=false);

      /**
       * Pack a single baseline of a cross spectrum
       */
      void packCross(unsigned iBase, std::complex<float>* data, 
		     Complex<float>::Data* ptr, 
		     Complex<float>::Data* avgPtr, 
		     Complex<float>::Data* varPtr,
		     float* ampPtr,
		     float* avgAmpPtr,
		     bool reverseFreqOrder=false);

#endif

      /**
       * Return the name associated with this band
       */
      std::string bandName(unsigned iBand);

      /**
       * Write our buffered data to the data frame
       */
      void writeRegisters(std::string& bName, bool swapSidebands);

      /**
       * Compare the passed string to the sources we care about and
       * AND the result into the boolean variables
       */
      void addSource(std::string& source);

    }; // End class CorrelatorDataFrameManager
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CORRELATORDATAFRAMEMANAGER_H
