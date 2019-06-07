// $Id: VisBrickReader.h,v 1.6 2011/08/18 23:25:48 abeard Exp $

#ifndef SZA_ANTENNA_CORBA_VISBRICKREADER_H
#define SZA_ANTENNA_CORBA_VISBRICKREADER_H

/**
 * @file VisBrickReader.h
 * 
 * Tagged: Tue Sep 15 13:14:59 PDT 2009
 * 
 * @version: $Revision: 1.6 $, $Date: 2011/08/18 23:25:48 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Complex.h"
#include "carma/szautil/RegDate.h"

#include <map>
#include <vector>

#include "carma/antenna/sza/antenna/corba/Corba.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/pipeline/VisBrickReader.h"

namespace sza {
  namespace util {
    class ArrayDataFrameManager;
  }
};

namespace carma {
  namespace pipeline {
    class CorrelatorVisBrickReader;
  }
};

namespace sza {
  namespace antenna {
    namespace corba {

      class VisBrickReader {
      public:

	/**
	 * Constructor.
	 */
	VisBrickReader(std::string confFile);

	/**
	 * Destructor.
	 */
	virtual ~VisBrickReader();

	bool atEnd();
	void loadFile(std::string fileName);
	void readFile(std::string fileName);
	void packNextRecord(sza::util::ArrayDataFrameManager* fm);
	void packBand(const carma::correlator::lib::CorrelatorBand& corrBand, sza::util::ArrayDataFrameManager* fm);
	void packBaseline(const carma::correlator::lib::CorrelatorBaseline& baseline);
	void packSideBand(const carma::correlator::lib::CorrelatorSideband & sideBand,
			  int iAnt1, int iAnt2);

	void setCrossBaselineReceived(unsigned iBase, bool received);
	void setBaselinesUnreceived();

	std::string bandName(carma::correlator::lib::CorrelatorBand& corrBand);
	std::string bandName(int iBand);

	void initialize();

	void closeFile();

	unsigned int getCurrentFrameCount();

	sza::util::RegDate mjd_;

      private:

	carma::pipeline::CorrelatorVisBrickReader* reader_;

	unsigned nAnt_;
	unsigned nBase_;
	unsigned nBand_;
	unsigned nChan_;

	std::string fileName_;
	std::string confFile_;

	// Statistics about correlator sources

	bool isRf_;
	bool isNoise_;

      /**
       * A map of antenna number <--> baseline indices for
       * cross-correlations
       */
	std::map<unsigned int, std::map<unsigned int, unsigned int> > 
	  crossBaselineIndex_;

	// Arrays in which visibilities will be stored until ready to write

	std::vector<sza::util::Complex<float>::Data> usbAvg_;
	std::vector<sza::util::Complex<float>::Data> lsbAvg_;
	std::vector<sza::util::Complex<float>::Data> usbVar_;
	std::vector<sza::util::Complex<float>::Data> lsbVar_;

	std::vector<float> usbAmplitude_;
	std::vector<float> lsbAmplitude_;

	std::vector<float> usbAvgAmplitude_;
	std::vector<float> lsbAvgAmplitude_;

	std::vector<sza::util::Complex<float>::Data> usb_;
	std::vector<sza::util::Complex<float>::Data> lsb_;

	std::vector<float> autoAvg_;
	std::vector<float> autoVar_;
	std::vector<float> auto_;

	std::vector<int> autoValid_;
	std::vector<int> lsbValid_;
	std::vector<int> usbValid_;

	std::vector<unsigned char> baselineReceived_;

	unsigned lsbNSample_;
	unsigned usbNSample_;
	unsigned autoNSample_;

	float lsbTint_;
	float usbTint_;
	float autoTint_;

	carma::pipeline::RecordsByFrameMap recs_;
	carma::pipeline::RecordsByFrameMap::iterator recIter_;

      }; // End class VisBrickReader

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_VISBRICKREADER_H
