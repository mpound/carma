// $Id: AstroBandListener.h,v 1.1 2012/08/14 22:03:11 eml Exp $

#ifndef SZA_ANTENNA_CORBA_ASTROBANDLISTENER_H
#define SZA_ANTENNA_CORBA_ASTROBANDLISTENER_H

/**
 * @file AstroBandListener.h
 * 
 * Tagged: Wed Jun 15 10:54:33 PDT 2011
 * 
 * @version: $Revision: 1.1 $, $Date: 2012/08/14 22:03:11 $
 * 
 * @author username: Command not found.
 */
#include "carma/correlator/obsRecord2/CorbaCorrConsumer.h"
#include "carma/correlator/lib/CorrelatorBand.h"
#include "carma/correlator/lib/CorrelatorSideband.h"

#include "carma/antenna/sza/antenna/corba/SzaMonitorSystemReg.h"

#include "carma/szautil/Complex.h"

#include "carma/szautil/RunnableTask.h"

namespace sza {
  namespace antenna {
    namespace corba {

      class AstroBandGather;

      class AstroBandListener : public sza::util::RunnableTask, 
	public carma::correlator::obsRecord2::CorbaCorrConsumer::Listener {
      public:

	  // Constructor.

	  AstroBandListener(AstroBandGatherer* parent, 
			    std::string imr,
			    unsigned astroBandNo,
			    sza::util::NetMonitorFrame* nmf,
			    unsigned nFrameAvg,
			    double coherenceLevelThreshold=0.0,
			    double coherenceMjdThreshold=0.0);
  
	  // Destructor.
  
	  virtual ~AstroBandListener();
  
	  void initialize(AstroBandGatherer* parent, 
			  std::string imr,
			  unsigned astroBandNo,
			  sza::util::NetMonitorFrame* nmf,
			  unsigned nFrameAvg,
			  double coherenceLevelThreshold=0.0,
			  double coherenceMjdThreshold=0.0);

	  void processData(carma::correlator::lib::CorrelatorData * cd);

	  void run();

	  void setupRegisterPointers(sza::util::NetMonitorFrame* nmf);

	  unsigned nCorrChan_;
	  unsigned nAnt_;
	  unsigned nBase_;

	  void incrementBufferCounters();

	  void clearCoherenceBuffers();

	  void packCoherence(double mjd, 
			     unsigned iBase, 
			     unsigned antIndex1,
			     unsigned antIndex2,
			     std::vector<float>&                                         coherence,
			     std::vector<sza::util::Complex<float>::Data>&               avg, 
			     std::vector<sza::util::Complex<float>::Data>&               avgSum, 				
			     std::vector<std::vector<sza::util::Complex<float>::Data> >& avgSamples,
			     std::vector<double>&                                        mjdLastCoherence,
			     std::vector<unsigned char>&                                 isCoherent,
			     std::vector<float>&                                         antCoherence,
			     std::vector<unsigned>&                                      nBaseline,
			     bool printDebug=false);

      private:

	  bool coherenceMonitor_;
	  std::vector<SzaMonitorSystemReg> regs_;
	  AstroBandGatherer* parent_;
	  std::string doName_;
	  std::string imr_;
	  unsigned astroBandNo_;
	  unsigned char rcvd_;

	  // A map of antenna number <--> baseline indices for
	  // cross-correlations
  
	  std::map<unsigned int, std::map<unsigned int, unsigned int> > crossBaselineIndex_;

	  std::vector<float> usbAmplitude_;
	  std::vector<float> lsbAmplitude_;

	  std::vector<float> usbAvgAmplitude_;
	  std::vector<float> lsbAvgAmplitude_;

	  std::vector<float> lsbFrequencyGHz_;
	  std::vector<float> usbFrequencyGHz_;
	  std::vector<float> autoFrequencyGHz_;

	  std::vector<sza::util::Complex<float>::Data> usbAvg_;
	  std::vector<sza::util::Complex<float>::Data> lsbAvg_;
	  std::vector<sza::util::Complex<float>::Data> usbVar_;
	  std::vector<sza::util::Complex<float>::Data> lsbVar_;

	  std::vector<float> autoAvg_;
	  std::vector<float> autoVar_;
	  std::vector<float> auto_;

	  std::vector<sza::util::Complex<float>::Data> usb_;
	  std::vector<sza::util::Complex<float>::Data> lsb_;

	  // Used only for coherence monitor

	  std::vector<float> usbCoherence_;
	  std::vector<float> lsbCoherence_;

	  std::vector<float> usbAntCoherence_;
	  std::vector<float> lsbAntCoherence_;

	  std::vector<unsigned> usbNBaseline_;
	  std::vector<unsigned> lsbNBaseline_;

	  std::vector<sza::util::Complex<float>::Data> usbAvgSum_;
	  std::vector<sza::util::Complex<float>::Data> lsbAvgSum_;
	  std::vector<std::vector<sza::util::Complex<float>::Data > > usbAvgSamples_;
	  std::vector<std::vector<sza::util::Complex<float>::Data > > lsbAvgSamples_;
	  std::vector<double> usbMjdLastCoherence_;
	  std::vector<double> lsbMjdLastCoherence_;
	  std::vector<unsigned char> usbIsCoherent_;
	  std::vector<unsigned char> lsbIsCoherent_;
	  double coherenceLevelThreshold_;
	  double coherenceMjdThreshold_;
	  unsigned nSamp_;
	  unsigned nFrameAvg_;
	  int iNextSample_;
	  int iOldestSample_;
	  bool bufferFull_;

	  void packSideband(double mjd, carma::correlator::lib::CorrelatorSideband& sideband, unsigned antNo1, unsigned antNo2);
	  void writeData(double mjd, std::string abName);

	}; // End class AstroBandListener

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_ASTROBANDLISTENER_H
