#include "carma/antenna/sza/antenna/corba/AstroBandGatherer.h"
#include "carma/antenna/sza/antenna/corba/AstroBandListener.h"
#include "carma/antenna/sza/antenna/corba/SzaMonitorSystemMap.h"

#include "carma/correlator/lib/CorrelatorBand.h"
#include "carma/correlator/lib/CorrelatorData.h"

#include "carma/szautil/FrameFlags.h"
#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/TimeVal.h"

#include "carma/util/Orb.h"

using namespace std;

using namespace sza::antenna::corba;
using namespace sza::util;
using namespace carma::correlator::lib;

AstroBandListener::AstroBandListener(AstroBandGatherer* parent, 
				     std::string imr,
				     unsigned astroBandNo,
				     sza::util::NetMonitorFrame* nmf,
				     unsigned nFrameAvg,
				     double coherenceLevelThreshold,
				     double coherenceMjdThreshold) :
  RunnableTask(true) 
{
  initialize(parent, imr, astroBandNo, nmf, nFrameAvg, coherenceLevelThreshold, coherenceMjdThreshold);
}

void AstroBandListener::initialize(AstroBandGatherer* parent, 
				   std::string imr,
				   unsigned astroBandNo,
				   sza::util::NetMonitorFrame* nmf,
				   unsigned nFrameAvg,
				   double coherenceLevelThreshold,
				   double coherenceMjdThreshold)
{
  //------------------------------------------------------------
  // Initialize to sensible defaults
  //------------------------------------------------------------

  imr_         = imr;
  parent_      = parent;
  astroBandNo_ = astroBandNo;

  std::ostringstream objectName;
  objectName.str("");
  objectName << "carma.correlator.astroband" << astroBandNo << ".data";
  doName_ = objectName.str();

  SzaMonitorSystemMap szaMsMap;

  nCorrChan_ = szaMsMap.nChan_;

  // Set the number of antennas appropriately for differnet correlator
  // types

  if(astroBandNo_ < 9) {
    nAnt_      = szaMsMap.nSlAnt_;
  } else {
    nAnt_      = szaMsMap.nWbAnt_;
  }

  nBase_     = (nAnt_ * (nAnt_ + 1))/2;

  //------------------------------------------------------------
  // Construct the hash map of antenna index <--> cross correlation
  // indices
  //------------------------------------------------------------

  unsigned iBase=0;
  for(unsigned iAnt1=0; iAnt1 < nAnt_-1; iAnt1++) {
 
   std::map<unsigned int, unsigned int>& antMap1 = 
      crossBaselineIndex_[iAnt1];

    for(unsigned iAnt2=iAnt1+1; iAnt2 < nAnt_; iAnt2++, iBase++) {

      std::map<unsigned int, unsigned int>& antMap2 = 
	crossBaselineIndex_[iAnt2];
      
      // Don't differentiate between 1-2 and 2-1
      
      antMap1[iAnt2] = iBase;
      antMap2[iAnt1] = iBase;
    }
  }

  //------------------------------------------------------------
  // Resize temporary arrays we will use to accumulate data from the
  // bands
  //------------------------------------------------------------

  rcvd_ = (unsigned char)FrameFlags::RECEIVED;

  autoAvg_.resize(nAnt_);
  autoVar_.resize(nAnt_);
  auto_.resize(nAnt_ * nCorrChan_);

  usbAmplitude_.resize(nBase_ * nCorrChan_);
  usbAvgAmplitude_.resize(nBase_);
  usbAvg_.resize(nBase_);
  usbVar_.resize(nBase_);
  usb_.resize(nBase_ * nCorrChan_);

  lsbAmplitude_.resize(nBase_ * nCorrChan_);
  lsbAvgAmplitude_.resize(nBase_);
  lsbAvg_.resize(nBase_);
  lsbVar_.resize(nBase_);
  lsb_.resize(nBase_ * nCorrChan_);

  lsbFrequencyGHz_.resize(nCorrChan_);
  usbFrequencyGHz_.resize(nCorrChan_);
  autoFrequencyGHz_.resize(nCorrChan_);

  // Check for coherence monitoring now

  nFrameAvg_               = nFrameAvg;
  coherenceLevelThreshold_ = coherenceLevelThreshold;
  coherenceMjdThreshold_   = coherenceMjdThreshold;

  if(nFrameAvg_ == 0) {

    coherenceMonitor_ = false;

  } else {

    coherenceMonitor_ = true;
    
    lsbCoherence_.resize(nBase_);
    usbCoherence_.resize(nBase_);

    lsbAntCoherence_.resize(nAnt_);
    usbAntCoherence_.resize(nAnt_);

    lsbNBaseline_.resize(nAnt_);
    usbNBaseline_.resize(nAnt_);

    lsbAvgSum_.resize(nBase_);
    usbAvgSum_.resize(nBase_);
    lsbAvgSamples_.resize(nBase_);
    usbAvgSamples_.resize(nBase_);

    usbMjdLastCoherence_.resize(nBase_);
    lsbMjdLastCoherence_.resize(nBase_);
    lsbIsCoherent_.resize(nBase_);
    usbIsCoherent_.resize(nBase_);

    for(unsigned iBase=0; iBase < nBase_; iBase++) {
      lsbAvgSamples_[iBase].resize(nFrameAvg_);
      usbAvgSamples_[iBase].resize(nFrameAvg_);
    }
    
    iNextSample_   =  0;
    iOldestSample_ = -1;
    bufferFull_    =  false;
    nSamp_         =  0;
  }

  // And set up pointers to pertinent registers in the net monitor
  // frame

  setupRegisterPointers(nmf);
}

AstroBandListener::~AstroBandListener()
{
}

/**.......................................................................
 * Take a target frame, and keep pointers to pertinent locations in it
 * where we will pack data for this astroband
 */
void AstroBandListener::setupRegisterPointers(sza::util::NetMonitorFrame* nmf)
{
  std::ostringstream abNameStr;
  std::string abName;
  abNameStr << "Astroband" << astroBandNo_;
  abName = abNameStr.str();

  regs_.push_back(SzaMonitorSystemReg("corr", abName, "received", &rcvd_));
  regs_.push_back(SzaMonitorSystemReg("corr", abName, "autoAvg", &autoAvg_[0]));

  regs_.push_back(SzaMonitorSystemReg("corr", abName, "autoVar", &autoVar_[0]));
  regs_.push_back(SzaMonitorSystemReg("corr", abName, "auto",    &auto_[0]));

  regs_.push_back(SzaMonitorSystemReg("corr", abName, "usbAvg",  &usbAvg_[0]));
  regs_.push_back(SzaMonitorSystemReg("corr", abName, "usbVar",  &usbVar_[0]));
  regs_.push_back(SzaMonitorSystemReg("corr", abName, "usb",     &usb_[0]));
  
  regs_.push_back(SzaMonitorSystemReg("corr", abName, "lsbAvg",  &lsbAvg_[0]));
  regs_.push_back(SzaMonitorSystemReg("corr", abName, "lsbVar",  &lsbVar_[0]));
  regs_.push_back(SzaMonitorSystemReg("corr", abName, "lsb",     &lsb_[0]));
  
  regs_.push_back(SzaMonitorSystemReg("corr", abName, "autoFrequency",  &autoFrequencyGHz_[0]));
  regs_.push_back(SzaMonitorSystemReg("corr", abName, "lsbFrequency",   &lsbFrequencyGHz_[0]));
  regs_.push_back(SzaMonitorSystemReg("corr", abName, "usbFrequency",   &usbFrequencyGHz_[0]));

  if(coherenceMonitor_) {
    regs_.push_back(SzaMonitorSystemReg("corr", abName, "lsbCoherence",    &lsbCoherence_[0]));
    regs_.push_back(SzaMonitorSystemReg("corr", abName, "usbCoherence",    &usbCoherence_[0]));
    regs_.push_back(SzaMonitorSystemReg("corr", abName, "lsbAntCoherence", &lsbAntCoherence_[0]));
    regs_.push_back(SzaMonitorSystemReg("corr", abName, "usbAntCoherence", &usbAntCoherence_[0]));
    regs_.push_back(SzaMonitorSystemReg("corr", abName, "lsbIsCoherent",   &lsbIsCoherent_[0]));
    regs_.push_back(SzaMonitorSystemReg("corr", abName, "usbIsCoherent",   &usbIsCoherent_[0]));
  }
}

/**.......................................................................
 * Process data from this astroband
 */
void AstroBandListener::processData(carma::correlator::lib::CorrelatorData * cd)
{
  //  COUT("ProvessData called");
  unsigned antNo1, antNo2;
  std::ostringstream abName;
  double mjd = cd->getHeader().getMJD();

  //  COUT("MJD is " << std::setprecision(15) << mjd);

  // Determine which band this is

  std::vector<carma::correlator::lib::CorrelatorBand>& bands = cd->getBands();

  if(bands.size() != 1) {
    ThrowError("I don't handle multiple bands");
  }

  carma::correlator::lib::CorrelatorBand& band = bands[0];
  unsigned astroBandNo = band.getBandNumber();
  abName << "Astroband" << astroBandNo;

  //------------------------------------------------------------
  // Iterate over baselines of this band
  //------------------------------------------------------------

  std::vector<CorrelatorBaseline>& baselines = band.getBaselines();
  unsigned nBase = baselines.size();

  for(unsigned iBase=0; iBase < nBase; iBase++) {
    CorrelatorBaseline& baseline = baselines[iBase];

    antNo1 = baseline.getAnt1Number();
    antNo2 = baseline.getAnt2Number();

    //------------------------------------------------------------
    // Iterate over sidebands of this baseline
    //------------------------------------------------------------

    std::vector<CorrelatorSideband>& sidebands = baseline.getSidebands();
    unsigned nSide = sidebands.size();

    for(unsigned iSide=0; iSide < nSide; iSide++) {
      CorrelatorSideband& sideband = sidebands[iSide];
      packSideband(mjd, sideband, antNo1, antNo2);
    }
  }

  // Increment the buffer counters

  incrementBufferCounters();

  // Now that we're done buffering all data from this band, write it
  // to the appropriate frame in our parent's frame buffer

  //  CTOUT("About to write data");

  if(coherenceMonitor_) {
    if(astroBandNo_ == 9) {
      COUT("FINAL ant coherence is now: " << lsbAntCoherence_[0] << " nbase = " << lsbNBaseline_[0]);
    }
  }

  writeData(mjd, abName.str());

  // And clear the coherence buffers now

  clearCoherenceBuffers();
}

void AstroBandListener::clearCoherenceBuffers()
{
  if(coherenceMonitor_) {
    for(unsigned iAnt=0; iAnt < nAnt_; iAnt++) {
      lsbAntCoherence_[iAnt] = 0.0;
      usbAntCoherence_[iAnt] = 0.0;
      lsbNBaseline_[iAnt] = 0;
      usbNBaseline_[iAnt] = 0;
    }
  }
}

void AstroBandListener::writeData(double mjd, std::string abName)
{
  // Write buffered data to the appropriate locations in the parent's
  // data frame

  ArrayDataFrameManager* adfm = parent_->getFrame(mjd);

  // If no frame was found, this means we received data too late.  Just dump it.

  if(adfm != 0) {
    for(unsigned i=0; i < regs_.size(); i++) {
      regs_[i].pack(adfm);
    }
  }
}

void AstroBandListener::packSideband(double mjd, CorrelatorSideband& sideband, unsigned antNo1, unsigned antNo2)
{
  unsigned antIndex1;
  unsigned antIndex2;

  if(astroBandNo_ < 9) {
    antIndex1 = antNo1 - 1; // Convert from C1-C23 to 0-22
    antIndex2 = antNo2 - 1; // Convert from C1-C23 to 0-22
  } else {
    antIndex1 = antNo1 - 1 - 15; // Convert from C16-C23 to 0-7
    antIndex2 = antNo2 - 1 - 15; // Convert from C16-C23 to 0-7
  }

  Complex<float> avgVal = sideband.getStats().getAvg();
  Complex<float> varVal = sideband.getStats().getStandardDeviation();

  std::vector<std::complex<float> >& data = sideband.getData();

  // The number of uncorrupted lags is NCHAN_TOTAL, but the actual
  // array can be larger if we're receiving simulated data, so we
  // won't throw an error.  However, it should be at least NCHAN_TOTAL
  // channels long.

  if(data.size() < nCorrChan_) {
    ThrowError("Invalid number of channels: " << data.size());
  }

  // Get frequencies too.  Frequencies in the CorrelatorSideband
  // object appear to be bogus, so I'm now just constructing IF
  // frequency from the astroBandNo

  float skyFreqFirstChannelGHz;

  if(astroBandNo_ < 9) {
    skyFreqFirstChannelGHz = 1.0 + (astroBandNo_ - 1) * 0.5;
  } else {
    skyFreqFirstChannelGHz = 1.0 + (astroBandNo_ - 9) * 0.5;
  }

  float skyFreqDeltaMHz        = sideband.getDeltaFrequency();

  // Pack the averages according to the type of sideband

  //-----------------------------------------------------------------------
  // AUTO
  //-----------------------------------------------------------------------

  if(sideband.isAuto()) {

    autoAvg_[antIndex1] = avgVal.real();
    autoVar_[antIndex1] = varVal.real();

    // Note these loops iterate only over nCorrChan_

    float* ptr     = &auto_[0];
    float* freqPtr = &autoFrequencyGHz_[0];
    unsigned index;

    for(unsigned iChan=0; iChan < nCorrChan_; iChan++) {
      index = antIndex1 * nCorrChan_ + iChan;
      *(ptr + index) = data[iChan+1].real();
      *(freqPtr + iChan) = skyFreqFirstChannelGHz + iChan * skyFreqDeltaMHz/1000;
    }

    //    COUT("SKY FREQ FIRST = " << skyFreqFirstChannelGHz << " DELTA = " << skyFreqDeltaMHz);

    //-----------------------------------------------------------------------
    // USB
    //-----------------------------------------------------------------------

  } else if(sideband.isUSB()) {

    unsigned iBase = crossBaselineIndex_[antIndex1][antIndex2];

    usbAvg_[iBase].real_ = avgVal.real();
    usbAvg_[iBase].imag_ = avgVal.imag();
    usbVar_[iBase].real_ = varVal.real();
    usbVar_[iBase].imag_ = varVal.imag();

    if(coherenceMonitor_) 
      packCoherence(mjd, iBase, antIndex1, antIndex2, usbCoherence_, usbAvg_, usbAvgSum_, usbAvgSamples_, 
		    usbMjdLastCoherence_, usbIsCoherent_,
		    usbAntCoherence_, usbNBaseline_);

    Complex<float>::Data* ptr = &usb_[0];
    float* freqPtr = &usbFrequencyGHz_[0];

    for(unsigned iChan=0; iChan < nCorrChan_; iChan++) {
      (ptr + iBase * nCorrChan_ + iChan)->real_ = data[iChan+1].real();
      (ptr + iBase * nCorrChan_ + iChan)->imag_ = data[iChan+1].imag();
      *(freqPtr + iChan) = skyFreqFirstChannelGHz + iChan * skyFreqDeltaMHz/1000;
    }
    
    //-----------------------------------------------------------------------
    // LSB
    //-----------------------------------------------------------------------

  } else {
    
#if 0
    if(antIndex1 == 0 && antIndex2 == 1) {
      COUT(astroBandNo_ << ": sky frequency is reported as: " << sideband.getSkyFrequency());
    }
#endif

    unsigned iBase = crossBaselineIndex_[antIndex1][antIndex2];

    lsbAvg_[iBase].real_ = avgVal.real();
    lsbAvg_[iBase].imag_ = avgVal.imag();
    lsbVar_[iBase].real_ = varVal.real();
    lsbVar_[iBase].imag_ = varVal.imag();

    if(coherenceMonitor_) {
      packCoherence(mjd, iBase, antIndex1, antIndex2, lsbCoherence_, lsbAvg_, lsbAvgSum_, lsbAvgSamples_, 
		    lsbMjdLastCoherence_, lsbIsCoherent_, 
		    lsbAntCoherence_, lsbNBaseline_);
      
      if(astroBandNo_ == 9 && antIndex1==0) {
	COUT("ant coherence is now: " << lsbAntCoherence_[0] << " nbase = " << lsbNBaseline_[0]);
      }
    }

    Complex<float>::Data* ptr = &lsb_[0];
    float* freqPtr = &lsbFrequencyGHz_[0];

    for(unsigned iChan=0; iChan < nCorrChan_; iChan++) {
      (ptr + iBase * nCorrChan_+iChan)->real_ = data[iChan+1].real();
      (ptr + iBase * nCorrChan_+iChan)->imag_ = data[iChan+1].imag();
      *(freqPtr + iChan) = skyFreqFirstChannelGHz + iChan * skyFreqDeltaMHz/1000;
    }
  }
}

/**.......................................................................
 * Run method of the listener class.  Just blocks waiting for data
 * from this astroband
 */
void AstroBandListener::run()
{
  // Just pass in the channel name for the orb name, and set the IMR
  // as well

  carma::util::Orb orb;

  orb.allowRegistrationWithImr(false);
  orb.setName(doName_);
  orb.setImrName(imr_);

  // Loop forever, attempting to get a reference to the notification
  // channel

  while(true) {

    try {

      CTOUT("Attempting to resolve doName: " << doName_);

      carma::correlator::obsRecord2::CorbaCorrConsumer consumer(&orb, doName_, (*this));
      
      // Blocking call on getData()
      
      consumer.getData();
      
    } catch(...) {
      
      CTOUT("Caught an error on " << doName_ << " waiting to reconnect");
      
      sza::util::TimeOut timeout;
      timeout.setIntervalInSeconds(2);
      timeout.activate(true);

      select(0, NULL, NULL, NULL, timeout.tVal());
    }

  }
}

/**.......................................................................
 * Pack the coherence
 */
void AstroBandListener::packCoherence(double mjd,
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
				      bool printDebug)
{
  // Precompute the absolute value by which we will scale the Re and
  // Im to force the value onto the unit circle

  float re = avg[iBase].real_;
  float im = avg[iBase].imag_;
  float amp = (float)sqrt(re*re + im*im);

  float rescaled = re/amp;
  float imscaled = im/amp;

  // If the buffer is full, remove the oldest sample first.

  if(printDebug) {
    COUT("nSamp_ = " << nSamp_ << " iNextSample_ = " << iNextSample_ << " iOldestSample_ =  " << iOldestSample_ << " bufferFull_ = " << bufferFull_ << " rescaled = " << rescaled << " imscaled = " << imscaled);
  }

  if(bufferFull_) {
    avgSum[iBase].real_ -= avgSamples[iBase][iOldestSample_].real_;
    avgSum[iBase].imag_ -= avgSamples[iBase][iOldestSample_].imag_;

    if(printDebug) {
      COUT("Buffer is full: removing sample re = " << avgSamples[iBase][iOldestSample_].real_ 
	   << " im = " << avgSamples[iBase][iOldestSample_].imag_);
    }
  }

  // Now add the new sample to the sum, and store it in the samples
  // array, so it can be removed later.

  avgSum[iBase].real_ += rescaled;
  avgSum[iBase].imag_ += imscaled;

  avgSamples[iBase][iNextSample_].real_ = rescaled;
  avgSamples[iBase][iNextSample_].imag_ = imscaled;

  // Now compute the absolute value of the vector average

  float remean = avgSum[iBase].real_/(nSamp_+1);
  float immean = avgSum[iBase].imag_/(nSamp_+1);

  // Calculate the instantaneous coherence

  float coh = (float)sqrt(remean*remean + immean*immean);
  coherence[iBase] = coh;

  // Now figure out if this baseline is coherent

  if(coh > coherenceLevelThreshold_ || nSamp_ == 0) {
    mjdLastCoherence[iBase] = mjd;
  }

  if((mjd - mjdLastCoherence[iBase]) < coherenceMjdThreshold_) {
    isCoherent[iBase] = 1;
  } else {
    isCoherent[iBase] = 0;
  }

  // Increment the running average of antenna-based coherence for both
  // antennas involved in this baseline

  antCoherence[antIndex1] += (coh - antCoherence[antIndex1])/(nBaseline[antIndex1]+1);
  antCoherence[antIndex2] += (coh - antCoherence[antIndex2])/(nBaseline[antIndex2]+1);

  ++nBaseline[antIndex1];
  ++nBaseline[antIndex2];
}

/**.......................................................................
 * Increment the sample counters
 */
void AstroBandListener::incrementBufferCounters()
{
  if(iNextSample_ == nFrameAvg_-1) {
    iNextSample_ = 0;
    bufferFull_  = true;
    nSamp_       = nFrameAvg_-1; // Use -1 so that we can use nSamp_
			      // transparently in calculation above
  } else {
    ++iNextSample_;
  }

  if(bufferFull_) {
    if(iOldestSample_ == nFrameAvg_-1) {
      iOldestSample_ = 0;
    } else {
      ++iOldestSample_;
    }
  }

  if(!bufferFull_)
    ++nSamp_;
}
