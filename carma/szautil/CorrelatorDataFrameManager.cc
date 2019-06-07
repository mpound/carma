#include "carma/szautil/AntNum.h"
#include "carma/szautil/AxisRange.h"
#include "carma/szautil/CorrelatorBand.h"
#include "carma/szautil/CorrelatorDataFrameManager.h"
#include "carma/szautil/DataFrameNormal.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/FrameFlags.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/TimeVal.h"

#include "carma/szaarrayutils/scanner.h"

#include <iomanip>
#include <cstring>

using namespace sza::util;
using namespace std;

#if DIR_USE_CORR
#include "cobra/Correlator.h"
#endif

#if DIR_USE_CORR
using namespace carma::correlator::lib;
using namespace cobra;
#endif

std::vector<std::vector<std::vector<Complex<float> > > > 
CorrelatorDataFrameManager::antennaBasedGains_;

bool CorrelatorDataFrameManager::gainsAreInitialized_ = false;

#define SWAP_LOWER_EIGHT_BANDS 0

/**.......................................................................
 * Constructor with no resizing of the initially zero-length DataFrame
 * buffer.  This constructor doesn't intialize the antenna number
 * associated with this manager object.
 */
void CorrelatorDataFrameManager::initialize(bool archivedOnly, 
					    DataFrame* frame) 
{
  regMap_ = 0;

  if((regMap_ = new_SzaCorrelatorRegMap())==0)
    throw Error("CorrelatorDataFrameManager::CorrelatorDataFrameManager: "
		" Unable to allocate regMap_.\n");
  
  archivedOnly_ = archivedOnly;

  // Make the frame large enough to accomodate the register map for all bands
  
  unsigned frameSize = SCAN_BUFF_BYTE_SIZE(regMap_->nByte(archivedOnly));

  // Allocate a frame for this manager

  frame_ = new sza::util::DataFrameNormal(frameSize);
  
  // Initialize the nBuffer variable

  nBuffer_ = frameSize;

  // Initialize from the passed frame, if any was passed.

  if(frame != 0)
    *frame_ = *frame;

  // Construct the hash map of antenna index <--> cross correlation
  // indices

  unsigned iBase=0;

  for(unsigned iAnt1=0; iAnt1 < AntNum::NANT-1; iAnt1++) {
 
   std::map<unsigned int, unsigned int>& antMap1 = 
      crossBaselineIndex_[iAnt1];

    for(unsigned iAnt2=iAnt1+1; iAnt2 < AntNum::NANT; iAnt2++, iBase++) {

      std::map<unsigned int, unsigned int>& antMap2 = 
	crossBaselineIndex_[iAnt2];
      
      // Don't differentiate between 1-2 and 2-1
      
      antMap1[iAnt2] = iBase;
      antMap2[iAnt1] = iBase;
    }
  }

  // Resize arrays

  usbAmplitude_.resize(AntNum::NBASE * CorrelatorBand::NCHAN_TOTAL);
  usbAvgAmplitude_.resize(AntNum::NBASE);
  usbAvg_.resize(AntNum::NBASE);
  usbVar_.resize(AntNum::NBASE);
  usb_.resize(AntNum::NBASE * CorrelatorBand::NCHAN_TOTAL);

  lsbAmplitude_.resize(AntNum::NBASE * CorrelatorBand::NCHAN_TOTAL);
  lsbAvgAmplitude_.resize(AntNum::NBASE);
  lsbAvg_.resize(AntNum::NBASE);
  lsbVar_.resize(AntNum::NBASE);
  lsb_.resize(AntNum::NBASE * CorrelatorBand::NCHAN_TOTAL);

  autoAvg_.resize(AntNum::NANT);
  autoVar_.resize(AntNum::NANT);
  auto_.resize(AntNum::NANT * CorrelatorBand::NCHAN_TOTAL);

  baselineReceived_.resize(AntNum::NBASE);

  // Finally, pre-install the frequencies, which are not currently
  // sent in the data stream

  for(CorrelatorBand iBand(CorrelatorBand::BAND0); 
      iBand <= CorrelatorBand(CorrelatorBand::BANDMAX); iBand++) {

    unsigned bandIndex = iBand.getIntId();

    float ghzPerBand    = CorrelatorBand::bandWidth().GHz();
    float ghzPerChannel = CorrelatorBand::bandWidthPerChannel().GHz();
    float centerFreq    = 1.0 + (bandIndex + 0.5) * ghzPerBand;
    float startFreq     = 1.0 + bandIndex * ghzPerBand;
    
    writeReg(iBand.bandName(), "centerFrequency", &centerFreq);

    // Each band is divided into NCHAN_TOTAL-1 channels.  Since we
    // throw away the end channels, our first (0) channel starts at
    // ghzPerChannel, hence the 1.  Furthermore, the center of each
    // channel is offset by 0.5, hence the 1.5.

    float chans[CorrelatorBand::NCHAN_TOTAL];
    for(int iChan=0; iChan < CorrelatorBand::NCHAN_TOTAL; iChan++) {
      chans[iChan] = startFreq + (iChan + 1.5) * ghzPerChannel;
    }
    
    writeReg(iBand.bandName(), "frequency", chans);

    CoordRange range((unsigned)0);
    Complex<float> fVal((float)bandIndex, 0.0);
    writeReg(iBand.bandName(), "usb", fVal.data(), &range);
    fVal.setReal((float)(bandIndex)+0.5);
    writeReg(iBand.bandName(), "usbAvg", fVal.data(), &range);
  }

  // Initialize the gains if they haven't already been

  if(!gainsAreInitialized_) {
    antennaBasedGains_.resize(AntNum::NANT);
    for(unsigned iAnt=0; iAnt < AntNum::NANT; iAnt++) {
      antennaBasedGains_[iAnt].resize(CorrelatorBand::NBAND);
      for(unsigned iBand=0; iBand < CorrelatorBand::NBAND; iBand++) {
	antennaBasedGains_[iAnt][iBand].resize(CorrelatorBand::NCHAN_TOTAL);
	for(unsigned iChan=0; iChan < CorrelatorBand::NCHAN_TOTAL; iChan++) {
	  antennaBasedGains_[iAnt][iBand][iChan].setReal(1.0);
	  antennaBasedGains_[iAnt][iBand][iChan].setImag(0.0);
	}
      }
    }
  }
}

/**.......................................................................
 * Copy constructor
 */
CorrelatorDataFrameManager::
CorrelatorDataFrameManager(CorrelatorDataFrameManager& fm)
{
  initialize(fm.archivedOnly_, fm.frame_);
}

/**.......................................................................
 * Constructor with initialization from a DataFrame object.
 */
CorrelatorDataFrameManager::
CorrelatorDataFrameManager(bool archivedOnly, DataFrame* frame)
{
  initialize(archivedOnly, frame);
}

#if DIR_USE_CORR
/**.......................................................................
 * Constructor from a CorrelatorData object
 */
CorrelatorDataFrameManager::
CorrelatorDataFrameManager(bool archivedOnly, 
			   carma::correlator::lib::CorrelatorData& corrData) 
{
  initialize(archivedOnly);

  setTo(corrData);
}

/**.......................................................................
 * Constructor from a CorrelatorData object
 */
void CorrelatorDataFrameManager::
setTo(carma::correlator::lib::CorrelatorData& corrData) 
{
  // Set the MJD from the correlator packet

  setMjd(corrData.getHeader()->getMJD());

  // And pack the data into our buffer

  packCorrData(corrData);
}

/**.......................................................................
 * Constructor from a CorrelatorData object
 */
void CorrelatorDataFrameManager::
setTo(cobra::CorrelatorBand& corrData, unsigned char antMask) 
{
  // Set the MJD from the correlator packet

  setMjd(corrData.timestamp().mjdDays());

  // And pack the data into our buffer

  packCorrData(corrData, antMask);
}
#endif      

/**.......................................................................
 * Destructor.
 */
CorrelatorDataFrameManager::~CorrelatorDataFrameManager() 
{
  if(regMap_ != 0) {
    regMap_ = del_SzaCorrelatorRegMap(regMap_);
    regMap_ = 0;
  }
}

/**.......................................................................
 * Reinitialize this frame before re-use.
 */
void CorrelatorDataFrameManager::reinitialize()
{
  sza::util::DataFrameManager::reinitialize();

  // If devices are missing, or late to report, some registers may
  // never be filled, so fill the entire frame with zeros to
  // initialize.

  fillBuffer(0);
}

#if DIR_USE_CORR
/**.......................................................................
 * Pack correlator data into our buffer
 */
void CorrelatorDataFrameManager::
packCorrData(carma::correlator::lib::CorrelatorData& corrData)
{
  std::vector<carma::correlator::lib::CorrelatorBand>* bands = 
    corrData.getBands();
  
  for(unsigned iband = 0; iband < corrData.getNumberOfBands(); iband++)
    packBand((*bands)[iband]); 
}

/**.......................................................................
 * Pack a single band.
 */
void CorrelatorDataFrameManager::
packBand(carma::correlator::lib::CorrelatorBand& corrBand)
{
  std::string bName = bandName(corrBand.getBandNumber());

  std::vector<carma::correlator::lib::CorrelatorBaseline>* baselines = 
    corrBand.getBaselines();
  
  // Mark this band as received

  writeReg(bName, "received", (unsigned char)FrameFlags::RECEIVED);

  // And pack all visibilities for it

  for(unsigned iBase=0; iBase < baselines->size(); iBase++)
    packBaseline((*baselines)[iBase]);

  // Now write all baselines for this band

  writeReg(bName, "autoAvg", &autoAvg_[0]);
  writeReg(bName, "autoVar", &autoVar_[0]);
  writeReg(bName, "auto",    &auto_[0]);

  writeReg(bName, "usbAvg",  &usbAvg_[0]);
  writeReg(bName, "usbVar",  &usbVar_[0]);
  writeReg(bName, "usb",     &usb_[0]);

  writeReg(bName, "lsbAvg",  &lsbAvg_[0]);
  writeReg(bName, "lsbVar",  &lsbVar_[0]);
  writeReg(bName, "lsb",     &lsb_[0]);
}

/**.......................................................................
 * Pack a single band.
 */
void CorrelatorDataFrameManager::
packCorrData(cobra::CorrelatorBand& corrBand, unsigned char antMask)
{
  CorrelatorParameters params = corrBand.parameters();
  signed int iBand = params.bandNumber();
  std::string sourceName = params.sourceName();
  bool received  = corrBand.status()==0;
  static Frequency freq;
  static Frequency bw;
  float bwInGHzPerChannel;
  float zeroethFreqInGHz;
  float centerFreq;
  float channelFreqs[sza::util::CorrelatorBand::NCHAN_TOTAL];

  // Check that this is a valid band for the SZA

  bool reverseFreqOrder = false;
  bool swapSidebands    = false;

  if(iBand >= 0 && iBand < sza::util::CorrelatorBand::NBAND) {

#if SWAP_LOWER_EIGHT_BANDS

    // If running with the new sum of frequency configuration, the
    // frequency order of the channels is always reversed for these
    // bands

    if(iBand < sza::util::CorrelatorBand::NBAND/2)
      reverseFreqOrder = true;

    // But only the RF source ends up in the upper sideband.  The
    // noise source always shows up in the lsb

    if(sourceName == "rf" && (iBand < sza::util::CorrelatorBand::NBAND/2))
      swapSidebands = true;

#endif

    // Get the name of the board corresponding to this band index

    std::string bName = bandName(iBand);

    // Mark whether or not this band was received
    
    writeReg(bName, "received", 
    	     received ? (unsigned char)FrameFlags::RECEIVED : 
    	     (unsigned char)FrameFlags::NOT_RECEIVED);

    // If it was received, go ahead to pack the rest of the data
    
    if(received) {
      std::vector <cobra::CorrelatorDataPtr>* data = corrBand.data();

      // Mark whether or not this band was received
      
      writeReg(bName, "source", (unsigned char*)sourceName.c_str());

      // Check the source

      addSource(sourceName);

      // Pack each spectrum returned

      for(unsigned ispec = 0; ispec < data->size(); ispec++)
	packSpectrum(data->at(ispec).get(), reverseFreqOrder, antMask);
      
      // Store the frequency of the zeroeth channel, and the bandwidth
      // of the whole band

      // EML commenting this out (12 Jan 2009) because the change to
      // the sum of the frequencies screws up the logic of the dcon
      // freq
      //
      // freq.setHz(params.frequency());
      //
      freq.setGHz(iBand * 0.5 + 1.0);

      bw.setHz(params.bandwidth());

      // Get the zeroeth frequency, in GHz

      zeroethFreqInGHz  = freq.GHz();

      bwInGHzPerChannel = bw.GHz()/sza::util::CorrelatorBand::NCHAN_TOTAL;

      // Now construct the array of frequencies for this band, and the
      // center frequency

      for(unsigned ichan=0; ichan < sza::util::CorrelatorBand::NCHAN_TOTAL; ichan++) {
	channelFreqs[ichan] = zeroethFreqInGHz + ichan * bwInGHzPerChannel;
      }
      
      centerFreq = zeroethFreqInGHz + 
	bwInGHzPerChannel * ((float)sza::util::CorrelatorBand::NCHAN_TOTAL/2 - 0.5);

      // Write the center frequency

      writeReg(bName, "centerFrequency", centerFreq);

      // Write the channelsfrequency

      writeReg(bName, "frequency", channelFreqs);

      // Now write all baselines for this band
      
      writeRegisters(bName, swapSidebands);
    }
  }
}

/**.......................................................................
 * Pack a single spectrum for this band
 */
void CorrelatorDataFrameManager::
packSpectrum(cobra::CorrelatorData* spectrum, bool reverseFreqOrder, unsigned char antMask)
{
  if(spectrum->type().isAuto())
    packAutoSpectrum(static_cast<cobra::AutoSpectra *>(spectrum), reverseFreqOrder);
  else if(spectrum->type().isCross())
    packCrossSpectrum(static_cast<cobra::CrossSpectra *>(spectrum), reverseFreqOrder, antMask);
}

/**.......................................................................
 * Pack a single spectrum for this band
 */
void CorrelatorDataFrameManager::
packAutoSpectrum(cobra::AutoSpectra* spectrum, bool reverseFreqOrder )
{
  // For which antenna is this spectrum?

  unsigned iAnt = spectrum->inputInfo().input(0);
  bool received  = spectrum->status()==0;

  //  std::cout << "Got autospec data for ant: " << iAnt << std::endl;

  // Not one of ours?

  if(iAnt < sza::util::AntNum::NANT) {
    vector<float>* data = spectrum->data();

    if(data->size() != sza::util::CorrelatorBand::NCHAN_TOTAL)
      ThrowError("Received an auto spectrum of length: " << data->size());

    packAuto(iAnt, &(*data)[0], reverseFreqOrder);
  }
}

/**.......................................................................
 * Pack a cross-spectrum
 */
void CorrelatorDataFrameManager::
packCrossSpectrum(cobra::CrossSpectra* spectrum, bool reverseFreqOrder, unsigned char antMask)
{
  // For which baseline is this spectrum?

  CorrelatorInputInfo info = spectrum->inputInfo();
  unsigned iAnt1 = info.input(0);
  unsigned iAnt2 = info.input(1);
  bool received  = spectrum->status()==0;

  // See if the bits for these two antennas are both set in the antMask

  unsigned char baseMask = 0x0;
  baseMask |= (1<<iAnt1) | (1<<iAnt2);
  bool delaysSent = (antMask & baseMask)==baseMask;

  // Is this one of ours?
  
  if(iAnt1 < sza::util::AntNum::NANT && iAnt2 < sza::util::AntNum::NANT) {
    
    // Get the baseline index corresponding to this antenna pair

    unsigned iBase = crossBaselineIndex_[iAnt1][iAnt2];
    vector< vector< complex <float> > >* data = spectrum->data();
    
    if(data->at(0).size() != sza::util::CorrelatorBand::NCHAN_TOTAL)
      ThrowError("Received a cross spectrum of length: " << data->at(0).size());

    // Pack both LSB and USB data
    
    packLsb(iBase, &(data->at(0))[0], reverseFreqOrder);
    packUsb(iBase, &(data->at(1))[0], reverseFreqOrder);

    // Mark this baseline as received
    //
    // As of the June 2009 correlator software upgrade, DWH has
    // changed the logic of these baseline flags.  While it used to be
    // that received == 0 only due to bad correlator hardware for that
    // baseline, now the 'received' flag will be zero if no delays
    // were sent for either antenna of that baseline, as would be the
    // case for example if an antenna were taken out of the array.
    // This is not however an error condition!
    //
    // To preserve the logic of this register, I now count as
    // 'received', or more properly 'no error', if delays were sent
    // AND data were received for a baseline, or if no delays were
    // sent AND data were not received.  Anything else constitutes an
    // error and we will set the 'received' flag to 0 to indicate it.

    if((delaysSent && received) || (!delaysSent && !received)) {
      received = true;
    } else {
      received = false;
    }

    setCrossBaselineReceived(iBase, received);
  }
}

/**.......................................................................
 * Pack the autocorrelation for a single antenna
 */
void CorrelatorDataFrameManager::
packAuto(unsigned iAnt, float* data, bool reverseFreqOrder)
{
  float avg=0.0, var=0.0, val;
  float* ptr    = &auto_[0];
  float* avgPtr = &autoAvg_[0];
  float* varPtr = &autoVar_[0];
  unsigned index;
  unsigned nChan = 0;
  unsigned nAvg = CorrelatorBand::NCHAN_TOTAL-2;
  float varScaleFac = (float)(nAvg)/(nAvg - 1);

  // Iterate over all frequency channels

  for(unsigned iChan=0; iChan < CorrelatorBand::NCHAN_TOTAL; iChan++) {

    unsigned iInputChan = iChan;

    if(reverseFreqOrder) {
      iInputChan = CorrelatorBand::NCHAN_TOTAL - 1 - iChan;
    }

    index = iAnt * sza::util::CorrelatorBand::NCHAN_TOTAL + iChan;
    *(ptr + index) = data[iInputChan];

    // Don't include the end channels in the range

    if(iChan > 0 && iChan < CorrelatorBand::NCHAN_TOTAL-1) {
      avg += (data[iInputChan] - avg)/(nChan + 1);
      ++nChan;
    }
  }

  // Store the average in the appropriate location
  
  *(avgPtr + iAnt) = avg;

  // Now calculate the estimate for the variance
  
  nChan = 0;
  for(unsigned iChan=0; iChan < CorrelatorBand::NCHAN_TOTAL; iChan++) {

    unsigned iInputChan = iChan;

    if(reverseFreqOrder) {
      iInputChan = CorrelatorBand::NCHAN_TOTAL - 1 - iChan;
    }

    val = data[iInputChan] - avg;

    // Accumulate x^2
   
    val *= val;

    // Running average of the V * (N-1)/N, excluding the end two channels
    
    if(iChan > 0 && iChan < CorrelatorBand::NCHAN_TOTAL-1) {
      var += (val - var)/(nChan+1);
      ++nChan;
    }
  }
  
  // Now we have V * (N-1)/N; multiply N/(N-1) to get the variance:
  
  *(varPtr + iAnt) = var * varScaleFac;
}

/**.......................................................................
 * Pack a single baseline of a LSB cross spectrum
 */
void CorrelatorDataFrameManager::
packLsb(unsigned iBase, complex<float>* data, bool reverseFreqOrder)
{
  packCross(iBase, data, &lsb_[0], &lsbAvg_[0], &lsbVar_[0], &lsbAmplitude_[0], &lsbAvgAmplitude_[0], reverseFreqOrder);
}

/**.......................................................................
 * Pack a single baseline of a USB cross spectrum
 */
void CorrelatorDataFrameManager::
packUsb(unsigned iBase, complex<float>* data, bool reverseFreqOrder)
{
  packCross(iBase, data, &usb_[0], &usbAvg_[0], &usbVar_[0], &usbAmplitude_[0], &usbAvgAmplitude_[0], reverseFreqOrder);
}

/**.......................................................................
 * Generic method for packing a cross spectrum
 */
void CorrelatorDataFrameManager::
packCross(unsigned iBase, complex<float>* data, 
  Complex<float>::Data* ptr, Complex<float>::Data* avgPtr, Complex<float>::Data* varPtr,
  float* ampPtr, float* avgAmpPtr, bool reverseFreqOrder)
{
  float re=0.0,im=0.0;
  float amp=0.0;
  float reAvg=0.0, imAvg=0.0;
  float reVar=0.0, imVar=0.0;
  float ampAvg=0.0;
  unsigned nChan=0;
  unsigned nAvg = CorrelatorBand::NCHAN_TOTAL-2;
  float varScaleFac = (float)(nAvg)/(nAvg - 1);

  // Iterate over all channels of this band

  for(unsigned iChan=0; iChan < CorrelatorBand::NCHAN_TOTAL; iChan++) {

    unsigned iInputChan = iChan;

    if(reverseFreqOrder) {
      iInputChan = CorrelatorBand::NCHAN_TOTAL - 1 - iChan;
    }

    re  = data[iInputChan].real();
    im  = data[iInputChan].imag();
    amp = abs(data[iInputChan]);

    // Store the channel data
    
    (ptr + iBase * sza::util::CorrelatorBand::NCHAN_TOTAL + iChan)->real_ = re;
    (ptr + iBase * sza::util::CorrelatorBand::NCHAN_TOTAL + iChan)->imag_ = im;

    // And the scalar average of the amplitude

    *(ampPtr + iBase * sza::util::CorrelatorBand::NCHAN_TOTAL + iChan) = amp;
    
    // And calculate averages, excluding the band edges
    
    if(iChan > 0 && iChan < CorrelatorBand::NCHAN_TOTAL-1) {
      reAvg += (re - reAvg)/(nChan+1);
      imAvg += (im - imAvg)/(nChan+1);
      ++nChan;
    }
  }

  // Store the vector-average of the Re and Im over all channels of
  // this band
  
  (avgPtr + iBase)->real_ = reAvg;
  (avgPtr + iBase)->imag_ = imAvg;
  
  // And store the average amplitude
  
  *(avgAmpPtr + iBase) = sqrt(reAvg*reAvg + imAvg*imAvg);

  // Now calculate the estimate for the variance
  
  nChan = 0;
  for(unsigned iChan=0; iChan < CorrelatorBand::NCHAN_TOTAL; iChan++) {

    unsigned iInputChan = iChan;

    if(reverseFreqOrder) {
      iInputChan = CorrelatorBand::NCHAN_TOTAL - 1 - iChan;
    }

    re = data[iInputChan].real() - reAvg;
    im = data[iInputChan].imag() - imAvg;

    // Accumulate x^2
   
    re *= re;
    im *= im;

    // Running averages of the V * (N-1)/N
    
    if(iChan > 0 && iChan < CorrelatorBand::NCHAN_TOTAL-1) {
      reVar += (re - reVar)/(nChan+1);
      imVar += (im - imVar)/(nChan+1);
      ++nChan;
    }
  }
  
  // Now we have V * (N-1)/N; multiply N/(N-1) to get the variance:
  
  (varPtr + iBase)->real_ = reVar * varScaleFac;
  (varPtr + iBase)->imag_ = imVar * varScaleFac;
}

/**.......................................................................
 * Write our buffered data to the data frame
 */
void CorrelatorDataFrameManager::
writeRegisters(std::string& bName, bool swapSidebands)
{
  writeReg(bName, "autoAvg",          &autoAvg_[0]);
  writeReg(bName, "autoVar",          &autoVar_[0]);
  writeReg(bName, "auto",             &auto_[0]);

  writeReg(bName, "usbAvg",           (swapSidebands ? &lsbAvg_[0]          : &usbAvg_[0]));		  
  writeReg(bName, "usbVar",           (swapSidebands ? &lsbVar_[0]          : &usbVar_[0]));		  
  writeReg(bName, "usbAmplitude",     (swapSidebands ? &lsbAmplitude_[0]    : &usbAmplitude_[0]));	  
  writeReg(bName, "usbAvgAmplitude",  (swapSidebands ? &lsbAvgAmplitude_[0] : &usbAvgAmplitude_[0]));  
  writeReg(bName, "usb",              (swapSidebands ? &lsb_[0]             : &usb_[0]));              

  writeReg(bName, "lsbAvg",           (swapSidebands ? &usbAvg_[0]          : &lsbAvg_[0]));		  
  writeReg(bName, "lsbVar",           (swapSidebands ? &usbVar_[0]          : &lsbVar_[0]));		  
  writeReg(bName, "lsbAmplitude",     (swapSidebands ? &usbAmplitude_[0]    : &lsbAmplitude_[0]));	  
  writeReg(bName, "lsbAvgAmplitude",  (swapSidebands ? &usbAvgAmplitude_[0] : &lsbAvgAmplitude_[0]));  
  writeReg(bName, "lsb",              (swapSidebands ? &usb_[0]             : &lsb_[0]));              

  writeReg(bName, "baselineReceived", &baselineReceived_[0]);
}

/**.......................................................................
 * Pack a single baseline.
 */
void CorrelatorDataFrameManager::
packBaseline(carma::correlator::lib::CorrelatorBaseline& baseline)
{
  // Convert to SZA-sanctioned indices

  unsigned iAnt1 = baseline.getInput1Number();
  unsigned iAnt2 = baseline.getInput2Number();

  if(iAnt1 > AntNum::NANT-1 || iAnt2 > AntNum::NANT-1)
    ThrowError("Antenna index out of range: (" 
	       << iAnt1 << ", " << iAnt2 << ")");

  std::vector<carma::correlator::lib::CorrelatorSideband*>* sidebands = 
    baseline.getSidebands();
  
  for(unsigned iSide = 0; iSide < sidebands->size(); iSide++) 
    packSideBand((*sidebands)[iSide], iAnt1, iAnt2);
}

/**.......................................................................
 * Pack a sideband.
 */
void CorrelatorDataFrameManager::
packSideBand(carma::correlator::lib::CorrelatorSideband* sideBand,
	     unsigned iAnt1, unsigned int iAnt2)
{
  // NULL sideBand means we have no data for it.

  if(sideBand == 0)
    return;

  Complex<float> avgVal = sideBand->getStats()->getAvg();
  Complex<float> varVal = sideBand->getStats()->getStandardDeviation();
  std::vector<std::complex<float> >* data = sideBand->getData();

  // The number of uncorrupted lags is NCHAN_TOTAL, but the actual
  // array returned should be at least NCHAN_TOTAL.  It might be
  // larger if we're receiving simulated data, so we won't throw an
  // error.

  if(data->size() < CorrelatorBand::NCHAN_TOTAL)
    ThrowError("Invalid number of bands: " << data->size());

  // Pack the averages according to the type of sideband

  if(sideBand->isAuto()) {

    autoAvg_[iAnt1] = avgVal.real();
    autoVar_[iAnt1] = varVal.real();

    // Note these loops iterate only over NCHAN_TOTAL

    float* ptr = &auto_[0];
    unsigned index;
    for(unsigned iChan=0; iChan < CorrelatorBand::NCHAN_TOTAL; iChan++) {

      index = iAnt1*sza::util::CorrelatorBand::NCHAN_TOTAL+iChan;
      *(ptr + index) = (*data)[iChan+1].real();
    }

  } else if(sideBand->isUSB()) {

    unsigned iBase = crossBaselineIndex_[iAnt1][iAnt2];
    usbAvg_[iBase].real_ = avgVal.real();
    usbAvg_[iBase].imag_ = avgVal.imag();
    usbVar_[iBase].real_ = varVal.real();
    usbVar_[iBase].imag_ = varVal.imag();

    Complex<float>::Data* ptr = &usb_[0];
    unsigned index;
    for(unsigned iChan=0; iChan < CorrelatorBand::NCHAN_TOTAL; iChan++) {
      (ptr + iBase*sza::util::CorrelatorBand::NCHAN_TOTAL+iChan)->real_ = (*data)[iChan+1].real();
      (ptr + iBase*sza::util::CorrelatorBand::NCHAN_TOTAL+iChan)->imag_ = (*data)[iChan+1].imag();
    }
    
  } else {
    
    unsigned iBase = crossBaselineIndex_[iAnt1][iAnt2];
    lsbAvg_[iBase].real_ = avgVal.real();
    lsbAvg_[iBase].imag_ = avgVal.imag();
    lsbVar_[iBase].real_ = varVal.real();
    lsbVar_[iBase].imag_ = varVal.imag();
    
    Complex<float>::Data* ptr = &lsb_[0];
    for(unsigned iChan=0; iChan < CorrelatorBand::NCHAN_TOTAL; iChan++) {
      (ptr + iBase*sza::util::CorrelatorBand::NCHAN_TOTAL+iChan)->real_ = (*data)[iChan+1].real();
      (ptr + iBase*sza::util::CorrelatorBand::NCHAN_TOTAL+iChan)->imag_ = (*data)[iChan+1].imag();
    }
  }
}

/**..............................5B.........................................
 * Return the name associated with this band
 */
std::string 
CorrelatorDataFrameManager::
bandName(carma::correlator::lib::CorrelatorBand& corrBand)
{
  return bandName(corrBand.getBandNumber());
}
#endif

/**.......................................................................
 * Return the name associated with this band
 */
std::string CorrelatorDataFrameManager::bandName(unsigned iBand)
{
  return CorrelatorBand::bandName(iBand);
}

/**.......................................................................
 * Set an antenna-based gain
 */
void CorrelatorDataFrameManager::
setAntennaBasedGain(Complex<float> gain, 
		    unsigned iAnt, unsigned iBand, unsigned iChan)
{
  if(iAnt > AntNum::NANT-1 || iBand > CorrelatorBand::NBAND-1 ||
     iChan > CorrelatorBand::NCHAN_TOTAL-1) {
    ThrowError("Out of range index");
  } else
    antennaBasedGains_[iAnt][iBand][iChan] = gain;
}

void CorrelatorDataFrameManager::printAntennaBasedGains()
{
    for(unsigned iBand=0; iBand < CorrelatorBand::NBAND; iBand++) {
      std::cout << "Band" << iBand << ": " << std::endl;
      for(unsigned iAnt=0; iAnt < AntNum::NANT; iAnt++) {
	for(unsigned iChan=0; iChan < CorrelatorBand::NCHAN_TOTAL; iChan++) 
	  std::cout << antennaBasedGains_[iAnt][iBand][iChan] << " ";
	std::cout << std::endl;
      }
      std::cout << std::endl;
    }
}

void CorrelatorDataFrameManager::
setBandReceived(CorrelatorBand::Id id, bool received)
{
  CorrelatorBand band(id);

  for(CorrelatorBand banditer(CorrelatorBand::BAND0); 
      banditer <= CorrelatorBand(CorrelatorBand::BANDMAX); 
      banditer++) {
    if(band.isSet(banditer)) {
      setBandReceived(banditer, received);
    }
  }
}

void CorrelatorDataFrameManager::
setBandReceived(CorrelatorBand& iBand, bool received)
{
  // Mark this band as received/not received

  writeReg(iBand.bandName(), "received", 
	   received ? (unsigned char)FrameFlags::RECEIVED : 
	   (unsigned char)FrameFlags::NOT_RECEIVED);

  // If data were not received, mark the source as unknown

  if(!received)
    writeReg(iBand.bandName(), "source", (unsigned char*)"unknown");

  for(unsigned iBase=0; iBase < AntNum::NBASE; iBase++)
    baselineReceived_[iBase] = (received) ? FrameFlags::RECEIVED : FrameFlags::NOT_RECEIVED;

  writeReg(iBand.bandName(), "baselineReceived", &baselineReceived_[0]);
}

void CorrelatorDataFrameManager::
setCrossBaselineReceived(unsigned iBase, bool received)
{
  baselineReceived_[iBase] = (received) ? FrameFlags::RECEIVED : FrameFlags::NOT_RECEIVED;
}

/**.......................................................................
 * Initialize the variables which will keep track of what sources were
 * input to the correlator
 */
void CorrelatorDataFrameManager::
initializeSources()
{
  isNoise_ = true;
  isRf_    = true;
}

/**.......................................................................
 * Compare the passed string to the sources we care about and AND the
 * result into the boolean variables
 */
void CorrelatorDataFrameManager::
addSource(std::string& source)
{
  isNoise_ &= strcmp(source.c_str(), "noise")==0;
  isRf_    &= strcmp(source.c_str(), "rf")==0;
}

/**.......................................................................
 * Return true if all bands reported they were observing the noise
 * source
 */
bool CorrelatorDataFrameManager::
isNoise()
{
  return isNoise_;
}

/**.......................................................................
 * Return true if all bands reported they were observing RF
 */
bool CorrelatorDataFrameManager::
isRf()
{
  return isRf_;
}
