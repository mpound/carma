#include "carma/szautil/ArrayDataFrameManager.h"
#include "carma/szautil/CorrelatorBand.h"
#include "carma/szautil/Complex.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/FrameFlags.h"

#include "carma/antenna/sza/antenna/corba/CarmaDataMapper.h"
#include "carma/antenna/sza/antenna/corba/VisBrickReader.h"

#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/correlator/lib/CorrelatorData.h"

#include "carma/pipeline/VisBrickReader.h"

#include <boost/foreach.hpp>

using namespace std;

using namespace sza::antenna::corba;
using namespace sza::util;

using namespace carma::correlator::lib;
using namespace carma::pipeline;

/**.......................................................................
 * Constructor.
 */
VisBrickReader::VisBrickReader(std::string confFile) :
  confFile_(confFile)
{
  initialize();
  CorrelatorConfigChecker::getInstance(confFile);
}

void VisBrickReader::readFile(std::string fileName)
{
  CorrelatorVisBrickReader* reader = new CorrelatorVisBrickReader(fileName);
  recs_ = reader->getRecordsKeyedByFrame(); 
  for( carma::pipeline::RecordsByFrameMap::iterator recIter = recs_.begin(); recIter != recs_.end(); recIter++) {
 
    carma::correlator::lib::CorrelatorData* data = recIter->second;
    mjd_.setMjd(data->getHeader().getMJD());
    COUT("MJD = " << mjd_);
    unsigned int count = carma::util::Time::computeClosestFrame((double)data->getHeader().getMJD());
    COUT("Count = " << count);

    const BandVector& bands = data->getBands();
    
    carma::util::frameType frame = recIter->first;
    unsigned nBands = bands.size();
    std::cout << frame << std::endl;
    
    for(BandVector::const_iterator iBand = bands.begin(); iBand != bands.end(); ++iBand) {

      int bandNo = iBand->getBandNumber();      
    
      const BaselineVector baselines = iBand->getBaselines();
      COUT("BandNo = " << bandNo << " nBaselines = " << baselines.size());
      for(BaselineVector::const_iterator iBase = baselines.begin(); iBase != baselines.end(); ++iBase) {

	int iAnt1 = iBase->getInput1Number();
	int iAnt2 = iBase->getInput2Number();
	COUT("(" << iAnt1 << "," << iAnt2 << ")");

	const SidebandVector sidebands = iBase->getSidebands();
    BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands ) {
	  COUT( "Sideband has nchan = " << sideband.getNumberOfChans() 
            << " is USB = " << sideband.isUSB());
	}
      }
    }
  }

  delete reader;
}

void VisBrickReader::closeFile()
{
  if(reader_ != 0) {
    delete reader_;
  }
}

void VisBrickReader::loadFile(std::string fileName)
{
  closeFile();

  reader_  = new CorrelatorVisBrickReader(fileName);
  recs_    = reader_->getRecordsKeyedByFrame(); 
  recIter_ = recs_.begin();

  COUT("Found " << recs_.size() << " data records");
}

bool VisBrickReader::atEnd()
{
  return !(recIter_ != recs_.end());
}

unsigned int VisBrickReader::getCurrentFrameCount()
{
  carma::correlator::lib::CorrelatorData* data = recIter_->second;
  return carma::util::Time::computeClosestFrame((double)data->getHeader().getMJD());
}

void VisBrickReader::packNextRecord(ArrayDataFrameManager* fm)
{
  setBaselinesUnreceived();

  carma::correlator::lib::CorrelatorData* data = recIter_->second;
  mjd_.setMjd(data->getHeader().getMJD());
  
  COUT("MJD = " << mjd_);
  unsigned int count = carma::util::Time::computeClosestFrame((double)data->getHeader().getMJD());
  COUT("Count = " << count);

  const BandVector& bands = data->getBands();
  fm->writeReg("array", "frame", "utc", mjd_.data());
  //  COUT("Found " << bands.size() << " bands");

  for(BandVector::const_iterator iBand = bands.begin(); iBand != bands.end(); ++iBand) {
    int bandNo = iBand->getBandNumber();
    //    COUT("BandNo = " << bandNo);
    packBand(*iBand, fm);
  }

  // Increment to the next record
  
  recIter_++;
}

/**.......................................................................
 * Destructor.
 */
VisBrickReader::~VisBrickReader() 
{
  closeFile();
}

void VisBrickReader::initialize()
{
  reader_ = 0;

  nAnt_  = CarmaDataMapper::nSlAnt_;
  nBase_ = CarmaDataMapper::nSlBase_;
  nBand_ = CarmaDataMapper::nBandMax_;
  nChan_ = CarmaDataMapper::nChan_;

  // Construct the hash map of antenna index <--> cross correlation
  // indices

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

  // Resize arrays

  usbAmplitude_.resize(nBase_ * nChan_);
  usbAvgAmplitude_.resize(nBase_);
  usbAvg_.resize(nBase_);
  usbVar_.resize(nBase_);
  usb_.resize(nBase_ * nChan_);
  usbValid_.resize(nBase_ * nChan_);

  lsbAmplitude_.resize(nBase_ * nChan_);
  lsbAvgAmplitude_.resize(nBase_);
  lsbAvg_.resize(nBase_);
  lsbVar_.resize(nBase_);
  lsb_.resize(nBase_ * nChan_);
  lsbValid_.resize(nBase_ * nChan_);

  autoAvg_.resize(nAnt_);
  autoVar_.resize(nAnt_);
  auto_.resize(nAnt_ * nChan_);
  autoValid_.resize(nAnt_ * nChan_);

  baselineReceived_.resize(nBase_);
}

/**.......................................................................
 * Pack a single band.
 */
void VisBrickReader::
packBand(const carma::correlator::lib::CorrelatorBand& corrBand, ArrayDataFrameManager* fm)
{
  int bandNo = corrBand.getBandNumber();

  // Quietly return if this band is not one of ours

  if(bandNo > nBand_) {
    return;
  }

  // Start numbering at 1, like CARMA

  std::string bName = bandName(bandNo);

  //  COUT("Inside packBand: " << bName << " " << corrBand.getBandwidth());

  const BaselineVector baselines = corrBand.getBaselines();

  //  COUT("size = " << baselines.size());

  // Mark this band as received

  fm->writeReg("corr", bName, "received", (unsigned char)FrameFlags::RECEIVED);

  // And pack all visibilities for it

  for(BaselineVector::const_iterator iBase = baselines.begin(); iBase != baselines.end(); ++iBase) {

    try {
      packBaseline(*iBase);
    } catch(Exception& err) {
      COUT("Caught an exception: " << err.what());
    } catch(...) {
      COUT("Caught an unknown exception");
    }

  }

  // Now write all baselines for this band

  fm->writeReg("corr", bName, "autoAvg",   &autoAvg_[0]);
  fm->writeReg("corr", bName, "autoVar",   &autoVar_[0]);
  fm->writeReg("corr", bName, "auto",      &auto_[0]);
  fm->writeReg("corr", bName, "nAuto",     &autoNSample_);
  fm->writeReg("corr", bName, "tintAuto",  &autoTint_);
  fm->writeReg("corr", bName, "autoValid", &autoValid_[0]);

  fm->writeReg("corr", bName, "usbAvg",   &usbAvg_[0]);
  fm->writeReg("corr", bName, "usbVar",   &usbVar_[0]);
  fm->writeReg("corr", bName, "usb",      &usb_[0]);
  fm->writeReg("corr", bName, "nUsb",     &usbNSample_);
  fm->writeReg("corr", bName, "tintUsb",  &usbTint_);
  fm->writeReg("corr", bName, "usbValid", &usbValid_[0]);

  fm->writeReg("corr", bName, "lsbAvg",   &lsbAvg_[0]);
  fm->writeReg("corr", bName, "lsbVar",   &lsbVar_[0]);
  fm->writeReg("corr", bName, "lsb",      &lsb_[0]);
  fm->writeReg("corr", bName, "nLsb",     &lsbNSample_);
  fm->writeReg("corr", bName, "tintLsb",  &lsbTint_);
  fm->writeReg("corr", bName, "lsbValid", &lsbValid_[0]);

  fm->writeReg("corr", bName, "baselineReceived",     &baselineReceived_[0]);
}

/**.......................................................................
 * Pack a single baseline.
 */
void VisBrickReader::
packBaseline(const carma::correlator::lib::CorrelatorBaseline& baseline)
{
  // Convert to SZA-sanctioned indices

  int iAnt1 = baseline.getInput1Number();
  int iAnt2 = baseline.getInput2Number();

  // Quietly return of this baseline is not one of ours

  if(iAnt1 > nAnt_ || iAnt2 > nAnt_) {
    return;
  }

  const SidebandVector & sidebands = baseline.getSidebands();

  BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands ) {
    packSideBand( sideband, iAnt1, iAnt2);
  }
}

/**.......................................................................
 * Pack a sideband.
 */
void VisBrickReader::
packSideBand( const carma::correlator::lib::CorrelatorSideband & sideBand,
	     int iAnt1, int iAnt2)
{
  iAnt1 -= 1;
  iAnt2 -= 1;

  Complex<float> avgVal = sideBand.getStats().getAvg();
  Complex<float> varVal = sideBand.getStats().getStandardDeviation();
  unsigned nSamp        = sideBand.getStats().getNumberOfSamples();
  float tInt            = sideBand.getStats().getIntegrationTime();
  std::vector<std::complex<float> >& data = sideBand.getData();
  const std::vector<int>& valid = sideBand.getDataValid();

  // The number of uncorrupted lags is NCHAN_TOTAL, but the actual
  // array returned should be at least NCHAN_TOTAL.  It might be
  // larger if we're receiving simulated data, so we won't throw an
  // error.

  if(data.size() < nChan_)
    ThrowError("Invalid number of bands: " << data.size());

  // Pack the averages according to the type of sideband

  if(sideBand.isAuto()) {
    autoAvg_[iAnt1] = avgVal.real();
    autoVar_[iAnt1] = varVal.real();
    autoNSample_    = nSamp;
    autoTint_       = tInt;

    // Note these loops iterate only over NCHAN_TOTAL

    float* ptr = &auto_[0];
    int* validPtr = &autoValid_[0];
    unsigned index;
    for(unsigned iChan=0; iChan < nChan_; iChan++) {

      index = iAnt1*nChan_+iChan;
      *(ptr + index) = data[iChan].real();
      *(validPtr + index) = valid[iChan];
    }

  } else if(sideBand.isUSB()) {
    unsigned iBase = crossBaselineIndex_[iAnt1][iAnt2];
    usbAvg_[iBase].real_ = avgVal.real();
    usbAvg_[iBase].imag_ = avgVal.imag();
    usbVar_[iBase].real_ = varVal.real();
    usbVar_[iBase].imag_ = varVal.imag();
    usbNSample_          = nSamp;
    usbTint_             = tInt;

    Complex<float>::Data* ptr = &usb_[0];
    int* validPtr = &usbValid_[0];
    unsigned index;
    for(unsigned iChan=0; iChan < nChan_; iChan++) {
      (ptr + iBase*nChan_+iChan)->real_ = data[iChan].real();
      (ptr + iBase*nChan_+iChan)->imag_ = data[iChan].imag();
      *(validPtr + iBase*nChan_+iChan)  = valid[iChan];
    }

    setCrossBaselineReceived(iBase, true);
    
  } else {
    unsigned iBase = crossBaselineIndex_[iAnt1][iAnt2];
    lsbAvg_[iBase].real_ = avgVal.real();
    lsbAvg_[iBase].imag_ = avgVal.imag();
    lsbVar_[iBase].real_ = varVal.real();
    lsbVar_[iBase].imag_ = varVal.imag();
    lsbNSample_          = nSamp;
    usbTint_             = tInt;

    Complex<float>::Data* ptr = &lsb_[0];
    int* validPtr = &lsbValid_[0];
    for(unsigned iChan=0; iChan < nChan_; iChan++) {
      (ptr + iBase*nChan_+iChan)->real_ = data[iChan].real();
      (ptr + iBase*nChan_+iChan)->imag_ = data[iChan].imag();
      *(validPtr + iBase*nChan_+iChan)  = valid[iChan];
    }

    setCrossBaselineReceived(iBase, true);
  }
}

/**.......................................................................
 * Return the name associated with this band
 */
std::string 
VisBrickReader::
bandName(carma::correlator::lib::CorrelatorBand& corrBand)
{
  return bandName(corrBand.getBandNumber());
}

/**.......................................................................
 * Return the name associated with this band
 */
std::string 
VisBrickReader::bandName(int iBand)
{
  return sza::util::CorrelatorBand::bandName((unsigned)iBand);
}

void VisBrickReader::
setBaselinesUnreceived()
{
  for(unsigned iBase=0; iBase < nBase_; iBase++)
    baselineReceived_[iBase] = sza::util::FrameFlags::NOT_RECEIVED;
}

void VisBrickReader::
setCrossBaselineReceived(unsigned iBase, bool received)
{
  baselineReceived_[iBase] = (received) ? sza::util::FrameFlags::RECEIVED : sza::util::FrameFlags::NOT_RECEIVED;
}
