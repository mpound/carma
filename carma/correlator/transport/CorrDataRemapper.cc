#include "carma/correlator/lib/CorrelatorBand.h"
#include "carma/correlator/lib/CorrelatorData.h"

#include "carma/correlator/transport/CorrDataRemapper.h"
#include "carma/correlator/transport/CorrDataRemapperControlImpl.h"
#include "carma/signalpath/SignalPathMapperControl.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/TimeOut.h"

#include "carma/util/Orb.h"
#include "carma/util/Time.h"

//#include "carma/util/programLogging.h"

using namespace std;

using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace sza::util;
using namespace carma::util;
using namespace carma::signalpath;

/**.......................................................................
 * Constructor.
 */
CorrDataRemapper::CorrDataRemapper(CorrDataRemapperControlImpl* parent, 
                   std::string notificationChannelName,
                   std::string imrSrc) :
  RunnableTask(true)
{
  imrSrc_ = imrSrc;
  parent_ = parent;
  notificationChannelName_ = notificationChannelName;
  if ( parent_ != 0 )
      parent_->setOnlineMP( notificationChannelName_, true );
}

/**.......................................................................
 * Destructor.
 */
CorrDataRemapper::~CorrDataRemapper() 
{
}

/**.......................................................................
 * Process data received for a correlator band
 */
void CorrDataRemapper::processData(carma::correlator::lib::CorrelatorData * cd)
{
  unsigned astroBandNo=0;
  // debug
  //programLogNoticeIfPossible("CorrDataRemapper::processData - entering");

  // not sure if this always has to be updated. OnlineCommon.mpml is
  // not marked persistent.
  if ( parent_ != 0 )
      parent_->setOnlineMP( notificationChannelName_, true );

  //------------------------------------------------------------
  // Remap the data as required
  //------------------------------------------------------------

  try {

    remapCorrData(cd, astroBandNo);

    //------------------------------------------------------------
    // Republish the data for this astroband
    //------------------------------------------------------------
    
    republishCorrData(cd, astroBandNo);

  } catch(sza::util::Exception& err) {

    // Just continue if an error occurred -- this will result in the frame just getting dumped

    ReportError(err.what());
  }

  //------------------------------------------------------------
  // Finally, check if the astroband map has changed.  
  //
  // Note: we do this last to minimize latency, ie, publish the data
  // as fast as possible, then do things that might take some time,
  // like locking a mutex
  //------------------------------------------------------------

  updateAstroBandMap();
  /*
  { // debug 
   ostringstream os;
   os << "CorrDataRemapper::processData - exiting astroband = " << astroBandNo;
   programLogNoticeIfPossible( os.str() );
  }
  */
}

/**.......................................................................
 * Remap visibility data from astroband input ordering to antenna
 * ordering, conjugating the data as necessary
 */
void CorrDataRemapper::remapCorrData(carma::correlator::lib::CorrelatorData * cd, unsigned& astroBandNo)
{
  unsigned antennaNo1, antennaNo2;
  Polarization pol1, pol2;
  bool requiresConjugation;
  

  parent_->setReceivedTimeMP( notificationChannelName_, Time::MJD() );
  // Determine which band this is

  std::vector<carma::correlator::lib::CorrelatorBand>& bands = cd->getBands();

  if(bands.size() != 1) {
    parent_->setNumConjugatedMP( notificationChannelName_, 0 );
    ThrowError("I don't handle multiple bands");
  }

  carma::correlator::lib::CorrelatorBand& band = bands[0];

  astroBandNo = band.getBandNumber();
  parent_->setAstrobandNoMP( notificationChannelName_, astroBandNo );

  // Check for out of bound astroBandNo

  if(astroBandNo < 1 || astroBandNo > carma::signalpath::AstroBand::nBandMax_) {
    parent_->setNumConjugatedMP( notificationChannelName_, 0 );
    ThrowError("Received invalid astroBandNo: " << astroBandNo << " while processing data from " << notificationChannelName_ );
  }

  // Iterate over baselines of this band

  std::vector<CorrelatorBaseline>& baselines = band.getBaselines();
  const size_t numBaselines = baselines.size();
  parent_->setNumValidBaselinesMP( notificationChannelName_, numBaselines );

  unsigned numConjugated = 0;
  for(unsigned iBase=0; iBase < numBaselines; iBase++) {
    CorrelatorBaseline& baseline = baselines[iBase];
    
    // Determine the antenna mapping
    
    getAntPols(baseline, astroBandNo, antennaNo1, antennaNo2, pol1, pol2, requiresConjugation);

    // Set the input numbers

    baseline.setAntPol1(antennaNo1, pol1);
    baseline.setAntPol2(antennaNo2, pol2);
    
    if(requiresConjugation) {
      conjugate(baseline);
      ++numConjugated;
    }
  }

  parent_->setNumConjugatedMP( notificationChannelName_, numConjugated );
}

/**.......................................................................
 * Retrieve antenna numbers
 */
void CorrDataRemapper::getAntennaNos(CorrelatorBaseline& baseline, unsigned astroBandNo, 
                     unsigned& antennaNo1, unsigned& antennaNo2, 
                     bool& requiresConjugation)
{
  // Get the input numbers corresponding to this baseline

  unsigned inputNo1 = baseline.getInput1Number();
  unsigned inputNo2 = baseline.getInput2Number();

  // Trap getting data from an astroband if we don't have a mapping
  // for that astroband

  if(astroBandMap_.astroBands_.find(astroBandNo) ==
     astroBandMap_.astroBands_.end()) {
    ThrowError("Found data for astroband: " << astroBandNo 
           << " for which no valid mapping has been specified");
  }

  // Fetch the map for this astroband

  std::map<unsigned, carma::signalpath::SignalPathMapperControl::AntennaIF>& abMap = 
    astroBandMap_.astroBands_[astroBandNo]->antennaIfMap_;

  // Initialize requiresConjugation to false

  requiresConjugation = false;

  // If no mapping has been specified for these inputs, set the
  // antenna nos to zero

  if(abMap.find(inputNo1) == abMap.end() ||
     abMap.find(inputNo2) == abMap.end()) {

    antennaNo1 = 0;
    antennaNo2 = 0;

    // Else get the antenna nos from the mapping

  } else {

    antennaNo1 = astroBandMap_.astroBands_[astroBandNo]->antennaIfMap_[inputNo1].antNo;
    antennaNo2 = astroBandMap_.astroBands_[astroBandNo]->antennaIfMap_[inputNo2].antNo;

    // If the order of the baseline is reversed, swap the antenna
    // numbers around, and conjugate the data

    if(antennaNo1 > antennaNo2) {
      requiresConjugation = true;

      unsigned tmp = antennaNo2;
      antennaNo2 = antennaNo1;
      antennaNo1 = tmp;
    }

  }

  if(antennaNo1 == 0 || antennaNo2 == 0) {
    ThrowError("Found inputs: (" << inputNo1 << ", " << inputNo2 
           << ") for which no valid mapping has been specified, while processing data for astroband " << astroBandNo);
  }
}

/**.......................................................................
 * Retrieve antenna numbers
 */
void CorrDataRemapper::getAntPols(CorrelatorBaseline& baseline, unsigned astroBandNo, 
                  unsigned& antennaNo1, unsigned& antennaNo2, 
                  carma::correlator::lib::Polarization& pol1,
                  carma::correlator::lib::Polarization& pol2, bool& requiresConjugation)
{
  // Get the input numbers corresponding to this baseline

  unsigned inputNo1 = baseline.getInput1Number();
  unsigned inputNo2 = baseline.getInput2Number();

  // Trap getting data from an astroband if we don't have a mapping
  // for that astroband

  if(astroBandMap_.astroBands_.find(astroBandNo) ==
     astroBandMap_.astroBands_.end()) {
    ThrowError("Found data for astroband: " << astroBandNo 
           << " for which no valid mapping has been specified");
  }

  // Fetch the map for this astroband

  std::map<unsigned, carma::signalpath::SignalPathMapperControl::AntennaIF>& abMap = 
    astroBandMap_.astroBands_[astroBandNo]->antennaIfMap_;

  // Initialize requiresConjugation to false

  requiresConjugation = false;

  // If no mapping has been specified for these inputs, set the
  // antenna nos to zero

  if(abMap.find(inputNo1) == abMap.end() ||
     abMap.find(inputNo2) == abMap.end()) {

    antennaNo1 = 0;
    antennaNo2 = 0;
    pol1 = LEFT_POL;
    pol2 = LEFT_POL;

    // Else get the antenna nos from the mapping

  } else {

    antennaNo1 = astroBandMap_.astroBands_[astroBandNo]->antennaIfMap_[inputNo1].antNo;
    antennaNo2 = astroBandMap_.astroBands_[astroBandNo]->antennaIfMap_[inputNo2].antNo;
    carma::signalpath::SignalPathMapperControl::PolarizationType tempPol = astroBandMap_.astroBands_[astroBandNo]->antennaIfMap_[inputNo1].polType;
    if(tempPol == carma::signalpath::SignalPathMapperControl::POL_LEFT){
      pol1 = LEFT_POL;
    }
    else if(tempPol == carma::signalpath::SignalPathMapperControl::POL_RIGHT){
      pol1 = RIGHT_POL;
    }
    else{
      pol1 = NONE_POL;
    }
    tempPol = astroBandMap_.astroBands_[astroBandNo]->antennaIfMap_[inputNo2].polType;
    if(tempPol == carma::signalpath::SignalPathMapperControl::POL_LEFT){
      pol2 = LEFT_POL;
    }
    else if(tempPol == carma::signalpath::SignalPathMapperControl::POL_RIGHT){
      pol2 = RIGHT_POL;
    }
    else{
      pol2 = NONE_POL;
    }

    // If the order of the baseline is reversed, swap the antenna
    // numbers around, and conjugate the data

    if(antennaNo1 > antennaNo2) {
      requiresConjugation = true;

      unsigned tmp = antennaNo2;
      antennaNo2 = antennaNo1;
      antennaNo1 = tmp;
      Polarization tmpP = pol2;
      pol2 = pol1;
      pol1 = tmpP;
    }

  }

  if(antennaNo1 == 0 || antennaNo2 == 0) {
    ThrowError("Found inputs: (" << inputNo1 << ", " << inputNo2 
           << ") for which no valid mapping has been specified, while processing data for astroband " << astroBandNo);
  }
}

/**.......................................................................
 * Conjugate all data for this baseline
 */
void CorrDataRemapper::conjugate(CorrelatorBaseline& baseline)
{
  // Iterate over all spectra for this baseline

  SidebandVector & sbs = baseline.getSidebands();

  for(unsigned iSb=0; iSb < sbs.size(); iSb++) {
    CorrelatorSideband & sb = sbs[iSb];

    // Don't bother conjugating if this sideband is an auto-correlation

    if(!sb.isAuto()) {
      std::vector<std::complex<float> >& data = sb.getData();
      for(unsigned iVis=0; iVis < data.size(); iVis++) {
        std::complex<float>& vis = data[iVis];
        imag(vis) = -imag(vis);
      }

    }
  }
}

/**.......................................................................
 * Republish remapped data on the appropriate channel
 */
void CorrDataRemapper::
republishCorrData(carma::correlator::lib::CorrelatorData * cd, unsigned astroBandNo)
{
  // Just push it to the parent, who's managing the notification
  // channels
  
  if(parent_) {
    bool published = parent_->publishData(cd, astroBandNo);
    if ( published ) 
        parent_->setDefaultPublishedObjectMP( notificationChannelName_, astroBandNo);
    else 
        parent_->setPublishedObjectMP( notificationChannelName_, "Not published");
    parent_->setPublishedTimeMP( notificationChannelName_, Time::MJD() );
  }
}

/**.......................................................................
 * Check if the astroband map requires updating
 */
void CorrDataRemapper::updateAstroBandMap()
{
  stagedAstroBandMap_.guard_.lock();

  if(stagedAstroBandMap_.hasChanged_) {
    astroBandMap_ = stagedAstroBandMap_;
    stagedAstroBandMap_.hasChanged_ = false;
  }

  stagedAstroBandMap_.guard_.unlock();
}

/**.......................................................................
 * Stage a new astroband mapping.  
 *
 * Note that we can change mappings for as many astrobands as we like;
 * they won't get used until updateAstroBandMap() gets called
 */
void CorrDataRemapper::repopulateAstroBandInputMap(AstroBandInputMap& astroBandInputMap)
{
  stagedAstroBandMap_.guard_.lock();
  *(stagedAstroBandMap_.astroBands_[astroBandInputMap.astroBandNo_]) = astroBandInputMap;
  stagedAstroBandMap_.hasChanged_ = true;
  stagedAstroBandMap_.guard_.unlock();
}

/**.......................................................................
 * Instantiate a consumer and collect data 
 */
void CorrDataRemapper::run()
{
  // Just pass in the channel name for the orb name, and set the IMR
  // as well

  carma::util::Orb orb;

  orb.allowRegistrationWithImr(false);
  orb.setName(notificationChannelName_);
  orb.setImrName(imrSrc_);

  // Loop forever, attempting to get a reference to the notification
  // channel

  while(true) {

    try {

      CTOUT("Attempting to resolve notificationChannelName: " << notificationChannelName_);

      carma::correlator::obsRecord2::CorbaCorrConsumer consumer(&orb, notificationChannelName_, (*this));
      
      // Blocking call on getData()
      
      consumer.getData();
      
    } catch(...) {
      
      CTOUT("Caught an error on " << notificationChannelName_ << " waiting to reconnect");
      
      TimeOut timeout;
      timeout.setIntervalInSeconds(2);
      timeout.activate(true);

      select(0, NULL, NULL, NULL, timeout.tVal());
    }

  }
}

//-----------------------------------------------------------------------
// Methods of AstroBandMap
//-----------------------------------------------------------------------

CorrDataRemapper::AstroBandMap::AstroBandMap() 
{
  // Resize the vector of maps to match the maximum number of astrobands

  for(unsigned iAstroBand=0; iAstroBand < carma::signalpath::AstroBand::nBandMax_; iAstroBand++) {
    unsigned astroBandNo = iAstroBand+1;

    AstroBandInputMap* inputMap = new AstroBandInputMap();
    inputMap->astroBandNo_ = astroBandNo;
  
    // Initialize the antenna IF map to have an entry for any
    // astroband input

#if 0
    for(unsigned iInput=0; iInput < carma::signalpath::AstroBand::nInputMax_; iInput++) {
      inputMap->antennaIfMap_[iInput+1].antNo   = 0;
      inputMap->antennaIfMap_[iInput+1].polType = carma::signalpath::SignalPathMapperControl::POL_NONE;
    }
#endif

    // And set the astroband map entry pointing to the oboject we just created

    astroBands_[astroBandNo]  = inputMap;
  }

  hasChanged_ = false;
}

CorrDataRemapper::AstroBandMap::~AstroBandMap() 
{
  for(unsigned iAstroBand=0; iAstroBand < carma::signalpath::AstroBand::nBandMax_; iAstroBand++) {
    unsigned astroBandNo = iAstroBand+1;
    delete astroBands_[astroBandNo];
    astroBands_[astroBandNo] = 0;
  }
}

void CorrDataRemapper::AstroBandMap::operator=(const AstroBandMap& astroBandMap)
{
  return operator=((AstroBandMap&) astroBandMap);
}

void CorrDataRemapper::AstroBandMap::operator=(AstroBandMap& astroBandMap)
{
  for(unsigned iAstroBand=0; iAstroBand < carma::signalpath::AstroBand::nBandMax_; iAstroBand++) {
    unsigned astroBandNo = iAstroBand+1;
    (*astroBands_[astroBandNo]) = (*astroBandMap.astroBands_[astroBandNo]);
  }
}

void CorrDataRemapper::spawn()
{
  Runnable::spawn();
}

unsigned CorrDataRemapper::getThreadId()
{
  return pthread_self();
}
