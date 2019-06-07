#include "carma/correlator/transport/CorrDataRemapper.h"
#include "carma/correlator/transport/CorrDataRemapperControlImpl.h"

#include "carma/signalpath/SignalPathMap.h"

#include "carma/szautil/Exception.h"

#include "carma/util/corrUtils.h"
#include "carma/util/Orb.h"
#include "carma/util/Program.h"

#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

using namespace std;

using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::monitor;
using namespace carma::util;

/**.......................................................................
 * Constructor.
 */
CorrDataRemapperControlImpl::CorrDataRemapperControlImpl(bool produce, std::string imrSrc, std::string imrDest, bool noPCS ) 
{
  //ScopedLogNdc ndc ("CorrDataRemapperControlImpl constructor");
  produce_ = produce;
  imrSrc_  = imrSrc;
  imrDest_ = imrDest;
  noPCS_   = noPCS;

  // If producing, then we create listeners on the correlator band
  // notification channels, and create producer threads for
  // republishing the data

  if(produce_) {

    //CARMALOGINFO("EML corrDataRemapper Here 0");
    createCorrBandListeners();
    createProducers();
    //CARMALOGINFO("EML corrDataRemapper Here 1");

    // Else just create listeners on the astroband notification
    // channels

  } else {
    createAstroBandListeners();
    setAstroBandInputMapsToDefaults();
  }

  // Now spawn the listener threads

    //CARMALOGINFO("EML corrDataRemapper Here 2");
    spawnListeners();
}

/**.......................................................................
 * Destructor.
 */
CorrDataRemapperControlImpl::~CorrDataRemapperControlImpl() 
{
  for(unsigned i=0; i < listeners_.size(); i++) {
    if(listeners_[i]) {
      delete listeners_[i];
      listeners_[i] = 0;
    }
  }

  for(unsigned i=0; i < producers_.size(); i++) {
    if(producers_[i]) {
      delete producers_[i];
      producers_[i] = 0;
    }
  }
}

/**.......................................................................
 * Publish remapped data for an astroband
 */
bool CorrDataRemapperControlImpl::
publishData(carma::correlator::lib::CorrelatorData* cd, unsigned astroBandNo)
{
  if(produce_) {

    try {

      // Note we don't lock, since DefaultCorrControl::sendCorrData()
      // already locks internally

      producers_[astroBandNo-1]->sendCorrelatorData(cd);

    } catch ( const CORBA::Exception & ex ) {
      COUT("Caught a CORBA error");
      return false;
    } catch(...) {
      COUT("Caught an unknown error");
      return false;
    }
  }

#if 0
  // Temporary debug facility -- not mutex protecting this because it's not for long-term use

  debugger_.printDebugInfo(cd);

#endif

   return produce_;
}

/**.......................................................................
 * Spawn listeners for correlator data
 */
void CorrDataRemapperControlImpl::createCorrBandListeners()
{
  std::ostringstream objectName;

  // Create spectral-line correlator objects

  for(unsigned iSlCorrBand=0; iSlCorrBand < carma::signalpath::BlockDownconverter::nBandSl_; 
      iSlCorrBand++) {
    objectName.str("");
    const unsigned bandNo = iSlCorrBand+1;
    objectName << "carma.correlator.slcData" << bandNo;
    addListener(objectName.str(), bandNo);
  }

  // Create wideband correlator objects

  for(unsigned iWbCorrBand=0; iWbCorrBand < carma::signalpath::BlockDownconverter::nBandWb_; 
      iWbCorrBand++) {
    objectName.str("");
    const unsigned bandNo = iWbCorrBand+1;
    objectName << "carma.correlator.wbcData" << bandNo;
    addListener(objectName.str(), bandNo);
  }

  // C3GMAX23
  for(unsigned iCorrBand=0; iCorrBand < NUM_C3G_BANDS; iCorrBand++) {
    objectName.str("");
    const unsigned bandNo = iCorrBand+1;
    objectName << "carma.correlator.c3gmax23Data" << bandNo;
    addListener(objectName.str(), bandNo);
  }

  // C3GMAX8
  for(unsigned iCorrBand=0; iCorrBand < NUM_C3G_BANDS; iCorrBand++) {
    objectName.str("");
    const unsigned bandNo = iCorrBand+1;
    objectName << "carma.correlator.c3gmax8Data" << bandNo;
    // count starts after C3GMAX23 highest band number.
    addListener(objectName.str(), bandNo+NUM_C3G_BANDS);
  }
}

void CorrDataRemapperControlImpl::createAstroBandListeners()
{
  std::ostringstream ncName;

  unsigned iAstroBandStop = carma::signalpath::AstroBand::nBandMax_;

  for(unsigned iAstroBand=0; iAstroBand < iAstroBandStop; iAstroBand++) {
    ncName.str("");
    const unsigned abNo = iAstroBand+1;
    ncName << "carma.correlator.astroband" << abNo << ".data";
    addListener(ncName.str(), abNo);
  }
}

void CorrDataRemapperControlImpl::addListener(std::string ncName, const unsigned bandNo)
{

    /* debug
    ScopedLogNdc ndc ("CorrDataRemapperControlImpl::addListener");
    {
        ostringstream os;
        os << "Adding listener for corr band " << bandNo;
        programLogNotice( os.str() );
    }
    */
  CorrDataRemapper* listener = new CorrDataRemapper(this, ncName, imrSrc_);

  if ( ncName.find("slc") != string::npos ) {
      slmon_ptr s( new SlRemapperSubsystem( bandNo ) );
      slMonsys_.insert( make_pair<string,slmon_ptr>(ncName,s) );
      if ( ! noPCS_ ) s->startAutoWriter( 0.0 );
  } else if ( ncName.find("wbc") != string::npos ) {
      wbmon_ptr s(new WbRemapperSubsystem( bandNo ) );
      wbMonsys_.insert( make_pair<string, wbmon_ptr>(ncName,s) );
      if ( !noPCS_ ) s->startAutoWriter( 0.0 );
  } else if ( ncName.find("c3g") != string::npos ) {
      c3gmon_ptr s(new C3gRemapperSubsystem( bandNo ) );
      c3gMonsys_.insert( make_pair<string, c3gmon_ptr>(ncName,s) );
      if ( !noPCS_ ) s->startAutoWriter( 0.0 );
  }  // if astroband listener, don't create monitor systems

  if ( noPCS_ ) 
      programLogNotice(" No CorrDataRemapper monitor systems will be written because keyword noPCS = true.");

  listeners_.push_back(listener);
}
  
/**.......................................................................
 * Spawn listeners once resources are all in place
 */
void CorrDataRemapperControlImpl::spawnListeners()
{
  unsigned iStop = listeners_.size();

  for(unsigned i=0; i < iStop; i++) {
    listeners_[i]->spawn();
  }

}

/**.......................................................................
 * Initialize data connections
 */
void CorrDataRemapperControlImpl::createProducers()
{
  orb_.allowRegistrationWithImr(false);
  orb_.setName( "SharedProducerOrb" );
  orb_.setImrName(imrDest_);

  // Then initialize the orb to Program defaults

  Program::getProgram().orbInit(&orb_);

  std::ostringstream ncName;
   
  // Iterate over all astrobands, creating a notification channel for
  // each one

  unsigned iAstroBandStop = carma::signalpath::AstroBand::nBandMax_;

  const string root("carma.correlator.");
  for(unsigned iAstroBand=0; iAstroBand < iAstroBandStop; iAstroBand++) {

    unsigned astroBandNo = iAstroBand+1;

    ostringstream abos;
    abos << "astroband" << astroBandNo << ".data";
    const string abStr = abos.str();
    ncName.str("");
    ncName << root  << abStr;
     
    // Control port of -1 puts the 'control interface' into simulation mode
     
    Producer* producer = new Producer( ncName.str() , orb_ );
    producers_.push_back(producer);

    // save for setting monitor point later.
    producerNames_.insert(make_pair<unsigned,string>( astroBandNo, abStr ));
    /*
    { //debug
        abos.str("");
        abos << " made producer for " << abStr;
        programLogNotice( abos.str() );
    }
    */
  }
}

/**.......................................................................
 * IDL method to update the mapping for an astro band
 */
void CorrDataRemapperControlImpl::
clearAstroBandInputMap(CORBA::UShort& astroBandNo)
{
  CorrDataRemapper::AstroBandInputMap astroBandInputMap;

  astroBandInputMap.astroBandNo_ = astroBandNo;
  astroBandInputMap.antennaIfMap_.clear();

  // Now pass it down to all listeners

  for(unsigned i=0; i < listeners_.size(); i++) {
    listeners_[i]->repopulateAstroBandInputMap(astroBandInputMap);
  }
}

/**.......................................................................
 * IDL method to update the mapping for an astro band
 */
void CorrDataRemapperControlImpl::
updateAstroBandInputMap(CORBA::UShort& astroBandNo,
            const CorrDataRemapperControl::AstroBandInputSeq& astroBandInputs)
{
  //programLogInfo("EML CDRCI: inside updateAstroBandInputMap");

  // Construct a map from the IDL structure

  CorrDataRemapper::AstroBandInputMap astroBandInputMap;

  astroBandInputMap.astroBandNo_ = astroBandNo;
  for(unsigned iInput=0; iInput < astroBandInputs.length(); iInput++) {
    const carma::correlator::CorrDataRemapperControl::AstroBandInput& input = astroBandInputs[iInput];

    astroBandInputMap.antennaIfMap_[input.inputNo] = input.antennaIF;
  }

  // Now pass it down to all listeners

  for(unsigned i=0; i < listeners_.size(); i++) {
    listeners_[i]->repopulateAstroBandInputMap(astroBandInputMap);
  }
}

/**.......................................................................
 * Set up default 1-1 mapping for any input if we are just monitoring data
 */
void CorrDataRemapperControlImpl::
setAstroBandInputMapsToDefaults()
{
  CorrDataRemapper::AstroBandInputMap astroBandInputMap;

  for(unsigned iInput=0; iInput < carma::signalpath::SignalPathMap::nAnt_; iInput++) {
    astroBandInputMap.antennaIfMap_[iInput+1].antNo   = iInput+1;
    astroBandInputMap.antennaIfMap_[iInput+1].polType = carma::signalpath::SignalPathMapperControl::POL_NONE;
  }

  for(unsigned iAstroBand=0; iAstroBand < carma::signalpath::AstroBand::nBandMax_; iAstroBand++) {
    astroBandInputMap.astroBandNo_ = iAstroBand+1;

    // Now pass it down to all listeners
    
    for(unsigned i=0; i < listeners_.size(); i++) {
      listeners_[i]->repopulateAstroBandInputMap(astroBandInputMap);
    }
  }
}
 
//=======================================================================
// Methods of the CorrDataRemapperControlImpl::Debugger class
//=======================================================================

CorrDataRemapperControlImpl::Debugger::Debugger()
{
  debug_     = false;
  sideband_  = SB_AUTO;
  inputNo1_  = 0;
  inputNo2_  = 0;
}

void CorrDataRemapperControlImpl::Debugger::printDebugInfo(CorrelatorData* cd)
{
  if(!debug_) {
    return;
  }
  
  try {
    CorrelatorBand& band = (cd->getBands())[0];

    if(band.getBandNumber() == (int)astroBandNo_) {

      const CorrelatorBaseline& baseline = band.getBaseline(inputNo1_, inputNo2_);

      switch (sideband_) {
      case SB_LSB:
        printSideband(baseline.getLowerSideband());
        break;
      case SB_USB:
        printSideband(baseline.getUpperSideband());
        break;
      default:
        printSideband(baseline.getAutoSideband());
        break;
      }
    }

  } catch(...) {
  }
}

void CorrDataRemapperControlImpl::Debugger::printSideband(const CorrelatorSideband& sb)
{
  ::std::vector< ::std::complex< float > > & data = sb.getData();

  COUT("Vis(" << inputNo1_ << ", " << inputNo2_ << "): " << sideband_);
  for(unsigned i=0; i < 1; i++) {
    COUT("     Channel " << i << " re = " << real(data[i]) << " im = " << imag(data[i]));
  }
}

CorrDataRemapperControlImpl::Debugger& CorrDataRemapperControlImpl::getDebugger()
{
  return debugger_;
}

//=======================================================================
// Methods of the CorrDataRemapperControlImpl::Producer class
//=======================================================================

/**.......................................................................
 * Constructor for our minimal correlator-data producer class, that
 * uses the DefaultCorrControl interface for sending corr data over a
 * notification channel.
 */
CorrDataRemapperControlImpl::Producer::Producer(
    std::string notificationChannelName,
    Orb & orb ) :
  CorbaCorrProducer( &orb, notificationChannelName )
{
  // Nothing
}

CorrDataRemapperControlImpl::Producer::~Producer()
{
}

/**.......................................................................
 * Send correlator data on our notification channel
 */
void CorrDataRemapperControlImpl::Producer::sendCorrelatorData(CorrelatorData* cd)
{
  // Just call through to DefaultCorrControl::sendCorData()

  sendCorData(*cd, 0, 0);
}

/** Call back methods for setting monitor points that indicate how well
 * data are flowing through the remapper 
 */

void 
CorrDataRemapperControlImpl::setOnlineMP( 
        const std::string & ncName, const bool online)
{
    if ( slMonsys_.find(ncName) != slMonsys_.end() ) {
        slMonsys_[ncName]->online().setValue( online );
        return;
    }

    if ( wbMonsys_.find(ncName) != wbMonsys_.end() ) {
        wbMonsys_[ncName]->online().setValue( online );
    }

    if ( c3gMonsys_.find(ncName) != c3gMonsys_.end() ) {
        c3gMonsys_[ncName]->online().setValue( online );
    }
}

void 
CorrDataRemapperControlImpl::setNumValidBaselinesMP( 
        const std::string & ncName, const unsigned num )
{
    if ( slMonsys_.find(ncName) != slMonsys_.end() ) {
        slMonsys_[ncName]->numValidBaselines().setValue(num);
        return;
    }
    if ( wbMonsys_.find(ncName) != wbMonsys_.end() ) {
        wbMonsys_[ncName]->numValidBaselines().setValue(num);
    }
    if ( c3gMonsys_.find(ncName) != c3gMonsys_.end() ) {
        c3gMonsys_[ncName]->numValidBaselines().setValue(num);
    }
}

/**
 * There is no way for the remapper to know the number of invalid baselines
 * because the correlator data received already has the invalid baselines
 * removed.
void 
CorrDataRemapperControlImpl::setNumInvalidBaselinesMP( 
        const std::string & ncName, const unsigned num )
{
    if ( slMonsys_.find(ncName) != slMonsys_.end() ) {
        slMonsys_[ncName]->numInvalidBaselines().setValue(num);
        return;
    }

    if ( wbMonsys_.find(ncName) != wbMonsys_.end() ) {
        wbMonsys_[ncName]->numInvalidBaselines().setValue(num);
    }
}
*/

void 
CorrDataRemapperControlImpl::setAstrobandNoMP( 
        const std::string & ncName, const unsigned abNo )
{
    if ( slMonsys_.find(ncName) != slMonsys_.end() ) {
        slMonsys_[ncName]->astrobandNo().setValue(abNo);
        return;
    }

    if ( wbMonsys_.find(ncName) != wbMonsys_.end() ) {
        wbMonsys_[ncName]->astrobandNo().setValue(abNo);
    }

    if ( c3gMonsys_.find(ncName) != c3gMonsys_.end() ) {
        c3gMonsys_[ncName]->astrobandNo().setValue(abNo);
    }
}

void 
CorrDataRemapperControlImpl::setNumConjugatedMP( 
        const std::string & ncName, const unsigned num )
{
    if ( slMonsys_.find(ncName) != slMonsys_.end() ) {
        slMonsys_[ncName]->numConjugated().setValue( num );
        return;
    }

    if ( wbMonsys_.find(ncName) != wbMonsys_.end() ) {
        wbMonsys_[ncName]->numConjugated().setValue( num );
    }

    if ( c3gMonsys_.find(ncName) != c3gMonsys_.end() ) {
        c3gMonsys_[ncName]->numConjugated().setValue( num );
    }
}

void CorrDataRemapperControlImpl::setDefaultPublishedObjectMP( 
        const std::string & ncName, const unsigned astroBandNo)
{
    if ( producerNames_.find( astroBandNo ) == producerNames_.end() ) {
        setPublishedObjectMP(ncName,"unknown");
        return;
    }

    if ( slMonsys_.find(ncName) != slMonsys_.end() ) {
        slMonsys_[ncName]->publishedObject().setValue( producerNames_[astroBandNo]);
        return;
    }

    if ( wbMonsys_.find(ncName) != wbMonsys_.end() ) {
        wbMonsys_[ncName]->publishedObject().setValue( producerNames_[astroBandNo]);
        return;
    }

    if ( c3gMonsys_.find(ncName) != c3gMonsys_.end() ) {
        c3gMonsys_[ncName]->publishedObject().setValue( producerNames_[astroBandNo]);
        return;
    }
}

void 
CorrDataRemapperControlImpl::setPublishedObjectMP( 
        const std::string & ncName, const std::string & objName )
{
    if ( slMonsys_.find(ncName) != slMonsys_.end() ) {
        slMonsys_[ncName]->publishedObject().setValue( objName );
        return;
    }

    if ( wbMonsys_.find(ncName) != wbMonsys_.end() ) {
        wbMonsys_[ncName]->publishedObject().setValue( objName );
        return;
    }

    if ( c3gMonsys_.find(ncName) != c3gMonsys_.end() ) {
        c3gMonsys_[ncName]->publishedObject().setValue( objName );
        return;
    }
}

void 
CorrDataRemapperControlImpl::setReceivedTimeMP( 
        const std::string & ncName, const double mjd)
{
    if ( slMonsys_.find(ncName) != slMonsys_.end() ) {
        slMonsys_[ncName]->receivedTime().setValue( mjd );
        return;
    }

    if ( wbMonsys_.find(ncName) != wbMonsys_.end() ) {
        wbMonsys_[ncName]->receivedTime().setValue( mjd );
        return;
    }

    if ( c3gMonsys_.find(ncName) != c3gMonsys_.end() ) {
        c3gMonsys_[ncName]->receivedTime().setValue( mjd );
        return;
    }

}

void 
CorrDataRemapperControlImpl::setPublishedTimeMP( 
        const std::string & ncName, const double mjd)
{
    if ( slMonsys_.find(ncName) != slMonsys_.end() ) {
        slMonsys_[ncName]->publishedTime().setValue( mjd );
        return;
    }

    if ( wbMonsys_.find(ncName) != wbMonsys_.end() ) {
        wbMonsys_[ncName]->publishedTime().setValue( mjd );
        return;
    }

    if ( c3gMonsys_.find(ncName) != c3gMonsys_.end() ) {
        c3gMonsys_[ncName]->publishedTime().setValue( mjd );
        return;
    }

}
