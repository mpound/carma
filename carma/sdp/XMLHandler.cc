 /**
 * @file XMLHandler.cc
 *
 * Content and error handler for SAX parser for astro hdr XML file.
 *
 * $Id: XMLHandler.cc,v 1.140 2013/02/25 17:19:50 friedel Exp $
 *
 * @author Harold Ravlin
 *
 */

// Carma includes
#include <carma/sdp/XMLHandler.h>
#include "carma/sdp/SDPUtil.h"
#include "carma/correlator/lib/CorrelatorSideband.h"
#include "carma/observertools/ProjectDatabaseManager.h"
#include "carma/observertools/ProjectDatabaseManager_skel.h"
#include "carma/services/AstroTime.h"
#include "carma/util/FileUtils.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/util/EOFException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Trace.h"
#include "carma/monitor/ControlCorrelEnum.h"
#include "carma/monitor/SignalPathCommonMonitorPoints.h"

// Carma tools includes
#include "log4cpp/Priority.hh"
#include <xercesc/sax2/Attributes.hpp>
#include <xercesc/sax/SAXParseException.hpp>
#include <xercesc/sax/SAXException.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLDouble.hpp>
#include <xercesc/util/XMLStringTokenizer.hpp>

// Standard C++ library includes
#include <sstream>
#include <math.h>
#include <algorithm>

// Namespace using statments
using namespace std;
using namespace XERCES_CPP_NAMESPACE;
using namespace carma::sdp;
using namespace carma::pipeline;
using namespace carma::correlator::lib;
using namespace carma::dbms;
using carma::util::Program;
using carma::sdp::TrialProjectData;
using carma::sdp::SDPUtil;
XERCES_CPP_NAMESPACE_USE;

namespace carma {
  namespace sdp {

    // Initialize static data
    XMLCh *XMLHandler::KW_ = 0;
    XMLCh *XMLHandler::UNK_ = 0;
    XMLCh *XMLHandler::NAME_ = 0;
    XMLCh *XMLHandler::TYPE_ = 0;
    XMLCh *XMLHandler::LENGTH_ = 0;
    XMLCh *XMLHandler::VALUE_ = 0;
    XMLCh *XMLHandler::INTEGRATION_ = 0;
    double XMLHandler::currentStartFrameMJD_ = 0;
    carma::util::frameType XMLHandler::currentStartFrame_ = 0;
    std::string XMLHandler::currentVisBrickFile_ = "";
    CorrelatorVisBrickReader* XMLHandler::currentVisBrickReader_ = NULL;
    CorrelatorData* XMLHandler::currentCorrelatorData_ = NULL;
    std::map<int, std::map<int,int> > XMLHandler::currentNewCorrelatorInputMap_ = 
      std::map<int, std::map<int,int> >();
    std::map<int, int> XMLHandler::currentCorrelatorInputMap_ =
      std::map<int, int>();
    std::vector<int> tempCorrInputMap_ = std::vector<int>();
    std::vector<int> XMLHandler::currentCorbit_ = std::vector<int>();
    std::vector<float> XMLHandler::currentCoreff_ = std::vector<float>();
    std::vector<double> XMLHandler::currentUVW_ = std::vector<double>();
    std::vector<float> XMLHandler::currentJyperka_ = std::vector<float>();
    std::vector<double> XMLHandler::currentBandFreq_ = std::vector<double>();
    std::vector<double> XMLHandler::currentRestFreq_ = std::vector<double>();
    double XMLHandler::maxRestfreq_ = 0;
    double XMLHandler::currentIntTime_ = 0;
    std::vector<float> XMLHandler::currentTsys_ = std::vector<float>();
    std::vector<float> tempTsysMap_ = std::vector<float>();
    std::map<std::string, std::map<int, std::map<int, SystemTempData> > > XMLHandler::currentNewTsys_ =
      std::map<std::string, std::map<int, std::map<int, SystemTempData> > >();
    std::vector<float> XMLHandler::currentPsys_ = std::vector<float>();
    std::vector<float> tempPsysMap_ = std::vector<float>();
    std::map<std::string, std::map<int, std::map<int, float> > > XMLHandler::currentNewPsys_ =
      std::map<std::string, std::map<int, std::map<int, float> > >();
    std::vector<float> XMLHandler::currentPsysAttn_ = std::vector<float>();
    std::vector<float> tempPsysAttnMap_ = std::vector<float>();
    std::map<std::string, std::map<int, std::map<int, float> > > XMLHandler::currentNewPsysAttn_ =
      std::map<std::string, std::map<int, std::map<int, float> > >();
    std::vector<float> XMLHandler::currentAmbPsys_ = std::vector<float>();
    std::vector<float> tempAmbPsysMap_ = std::vector<float>();
    std::map<std::string, std::map<int, std::map<int, float> > > XMLHandler::currentNewAmbPsys_ =
      std::map<std::string, std::map<int, std::map<int, float> > >();
    std::vector<int> XMLHandler::currentAntennas_ = std::vector<int>();
    std::vector<int> XMLHandler::currentGoodBands_ = std::vector<int>();
    std::map<int, std::map<int, std::string> > XMLHandler::currentPolState_ = 
      std::map<int, std::map<int, std::string> >();
    std::vector<std::string> tempPolState_;
    std::vector<int> XMLHandler::currentVersion_ = std::vector<int>();
    int XMLHandler::currentMaxBand_ = 0;
    std::string XMLHandler::visBrickDir_ = std::string();
    carma::util::frameType XMLHandler::startFrame_ = 0;
    carma::util::frameType XMLHandler::endFrame_ = 0;
    int XMLHandler::dataNotPresentCount_ = 0;
    std::string XMLHandler::dataNotPresentFile_ = "";
    std::string XMLHandler::corrType_ = "";
    bool XMLHandler::writeData_ = false;
    bool XMLHandler::havePolState_ = false;
    carma::util::frameType XMLHandler::lastFrame_ = 0;
    TrialProjectData* XMLHandler::trialProjectData_ = NULL;
    std::string XMLHandler::inputAstroHdrFile_ = "";
    int XMLHandler::maxSlCorrelatorInput_ = 15;
    int XMLHandler::maxABCorrelatorInput_ = 32;
    int XMLHandler::maxWbCorrelatorInput_ = 8;
    int XMLHandler::maxSlBandIndex_ = 8;
    int XMLHandler::maxWbBandIndex_ = 16;
    int XMLHandler::maxABBandIndex_ = 24;
    int XMLHandler::SlBandStart_ = 1;
    int XMLHandler::WbBandStart_ = 9;
    MonitorAverageType correlatorType_ = static_cast<MonitorAverageType>(0);
    int32_t XMLHandler::visbrickVersionNumber_ = 0;
    int XMLHandler::maxAntIndex_ = 23;
    double XMLHandler::currentLatitude_ = 0;
    double XMLHandler::currentLst_ = 0;
    double XMLHandler::currentObsdec_ = 0;
    double XMLHandler::currentObsra_ = 0;
    double XMLHandler::evector_ = 0.5 * 3.14159265358979323846;
    std::map<std::string,int> XMLHandler::lastNChan_ = std::map<std::string,int>();
    std::map<std::string, std::vector<int> > XMLHandler::lastNsChan_ = std::map<std::string,std::vector<int> >();
    std::map<std::string, std::vector<int> > XMLHandler::lastIsChan_ = std::map<std::string,std::vector<int> >();
    std::string XMLHandler::currentFile_ = "";

    // Keep track of # of keywords and unknown keywords seen.
    int XMLHandler::nkws_ = 0;
    int XMLHandler::nunkws_ = 0;

    // Print an XMLCh * type string.
    ostream& operator<< (ostream& target, const XMLCh *s) 
    {
      char *p = XMLString::transcode(s);
      if(p != 0) {
	target << p;
	XMLString::release(&p);
      } else
	target << "(NULL)";
      return target;
    }

    void XMLHandler::setCurrentFile(std::string fileName){
      currentFile_ = fileName;
      if(lastNChan_.count(fileName) == 0){
	lastNChan_[fileName] = 0;
	lastNsChan_[fileName].resize(0);
	lastIsChan_[fileName].resize(0);
      }
    }

    XMLHandler::XMLHandler() 
    {
      muv_ = new MiriadUVBin();
      inCDATA_ = false;
      KW_ = XMLString::transcode("KW");
      UNK_ = XMLString::transcode("__UNK__");
      NAME_ = XMLString::transcode("name");
      TYPE_ = XMLString::transcode("type");
      LENGTH_ = XMLString::transcode("length");
      VALUE_ = XMLString::transcode("value");
      INTEGRATION_ = XMLString::transcode("INTEGRATION");
      nkws_ = 0;
      nunkws_ = 0;
      recordNumber_ = 0;
      recordLevel_ = 0;
      preambleBits_ = 0;
      for(int i=0; i < 2; i++) {
	correlationData_[i].bits_ = 0;
	correlationData_[i].ndata_ = 0;
	correlationData_[i].data_ = 0;
	correlationData_[i].flags_ = 0;
      }
      currentStartFrameMJD_ = 0;
      currentStartFrame_ = 0;
      currentVisBrickFile_ = "";
      currentVisBrickReader_ = NULL;
      currentCorrelatorData_ = NULL;
      lastFrame_ = 0;
      dataNotPresentCount_ = 0;
      dataNotPresentFile_ = "";
      trialProjectData_ = NULL;
      inputAstroHdrFile_ = "";
      currentMaxBand_ = 0;
    }

    XMLHandler::~XMLHandler() 
    {
      muv_->uvclose();
      delete muv_;
      muv_ = 0;

      for(int i=0; i<2; i++) {
	delete [] correlationData_[i].data_;
	delete [] correlationData_[i].flags_;
	correlationData_[i].data_ = 0;
	correlationData_[i].flags_ = 0;
	correlationData_[i].ndata_ = 0;
      }
      XMLString::release(&KW_);
      XMLString::release(&UNK_);
      XMLString::release(&NAME_);
      XMLString::release(&TYPE_);
      XMLString::release(&LENGTH_);
      XMLString::release(&VALUE_);
      XMLString::release(&INTEGRATION_);

      if (currentVisBrickReader_ != NULL) delete(currentVisBrickReader_);
      if (currentCorrelatorData_ != NULL) delete(currentCorrelatorData_);
      if (trialProjectData_      != NULL) delete(trialProjectData_);
    }

    // Pull keyword info from Attributes lists and call the appropriate handler.
    void XMLHandler::writeKeyword(const Attributes &a) 
    {
      const Keyword *kw;
      const XMLCh *name, *value, *type;
      int length;

      name = value = type = 0;
      length = 0;

      // Two ways of getting the info from the list. a) Scan the list
      // and pick them up as we go or b) ask explicitly.
      name = a.getValue(NAME_);
      type = a.getValue(TYPE_);
      length = buildInt(a.getValue(LENGTH_));
      value = a.getValue(VALUE_);

      ostringstream oss4;
      oss4 << "NAME " << name;

      CARMA_CPTRACE(carma::util::Trace::TRACE4,oss4.str());
      
      kw= map_.getItem(name);

      nkws_++;
      if(kw) {
	kw->funcp(this, name, kw, type, length, value);
      } else {
	nunkws_++;
	// Do not output unidentified keywords
	// putUnknown(this, name, 0, type, length, value);
      }
    }

    std::map<int, std::map<int,SystemTempData> > XMLHandler::reconfigureTsysMap(std::vector<float> &tempMap){
      std::map<int, std::map<int,SystemTempData> > outMap;
      int bandIndex = maxABBandIndex_;
      int correlIndex = maxABCorrelatorInput_;
      if(tempMap.size() != static_cast<unsigned int>(bandIndex * correlIndex * 3)){
	throw CARMA_EXCEPTION(carma::util::ErrorException, "Map sizes do not match");
      }
      for(int i = 0; i < bandIndex; i++){
	for(int j = 0; j < correlIndex; j++){
	  outMap[i+1][j+1].LSB = tempMap[3 * i * correlIndex + 3 * j];
	  outMap[i+1][j+1].USB = tempMap[3 * i * correlIndex + 3 * j + 1];
	  outMap[i+1][j+1].DSB = tempMap[3 * i * correlIndex + 3 * j + 2];
	}
      }
      return outMap;
    }

    template <class T>
    std::map<int, std::map<int,T> > XMLHandler::reconfigureMap(std::vector<T> &tempMap){
      std::map<int, std::map<int,T> > outMap;
      int bandIndex = maxABBandIndex_;
      int correlIndex = maxABCorrelatorInput_;
      if(tempMap.size() != static_cast<unsigned int>(bandIndex * correlIndex)){
	throw CARMA_EXCEPTION(carma::util::ErrorException, "Map sizes do not match");
      }
      for(int i = 0; i < bandIndex; i++){
	for(int j = 0; j < correlIndex; j++){
	  outMap[i+1][j+1] = tempMap[i * correlIndex + j];
	}
      }
      return outMap;
    }

    template <class T>
    void XMLHandler::reconfigureMultiMap(std::map<int, std::map<int,T> > inMap,
			     std::map<std::string, std::map<int, std::map<int, T> > > &outMap){
      for(typename std::map<int, std::map<int, T> >::iterator ii = inMap.begin(); ii != inMap.end(); ++ii){
	std::map<int, T> tempMap = (*ii).second;
	for(typename std::map<int, T>::iterator jj = tempMap.begin(); jj != tempMap.end(); ++jj){
	  std::string pol = currentPolState_[(*ii).first][(*jj).first];
	  int ant = currentNewCorrelatorInputMap_[(*ii).first][(*jj).first];
	  // convert to maping: polarization/antenna/band
	  outMap[pol][ant][(*ii).first] = (*jj).second;
	}
      }
    }

    void XMLHandler::reconfigureAntTsys(std::vector<float> &tempMap, std::map<std::string, std::map<int, std::map<int, SystemTempData> > > &outMap){
      int stepSize = maxAntIndex_ * maxABBandIndex_ * 3;
      for(int jant = 0; jant < maxAntIndex_; jant++){
	for(int jband = 0; jband < maxABBandIndex_; jband++){
	  outMap["L"][jant+1][jband+1].LSB = tempMap[3 * jant * maxABBandIndex_ + 3 * jband];
	  outMap["L"][jant+1][jband+1].USB = tempMap[3 * jant * maxABBandIndex_ + 3 * jband + 1];
	  outMap["L"][jant+1][jband+1].DSB = tempMap[3 * jant * maxABBandIndex_ + 3 * jband + 2];
	  outMap["R"][jant+1][jband+1].LSB = tempMap[3 * jant * maxABBandIndex_ + 3 * jband + stepSize];
	  outMap["R"][jant+1][jband+1].USB = tempMap[3 * jant * maxABBandIndex_ + 3 * jband + 1 + stepSize];
	  outMap["R"][jant+1][jband+1].DSB = tempMap[3 * jant * maxABBandIndex_ + 3 * jband + 2 + stepSize];
	}
      }
    }

    void XMLHandler::reconfigureAntAmbPsys(std::vector<float> &tempMap, std::map<std::string, std::map<int, std::map<int, float> > > &outMap){
      int stepSize = maxAntIndex_ * maxABBandIndex_;
      for(int jant = 0; jant < maxAntIndex_; jant++){
	for(int jband = 0; jband < maxABBandIndex_; jband++){
	  outMap["L"][jant+1][jband+1] = tempMap[jant * maxABBandIndex_ + jband];
	  outMap["R"][jant+1][jband+1] = tempMap[jant * maxABBandIndex_ + jband + stepSize];
	}
      }
    }

    template <class T>
    T XMLHandler::calculateMedian(std::vector<T> &data){
      T median = 0;
      if(data.size() == 0)
	return median;
      if(data.size() == 1)
	return data[0];
      sort(data.begin(),data.end());
      if(data.size() % 2 == 0){
	median = (data[static_cast<int>(data.size()/2)] +
		  data[static_cast<int>(data.size()/2) - 1]) /2.0;
      }
      else{
	median = data[static_cast<int>(data.size()/2)];
      }
      return median;
    }

    int XMLHandler::getPolCode(int band, int input1, int input2){
      std::string polarization = "";
      polarization += currentPolState_[band][input1];
      polarization += currentPolState_[band][input2];
      if(polarization == "RR")
	return -1;
      if(polarization == "LL")
	return -2;
      if(polarization == "RL")
	return -3;
      if(polarization == "LR")
	return -4;
      if(polarization == "XX")
	return -5;
      if(polarization == "YY")
	return -6;
      if(polarization == "XY")
	return -7;
      if(polarization == "YX")
	return -8;
      return 0;
    }

    int XMLHandler::getPolCode(std::string polarization){
      if(polarization == "RR")
	return -1;
      if(polarization == "LL")
	return -2;
      if(polarization == "RL")
	return -3;
      if(polarization == "LR")
	return -4;
      if(polarization == "XX")
	return -5;
      if(polarization == "YY")
	return -6;
      if(polarization == "XY")
	return -7;
      if(polarization == "YX")
	return -8;
      return 0;
    }

    int XMLHandler::getAntNumber(int band, int input){
      return currentNewCorrelatorInputMap_[band][input];
    }

    void XMLHandler::writeTsys(int ant1, std::string pol1, int ant2, std::string pol2,int nspect){
      std::vector<float> systemp;
      std::vector<float> wsystemp;
      std::vector<float> psys;
      std::vector<float> psysattn;
      std::vector<float> ambpsys;
      int nant = currentAntennas_.size();
      if(currentVersion_[0] == 2){
	if ((currentAntennas_.size() > 0) && (currentNewTsys_[pol1].size() > 0) && (currentNewTsys_[pol2].size() > 0)) {
	  int startIndex = 1;
	  int offset = 0;
	  switch (correlatorType_){
	  case carma::monitor::CorrelatorDesignationMonitorPointEnum::SPECTRAL :
	    startIndex = SlBandStart_;
	    offset = 0;
	    break;
	    
	  case carma::monitor::CorrelatorDesignationMonitorPointEnum::WIDEBAND :
	    startIndex = WbBandStart_;
	    offset = WbBandStart_ - 1;
	    break;
	  default :
	    break;
	  }
	  
	  for(int band = startIndex; band <= nspect + offset; band++){
	    for(int ant = 1; ant <= ant1; ant++){
	      if((band - startIndex + 1) <= nspect/2){
		systemp.push_back((currentNewTsys_[pol1][ant][band].LSB > 0.0) ? currentNewTsys_[pol1][ant][band].LSB : 10000.0);
		wsystemp.push_back((currentNewTsys_[pol1][ant][band].LSB > 0.0) ? currentNewTsys_[pol1][ant][band].LSB : 10000.0);
		psys.push_back(currentNewPsys_[pol1][ant][band]);
		psysattn.push_back(currentNewPsysAttn_[pol1][ant][band]);
		ambpsys.push_back(currentNewAmbPsys_[pol1][ant][band]);
	      }
	      else{
		systemp.push_back((currentNewTsys_[pol1][ant][band-(nspect/2)].USB > 0.0) ? currentNewTsys_[pol1][ant][band-(nspect/2)].USB : 10000.0);
		wsystemp.push_back((currentNewTsys_[pol1][ant][band-(nspect/2)].USB > 0.0) ? currentNewTsys_[pol1][ant][band-(nspect/2)].USB : 10000.0);
		psys.push_back(currentNewPsys_[pol1][ant][band-(nspect/2)]);
		psysattn.push_back(currentNewPsysAttn_[pol1][ant][band-(nspect/2)]);
		ambpsys.push_back(currentNewAmbPsys_[pol1][ant][band-(nspect/2)]);
	      }
	    }
	    for(int ant = ant1 + 1; ant <= nant; ant++){
	      if((band - startIndex + 1) <= nspect/2){
		systemp.push_back((currentNewTsys_[pol2][ant][band].LSB > 0.0) ? currentNewTsys_[pol2][ant][band].LSB : 10000.0);
		wsystemp.push_back((currentNewTsys_[pol2][ant][band].LSB > 0.0) ? currentNewTsys_[pol2][ant][band].LSB : 10000.0);
		psys.push_back(currentNewPsys_[pol2][ant][band]);
		psysattn.push_back(currentNewPsysAttn_[pol2][ant][band]);
		ambpsys.push_back(currentNewAmbPsys_[pol2][ant][band]);
	      }
	      else{
		systemp.push_back((currentNewTsys_[pol2][ant][band-(nspect/2)].USB > 0.0) ? currentNewTsys_[pol2][ant][band-(nspect/2)].USB : 10000.0);
		wsystemp.push_back((currentNewTsys_[pol2][ant][band-(nspect/2)].USB > 0.0) ? currentNewTsys_[pol2][ant][band-(nspect/2)].USB : 10000.0);
		psys.push_back(currentNewPsys_[pol2][ant][band-(nspect/2)]);
		psysattn.push_back(currentNewPsysAttn_[pol2][ant][band-(nspect/2)]);
		ambpsys.push_back(currentNewAmbPsys_[pol2][ant][band-(nspect/2)]);
	      }
	    }
	  }

	  muv_->uvputvrr("systemp", &systemp[0], nant*nspect);
	  muv_->uvputvrr("wsystemp", &wsystemp[0], nant*nspect);
	  if(maxRestfreq_ < 50.0){
	    muv_->uvputvrr("psys", &psys[0], nant*nspect);
	    muv_->uvputvrr("psysattn", &psysattn[0], nant*nspect);
	    muv_->uvputvrr("ambpsys", &ambpsys[0], nant*nspect);
	  }
	}
      }
      else{
	// Write Tsys values
	if ((currentAntennas_.size() > 0) && (currentTsys_.size() > 0)) {
	  int nant = currentAntennas_.size();
	  for (int spw=0; spw < nspect; spw++) {
	    int band = SpwBandRelationships::spwToBand(spw);
	    SpwBandRelationships::SIDEBAND sb = 
	      SpwBandRelationships::spwToSideband(spw);
	    for (int ant=0; ant < nant; ant++) {
	      int indx = 
		SpwBandRelationships::sysTempIndex(ant,nant,band,sb);
	      systemp.push_back(currentTsys_[indx]);
	      wsystemp.push_back(currentTsys_[indx]);
	    }
	  }
	  muv_->uvputvrr("systemp", &systemp[0], nant*nspect);
	  muv_->uvputvrr("wsystemp", &wsystemp[0], nant*nspect);
	}
	if(maxRestfreq_ < 50.0){

	  // Write Psys, AmbPsys, and PsysAttn values
	  if ((currentAntennas_.size() > 0) && (currentPsys_.size() > 0)) {
	    int nant = currentAntennas_.size();
	    for (int spw=0; spw < nspect; spw++) {
	      int band = SpwBandRelationships::spwToBand(spw);
	      for (int ant=0; ant < nant; ant++) {
		int indx = 
		  SpwBandRelationships::psysIndex(ant,nant,band);
		psys.push_back(currentPsys_[indx]);
		psysattn.push_back(currentPsysAttn_[indx]);
		ambpsys.push_back(currentAmbPsys_[indx]);
	      }
	    }
	  /*		    CARMA_CPTRACE(carma::util::Trace::TRACE6,"tsys "
	    << systemp[0] << " "
	    << wsystemp[0]);
	    CARMA_CPTRACE(carma::util::Trace::TRACE6,"psys "
	    << psys[0] << " "
	    << psysattn[0] << " "
	    << ambpsys[0]);*/
	    muv_->uvputvrr("psys", &psys[0], nant*nspect);
	    muv_->uvputvrr("psysattn", &psysattn[0], nant*nspect);
	    muv_->uvputvrr("ambpsys", &ambpsys[0], nant*nspect);
	  }
	}
      }
    }

    void XMLHandler::putChi(){
      double ha = currentLst_ - currentObsra_;
      double sinq = cos(currentLatitude_) * sin(ha);
      double cosq = sin(currentLatitude_) * cos(currentObsdec_) - cos(currentLatitude_) * sin(currentObsdec_) * cos(ha);
      float chi = atan2(sinq,cosq);
      muv_->uvputvrr("chi",chi+evector_);
      muv_->uvputvrr("evector",evector_);
    }
    
    /// If element is 'KW' find out what variable it's for and call the
    /// appropriate handler.
    /// Ignore otherwise.
    void XMLHandler::startElement(const XMLCh* const uri,
				  const XMLCh* const localname,
				  const XMLCh* const qname,
				  const Attributes& attrs) 
    {
      ostringstream oss4;
      oss4 << "STARTING " << qname;
      CARMA_CPTRACE(carma::util::Trace::TRACE4, oss4.str());

      if( XMLString::equals(INTEGRATION_, qname)) {
	recordLevel_++;
	recordNumber_ ++;
	// Compute start frame MJD for this integration
	XMLCh* frameCode = XMLString::transcode("startframe");
	carma::util::frameType startFrame = 
	  static_cast<carma::util::frameType>
	  (buildDouble(attrs.getValue(frameCode)));
	currentStartFrame_ = startFrame;
	CARMA_CPTRACE(carma::util::Trace::TRACE2,"SF " << startFrame);
	currentStartFrameMJD_ = carma::util::Time::MJD(startFrame);
	// Does current integration fall within selected frame range ?
	writeData_ = ((startFrame >= startFrame_) &&
		      (startFrame <= endFrame_));
	CARMA_CPTRACE(carma::util::Trace::TRACE6, 
		      "Processing integration starting at frame: "
		      << startFrame << ", write= " << writeData_);
	// Reset project data accumulator at start of integration
	if (trialProjectData_ != NULL) {
	  trialProjectData_->resetIntegrationData();
	}
	delete frameCode;
      } else {
	if( XMLString::equals(KW_, qname)) {
	  if (writeData_) {
	    // Write output science data
	    writeKeyword(attrs);
	    // Accumulate project data
	    if (trialProjectData_ != NULL) {
	      trialProjectData_->addAstroHdrElement(attrs);
	    }
	  }
	}
      }
    }

    std::string reversePol(int polcode, int place){

      if(place == 1){
	if(polcode == -1 || polcode == -3)
	  return "R";
	if(polcode == -2 || polcode == -4)
	  return "L";
	if(polcode == -5 || polcode == -7)
	  return "X";
	return "Y";
      }
      if(polcode == -1 || polcode == -4)
	return "R";
      if(polcode == -2 || polcode == -3)
	return "L";
      if(polcode == -5 || polcode == -8)
	return "X";
      return "Y";
    }

    // When the end of a 'INTEGRATION' element is seen, call uvwwrite and uvwrite.
    void XMLHandler::endElement(const XMLCh *const uri,
				const XMLCh *const localname,
				const XMLCh *const qname) 
    {
      ostringstream oss4;
      oss4 << "ENDING " << qname;
      CARMA_CPTRACE(carma::util::Trace::TRACE4, oss4.str());

      if(XMLString::equals(INTEGRATION_, qname)) {
	int writeCount = 0;
	recordLevel_--;

	// Skip if no valid correlator data associated with this integration.
	if ((currentCorrelatorData_ != NULL) && writeData_) {
	  // Extract visbrick data (AC & XC)
	  const std::string sortOrder = "acfirst";
	  int nspect, nchan, nwide, npol;
	  double time, freq;
	  std::vector<int> ischan, nschan;
	  std::vector<double> restfreq, sfreq, sdf;
	  std::vector<float> wfreq, wwidth;
	  std::map<int, std::map<int, std::vector<float> > > corr, wcorr;
	  std::map<int, std::map<int, std::vector<int> > > flags, wflags,flagReason;
	  std::map<int, std::map<int, std::vector<float> > > intTime;

	  for(int i = -1; i >= -8; i--){
	    std::map<int, std::vector<float> > temp1;
	    std::map<int,std::vector<int> > tempi;
	    corr[i] = temp1;
	    wcorr[i] = temp1;
	    flags[i] = tempi;
	    wflags[i] = tempi;
	    intTime[i] = temp1;
	    flagReason[i] = tempi;
	  }

	  // handle the new monitor point structure
	  if(currentVersion_[0] == 2){
	    currentNewCorrelatorInputMap_ = reconfigureMap(tempCorrInputMap_);
	    currentPolState_ = reconfigureMap(tempPolState_);
	    reconfigureMultiMap(reconfigureMap(tempPsysMap_), currentNewPsys_);
	    if(currentVersion_[1] == 1){
	      reconfigureAntTsys(tempTsysMap_, currentNewTsys_);
	      reconfigureAntAmbPsys(tempAmbPsysMap_, currentNewAmbPsys_);
	    }
	    else{
	      reconfigureMultiMap(reconfigureTsysMap(tempTsysMap_), currentNewTsys_);
	      reconfigureMultiMap(reconfigureMap(tempAmbPsysMap_), currentNewAmbPsys_);
	    }
	    reconfigureMultiMap(reconfigureMap(tempPsysAttnMap_), currentNewPsysAttn_);
	  }

	  unpackCorrelatorData(currentCorrelatorData_, corrType_, sortOrder,
			       currentCorrelatorInputMap_, 
			       nspect, nchan, ischan, 
			       nschan, restfreq, sfreq, sdf, freq, nwide, 
			       wfreq, wwidth, time, 
			       corr, flags, wcorr, wflags, intTime, flagReason);
	  bool isData = true;
	  float lastIntTime = -1.0;
	  //	  npol = 1;
	  if(isData){
	    // Write MIRIAD coordinates for this integration
	    /*	    CARMA_CPTRACE(carma::util::Trace::TRACE6,"stuff "
			  << nspect << " "
			  << nchan << " "
			  << npol << " "
			  << nschan[0] << " "
			  << ischan[0] << " "
			  << sfreq[0] << " "
			  << freq << " "
			  << sdf[0] << " "
			  << nwide << " "
			  << wfreq[0] << " "
			  << wwidth[0]);*/
	    muv_->uvputvri("nspect",   nspect);
	    muv_->uvputvri("nchan",    nchan);
	    //	    muv_->uvputvri("npol",     npol);
	    muv_->uvputvri("nschan",  &nschan[0],   nspect);
	    muv_->uvputvri("ischan",  &ischan[0],   nspect);
	    muv_->uvputvrd("sfreq",   &sfreq[0],    nspect);
	    muv_->uvputvrd("freq",     freq);
	    muv_->uvputvrd("sdf",     &sdf[0],      nspect);
	    muv_->uvputvrd("restfreq",&restfreq[0], nspect);
	    muv_->uvputvri("nwide",    nwide);
	    muv_->uvputvrr("wfreq",   &wfreq[0],    nwide);
	    muv_->uvputvrr("wwidth",  &wwidth[0],   nwide);
	    putChi();
	    // Update correlator frequency setup project data
	    if (trialProjectData_ != NULL) {
	      trialProjectData_->addFreqInfo(sfreq, sdf, nschan);
	      // Process project data
	      trialProjectData_->processIntegration();
	    }
	    // LOOP OVER AC BASELINES THEN CC BASELINES
	    int currentNpol = 0;
	    npol = 0;
	    std::vector<int> baselineOrder;
	    if ((sortOrder.find("ACFIRST") != std::string::npos) ||
		(sortOrder.find("acfirst") != std::string::npos)) {
	      // put autocorrelations first
	      for(int i = 1; i <= maxAntIndex_; i++){
		baselineOrder.push_back(256 * i + i);
	      }
	      for(int i = 1; i <= maxAntIndex_; i++){
	        for(int j = i+1; j <= maxAntIndex_; j++){
		  baselineOrder.push_back(256 * i + j);
		}
	      }
	    }
	    else{
	      for(int i = 1; i <= maxAntIndex_; i++){
	        for(int j = i; j <= maxAntIndex_; j++){
		  baselineOrder.push_back(256 * i + j);
		}
	      }
	    }

	    bool first = true;

	    for(unsigned int bl = 0; bl <= baselineOrder.size(); bl++){
	      npol = 0;
	      
	      //CARMA_CPTRACE(carma::util::Trace::TRACE7,"BLO " << baselineOrder.size());
	      for(int polarization = -1; polarization >= -8; polarization--){
		if(corr[polarization].size() > 0 && corr[polarization].count(baselineOrder[bl]) != 0)
		  npol++;
	      }
	      if(npol != currentNpol){
		muv_->uvputvri("npol",     npol);
		currentNpol = npol;
		//CARMA_CPTRACE(carma::util::Trace::TRACE7,"NPOL " << npol);
	      }
	      //CARMA_CPTRACE(carma::util::Trace::TRACE7,"NPOL " << npol);
	      for(int polarization = -1; polarization >= -8; polarization--){
		//ostringstream oss3;
		//for(std::map<int,std::vector<float> >::iterator iter = corr[polarization].begin(); iter != corr[polarization].end(); ++iter)
		//  {
		//    oss3 << iter->first << "  ";
		//  }
		//CARMA_CPTRACE(carma::util::Trace::TRACE3,"WBD1 " << oss3.str());
		if(corr[polarization].size() > 0 && corr[polarization].count(baselineOrder[bl]) != 0){
		  muv_->uvputvri("pol", polarization);
		  int ant1 = baselineOrder[bl]/256;
		  int ant2 = baselineOrder[bl] - (ant1 * 256);
		  if(first || (npol > 1) || (npol == 1 && (polarization == -3 || polarization == -4))){
		    if(npol < 2)
		      first = false;
		    std::string pol1 = "L";
		    std::string pol2 = "L";
		    if(polarization == -1){
		      pol1 = "R";
		      pol2 = "R";
		    }
		    else if(polarization == -3){
		      pol1 = "R";
		    }
		    else if(polarization == -4){
		      pol2 = "R";
		    }
		    else if(polarization == -5){
		      pol1 = "X";
		      pol2 = "X";
		    }
		    else if(polarization == -6){
		      pol1 = "Y";
		      pol2 = "Y";
		    }
		    else if(polarization == -7){
		      pol1 = "X";
		      pol2 = "Y";
		    }
		    else if(polarization == -8){
		      pol1 = "Y";
		      pol2 = "X";
		    }
		    writeTsys(ant1,pol1,ant2,pol2,nspect);
		  }
		  // write xtsys and ytsys, but only if there is more than 1 polarization
		  if(npol > 1 && first){
		    first = false;
		    std::vector<float> xtsys;
		    std::vector<float> ytsys;
		    int nant = currentAntennas_.size();
		    if(currentVersion_[0] == 2){
		      if ((currentAntennas_.size() > 0) && (currentNewTsys_["R"].size() > 0) && (currentNewTsys_["L"].size() > 0)) {
			int startIndex = 1;
			int offset = 0;
			switch (correlatorType_){
			case carma::monitor::CorrelatorDesignationMonitorPointEnum::SPECTRAL :
			  startIndex = SlBandStart_;
			  offset = 0;
			  break;
			  
			case carma::monitor::CorrelatorDesignationMonitorPointEnum::WIDEBAND :
			  startIndex = WbBandStart_;
			  offset = WbBandStart_ - 1;
			  break;
			default :
			  break;
			}
	  
			for(int band = startIndex; band <= nspect + offset; band++){
			  for(int ant = 1; ant <= nant; ant++){
			    if((band - startIndex + 1) <= nspect/2){
			      xtsys.push_back((currentNewTsys_["R"][ant][band].LSB > 0.0) ? currentNewTsys_["R"][ant][band].LSB : 10000.0);
			      ytsys.push_back((currentNewTsys_["L"][ant][band].LSB > 0.0) ? currentNewTsys_["L"][ant][band].LSB : 10000.0);
			    }
			    else{
			      xtsys.push_back((currentNewTsys_["R"][ant][band-(nspect/2)].USB > 0.0) ? currentNewTsys_["R"][ant][band-(nspect/2)].USB : 10000.0);
			      ytsys.push_back((currentNewTsys_["L"][ant][band-(nspect/2)].USB > 0.0) ? currentNewTsys_["L"][ant][band-(nspect/2)].USB : 10000.0);
			    }
			  }
			}
			muv_->uvputvrr("xtsys", &xtsys[0], nant*nspect);
			muv_->uvputvrr("ytsys", &ytsys[0], nant*nspect);
		      }
		    }
		  }
		  // Assemble preamble coordinates (u,v,w,time,baseline)
		  string::size_type jUVW1 = 3 * (ant1 - 1);
		  string::size_type jUVW2 = 3 * (ant2 - 1);
		  if ((jUVW1 < currentUVW_.size()) && (jUVW2 < currentUVW_.size())) {
		    preamble_[0] = currentUVW_[jUVW2] - currentUVW_[jUVW1];
		    preamble_[1] = currentUVW_[jUVW2+1] - currentUVW_[jUVW1+1];
		    preamble_[2] = currentUVW_[jUVW2+2] - currentUVW_[jUVW1+2];
		  } else {
		    // @todo  should this just never happen, and if so, throw exception? [PJT]
		    preamble_[0] = 0;
		    preamble_[1] = 0;
		    preamble_[2] = 0;
		  }

		  preamble_[3] = time;
		  preamble_[4] = baselineOrder[bl];
		  // Write telescope type variable
		  std::string telescopeType;
		  if (currentAntennas_.size() > 0) {

		    int type1 = currentAntennas_[ant1-1];
		    int type2 = currentAntennas_[ant2-1];
		    //		    ostringstream oss;
		    //for(unsigned int g = 0; g < currentAntennas_.size(); g++){
		    //  oss << currentAntennas_[g] << "  ";
		    //}
		  //CARMA_CPTRACE(carma::util::Trace::TRACE4,"TELESCOPE " << type1 << "  " << type2 << " -- " << ant1 << "  " << ant2);
		    if ((type1 == 1) && (type2 == 1)) {
		      telescopeType = "OVRO";  //10m-10m
		    } else if ((type1 == 2) && (type2 == 2)) {
		      telescopeType = "BIMA";  //6m-6m
		    } else if (((type1 == 1) && (type2 == 2)) ||
			       ((type1 == 2) && (type2 == 1))){
		      telescopeType = "CARMA";  // 10m-6m
		    } else if ((type1 == 3) && (type2 == 3)){
		      telescopeType = "SZA";  //SZA-SZA
		    } else if (((type1 == 1) && (type2 == 3)) ||
			       ((type1 == 3) && (type2 == 1))){
		      telescopeType = "SZA10";  //SZA-10m
		    } else if (((type1 == 2) && (type2 == 3)) ||
			       ((type1 == 3) && (type2 == 2))){
		      telescopeType = "SZA6";  //SZA-6m
		    }
		    muv_->uvputvra("telescop", telescopeType.c_str());
		    // Write baseline-based Jy/K value
		    float jyperk1 = 0;
		    float jyperk2 = 0;
		    // Extract anntena-based Jy/K values
		    if ((ant1-1) < static_cast<int>(currentJyperka_.size())) {
		      jyperk1 = currentJyperka_[ant1-1];
		    }
		    if ((ant2-1) < static_cast<int>(currentJyperka_.size())) {
		      jyperk2 = currentJyperka_[ant2-1];
		    }
		    if (jyperk1*jyperk2 == 0) {
		      jyperk1 = std::max(jyperk1, jyperk2);
		      jyperk2 = std::max(jyperk1, jyperk2);
		    }
		    // Compute baseline-based Jy/K value
		    float jyperk = std::sqrt(jyperk1 * jyperk2);
		    muv_->uvputvrr("jyperk", &jyperk, 1);
		    float tempItime = calculateMedian(intTime[polarization][baselineOrder[bl]]);
		    if(tempItime != lastIntTime){
		      muv_->uvputvrr("inttime",tempItime);
		      lastIntTime = tempItime;
		    }
		    muv_->uvputvri("bfmask",&flagReason[polarization][baselineOrder[bl]][0],nspect);
		    // Write spectral-line and wideband visibility spectra
		    // note uvwrite() closes the record in the UV data stream
		    int ncitems = nchan;
		    //CARMA_CPTRACE(carma::util::Trace::TRACE2,"NC " << nchan << "  " << nwide);
		    //		    ostringstream oss2;
		    //for(std::map<int,std::vector<float> >::iterator iter = corr[polarization].begin(); iter != corr[polarization].end(); ++iter)
		    //{
			//			CARMA_CPTRACE(carma::util::Trace::TRACE4,"WB " << iter->first);
			//oss2 << iter->first << "  ";
			//ignore value
			//Value v = iter->second;
		    //}
		    muv_->uvwwrite(&wcorr[polarization][baselineOrder[bl]][0], &wflags[polarization][baselineOrder[bl]][0], nwide);
		    muv_->uvwrite(preamble_, &corr[polarization][baselineOrder[bl]][0], &flags[polarization][baselineOrder[bl]][0], 
				  ncitems);
		    writeCount++;
		    // Update record of starting frame of last integration processed
		    lastFrame_ = currentStartFrame_;
		  }
		} // if (nbasl > 0)
	      }
	    }
	  }
	} // if (currentCorrelatorData_ != NULL)
	//Program::getLogger() << log4cpp::Priority::INFO << "NUM WRITE " << writeCount;
      } // if(XMLString::equals(INTEGRATION_, qname))
      return;
    }

    void XMLHandler::resetDocument()
    {
      inCDATA_ = false;
      nkws_ = 0;
      nunkws_ = 0;
      recordNumber_ = 0;
      recordLevel_ = 0;
    }

    void XMLHandler::startCDATA()
    {
      inCDATA_ = true;
    }

    void XMLHandler::endCDATA()
    {
      inCDATA_ = false;
    }


    // ---------------------------------------------------------------------------
    //  XMLHandler: Overrides of the SAX ErrorHandler interface
    // ---------------------------------------------------------------------------
    void XMLHandler::error(const SAXParseException& e)
    {
      cerr << "\nError at file " << e.getSystemId()
	   << ", line " << e.getLineNumber()
	   << ", char " << e.getColumnNumber()
	   << "\n  Message: " << e.getMessage() <<  endl;
    }

    void XMLHandler::fatalError(const SAXParseException& e)
    {
      cerr << "\nFatal Error at file " << e.getSystemId()
	   << ", line " << e.getLineNumber()
	   << ", char " << e.getColumnNumber()
	   << "\n  Message: " << e.getMessage() << endl;
    }

    void XMLHandler::warning(const SAXParseException& e)
    {
      cerr << "\nWarning at file " << e.getSystemId()
	   << ", line " << e.getLineNumber()
	   << ", char " << e.getColumnNumber()
	   << "\n  Message: " << e.getMessage() << endl;
    }

    void XMLHandler::resetErrors()
    {
    }

    /////////////////////////////////////////////////////////////////////////////

    void XMLHandler::openMiriad(const std::string& inputAstroHdrFile,
				const char *filename, const std::string& mode,
				bool writeFloats, bool justGatherPdb)
    {
      //CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX1");
      // Save input astro header file name
      inputAstroHdrFile_ = inputAstroHdrFile;
      //CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX2");
     // reset the band accounting
      currentMaxBand_ = 0;
      //CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX3");
      if(justGatherPdb){
	//CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX4");
	muv_->setGather(true);
      }
      //CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX5");
      // Open miriad output file
      Program::getLogger() << log4cpp::Priority::INFO << "OPEN";
      muv_->uvopen(filename, mode.c_str());
      Program::getLogger() << log4cpp::Priority::INFO << "OPEN-DONE";
      //CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX6");

      // Write reals instead of scaled integers if requested.
      if ( writeFloats )
	muv_->uvset("corr","r",0,0.0,0.0,0.0);
      //CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX7");

      Program::getLogger() << log4cpp::Priority::INFO << "PRE";
      muv_->uvset("preamble","uvw/time/baseline",0,0.0,0.0,0.0);
      //CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX8");
      Program::getLogger() << log4cpp::Priority::INFO << "PRE-DONE";
      std::string hismode = mode;
      bool doMask = false;
      if (mode.find("new") != std::string::npos) {
	hismode = "write";
	doMask = true;
      } else if (mode.find("old") != std::string::npos) {
	hismode = "read";
      } else if (mode.find("append") != std::string::npos) {
	hismode = "append";
      };
      //CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX9");
      Program::getLogger() << log4cpp::Priority::INFO << "HISTORY";
      muv_->hisopen(hismode.c_str());
      Program::getLogger() << log4cpp::Priority::INFO << "HISTORY-DONE";
      if(doMask){
	//CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX10");
	int bfhandle;
	int iostat;
	Program::getLogger() << log4cpp::Priority::INFO << "MASK";
	muv_->haccess(bfhandle,"blfmask",hismode.c_str(),iostat);
	Program::getLogger() << log4cpp::Priority::INFO << "MASK-DONE";
	for(unsigned int i = 0; i < 32; i++){   // 32=sizeof(int)
	  muv_->hwritea(bfhandle,bfReasons[i].c_str(),
			bfReasons[i].size()+1,iostat); 
	}
	Program::getLogger() << log4cpp::Priority::INFO << "HDA";
	muv_->hdaccess(bfhandle,iostat);
      }
      Program::getLogger() << log4cpp::Priority::INFO << "DONE";
      //CARMA_CPTRACE(carma::util::Trace::TRACE2,"XX11");

      dataNotPresentCount_ = 0;  // Reset warning count.
      dataNotPresentFile_ = "";  // & associated filename.
    }

    void XMLHandler::closeMiriad()
    {
      // Close miriad output file
      Program::getLogger() << log4cpp::Priority::INFO << "CLOSEH";
      muv_->hisclose();
      Program::getLogger() << log4cpp::Priority::INFO << "CLOSEU";
      muv_->uvclose();
      Program::getLogger() << log4cpp::Priority::INFO << "GATHER";
      muv_->setGather(false);

      // Save in case it's needed.
      dataNotPresentFile_ = inputAstroHdrFile_;
      // Reset input astro header file name
      inputAstroHdrFile_ = "";
    }

    void XMLHandler::printVisBrickWarnings(bool isIncremental)
    {
      // Don't print if incremental fill or there were no warnings.
      if(!isIncremental && (dataNotPresentCount_ > 0)) {
	std::ostringstream wmsg;
	wmsg << "No matching visbrick records found for "
	     << dataNotPresentCount_ << " integrations in "
	     << dataNotPresentFile_;
	Program::getLogger() << log4cpp::Priority::WARN << wmsg.str();
	CARMA_CPTRACE(carma::util::Trace::TRACE3, wmsg.str());
      }
    }

    void XMLHandler::configure(const std::string& visBrickDir,
			       const std::string& corrType)
    {
      // Pass SDP configuration parameters to the XML handler.

      // Set visbrick directory
      visBrickDir_ = carma::util::StringUtils::trimWhiteSpace(visBrickDir);

      // Set correlation type selection
      corrType_ = corrType;

      return;
    }

    void XMLHandler::selectFrameRange(const carma::util::frameType& startFrame,
				      const carma::util::frameType& endFrame)
    {
      // Set frame range selection.

      // Set start and end frame count selection
      startFrame_ = startFrame;
      endFrame_ = endFrame;

      return;
    }

    int XMLHandler::frameOffset(const double& frameMJD)
    {
      // Return frame offset between visbrick and monitor stream

      // Initialization
      int retval = 0;

      // Pre 25 Oct 2006 (Original correlator code: ~3 offset).
      if (frameMJD < carma::util::Time::MJD(430227960)) {
	retval = 3;
      } else if (frameMJD < carma::util::Time::MJD(502977600)) {
	// Post 25 Oct 2006
	retval = 2; //(Correlator code rework (Oct '06, AB): ~ 2 frame offset.
      } else {
	// As of "10:00 AM PST on Thurs" (502977600) new offset: ~ 0.
	retval = 0;
      }
      return retval;
    }

    carma::util::frameType XMLHandler::getLastFrame()
    {
      // Return last integration start frame processed.

      return lastFrame_;
    }

    void XMLHandler::resetProjectData()
    {
      // Reset the accumulated project data.

      if (trialProjectData_ != NULL) delete(trialProjectData_);
      trialProjectData_ = new 
	TrialProjectData(SDPUtil::extractObsBlockId(inputAstroHdrFile_));
      return;
    }

    void XMLHandler::addScript(std::string miriadFile){
      //CARMA_CPTRACE(carma::util::Trace::TRACE4, "Add " << miriadFile);
      if(trialProjectData_ == NULL){
	trialProjectData_ = new 
	TrialProjectData(SDPUtil::extractObsBlockId(inputAstroHdrFile_));
      }
      //CARMA_CPTRACE(carma::util::Trace::TRACE4, "TPD");
      trialProjectData_ -> addScriptToMiriad(miriadFile);
      //CARMA_CPTRACE(carma::util::Trace::TRACE4, "FINISH");
    }

    void XMLHandler::updateProjectDatabase()
    {
      // Update the project database.

      if (trialProjectData_ != NULL) {
	bool error = false;
	std::string errMsg = "Failed to update project database: ";
	try {
	  trialProjectData_->updateProjectDatabase();
	} catch ( const carma::util::ErrorException& exc) {
	  error = true;
	  errMsg += exc.getErrorMessage();
	} catch ( const carma::observertools::ProjectDatabaseException& exc) {
	  error = true;
	  errMsg += exc.errorMsg;
	} catch ( const CORBA::SystemException& exc) {
	  error = true;
	  errMsg += exc._name();
	} catch ( const carma::util::UserException& exc) {
	  error = true;
	  errMsg += exc.errorMsg;
	} catch (const std::exception& exc) {
	  error = true;
	  errMsg += exc.what();
	} catch (...) {
	  error = true;
	  errMsg += "Unidentified exception";
	}
	// Re-throw if project database update failed
	if (error) {
	  Program::getLogger() << log4cpp::Priority::ERROR << errMsg;
	  throw CARMA_EXCEPTION(carma::util::ErrorException, errMsg);
	}
      }
      return;
    }
  
    ////////////////////////////////////////////////////////////////
    //		keyword handler callbacks.

    // Keyword isn't in keyword list.
    // Fake a keyword entry for it and try to figure out its datatype.
    // If we can, then go ahead and write it.
    void XMLHandler::putUnknown(XMLHandler *h, const XMLCh *xmlname,
				const Keyword *unkkw,
				const XMLCh *xmltype, int length,
				const XMLCh *value)
    {
      ostringstream msg;
      char *name = XMLString::transcode(xmlname);
      char *type = XMLString::transcode(xmltype);
      Keyword kw = {name, type, 0, "", ""};
      const char *notw="";

      if(strcmp(type, "a") == 0)
	putA(h, xmlname, &kw, xmltype, length, value);
      else
	if(strcmp(type, "i") == 0)
	  putIv(h, xmlname, &kw, xmltype, length, value);
	else
	  if(strcmp(type, "r") == 0)
	    putRv(h, xmlname, &kw, xmltype, length, value);
	  else
	    if(strcmp(type, "d") == 0)
	      putDv(h, xmlname, &kw, xmltype, length, value);
	    else
	      if(strcmp(type, "c") == 0)
		putCv(h, xmlname, &kw, xmltype, length, value);
	      else
		notw = " not";
      
      msg << "Unknown kw: name = " << name << " type = " << type
	  << " length = " << length;
      if(length <= 2)
	msg << " " << value;
      msg << notw << " written to visdata.";

      // msg << endl;  // (endl isn't needed).
      
      // Write unknown keywords to the history file. (Most could be
      // written to the dataset).
      h->muv_->hiswrite(msg.str().c_str());
      
      XMLString::release(&name);
      XMLString::release(&type);
    }

    void XMLHandler::putNothing(XMLHandler *h, const XMLCh *name,
				const Keyword *kw,
				const XMLCh *type, int length,
				const XMLCh *value)
    {
      cout << "Ignored kw:" << name << endl;
    }

    // String valued keywords.
    void XMLHandler::putA(XMLHandler *h, const XMLCh *name,
			  const Keyword *kw,
			  const XMLCh *type, int length,
			  const XMLCh *value)
    {

      char *sname =  XMLString::transcode(name);
      char *svalue = XMLString::transcode(value);
      h->muv_->uvputvra(sname, svalue);
      XMLString::release(&sname);
      XMLString::release(&svalue);
    }

    // Complex vector valued keywords.
    void XMLHandler::putCv(XMLHandler *h, const XMLCh *name,
			   const Keyword *kw,
			   const XMLCh *type, int length,
			   const XMLCh *value)
    {
      int nitems=0, ncitems;
      float *cv = buildFloatv(value, nitems);
      ncitems = nitems/2;	// Complex
      h->muv_->uvputvrc(kw->name, cv, ncitems);
      delete [] cv;
    }

    //using namespace xercesc;

    // Keyword with a singe double value.
    void XMLHandler::putD(XMLHandler *h, const XMLCh *name,
			  const Keyword *kw,
			  const XMLCh *type, int length,
			  const XMLCh *value)
    {
      double dv = buildDouble(value);
      h->muv_->uvputvrd(kw->name, dv);
      if(strcmp(kw->name,"latitud") == 0){
	currentLatitude_ = dv;
      }
      else if(strcmp(kw->name,"obsdec") == 0){
	currentObsdec_ = dv;
      }
      else if(strcmp(kw->name,"obsra") == 0){
	currentObsra_ = dv;
      }
    }

    void XMLHandler::putDv(XMLHandler *h, const XMLCh *name,
			   const Keyword *kw,
			   const XMLCh *type, int length,
			   const XMLCh *value)
    {
      int nitems=0;
      double *dv = buildDoublev(value, nitems);
      h->muv_->uvputvrd(kw->name, dv, nitems);
      delete [] dv;
    }

    void XMLHandler::putI(XMLHandler *h, const XMLCh *name,
			  const Keyword *kw,
			  const XMLCh *type, int length,
			  const XMLCh *value)
    {
      int iv = buildInt(value);
      h->muv_->uvputvri(kw->name, iv);
    }

    void XMLHandler::putIv(XMLHandler *h, const XMLCh *name,
			   const Keyword *kw,
			   const XMLCh *type, int length,
			   const XMLCh *value)
    {
      int nitems=0;
      int *iv = buildIntv(value, nitems);
      h->muv_->uvputvri(kw->name, iv, nitems);
      delete [] iv;
    }

    void XMLHandler::putR(XMLHandler *h, const XMLCh *name,
			  const Keyword *kw,
			  const XMLCh *type, int length,
			  const XMLCh *value)
    {
      float rv = buildFloat(value);
      h->muv_->uvputvrr(kw->name, rv);
    }

    void XMLHandler::putRv(XMLHandler *h, const XMLCh *name,
			   const Keyword *kw,
			   const XMLCh *type, int length,
			   const XMLCh *value)
    {
      int nitems=0;
      float *rv = buildFloatv(value, nitems);
      h->muv_->uvputvrr(kw->name, rv, nitems);
      delete [] rv;
    }

    ///  Some keywords are handled specially.

    // "coord" keyword (for uvwrite preamble).
    void XMLHandler::putCoord(XMLHandler *h, const XMLCh *, const Keyword *kw,
			      const XMLCh *, int length, const XMLCh *value)
    { 
      int len=2;
      buildDoublev(value, len, &h->preamble_[0]);
      h->correlationData_[1].bits_ |= COORD;
    }

    // time - (for uvwrite preamble).
    void XMLHandler::putTime(XMLHandler *h, const XMLCh *name, const Keyword *kw,
			     const XMLCh *, int length, const XMLCh *value)
    {
      h->preamble_[2] = buildDouble(value);
      h->correlationData_[1].bits_ |= TIME;
    }

    // used by lst,ut - in radians - that need 1/2 inttime correction
    void XMLHandler::putDTime(XMLHandler *h, const XMLCh *name,
			      const Keyword *kw,
			      const XMLCh *type, int length,
			      const XMLCh *value)
    {
      double dv = buildDouble(value);
      // correct by 1/2 inttime (in radians), and keep it in 0..2pi bounds
      // if (currentIntTime_ == 0) throw a_tantrum;
      dv += currentIntTime_*M_PI/86400.0;
      if (dv > 2.0*M_PI) dv -= 2.0*M_PI;
      if(strcmp(kw->name,"lst") == 0){
	currentLst_ = dv;
      }
      h->muv_->uvputvrd(kw->name, dv);
    }


    // Baseline - (for uvwrite preamble).
    void XMLHandler::putBaseline(XMLHandler *h, const XMLCh *name, const Keyword *kw,
				 const XMLCh *, int length, const XMLCh *value)
    {
      h->preamble_[3] = buildDouble(value);
      h->correlationData_[1].bits_ |= BASELINE;
    }

    // Store correlation or wideband correlation data.
    void XMLHandler::putCorr(XMLHandler *h, const XMLCh *name, const Keyword *kw,
			     const XMLCh *, int length, const XMLCh *value)
    {
      CorrelationData *cdp;

      if(strcmp(kw->name, "corr")==0)
	cdp = &h->correlationData_[1];
      else
	if(strcmp(kw->name, "wcorr")==0)
	  cdp = &h->correlationData_[0];
	else {
	  cout << "Unknown correlation kw: " << name << endl;
	  return;
	}
      cdp->data_ = buildFloatv(value, cdp->ndata_, cdp->data_);
      cdp->bits_ |= DATA;
    }

    // Flags - for uvwrite or uvwwrite.
    void XMLHandler::putFlags(XMLHandler *h, const XMLCh *name, const Keyword *kw,
			      const XMLCh *, int length, const XMLCh *value)
    {
      CorrelationData *cdp;

      if(strcmp(kw->name, "*flags")==0)
	cdp = &h->correlationData_[1];
      else
	if(strcmp(kw->name, "*wflags")==0)
	  cdp = &h->correlationData_[0];
	else {
	  cout << "Unknown correlation flags kw: " << name << endl;
	  return;
	}

      // Don't use cdp->ndata_ directly since there 1/2 as many flags as
      // complex data elements and buildIntv will overwrite.
      int nflags= cdp->ndata_;
      cdp->flags_ = buildIntv(value, nflags, cdp->flags_);
      cdp->bits_ |= FLAGS;
    }

    void XMLHandler::storeCorrelatorData(XMLHandler *h, const XMLCh *name, 
					 const Keyword *kw, const XMLCh *, 
					 int length, const XMLCh *value)
    {
      // Store correlator data from the current integration
  
      // Extract visbrick file name from the XML element
      std::string vfile 
	= carma::util::StringUtils::trimWhiteSpace(XMLString::transcode(value));
      // Check visbrick file name
      if (!vfile.empty()) {
	// Drop .write suffix as needed
	std::string::size_type widx = vfile.find(".write");
	if ((widx != std::string::npos) && 
	    !carma::util::FileUtils::exists(vfile)) {
	  vfile = vfile.substr(0, widx);
	}
      } else {
	// Use the default visbrick file name if the astroheader 
	// visbrick file name is null.
	vfile = visBrickDir_.empty() ? "visBrickData" : visBrickDir_ + 
	  "/visBrickData";
      }
      
      // If visbrick file not found in specified path, then search
      // in the default visbrick directory
      if (!carma::util::FileUtils::exists(vfile)) {
	std::string::size_type pathidx = vfile.find_last_of("/");
	if (pathidx != std::string::npos) {
	  std::string vbFileName = vfile.substr(pathidx);
	  std::string::size_type dirsiz = visBrickDir_.size();
	  if (dirsiz > 0) {
	    std::string::size_type diridx = 
	      (visBrickDir_.substr(dirsiz-1, 1) == "/") ? dirsiz - 1 : dirsiz;
	    vfile = visBrickDir_.substr(0, diridx) + vbFileName;
	  }
	}
      }
      CARMA_CPTRACE(carma::util::Trace::TRACE6, "Processing visbrick: " 
		    << vfile);

      bool error = !carma::util::FileUtils::exists(vfile);
      string error_what;
      if (error)
	error_what = "File not found: " + vfile;
      else
	error_what = "--No error--";

      bool warning = false;
      int nAdvance = 0;
      int frameDiff = 0;
      double recordMJD = 0;
      if (!error) {
	try {
	  // Check if this matches the current visbrick file being read,
	  // else update the visbrick reader.
	  if (vfile != currentVisBrickFile_) {
	    // Re-initialize the visbrick reader
	    currentVisBrickFile_ = vfile;
	    try {
	      if (currentVisBrickReader_ != NULL) delete currentVisBrickReader_;
	    } catch (...) {
	    }
	    currentVisBrickReader_ = new CorrelatorVisBrickReader(vfile);
	    if (currentCorrelatorData_ != NULL) {
	      delete currentCorrelatorData_;
	      currentCorrelatorData_ = NULL;
	    }
	  }
	  // Locate the visbrick record for the current integration
	  if (currentCorrelatorData_ == NULL) {
	    currentCorrelatorData_ = new 
	      CorrelatorData(currentVisBrickReader_->readOne());
	  }
	  recordMJD = currentCorrelatorData_->getHeader().getMJD();
	  visbrickVersionNumber_ = currentCorrelatorData_->getHeader().getVersionNumber();
	  CARMA_CPTRACE(carma::util::Trace::TRACE4,"Current visbrick version number " << visbrickVersionNumber_);
	  // Locate the correlator data record in the visbrick file
	  CARMA_CPTRACE(carma::util::Trace::TRACE6,
			"Current start frame time: " << currentStartFrameMJD_);
	  nAdvance = 0;
	  frameDiff = (int)(std::floor((86400.0 * (currentStartFrameMJD_ - 
					   recordMJD) / 0.5 + 0.5)));
	  while (frameDiff > XMLHandler::frameOffset(currentStartFrameMJD_)) {
	    if (currentCorrelatorData_ != NULL) {
	      delete currentCorrelatorData_;
	      currentCorrelatorData_ = NULL;
	    }
	    currentCorrelatorData_ = new 
	      CorrelatorData(currentVisBrickReader_->readOne());
	    nAdvance++;
	    recordMJD = currentCorrelatorData_->getHeader().getMJD();
	    frameDiff = (int)(std::floor((86400.0 * (currentStartFrameMJD_ - 
					     recordMJD) / 0.5 + 0.5)));
	  }
	} catch (const carma::util::EOFException) {
	  // An end of file exception means that the file is still being
	  // appended to and this time doesn't exist. It will be
	  // handled below.
	  CARMA_CPTRACE(carma::util::Trace::TRACE6, "EOF detected");
	} catch (const carma::util::NotFoundException& exc) {
	  error = true;
	  error_what = exc.what();
	} catch (const std::exception& exc) {
	  error = true;
	  error_what = exc.what();
	} catch (...) {
	  error = true;
	  error_what = ::carma::util::getStringForCaught();
	}
      }

      if (std::abs(frameDiff) > XMLHandler::frameOffset(currentStartFrameMJD_)) {
	warning = true;
	// No need to print each warning.
	CARMA_CPTRACE(carma::util::Trace::TRACE6, "MISSING DATA: " << std::abs(frameDiff) << "---" << XMLHandler::frameOffset(currentStartFrameMJD_) <<"---" <<currentStartFrameMJD_);
	dataNotPresentCount_++; // Only type of warning for now.
      }

      if (error || warning) {
	// Correlator data not found; reset current visbrick file
	currentVisBrickFile_ = "";
	try {
	  if (currentVisBrickReader_ != NULL) delete currentVisBrickReader_;
	} catch (...) {
	}
	currentVisBrickReader_ = NULL;
	try {
	  if (currentCorrelatorData_ != NULL) delete currentCorrelatorData_;
	} catch (...) {
	}
	currentCorrelatorData_ = NULL;
	if(error) {
	  std::ostringstream emsg;
	  emsg << "Matching visibility data not found in visbrick file: "
	       << vfile
	       << " at frame: " << currentStartFrame_
	       << " Reason: " << error_what;
	  Program::getLogger() << log4cpp::Priority::ERROR << emsg.str();
	  CARMA_CPTRACE(carma::util::Trace::TRACE6, emsg.str());
	}
      } else {
	CARMA_CPTRACE(carma::util::Trace::TRACE6, "Advanced " << nAdvance
		      << " records in visbrick file");
    /* too verbose!
	std::ostringstream msg;
	msg << "Matched header " 
	    << carma::util::Time::computeClosestFrame(recordMJD)
	    << " for MP frame " 
	    << carma::util::Time::computeClosestFrame(currentStartFrameMJD_);
	Program::getLogger() << log4cpp::Priority::INFO << msg.str();
    */
      }
      return;
    }

    void XMLHandler::storeCorbit(XMLHandler *h, const XMLCh *name, 
				 const Keyword *kw, const XMLCh *, 
				 int length, const XMLCh *value)
    {
      int nitems = 0;
      int* iv = buildIntv(value, nitems);
      int* refv = iv;
      currentCorbit_.resize(nitems*2,0);
      for (int j=0; j < nitems; j++) {
	currentCorbit_[j] = *iv++;
	currentCorbit_[j+nitems] = currentCorbit_[j];
      }
      h->muv_->uvputvri(kw->name, &currentCorbit_[0] ,nitems*2);
      delete [] refv;
      return;
    }

    void XMLHandler::storeCoreff(XMLHandler *h, const XMLCh *name,
				 const Keyword *kw, const XMLCh *,
				 int length, const XMLCh *value)
    {
      //      CARMA_CPTRACE(carma::util::Trace::TRACE6,"COREFF");
      int nitems = 0;
      float* iv = buildFloatv(value, nitems);
      float* refv = iv;
      currentCoreff_.resize(nitems*2,0.0);
      for (int j=0; j < nitems; j++) {
	currentCoreff_[j] = *iv++;
	currentCoreff_[j+nitems] = currentCoreff_[j];
      }
      //CARMA_CPTRACE(carma::util::Trace::TRACE6,"GOT IT");
      h->muv_->uvputvrr(kw->name, &currentCoreff_[0] ,nitems*2);
      //CARMA_CPTRACE(carma::util::Trace::TRACE6,"PUT IT");
      delete [] refv;
      return;
    }

    void XMLHandler::storeCorrInp(XMLHandler *h, const XMLCh *name, 
				  const Keyword *kw, const XMLCh *, 
				  int length, const XMLCh *value)
    {
      // Store map of correlator input to antenna number 
      // for the current integration.
      int nitems = 0;
      int* iv = buildIntv(value, nitems);
      int* refv = iv;
      for (int j=0; j < nitems; j++) {
	currentCorrelatorInputMap_[*iv++] = j + 1;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storeNewCorrInp(XMLHandler *h, const XMLCh *name, 
				  const Keyword *kw, const XMLCh *, 
				  int length, const XMLCh *value)
    {
      // Store map of correlator input to antenna number 
      // for the current integration.
      int nitems = 0;
      int* iv = buildIntv(value, nitems);
      int* refv = iv;
      tempCorrInputMap_.resize(nitems);
      for (int j=0; j < nitems; j++) {
	tempCorrInputMap_[j] = *iv++;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storeUVW(XMLHandler *h, const XMLCh *name, 
			      const Keyword *kw, const XMLCh *, 
			      int length, const XMLCh *value)
    {
      // Store UVW values per antenna for the current integration.
      int nitems = 0;
      double* dv = buildDoublev(value, nitems);
      double* refv = dv;
      // Round down to the nearest multiple of three.
      int nUVW = 3 * static_cast<int>((nitems + 0.5) / 3);
      currentUVW_.resize(nUVW, 0);
      for (int j=0; j < nUVW; j++) {
	currentUVW_[j] = *dv++;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storeJyperka(XMLHandler *h, const XMLCh *name, 
				  const Keyword *kw, const XMLCh *, 
				  int length, const XMLCh *value)
    {
      // Store Jy/K values per antenna for the current integration.
      int nitems = 0;
      float* rv = buildFloatv(value, nitems);

      // Store current Jy/K for each antenna
      currentJyperka_.resize(nitems, 0);
      float *fv = rv;
      for (int j=0; j < nitems; j++) {
	currentJyperka_[j] = *fv++;
      }

      // Write through to output miriad file
      h->muv_->uvputvrr(kw->name, rv, nitems);
      delete [] rv;
      return;
    }

    void XMLHandler::storeBandFreq(XMLHandler *h, const XMLCh *name, 
				   const Keyword *kw, const XMLCh *, 
				   int length, const XMLCh *value)
    {
      // Store band frequencies for the current integration.
      int nitems = 0;
      double* dv = buildDoublev(value, nitems);
      double* refv = dv;
      currentBandFreq_.resize(nitems, 0);
      double dmax = 0;
      for (int j=0; j < nitems; j++) {
	currentBandFreq_[j] = *dv++;
	dmax = std::max(dmax, currentBandFreq_[j]);
      }

      // Set default values if all band frequencies are unset
      if (dmax < 1) {
	std::ostringstream emsg;
	emsg << "Setting default band frequencies: [100+bandno] GHz";
	Program::getLogger() << log4cpp::Priority::WARN << emsg.str();
	for (int j=0; j < nitems; j++) {
	  currentBandFreq_[j] = 100.0 + (j+1);
	}
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storeRestFreq(XMLHandler *h, const XMLCh *name, 
				   const Keyword *kw, const XMLCh *, 
				   int length, const XMLCh *value)
    {
      // Store rest frequencies for the current integration.
      int nitems = 0;
      double* dv = buildDoublev(value, nitems);
      double* refv = dv;
      currentRestFreq_.resize(nitems, 0);
      double dmax = 0;
      for (int j=0; j < nitems; j++) {
	currentRestFreq_[j] = *dv++;
	dmax = std::max(dmax, currentRestFreq_[j]);
      }
      maxRestfreq_ = dmax;
      delete [] refv;
      return;
    }

    void XMLHandler::storeTsys(XMLHandler *h, const XMLCh *name,
			       const Keyword *kw, const XMLCh *,
			       int length, const XMLCh *value)
    {
      // Store Tsys values for the current integration.
      int nitems = 0;
      float* fv = buildFloatv(value, nitems);
      float* refv = fv;
      currentTsys_.resize(nitems, 0);
      for (int j=0; j < nitems; j++) {
	currentTsys_[j] = *fv++;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storeNewTsys(XMLHandler *h, const XMLCh *name,
				  const Keyword *kw, const XMLCh *,
				  int length, const XMLCh *value)
    {
      // Store map of correlator input to antenna number 
      // for the current integration.
      int nitems = 0;
      float* iv = buildFloatv(value, nitems);
      float* refv = iv;
      tempTsysMap_.resize(nitems);
      for (int j=0; j < nitems; j++) {
	tempTsysMap_[j] = *iv++;
      }
      delete [] refv;
      return;
    }


    void XMLHandler::storePsys(XMLHandler *h, const XMLCh *name,
			       const Keyword *kw, const XMLCh *,
			       int length, const XMLCh *value)
    {
      // Store Psys values for the current integration.
      int nitems = 0;
      float* fv = buildFloatv(value, nitems);
      float* refv = fv;
      currentPsys_.resize(nitems, 0);
      for (int j=0; j < nitems; j++) {
	currentPsys_[j] = *fv++;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storeNewPsys(XMLHandler *h, const XMLCh *name,
				  const Keyword *kw, const XMLCh *,
				  int length, const XMLCh *value)
    {
      // Store map of correlator input to antenna number 
      // for the current integration.
      int nitems = 0;
      float* iv = buildFloatv(value, nitems);
      float* refv = iv;
      tempPsysMap_.resize(nitems);
      for (int j=0; j < nitems; j++) {
	tempPsysMap_[j] = *iv++;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storePsysAttn(XMLHandler *h, const XMLCh *name,
				   const Keyword *kw, const XMLCh *,
				   int length, const XMLCh *value)
    {
      // Store PsysAttn values for the current integration.
      int nitems = 0;
      float* fv = buildFloatv(value, nitems);
      float* refv = fv;
      currentPsysAttn_.resize(nitems, 0);
      for (int j=0; j < nitems; j++) {
	currentPsysAttn_[j] = *fv++;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storeNewPsysAttn(XMLHandler *h, const XMLCh *name, 
				      const Keyword *kw, const XMLCh *, 
				      int length, const XMLCh *value)
    {
      // Store map of correlator input to antenna number 
      // for the current integration.
      int nitems = 0;
      float* iv = buildFloatv(value, nitems);
      float* refv = iv;
      tempPsysAttnMap_.resize(nitems);
      for (int j=0; j < nitems; j++) {
	tempPsysAttnMap_[j] = *iv++;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storeAmbPsys(XMLHandler *h, const XMLCh *name,
				  const Keyword *kw, const XMLCh *,
				  int length, const XMLCh *value)
    {
      // Store AmbPsys values for the current integration.
      int nitems = 0;
      float* fv = buildFloatv(value, nitems);
      float* refv = fv;
      currentAmbPsys_.resize(nitems, 0);
      for (int j=0; j < nitems; j++) {
	currentAmbPsys_[j] = *fv++;
      }
      delete [] refv;
      return;
    }


    void XMLHandler::storeNewAmbPsys(XMLHandler *h, const XMLCh *name, 
				     const Keyword *kw, const XMLCh *, 
				     int length, const XMLCh *value)
    {
      // Store map of correlator input to antenna number 
      // for the current integration.
      int nitems = 0;
      float* iv = buildFloatv(value, nitems);
      float* refv = iv;
      tempAmbPsysMap_.resize(nitems);
      for (int j=0; j < nitems; j++) {
	tempAmbPsysMap_[j] = *iv++;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storeAntennas(XMLHandler *h, const XMLCh *name,
				   const Keyword *kw, const XMLCh *,
				   int length, const XMLCh *value)
    {
      // Store antenna list for the current integration.
      int nitems = 0;
      int* iv = buildIntv(value, nitems);
      int* refv = iv;
      currentAntennas_.resize(nitems, 0);
      for (int j=0; j < nitems; j++) {
	currentAntennas_[j] = *iv++;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storeBandGood(XMLHandler *h, const XMLCh *name,
				   const Keyword *kw, const XMLCh *,
				   int length, const XMLCh *value)
    {
      // Store band list for the current integration.
      int nitems = 0;
      int* iv = buildIntv(value, nitems);
      int* refv = iv;
      currentGoodBands_.resize(nitems, 0);
      if(nitems == 0)
	currentMaxBand_ = -1;
      //      ostringstream oss;
      for (int j=0; j < nitems; j++) {
	currentGoodBands_[j] = *iv++;
	//oss << currentGoodBands_[j] << "-";
	if(currentGoodBands_[j] == 1 && j + 1 > currentMaxBand_)
	  currentMaxBand_ = j+1;
      }
      delete [] refv;
      return;
    }

    void XMLHandler::storePolState(XMLHandler *h, const XMLCh *name,
				   const Keyword *kw, const XMLCh *,
				   int length, const XMLCh *value)
    {
      // Store the polarization state of each input
      int nitems = 0;
      int* iv = buildIntv(value, nitems);
      int* refv = iv;
      tempPolState_.resize(nitems, "");
      //ostringstream oss;
      for (int j=0; j < nitems; j++) {
	switch (*iv++){
	case carma::monitor::PolarizationMonitorPointEnum::L :
	  tempPolState_[j] = "L";
	  break;
	case carma::monitor::PolarizationMonitorPointEnum::R :
	  tempPolState_[j] = "R";
	  break;
	case carma::monitor::PolarizationMonitorPointEnum::V :
	  tempPolState_[j] = "Y";
	  break;
	case carma::monitor::PolarizationMonitorPointEnum::H :
	  tempPolState_[j] = "X";
	  break;
	default :
	  tempPolState_[j] = "U";
	  break;
	};
      }
      havePolState_ = true;
      delete [] refv;
      return;
    }

    void XMLHandler::storeVersion(XMLHandler *h, const XMLCh *name, 
					 const Keyword *kw, const XMLCh *, 
					 int length, const XMLCh *value)
    {
      //store it in miriad
      char *svalue = XMLString::transcode(value);
      h->muv_->uvputvra("version", svalue);
      XMLString::release(&svalue);

      // keep it for further processing
      std::string version = 
	carma::util::StringUtils::trimWhiteSpace(XMLString::transcode(value));
      char* pch;
      currentVersion_.resize(0,0);
      pch = strtok(const_cast<char*>(version.c_str()),".");
      while(pch != NULL){
	currentVersion_.push_back(atoi(pch));
	pch = strtok(NULL,".");
      } 
      return;
    }

    // CorrType
    void XMLHandler::storeCorrtype(XMLHandler *h, const XMLCh *name,
				   const Keyword *kw,
				   const XMLCh *type, int length,
				   const XMLCh *value)
    {
      std::string temp 
	= carma::util::StringUtils::trimWhiteSpace(XMLString::transcode(value));
      if(temp == "WIDEBAND"){
	correlatorType_ = static_cast<MonitorAverageType>(carma::monitor::CorrelatorDesignationMonitorPointEnum::WIDEBAND);
	return;
      }
      correlatorType_ = static_cast<MonitorAverageType>(carma::monitor::CorrelatorDesignationMonitorPointEnum::SPECTRAL);
      return;
    }


    void XMLHandler::putIntTime(XMLHandler *h, const XMLCh *name,
				const Keyword *kw,
				const XMLCh *type, int length,
				const XMLCh *value)
    {
      float rv = buildFloat(value);
      //since we are adding integration times on a baseline basis we don't want to write it out here
      //h->muv_->uvputvrr(kw->name, rv);
      currentIntTime_ = rv;  // remember it to correct time, ut, lst for 1/2 inttime
    }

    void XMLHandler::putPntRaDec(XMLHandler *h, const XMLCh *name, 
				 const Keyword *kw, const XMLCh *, 
				 int length, const XMLCh *value)
    {
      // Process header element containing pointing centers.
      int nitems = 0;
      double* dv = buildDoublev(value, nitems);

      // Select first value in array of antenna pointing positions; 
      // MIRIAD assumes one pointing position per array
      h->muv_->uvputvrd(kw->name, *dv);

      delete [] dv;
    }

    //////////////////////////////////////////////////////////////////////
    // 	Convert from XMLCh * to binary

    // Return a pointer to a list of ints generated from list.
    // length is set to the number of entries.
    // If list is non 0, it is used and is assumed to be at most nitems long.
    // If list is 0, a new array is allocated.
    // On return, nitems is set to the # of values converted.
    int *XMLHandler::buildIntv(const XMLCh *value, int &nitems, int *list)
    {
      XMLStringTokenizer tokens(value);
      int *v, maxitems;

      int ntokens = tokens.countTokens();
      if(ntokens <= 0) {
	nitems = 0;
	return 0;
      }

      if(list == 0) {
	v = new int[ntokens];
	maxitems = ntokens;
      } else {
	v = list;
	maxitems = (nitems <= ntokens) ? nitems : ntokens;
      }
      int *ptr = v;

      for(int i=0; i< maxitems; i++) {
	const XMLCh *t = tokens.nextToken();
	int vint = XMLString::parseInt(t);
	*ptr++ = vint;
      }
      nitems = maxitems;
      return v;
    }

    // Same as buildIntv, but for floats.
    float *XMLHandler::buildFloatv(const XMLCh *value, int &nitems, float *list)
    {
      XMLStringTokenizer tokens(value);
      float *v;
      int  maxitems;

      int ntokens = tokens.countTokens();
      if(ntokens <= 0) {
	nitems = 0;
	return 0;
      }

      if(list == 0) {
	v = new float[ntokens];
	maxitems = ntokens;
      } else {
	v = list;
	maxitems = (nitems <= ntokens) ? nitems : ntokens;
      }
      float *ptr = v;

      for(int i=0; i< maxitems; i++) {
	const XMLCh *t = tokens.nextToken();
	XMLDouble dbl(t);
	*ptr++ = dbl.getValue();
      }
      nitems = maxitems;
      return v;
    }

    // Same as buildIntv, but for doubles.
    double *XMLHandler::buildDoublev(const XMLCh *value, int &nitems, double *list)
    {
      XMLStringTokenizer tokens(value);
      double *v;
      int  maxitems;

      int ntokens = tokens.countTokens();
      if(ntokens <= 0) {
	nitems = 0;
	return 0;
      }

      if(list == 0) {
	v = new double[ntokens];
	maxitems = ntokens;
      } else {
	v = list;
	maxitems = (nitems <= ntokens) ? nitems : ntokens;
      }
      double *ptr = v;

      for(int i=0; i< maxitems; i++) {
	const XMLCh *t = tokens.nextToken();
	XMLDouble dbl(t);
	*ptr++ = dbl.getValue();
      }
      nitems = maxitems;
      return v;
    }

    //	Convert and return a single value.
    int XMLHandler::buildInt(const XMLCh *value)
    {
      int v = XMLString::parseInt(value);
      return v;
    }

    float XMLHandler::buildFloat(const XMLCh *value)
    {
      float v;

      XMLDouble dbl(value);
      v = dbl.getValue();
      return v;
    }

    double XMLHandler::buildDouble(const XMLCh *value)
    {
      double v;

      XMLDouble dbl(value);
      v = dbl.getValue();    // could you do:  return (double) dbl.getValue();
      return v;
    }

    // get the most common value form an array, used to get the frequency based
    // header values. This may not be the best way to determine the proper
    // vlaue, but if more than half the values are bad then we are probably
    // screwed anyway
    float getValue(std::vector<float> &values){
      float value = 0;
      std::map<float,unsigned int> valMap;
      for(unsigned int i = 0; i < values.size(); i++){
	if(valMap.count(values[i]) == 0){
	  valMap[values[i]] = 1;
	}
	else{
	  valMap[values[i]]++;
	}
      }
      std::map<float,unsigned int>::iterator itr;
      unsigned int count = 0;
      for(itr = valMap.begin(); itr != valMap.end(); itr++){
	if(itr->second > count){
	  count = itr->second;
	  value = itr->first;
	}
      }
      if(value == 0.0){
        count = 0;
        for(itr = valMap.begin();itr != valMap.end(); itr++){
          if((itr->second > count) && (itr->first != 0.0)){
            count = itr->second;
            value = itr->first;
          }
        }
      } 

      return value;
    }

    int getValue(std::vector<int> &values){
      int value = 0;
      std::map<int,unsigned int> valMap;
      for(unsigned int i = 0; i< values.size(); i++){
	if(valMap.count(values[i]) == 0){
	  valMap[values[i]] = 1;
	}
	else{
	  valMap[values[i]]++;
	}
      }
      std::map<int,unsigned int>::iterator itr;
      unsigned int count = 0;
      for(itr = valMap.begin();itr != valMap.end(); itr++){
	if(itr->second > count){
	  count = itr->second;
	  value = itr->first;
	}
      }
      if(value == 0){
	count = 0;
	for(itr = valMap.begin();itr != valMap.end(); itr++){
	  if((itr->second > count) && (itr->first != 0)){
	    count = itr->second;
	    value = itr->first;
	  }
	}
      }
      return value;
    }

    void XMLHandler::unpackCorrelatorData
    (carma::correlator::lib::CorrelatorData*& correlatorData,
     std::string& corrType, const std::string& sortOrder,
     std::map<int,int>& mapCorrInputToAntNumber, 
     int& nspect, int& nchan, std::vector<int>& ischan, 
     std::vector<int>& nschan, 
     std::vector<double>& restfreq,
     std::vector<double>& sfreq, 
     std::vector<double>& sdf, double& freq,
     int& nwide, 
     std::vector<float>& wfreq,
     std::vector<float>& wwidth, 
     double& time,
     std::map<int, std::map<int, std::vector<float> > >& corr, 
     std::map<int, std::map<int, std::vector<int> > >& flags,
     std::map<int, std::map<int, std::vector<float> > >& wcorr,
     std::map<int, std::map<int, std::vector<int> > >& wflags,
     std::map<int, std::map<int, std::vector<float> > >& intTime,
     std::map<int, std::map<int, std::vector<int> > >& flagReason)
    {
      // Unpack a CorrelatorData object into MIRIAD form.
      
      // Initialization:


      // Output parameters
      nspect = 0;
      nchan = 0;
      ischan.clear();
      nschan.clear();
      restfreq.clear();
      sfreq.clear();
      sdf.clear();
      freq = 0;
      nwide = 0;
      wfreq.clear();
      wwidth.clear();
      time = 0;
      for(int i = -1; i >= -8; i--){
	corr[i].clear();
	flags[i].clear();
	wcorr[i].clear();
	wflags[i].clear();
	intTime[i].clear();
	flagReason[i].clear();
      }
      
      std::map<int, bool> bandMap = std::map<int, bool>();
      
      //for tracking bands that go offline
      std::vector<std::vector<bool> > bandChange;
      std::vector<bool> tempChange(24,false);
      
      // Retrieve bands
      int numBands;
      if(currentMaxBand_ <= 0){
	numBands = correlatorData->getNumberOfBands();
	if(numBands > 3) {
	  numBands = 4;
	}
	else{
	  numBands = 3;
	}
      }
      else{
	numBands = currentMaxBand_;
      }
      SpwBandRelationships::setNumWin(numBands*2);
      // Extract frequency axis coordinates; these are checked for 
      // invariance across baselines and sidebands.
      std::map<int,int> nchanMap;
      std::map<int,float> offsetFreqMap, deltaFreqMap;
      
      // Loop over band
      int startIndex = 0;
      int offset = 0;
      if(currentVersion_[0] == 2){
	switch (correlatorType_){
	case carma::monitor::CorrelatorDesignationMonitorPointEnum::SPECTRAL :
	  startIndex = SlBandStart_ - 1;
	  offset = 0;
	  break;
	  
	case carma::monitor::CorrelatorDesignationMonitorPointEnum::WIDEBAND :
	  startIndex = WbBandStart_ - 1;
	  offset = WbBandStart_ - 1;
	  break;
	default :
	  break;
	}
      }

      nspect = SpwBandRelationships::numSpw();
      std::vector<std::vector<int> > tchanMap;
      std::vector<std::vector<float> > tdfreqMap;
      std::vector<std::vector<float> > toffsetMap;

      tchanMap.resize(nspect);
      tdfreqMap.resize(nspect);
      toffsetMap.resize(nspect);

      for (int jband = startIndex; jband < numBands + offset; jband++) {
	bandMap[jband+1] = false;
	try {
	  // Retrieve this band number
	  CorrelatorBand band = correlatorData->getBand(jband+1);
	  bandMap[jband+1] = true;
	  // Skip invalid or non-science data
	  bool skipBand = !band.isValid() || band.isSelfTest();
	  //	  || band.isSimulation();
	  if (!skipBand) {
	    // Loop over baseline
	    int numBaselines = band.getNumberOfBaselines();
	    std::vector<CorrelatorBaseline> baselines = band.getBaselines();
	    for (int jbasl=0; jbasl < numBaselines; jbasl++) {
	      // Loop over sideband
	      bandChange.push_back(tempChange);
	      int numSidebands = baselines[jbasl].getNumberOfSidebands();
	      std::vector<CorrelatorSideband> & sidebands = 
		baselines[jbasl].getSidebands();
	      for (int jside=0; jside < numSidebands; jside++) {
		CorrelatorSideband & csb = sidebands.at( jside );
		// Select on correlation type
		if (selectedCorrType(corrType, csb)) {
	  
		  // Compute sideband and map indices
		  SpwBandRelationships::SIDEBAND sb = sideband(csb);
		  int kindx = SpwBandRelationships::bandSbIndex(jband,sb);
		  int numChan = csb.getNumberOfChans();

		  // Extract key frequency coordinate values
		  tchanMap[jband].push_back(numChan);
		  CARMA_CPTRACE(carma::util::Trace::TRACE1,"ADDING " << jbasl << "  " <<  baselines[jbasl].getAnt1Number() << "  " << baselines[jbasl].getAnt2Number() << "  " << jside << "  " << jband << " " << numChan << "  " << csb.getNumberOfChans());

		  tdfreqMap[jband].push_back(csb.getDeltaFrequency() / 1e3);
		  toffsetMap[jband].push_back(csb.getOffsetFrequency() / 1e3);
		  //CARMA_CPTRACE(carma::util::Trace::TRACE3,"N2 " << csb.getDeltaFrequency() / 1e3 << "  " << csb.getOffsetFrequency() / 1e3);
		  //ostringstream oss,oss1;
		  //for(unsigned int q=0; q < toffsetMap[jband].size(); q++){
		  //  oss << toffsetMap[jband][q] << "  ";
		  //  oss1 << tdfreqMap[jband][q] << "  ";
		  // }
		  //CARMA_CPTRACE(carma::util::Trace::TRACE3,"N4 " <<oss.str());
                  //CARMA_CPTRACE(carma::util::Trace::TRACE3,"N5 " <<oss1.str());
 		  // Update frequency coordinate maps and check for consistency
		  //ostringstream oss1;
		  
		  if (nchanMap.count(kindx) == 0) {
		    nchanMap[kindx] = numChan;
		  }
		  else if (nchanMap[kindx] != numChan && currentStartFrame_ < 769936000) {
		    bandChange[jbasl][jband] = true;
		  }
		} // if (selected)
	      } // for (jside)
	    } // for (jbasl)
	  } // if (!skipBand)
	} catch ( const carma::util::NotFoundException& exc) {
	  //	  CARMA_CPTRACE(carma::util::Trace::TRACE4,"BAND " << jband+1 << " NOT FOUND");
	  // Band not found
	}
      } // for (jband)

      for (int jband = startIndex; jband < numBands + offset; jband++) {
	// Loop over sideband
	int cm = getValue(tchanMap[jband]);
	float df = getValue(tdfreqMap[jband]);
	float of = getValue(toffsetMap[jband]);
	for (int jside=0; jside < 3; jside++) {
	  // Compute sideband and map indices
	  int kindx = SpwBandRelationships::bandSbIndex(jband,static_cast<SpwBandRelationships::SIDEBAND>(jside));
	  //CARMA_CPTRACE(carma::util::Trace::TRACE3,"N7.1 " << kindx << "  " << nchanMap[kindx] << "  "<<cm << "  " << df << "  " << of);
	  nchanMap[kindx] = cm;
	  deltaFreqMap[kindx] = df;
	  offsetFreqMap[kindx] = of;
	}
      }



      // Set miriad frequency coordinate arrays
      ischan.resize(nspect, 0);
      nschan.resize(nspect, 0);
      restfreq.resize(nspect, 0);
      sfreq.resize(nspect, 0);
      sdf.resize(nspect, 0);
      wfreq.resize(nspect, 0);
      wwidth.resize(nspect, 0);

      // Flag array to indicate if each spectral window is present in the data
      std::vector<bool> spwPresent(nspect, false);

      // Iterate through all bands and sidebands found
      std::map<int, int>::iterator pos;
      offset = 0;
      if(correlatorType_ == static_cast<MonitorAverageType>(carma::monitor::CorrelatorDesignationMonitorPointEnum::WIDEBAND))
	offset = WbBandStart_ - 1;
      for (pos = nchanMap.begin(); pos != nchanMap.end(); ++pos) {
	int index = pos->first;
	  // Retrieve band and sideband tuple for this map index
	std::pair<int, SpwBandRelationships::SIDEBAND> tuple =
	  SpwBandRelationships::bandSbIndexInv(index);
	int band = tuple.first - offset;
	SpwBandRelationships::SIDEBAND sb = tuple.second;

	// Determine matching output spectral windows
	std::vector<int> spectralWindows = 
	  SpwBandRelationships::bandSbToSpw(band, sb);

	// Loop over spectral windows
	for (int jspw=0; jspw < static_cast<int>(spectralWindows.size()); 
	     jspw++) {
	  int spw = spectralWindows[jspw];
	  // Retrieve frequency coordinates
	  int numChan = nchanMap[index];
	  //CARMA_CPTRACE(carma::util::Trace::TRACE3,"INDEX " << index << "  " << nchanMap[index] << " " << deltaFreqMap[index] << " " << offsetFreqMap[index]); 
	  // Determine net sideband
	  SpwBandRelationships::SIDEBAND netSb;
	  int sbSign;
	  if ((SpwBandRelationships::isLSB(spw) ||
	       (sb == SpwBandRelationships::LSB))) {
	    netSb = SpwBandRelationships::LSB;
	    sbSign = -1;
	  } else {
	    netSb = SpwBandRelationships::USB;
	    sbSign = +1;
	  };
	  double chanWidth = sbSign * deltaFreqMap[index];
	  int bandFreqIdx = SpwBandRelationships::bandFreqIndex(band, netSb);
	  double edgeFreq = currentBandFreq_[bandFreqIdx] + 
	    sbSign * offsetFreqMap[index];
	  double restFreq = currentRestFreq_[bandFreqIdx];
	  if (spwPresent[spw]) {
	    if ((numChan != nschan[spw]) || (chanWidth != sdf[spw]) ||
		(edgeFreq != sfreq[spw]) || (restFreq != restfreq[spw])) {
	      std::ostringstream emsg; 	 
	      emsg << "Inconsistent frequency coordinates for spectral window:"	 
		   << (spw+1); 	 
	      CARMA_CPTRACE(carma::util::Trace::TRACE4, " " << emsg.str()); 	 
	      Program::getLogger() << log4cpp::Priority::ERROR << emsg.str(); 	 
	      throw CARMA_EXCEPTION(carma::util::ErrorException, emsg.str()); 	 
	    }
	  }
	  //CARMA_CPTRACE(carma::util::Trace::TRACE3,"N7 " << index << "  " << chanWidth << " " << deltaFreqMap[index] << " B " << edgeFreq << "  " << offsetFreqMap[index]);
	  // Update miriad frequency coordinate arrays
	  nschan[spw] = numChan;
	  restfreq[spw] = restFreq;
	  sdf[spw] = chanWidth;
	  sfreq[spw] = edgeFreq;
	  wwidth[spw] = nschan[spw] * sdf[spw];
	  wfreq[spw] = sfreq[spw] + wwidth[spw] / 2;
	  // Mark spectral window present.
	  spwPresent[spw] = true;
	} // for (spw)
      } // for (pos)
      // Set start channel numbers

      int jchan = 1;
      for (int spw=0; spw < nspect; spw++) {
	ischan[spw] = jchan;
	jchan += nschan[spw];
      }

      // Ensure valid rest frequencies
      for (int spw=0; spw < nspect; spw++) {
	int mspw = SpwBandRelationships::matchingSbSpw(spw);
	if (std::max(restfreq[spw], restfreq[mspw]) == 0) {
	  restfreq[spw] = sfreq[spw];
	} else if (restfreq[spw] * restfreq[mspw] == 0) {
	  restfreq[spw] = std::max(restfreq[spw], restfreq[mspw]);
	}
      }
      // Set total no. of channels, reference frequency, and time
      nchan = jchan - 1;
      if(nchan == 0){
	if(lastNChan_[currentFile_] == 0){
	  lastNChan_[currentFile_] = 39 * nspect;
	  nchan = lastNChan_[currentFile_];
	  lastNsChan_[currentFile_].resize(nspect,0);
	  lastIsChan_[currentFile_].resize(nspect,0);
	  //	  ostringstream oss1;
	  //oss1 << "N1 " << nchan << " -- ";
	  for(int spw = 0; spw < nspect; spw++){
	    lastNsChan_[currentFile_][spw] = 39;
	    lastIsChan_[currentFile_][spw] = (39 * spw) + 1;
	    nschan[spw] = lastNsChan_[currentFile_][spw];
	    ischan[spw] = lastIsChan_[currentFile_][spw];
	    //oss1 << nschan[spw] << "  " << ischan[spw] << " XX ";
	  }
	  //CARMA_CPTRACE(carma::util::Trace::TRACE2,oss1.str());
	}
	else{
	  nchan = lastNChan_[currentFile_];
	  //ostringstream oss1;
	  //oss1 << "N2 " << nchan << " -- ";
	  for(int spw = 0; spw < nspect; spw++){
	    nschan[spw] = lastNsChan_[currentFile_][spw];
	    ischan[spw] = lastIsChan_[currentFile_][spw];
	    //oss1 << nschan[spw] << "  " << ischan[spw] << " XX ";
	  }
	  //CARMA_CPTRACE(carma::util::Trace::TRACE2,oss1.str());
	}
      }
      else{
	lastNChan_[currentFile_] = nchan;
	lastNsChan_[currentFile_].resize(nspect,0);
	lastIsChan_[currentFile_].resize(nspect,0);
	//ostringstream oss1;
	//oss1 << "N3 " << nchan << " -- ";
	for(int spw = 0; spw < nspect; spw++){
	  //oss1 << nschan[spw] << "  " << ischan[spw] << " XX ";
	  lastNsChan_[currentFile_][spw] = nschan[spw];
	  lastIsChan_[currentFile_][spw] = ischan[spw];
	}
	//CARMA_CPTRACE(carma::util::Trace::TRACE2,oss1.str());
      }
      nwide = nspect;
      int choice = 0;
      for ( vector<double>::size_type i = 0; i < sfreq.size(); ++i ) {
          if(sfreq[i] > 0.0){
              choice = i;
              break;
          }
      }
      freq = sfreq.size() > 0 ? sfreq[choice] : 0;
      time = currentStartFrameMJD_ 
	+ carma::services::AstroTime::JULIAN_DAY_ZERO
	+ currentIntTime_/86400.0/2 
	- XMLHandler::frameOffset(currentStartFrameMJD_) 
	/ carma::util::Time::FRAMES_PER_DAY;

      // TODO:  should adjust 'lst' and 'ut' here as well, and write it out again
      // to account for the fact that they are meant to be measured mid-interval

      // Fill MIRIAD data variables
      // Loop over band
      CorrelatorBand band;
      for (int jband = startIndex; jband < numBands + offset; jband++) {
	if(bandMap[jband+1]) {
	  try {
	    // Retrieve this band number
	    band = correlatorData->getBand(jband+1);
	    
	    // Skip invalid or non-science data
	    bool skipBand = !band.isValid() || band.isSelfTest();
	    //|| band.isSimulation();
	    if (!skipBand) {
	      // Loop over baseline
	      int numBaselines = band.getNumberOfBaselines();
	      std::vector<CorrelatorBaseline> baselines = band.getBaselines();
	      for (int jbasl=0; jbasl < numBaselines; jbasl++) {
		int baselinePolCode = 0;
		CorrelatorBaseline& cbl = baselines[jbasl];
		
		// Set baseline
		int in1 = cbl.getInput1Number();
		int in2 = cbl.getInput2Number();
		// Set polarization code
		int iant1 = 0;
		int iant2 = 0;
		if(currentVersion_[0] == 2){
		  if(visbrickVersionNumber_ >= 1){   // get ant#'s and pol from visbrick
		    iant1 = cbl.getAnt1Number();
		    iant2 = cbl.getAnt2Number();
		    baselinePolCode = getPolCode(cbl.getPolarization());
		    //CARMA_CPTRACE(carma::util::Trace::TRACE4,"NEW VB VERSION " << iant1 << "  " << iant2 << "  " << baselinePolCode << "  " << cbl.getPolarization());
		  }
		  if(iant1 < 1 || iant2 < 1){       // if the above fails, fall back
		    baselinePolCode = getPolCode(jband+1, in1, in2);
		    iant1 = getAntNumber(jband+1,in1);
		    iant2 = getAntNumber(jband+1,in2);
		  }
		}
		else{
		  baselinePolCode = -1;
		  iant1 = mapCorrInputToAntNumber[in1];
		  iant2 = mapCorrInputToAntNumber[in2];
		  //CARMA_CPTRACE(carma::util::Trace::TRACE4,"OLD VB VERSION");
		}
		
		int baselineCode = 256 * iant1 + iant2;
		// Skip baselines that are not part of this subarray
		if ((currentAntennas_.size() == 0) || 
		    ((currentAntennas_.size() > 0) && 
		     (iant1 != 0) && (iant2 != 0) &&
		     (currentAntennas_[iant1-1] != 0) &&
		     (currentAntennas_[iant2-1] != 0))) {
		  bool conjugate = false;  //do we need to conjugate the data (if flipping antenna order)

		  if(iant1 > iant2) {
		    int temp = iant1;
		    iant1 = iant2;
		    iant2 = temp;
		    conjugate = true;
		    if(baselinePolCode == -3){
		      baselinePolCode = -4;
		    }
		    else if(baselinePolCode == -4){
		      baselinePolCode = -3;
		    }
		    else if(baselinePolCode == -7){
		      baselinePolCode = -8;
		    }
		    else if(baselinePolCode == -8){
		      baselinePolCode =-7;
		    }
		    baselineCode = 256 * iant1 + iant2;
		  }
		  // Allocate output arrays if new baseline

		  if (corr[baselinePolCode][baselineCode].size() == 0) {
		    // Add row for visibility spectrum and flags for this baseline
		    corr[baselinePolCode][baselineCode].assign(2*nchan,0);
		    flags[baselinePolCode][baselineCode].assign(nchan,0);
		    wcorr[baselinePolCode][baselineCode].assign(2*nwide,0);
		    wflags[baselinePolCode][baselineCode].assign(nwide,0);
		    flagReason[baselinePolCode][baselineCode].assign(nwide,0);
		  }
		  else{
		    //CARMA_CPTRACE(carma::util::Trace::TRACE4,"P4 NO ADD FOR " << baselineCode << "  " << baselinePolCode);
		  }


		  int indxBasl = baselineCode;
		  // Loop over sideband
		  int numSidebands = baselines[jbasl].getNumberOfSidebands();
		  std::vector<CorrelatorSideband> & sidebands 
		    = baselines[jbasl].getSidebands();
		  for (int jside=0; jside < numSidebands; jside++) {
		    CorrelatorSideband & csb = sidebands.at(jside);
		    const CorrelatorStats &stats = csb.getStats();

		    // Check blanking/flagging status for this sideband 
		    carma::monitor::MonitorPoint::BLANKING_FLAGGING blankFlag;
		    if ( visbrickVersionNumber_ == 2 ) {
		      blankFlag = csb.getBlankFlagStatus();
		    } else {
		      blankFlag = baselines[jbasl].getBlankFlagStatusDEPRECATED();
		    }
		    /////HERE IS WHERE WE NEED TO WORK
		    //if ( blankFlag == carma::monitor::MonitorPoint::BLANKED )
		    //  continue;
		    
		    bool blankFlagBasl = 
		      ((blankFlag == carma::monitor::MonitorPoint::FLAGGED) ||
		       ( blankFlag == carma::monitor::MonitorPoint::BLANKED_FLAGGED) ||
		       (blankFlag == carma::monitor::MonitorPoint::BLANKED));

		    // Select on correlation type
		    if (selectedCorrType(corrType, csb)) {
		      // Determine matching output spectral windows
		      SpwBandRelationships::SIDEBAND sb = sideband(csb);
		      std::vector<int> spectralWindows =
			SpwBandRelationships::bandSbToSpw(jband - offset, sb);
		      
		      // Loop over matching spectral windows
		      for (int jspw=0; 
			   jspw < static_cast<int>(spectralWindows.size()); 
			   jspw++) {
			int spw = spectralWindows[jspw];

			// Retrieve visibility data and data validity flags 
			// for this sideband
			std::vector<std::complex<float> > data = csb.getData();
			std::vector<int> dataValid = csb.getDataValid();
			
			// Insert in the visibility spectrum for this baseline,
			// and compute wide-band average
			int j = 2 * (ischan[spw] - 1);
			//CARMA_CPTRACE(carma::util::Trace::TRACE4,"J " << j << " " << spw << " " << ischan[spw] << " " << corr[baselinePolCode][indxBasl].size() << "  " << data.size() << "  " << csb.getNumberOfChans());
			std::complex<double> wavg;
			int nwavg = 0;
			flagReason[baselinePolCode][indxBasl][spw] = static_cast<int>(csb.getValidReason());
			ostringstream oss4;
			//oss4 << "XX " << flagReason[baselinePolCode][indxBasl][spw] << "  " << jspw << "  " << spw << " " << csb.getValidReason();
			//CARMA_CPTRACE(carma::util::Trace::TRACE2,oss4.str());
			if(!bandChange[jbasl][jband]){

			  for (int k=0; k < (int)data.size(); k++) {
			    // Conjugate to comply with miriad conventions
			    if (dataValid[k]) {
			      corr[baselinePolCode][indxBasl][j] = data[k].real();
			      if(conjugate) {
				//CARMA_CPTRACE(carma::util::Trace::TRACE3,"CONJUGATING " << iant1 << "  " << iant2 << "  " << baselinePolCode);
				corr[baselinePolCode][indxBasl][j+1] = data[k].imag();
			      } else {
				corr[baselinePolCode][indxBasl][j+1] = -data[k].imag();
			      }
			    }
			    j = j + 2;
			    if (blankFlagBasl) {
			      flags[baselinePolCode][indxBasl][ischan[spw]+k-1] = 0;
			    } else {
			      flags[baselinePolCode][indxBasl][ischan[spw]+k-1] = 1;
			    }
			    if (flags[baselinePolCode][indxBasl][ischan[spw]+k-1] == 1) {
			      wavg += std::conj(data[k]);
			      nwavg++;
			    }
			  }
			  float itime = stats.getIntegrationTime()/1000.0;
			  if(itime > 0.0)
			    intTime[baselinePolCode][indxBasl].push_back(itime);
			  wavg = wavg / static_cast<std::complex<double> >(nwavg);
			  wcorr[baselinePolCode][indxBasl][2*spw] = wavg.real();
			  if(conjugate) {
			    wcorr[baselinePolCode][indxBasl][2*spw+1] = -wavg.imag();
			  } else {
			    wcorr[baselinePolCode][indxBasl][2*spw+1] = wavg.imag();
			  }
			  if (nwavg > 0) {
			    wflags[baselinePolCode][indxBasl][spw] = 1;
			  } else {
			    wflags[baselinePolCode][indxBasl][spw] = 0;
			  }
			}
		      } // for (jspw)
		    } // if (selectedCorrType)
		  } // for (jside)
		} // Skip antennas not in subarray
	      } // for (jbasl)
	    } // if (!skipBand)
	  } catch ( const carma::util::NotFoundException& exc) {
	    // Band not found
	  }
	  catch(...) {

	  }
	}
      } // for (jband)
      //CARMA_CPTRACE(carma::util::Trace::TRACE3,"DONE UNPACK");
      return;
    }


    SpwBandRelationships::SIDEBAND 
    XMLHandler::sideband(const CorrelatorSideband & sb)
    {
      // Determine sideband type.

      SpwBandRelationships::SIDEBAND retval;
      if (sb.isLSB()) {
	retval = SpwBandRelationships::LSB;
      } else if (sb.isUSB()) {
	retval = SpwBandRelationships::USB;
      } else {
	retval = SpwBandRelationships::DSB;
      }
      return retval;
    }

    bool XMLHandler::selectedCorrType(const std::string& corrType,
				      const CorrelatorSideband & sb)
    {
      // Check if a sideband is selected by correlation type.
      bool selected = false;
      if (((corrType.find("ac") != std::string::npos) ||
	   (corrType.find("AC") != std::string::npos)) &&
	  sb.isAuto()) selected = true;
      if (((corrType.find("xc") != std::string::npos) ||
	   (corrType.find("XC") != std::string::npos)) &&
	  !sb.isAuto()) selected = true;
      return selected;
    }

    //////////////////////////////////////////////////////////////////
    // Print variable info to terminal.

    void XMLHandler::printIv(const char *str, const XMLCh *name,
			     const XMLCh *type, int xmllength,
			     const XMLCh *value)
    {
      int length;
      int *v = buildIntv(value, length);

      cout << str << ": name = " << name << " type = " << type
	   << " XMLlength = " << xmllength
	   << " length = " << length;
      if(length > 1)
	cout << "\n  ";
      else
	cout << " ";

      // @todo: why 16 here ?
      if(length > 16) length = 16;
      for(int i=0; i< length; i++)
	cout << v[i] << ", ";
      cout << endl;
      delete [] v;
    }

    void XMLHandler::printDv(const char *str, const XMLCh *name,
			     const XMLCh *type, int xmllength,
			     const XMLCh *value)
    {
      int length;
      double *v = buildDoublev(value, length);

      cout << str << ": name = " << name << " type = " << type
	   << " XMLlength = " << xmllength
	   << " length = " << length;
      if(length > 1)
	cout << "\n  ";
      else
	cout << " ";

      // @todo: why 16 here ?
      if(length > 16) length = 16;
      for(int i=0; i< length; i++)
	cout << v[i] << ", ";
      cout << endl;
      delete [] v;
    }

    void XMLHandler::printRv(const char *str, const XMLCh *name,
			     const XMLCh *type, int xmllength,
			     const XMLCh *value)
    {
      int length;
      float *v = buildFloatv(value, length);

      cout << str << ": name = " << name << " type = " << type
	   << " XMLlength = " << xmllength
	   << " length = " << length;
      
      if(length > 1)
	cout << "\n  ";
      else
	cout << " ";
      
      // Limit the number written.
      if(length > 16) length = 16;
      for(int i=0; i< length; i++)
	cout << v[i] << ", ";
      cout << endl;
      delete [] v;
    }
  } // namespace sdp
} // namespace carma
