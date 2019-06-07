#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/ui/rtd/common/AntennaMapper.h"
#include "carma/util/ErrorException.h"

#include<iostream>

using namespace std;

using namespace carma::ui::rtd;

const unsigned AntennaMapper::nOvro_ = 6;
const unsigned AntennaMapper::nBima_ = 9;
const unsigned AntennaMapper::nSza_  = 8;
const unsigned AntennaMapper::nAntenna_ = 23;

/**.......................................................................
 * Constructor.
 */
AntennaMapper::AntennaMapper(carma::monitor::CarmaMonitorSystem& cms) 
{
  mappingMs_ = &cms.signalPath().mapping();
  cms_       = &cms;

  for(unsigned iAnt=0; iAnt < nAntenna_; iAnt++) {
    antennas_.push_back(new Antenna(iAnt+1));
  }

  antennaMapsByType_[ANT_SZA]  = &szaMap_;
  antennaMapsByType_[ANT_BIMA] = &bimaMap_;
  antennaMapsByType_[ANT_OVRO] = &ovroMap_;

  antennaMapsBySubarray_[0] = &subarray0Map_;
  antennaMapsBySubarray_[1] = &subarray1Map_;
  antennaMapsBySubarray_[2] = &subarray2Map_;
  antennaMapsBySubarray_[3] = &subarray3Map_;
  antennaMapsBySubarray_[4] = &subarray4Map_;
  antennaMapsBySubarray_[5] = &subarray5Map_;

  initializeTypeMaps();
  rebuildSubarrayMaps();
}

/**.......................................................................
 * Destructor.
 */
AntennaMapper::~AntennaMapper() 
{
  for(unsigned iAnt=0; iAnt < nAntenna_; iAnt++)
    delete antennas_[iAnt];
}

bool AntennaMapper::mapsNeedRebuilding()
{
  for(unsigned iAnt=0; iAnt < nAntenna_; iAnt++) {
    if(mappingMs_->antenna(iAnt).subarrayNo().getValue() != antennas_[iAnt]->subarrayNo_) {
      return true;
    }
  }
  return false;
}

void AntennaMapper::initializeTypeMaps()
{
  for(unsigned iAnt=0; iAnt < nAntenna_; iAnt++) {
    Antenna* ant = antennas_[iAnt];
    AntennaType type = ant->type_;
    std::vector<Antenna*>* typeMap = antennaMapsByType_[type];
    typeMap->push_back(ant);
    ant->antennasByType_ = typeMap;
  }
}


void AntennaMapper::rebuildSubarrayMaps()
{
  if(mapsNeedRebuilding()) {

    subarray0Map_.clear();
    subarray1Map_.clear();
    subarray2Map_.clear();
    subarray3Map_.clear();
    subarray4Map_.clear();
    subarray5Map_.clear();

    for(unsigned iAnt=0; iAnt < nAntenna_; iAnt++) {
      Antenna* ant = antennas_[iAnt];

      unsigned subarrayNo  = mappingMs_->antenna(iAnt).subarrayNo().getValue();

      if(subarrayNo == 0) {
	ant->antennasBySubarray_ = 0;
	continue;
      }

      unsigned subarrayInd = subarrayNo - 1;
      
      std::vector<Antenna*>* subarrayMap = antennaMapsBySubarray_[subarrayNo];
      subarrayMap->push_back(ant);
      ant->antennasBySubarray_ = subarrayMap;
      ant->frequency_.setGHz(cms_->control().subarray(subarrayInd).loFreq().getValue());
    }
  }
}

void AntennaMapper::printSubarrayMaps()
{
  std::cout << "subarray 0: " << std::endl;
  for(unsigned i=0; i < subarray0Map_.size(); i++) {
    std::cout << subarray0Map_[i]->carmaAntNo_ << std::endl;
  }
  std::cout << "subarray 1: " << std::endl;
  for(unsigned i=0; i < subarray1Map_.size(); i++) {
    std::cout << subarray1Map_[i]->carmaAntNo_ << std::endl;
  }
  std::cout << "subarray 2: " << std::endl;
  for(unsigned i=0; i < subarray2Map_.size(); i++) {
    std::cout << subarray2Map_[i]->carmaAntNo_ << std::endl;
  }
  std::cout << "subarray 3: " << std::endl;
  for(unsigned i=0; i < subarray3Map_.size(); i++) {
    std::cout << subarray3Map_[i]->carmaAntNo_ << std::endl;
  }
  std::cout << "subarray 4: " << std::endl;
  for(unsigned i=0; i < subarray4Map_.size(); i++) {
    std::cout << subarray4Map_[i]->carmaAntNo_ << std::endl;
  }
  std::cout << "subarray 5: " << std::endl;
  for(unsigned i=0; i < subarray5Map_.size(); i++) {
    std::cout << subarray5Map_[i]->carmaAntNo_ << std::endl;
  }
}

AntennaMapper::Antenna* AntennaMapper::getAntenna(AntennaMapper::AntennaType type, unsigned iAnt)
{
  unsigned iAntStart=0;

  switch (type) {
  case ANT_OVRO:
    iAntStart = 0;
    break;
  case ANT_BIMA:
    iAntStart = 6;
    break;
  default:
    iAntStart = 15;
    break;
  }

  unsigned index = iAntStart + iAnt;

  if(iAnt > antennas_.size()) {
    ThrowCarmaError("Invalid index: " << iAnt << " for antenna type: " << type);
  }

  return antennas_[index];
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
carma::ui::rtd::operator<<(ostream& os, AntennaMapper::AntennaType& type)
{
  switch(type) {
  case AntennaMapper::ANT_OVRO:
    os << "OVRO";
    break;
  case AntennaMapper::ANT_BIMA:
    os << "BIMA";
    break;
  default:
    os << "SZA";
    break;
  }

  return os;
}
