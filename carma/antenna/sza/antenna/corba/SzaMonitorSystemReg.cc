#include "carma/antenna/sza/antenna/corba/SzaMonitorSystemReg.h"

#include "carma/szautil/ArrayDataFrameManager.h"

using namespace std;

using namespace sza::antenna::corba;

/**
 * Constructor.
 */
SzaMonitorSystemReg::SzaMonitorSystemReg(std::string regmapName, std::string boardName, std::string blockName, void* data) 
{
  byteOffset_  = 0;
  type_        = sza::util::DataType::UNKNOWN;
  nByte_       = 0;
  initialized_ = false;
  data_        = (unsigned char*)data;
  regmapName_  = regmapName;
  boardName_   = boardName;
  blockName_   = blockName;
};

SzaMonitorSystemReg::SzaMonitorSystemReg(const SzaMonitorSystemReg& reg) 
{
  *this = reg;
}

SzaMonitorSystemReg::SzaMonitorSystemReg(SzaMonitorSystemReg& reg) 
{
  *this = reg;
}

/**
 * Destructor.
 */
SzaMonitorSystemReg::~SzaMonitorSystemReg()
{
}

void SzaMonitorSystemReg::operator=(const SzaMonitorSystemReg& reg) 
{
  *this = (SzaMonitorSystemReg&)reg;
}

void SzaMonitorSystemReg::operator=(SzaMonitorSystemReg& reg) 
{
  byteOffset_  = reg.byteOffset_;
  type_        = reg.type_;
  nByte_       = reg.nByte_;
  initialized_ = reg.initialized_;
  data_        = reg.data_;
  regmapName_  = reg.regmapName_;
  boardName_   = reg.boardName_;
  blockName_   = reg.blockName_;
}

void SzaMonitorSystemReg::initialize(sza::util::ArrayDataFrameManager* adfm) 
{
  try {
    byteOffset_ = adfm->byteOffsetInFrameOf(regmapName_, boardName_, blockName_);
    RegMapBlock* block = adfm->findReg(regmapName_, boardName_, blockName_);
    
    type_ = sza::util::DataType::typeOf(block);
    nByte_  = block->nEl() * sza::util::DataType::sizeOf(block);
    
    initialized_ = true;
  } catch(sza::util::Exception& err) {
    COUT("Caught an error: " << err.what());
    throw err;
  }
};
	
void SzaMonitorSystemReg::pack(sza::util::ArrayDataFrameManager* adfm) 
{
  if(!initialized_) {
    initialize(adfm);
  }

  unsigned char* destPtr = adfm->frame_->getUcharPtr(byteOffset_);


  for(unsigned i=0; i < nByte_; i++) {
    destPtr[i] = data_[i];
  }
}
