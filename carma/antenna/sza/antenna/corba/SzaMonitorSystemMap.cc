#include "carma/antenna/sza/antenna/corba/SzaMonitorSystemMap.h"

#include "carma/szautil/String.h"

using namespace std;

using namespace sza::antenna::corba;
using namespace sza::util;

using namespace carma::monitor;
using namespace carma::util;

//=======================================================================
// Methods of SzaBlkTemp
//=======================================================================

SzaMonitorSystemMap::SzaBlkTemp::SzaBlkTemp()
{
  mp_         = 0;
  aregmap_    = 0;
  blk_        = 0;
  nSamp_      = 0;
  packFn_     = 0;
  ptr_        = 0;
};

/**.......................................................................
 * Pack the data from a CARMA monitor point into the array map 
 */
void SzaMonitorSystemMap::SzaBlkTemp::packData(sza::util::BitMask& bitMask) 
{
  // Pack the data for this monitor point

  if(packFn_)
    (*packFn_)(mp_, nSamp_, dfm_, aregmap_, blk_, ptr_);

  // And pack validity flags for all samples in this monitor point

  unsigned validityBitIndex;

#if 1
  bool printDebug = false;
  if(mp_ && mp_->getCanonicalName() == "Astro.Antenna22.Band23.RightPol.Usb.Tsys") {
    printDebug = true;
  }
#endif

  if(mp_) {
    for(unsigned iSamp = 0; iSamp < nSamp_; iSamp++) {

      validityBitIndex = blk_->carmaValidityBitIndex_ + iSamp;

      //      COUT("About to set bitindex: " << validityBitIndex << " iSamp = " << iSamp << " nSamp_ = " << nSamp_);
      switch (mp_->getValidity(iSamp)) {
      case MonitorPoint::INVALID_NO_DATA:
      case MonitorPoint::INVALID_NO_HW:
      case MonitorPoint::INVALID_HW_BAD:
	{
#if 1
	  if(printDebug) {
	    COUT("Setting validity high (invalid) bitindex = " << validityBitIndex);
	  }
#endif
	  bitMask.setBitHigh(validityBitIndex);
	}
	break;
      default:
	{
#if 1
	  if(printDebug) {
	    COUT("Setting validity low (valid)");
	  }
#endif
	  bitMask.setBitLow(validityBitIndex);
	}
	break;
      }
    }
  }
}

void SzaMonitorSystemMap::SzaBlkTemp::addRegister(carma::monitor::MonitorPoint* mp)
{
  // If the monitor point is null, do nothing
  
  if(mp == 0) {
    mp_    = 0;
    nSamp_ = 0;
    return;
  }
  
  mp_    = mp;
  nSamp_ = mp->getNumSamples();
  
  switch (mp->getValuetype()) {
  case MONITOR_VALUE_TYPE_BYTE:
    packFn_ = packByte;
    break;
  case MONITOR_VALUE_TYPE_SHORT:
    packFn_ = packShort;
    break;
  case MONITOR_VALUE_TYPE_INTEGER:
    packFn_ = packInt;
    break;
  case MONITOR_VALUE_TYPE_BOOLEAN:
    packFn_ = packBool;
    break;
  case MONITOR_VALUE_TYPE_FLOAT:
    packFn_ = packFloat;
    break;
  case MONITOR_VALUE_TYPE_DOUBLE:
    packFn_ = packDouble;
    break;
  case MONITOR_VALUE_TYPE_COMPLEX:
    packFn_ = packComplexFloat;
    break;
  case MONITOR_VALUE_TYPE_STRING:
    nSamp_  = 1;
    packFn_ = packString;
    break;
  case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
    packFn_ = packInt;
    break;
  default:
    ThrowError("Unhandled CARMA data type: " << mp->valuetypeToString());
    break;
  }
};

//=======================================================================
// Methods of SzaBrdTemp
//=======================================================================

SzaMonitorSystemMap::SzaBrdTemp::SzaBrdTemp() {
  nBlock_ = 0;
};

SzaMonitorSystemMap::SzaBrdTemp::~SzaBrdTemp() {
  for(std::map<std::string, SzaBlkTemp*>::iterator iter=blockMap_.begin();
      iter != blockMap_.end(); iter++) {
    delete iter->second;
  }
};

/**.......................................................................
 * Add a register associated with a CARMA monitor point
 */
void SzaMonitorSystemMap::SzaBrdTemp::addRegister(std::string blockName, carma::monitor::MonitorPoint* mp, 
						  ArrayMap* szaArrayMap) 
{
  std::map<std::string, SzaBlkTemp*>::iterator iter = blockMap_.find(blockName);
  if(iter == blockMap_.end()) {
    SzaBlkTemp* blockTemp = new SzaBlkTemp();
    blockTemp->addRegister(mp);
    blockMap_[blockName] = blockTemp;
  } else {
    SzaBlkTemp* blockTemp = iter->second;
    blockTemp->addRegister(mp);
  }

  // And add this register to the vector of register blocks managed
  // by this board

  blockVec_.push_back(RegBlockTemp("", blockName, carmaMpToFlags(mp, szaArrayMap), 0, carmaMpToNel(mp), 0, 0, 
				   mp->getUnits()));
}

/**.......................................................................
 * Add a register not associated with a CARMA monitor point
 */
void SzaMonitorSystemMap::SzaBrdTemp::addRegister(std::string blockName, unsigned flags, 
						  unsigned nEl1, unsigned nEl2, std::string units)
{
  std::map<std::string, SzaBlkTemp*>::iterator iter = blockMap_.find(blockName);
  if(iter == blockMap_.end()) {
    SzaBlkTemp* blockTemp = new SzaBlkTemp();
    blockTemp->addRegister(0);
    blockMap_[blockName] = blockTemp;
  } else {
    SzaBlkTemp* blockTemp = iter->second;
    blockTemp->addRegister(0);
  }

  // And add this register to the vector of register blocks managed
  // by this board

  blockVec_.push_back(RegBlockTemp("", blockName, flags, 0, nEl1, nEl2, 0, units));
}

//=======================================================================
// Methods of SzaRegmapTemp
//=======================================================================

SzaMonitorSystemMap::SzaRegmapTemp::SzaRegmapTemp() {
  nBoard_ = 0;
};

SzaMonitorSystemMap::SzaRegmapTemp::~SzaRegmapTemp() {
  for(std::map<std::string, SzaBrdTemp*>::iterator iter=boardMap_.begin();
      iter != boardMap_.end(); iter++) {
    delete iter->second;
  }
};

void SzaMonitorSystemMap::SzaRegmapTemp::addRegister(std::string boardName, std::string blockName, 
						     carma::monitor::MonitorPoint* mp, ArrayMap* szaArrayMap) 
{
  std::map<std::string, SzaBrdTemp*>::iterator iter = boardMap_.find(boardName);
  if(iter == boardMap_.end()) {
    SzaBrdTemp* boardTemp = new SzaBrdTemp();
    boardTemp->addRegister(blockName, mp, szaArrayMap);
    boardMap_[boardName] = boardTemp;
  } else {
    SzaBrdTemp* boardTemp = iter->second;
    boardTemp->addRegister(blockName, mp, szaArrayMap);
  }
}

void SzaMonitorSystemMap::SzaRegmapTemp::addRegister(std::string boardName, std::string blockName, unsigned flags, 
						     unsigned nEl1, unsigned nEl2, std::string units)
{
  std::map<std::string, SzaBrdTemp*>::iterator iter = boardMap_.find(boardName);
  if(iter == boardMap_.end()) {
    SzaBrdTemp* boardTemp = new SzaBrdTemp();
    boardTemp->addRegister(blockName, flags, nEl1, nEl2, units);
    boardMap_[boardName] = boardTemp;
  } else {
    SzaBrdTemp* boardTemp = iter->second;
    boardTemp->addRegister(blockName, flags, nEl1, nEl2, units);
  }
}

//=======================================================================
// Methods of SzaMonitorSystemMap
//=======================================================================

SzaMonitorSystemMap::SzaMonitorSystemMap() 
{
  initialized_  = false;
  nRegmap_      = 0;
  arrayMap_     = 0;
  arrayTemplateIsInitialized_ = false;
  registerPointersAreInitialized_ = false;

  nSlAnt_       = 23;  // SL bands can have up to 23 antennas (CARMA23 mode)
  nSlBase_      = (nSlAnt_ * (nSlAnt_-1))/2;
  iSlBandStart_ =  1;  
  nSlBandMax_   =  8;
  nChan_        = 17;  // Raw correlator data preserves ALL channels,
		       // including the end channels


  nWbAnt_       =  8;
  nWbBase_      = (nWbAnt_ * (nWbAnt_-1))/2;
  iWbBandStart_ =  9;  
  nWbBandMax_   = 16;

  nAntTotal_    = 23;

  // And keep a reference to the original SZA array map from which the
  // CARMA mpml is ultimately derived

  szaArrayMap_ = 0;
  szaArrayMap_ = new_SzaArrayMap();

  longestStringLen_ = 0;
  nValidityFlags_   = 0;
  nRegs_            = 0;
};

SzaMonitorSystemMap::~SzaMonitorSystemMap() 
{
  if(szaArrayMap_) {
    szaArrayMap_ = del_SzaArrayMap(szaArrayMap_);
  }

  for(std::map<std::string, SzaRegmapTemp*>::iterator iter=regmapMap_.begin();
      iter != regmapMap_.end(); iter++) {
    delete iter->second;
  }
  
  if(arrayMap_)
    delete arrayMap_;
}

void SzaMonitorSystemMap::addWbCorrelatorRegisters(bool coherenceMonitor)
{
  std::ostringstream os;

  for(unsigned iBand=iWbBandStart_; iBand < iWbBandStart_+nWbBandMax_; iBand++) {
    os.str("");
    os << "Astroband" << iBand;
    addCorrelatorBoard("corr", os.str(), nWbAnt_, coherenceMonitor);
  }

  initialized_ = true;
}

void SzaMonitorSystemMap::addSlCorrelatorRegisters(bool coherenceMonitor)
{
  std::ostringstream os;

  for(unsigned iBand=iSlBandStart_; iBand < iSlBandStart_+nSlBandMax_; iBand++) {
    os.str("");
    os << "Astroband" << iBand;
    addCorrelatorBoard("corr", os.str(), nSlAnt_, coherenceMonitor);
  }

  initialized_ = true;
}

void SzaMonitorSystemMap::addCorrelatorBoard(std::string regmapName, std::string boardName, unsigned nAnt, bool coherenceMonitor)
{
  unsigned nBase = (nAnt * (nAnt-1))/2;

  addRegister(regmapName, boardName, "received",       REG_UCHAR|REG_UNION, 
	      1);

  addRegister(regmapName, boardName, "baselineReceived", REG_UCHAR|REG_UNION, 
	      nBase);

  addRegister(regmapName, boardName, "source",         REG_UCHAR|REG_STRING, 
	      12);

  addRegister(regmapName, boardName, "nUsb",           REG_UINT, 
	      1);

  addRegister(regmapName, boardName, "tIntUsb",        REG_FLOAT, 
	      1);

  addRegister(regmapName, boardName, "usb",            REG_PREAVG|REG_COMPLEX|REG_FLOAT, 
	      nBase, nChan_);

  addRegister(regmapName, boardName, "usbValid",       REG_INT, nSlBase_, 
	      nChan_);
  
  addRegister(regmapName, boardName, "usbAvg",         REG_PREAVG|REG_COMPLEX|REG_FLOAT, 
	      nBase);
  
  addRegister(regmapName, boardName, "usbVar",         REG_PREAVG|REG_COMPLEX|REG_FLOAT, 
	      nBase);
  
  if(coherenceMonitor) {
    addRegister(regmapName, boardName, "usbCoherence", REG_PREAVG|REG_FLOAT, 
		nBase);
    addRegister(regmapName, boardName, "usbIsCoherent", REG_UCHAR,
		nBase);
    addRegister(regmapName, boardName, "usbAntCoherence", REG_FLOAT,
		nAnt);

    addRegister(regmapName, boardName, "lsbCoherence", REG_PREAVG|REG_FLOAT, 
		nBase);
    addRegister(regmapName, boardName, "lsbIsCoherent", REG_UCHAR,
		nBase);
    addRegister(regmapName, boardName, "lsbAntCoherence", REG_FLOAT,
		nAnt);
  }

  addRegister(regmapName, boardName, "nLsb",           REG_UINT, 1);
  
  addRegister(regmapName, boardName, "tIntLsb",        REG_FLOAT, 1);
  
  addRegister(regmapName, boardName, "lsb",            REG_PREAVG|REG_COMPLEX|REG_FLOAT, 
	      nBase, nChan_);
    
  addRegister(regmapName, boardName, "lsbValid",       REG_INT, nSlBase_, 
	      nChan_);
    
  addRegister(regmapName, boardName, "lsbAvg",         REG_PREAVG|REG_COMPLEX|REG_FLOAT, 
	      nBase);
  
  addRegister(regmapName, boardName, "lsbVar",         REG_PREAVG|REG_COMPLEX|REG_FLOAT, 
	      nBase);

  addRegister(regmapName, boardName, "nAuto",          REG_UINT, 
	      1);

  addRegister(regmapName, boardName, "tIntAuto",       REG_FLOAT, 
	      1);
  
  addRegister(regmapName, boardName, "auto",           REG_PREAVG|REG_FLOAT,      
	      nAnt, nChan_);
  
  addRegister(regmapName, boardName, "autoValid",      REG_INT, 
	      nAnt, nChan_);
    
  addRegister(regmapName, boardName, "autoAvg",        REG_PREAVG|REG_FLOAT,      
	      nAnt);
  
  addRegister(regmapName, boardName, "autoVar",        REG_PREAVG|REG_FLOAT,      
	      nAnt);
  
  addRegister(regmapName, boardName, "centerFrequency",REG_PREAVG|REG_FLOAT,      
	      1);
  
  addRegister(regmapName, boardName, "frequency",      REG_PREAVG|REG_FLOAT,      
	      nChan_);

  addRegister(regmapName, boardName, "lsbFrequency",   REG_PREAVG|REG_FLOAT,      
	      nChan_);

  addRegister(regmapName, boardName, "usbFrequency",   REG_PREAVG|REG_FLOAT,      
	      nChan_);

  addRegister(regmapName, boardName, "autoFrequency",  REG_PREAVG|REG_FLOAT,      
	      nChan_);
}

/**.......................................................................
 * Take a canonical CARMA monitor point string and turn it into a
 * SZA-style string hierarchy
 */
void SzaMonitorSystemMap::parseIntoRegisterStrings(carma::monitor::MonitorPoint* mp, 
						   String& regmapName, String& boardName, String& blockName)
{
  String str(mp->getCanonicalName());

  // Get the register map name -- this is always the first
  // '.'-separated string in the canonical name

  regmapName = str.findNextInstanceOf("", false, ".", true, true);

  // Board name is harder.  SZA register map only allows three levels
  // of hierarchy.  Therefore, I concatenate all additional substrings
  // between the first and last into one 'boardName'.

  std::ostringstream boardNameOs;
  boardNameOs.str("");

  bool first=true;

  String tmpStr;
  do {

    tmpStr = str.findNextInstanceOf("", false, ".", true, true);

    if(!tmpStr.isEmpty()) {
      if(first) {
	boardNameOs << tmpStr.str();
	first = false;
      } else {
	boardNameOs << "." << tmpStr.str();
      }
    }
    
  } while(str.remainderContains("."));
  
  // Also, CARMA allows for only two levels of hierarchy.  For SZA, I
  // enforce three by adding a bogus board name: Subsystem

  if(boardNameOs.str().size() == 0)
    boardNameOs << "Subsystem";
  
  boardName = boardNameOs.str();

  // Finally, whatever is left is the block name

  blockName = str.remainder();
}

/**.......................................................................
 * Take the vector of all CARMA monitor points, and turn it into an
 * SZA-style array map
 */
void SzaMonitorSystemMap::constructArrayMapFromCarmaMonitorSystem(std::vector<carma::monitor::MonitorPoint*>& mpVec)
{
  unsigned nMp=mpVec.size();
  sza::util::String regmapName, blockName, tmpStr;
  std::ostringstream boardName;

  struct timespec ts;
  ts.tv_sec  =  0;
  ts.tv_nsec = 1000;

  // Iterate over all monitor points, constructing SZA-style register
  // map names for each one

  for(unsigned iMp=0; iMp < nMp; iMp++) {

    carma::monitor::MonitorPoint* mp = mpVec[iMp];

    String str(mp->getCanonicalName());

    regmapName = str.findNextInstanceOf("", false, ".", true, true);

    boardName.str("");
    bool first=true;

    do {

      tmpStr = str.findNextInstanceOf("", false, ".", true, true);

      if(!tmpStr.isEmpty()) {
	if(first) {
	  boardName << tmpStr.str();
	  first = false;
	} else {
	  boardName << "." << tmpStr.str();
	}
      }

    } while(str.remainderContains("."));

    if(boardName.str().size() == 0)
      boardName << "Subsystem";

    blockName = str.remainder();

    addRegister(regmapName.str(), boardName.str(), blockName.str(), mp);
  }

  initialized_ = true;
}

/**.......................................................................
 * Add a register associated with a CARMA monitor point
 */
void SzaMonitorSystemMap::addRegister(std::string regmapName, std::string boardName, std::string blockName, 
				      carma::monitor::MonitorPoint* mp)
{
#if 1
  if(regmapName.length() > longestStringLen_) {
    longestStringLen_ = regmapName.length();
  }

  if(boardName.length() > longestStringLen_) {
    longestStringLen_ = boardName.length();
  }

  if(blockName.length() > longestStringLen_) {
    longestStringLen_ = blockName.length();
  }
#endif

  std::map<std::string, SzaRegmapTemp*>::iterator iter = regmapMap_.find(regmapName);
  if(iter == regmapMap_.end()) {
    SzaRegmapTemp* regmapTemp = new SzaRegmapTemp();
    regmapTemp->addRegister(boardName, blockName, mp, szaArrayMap_);
    regmapMap_[regmapName] = regmapTemp;
  } else {
    SzaRegmapTemp* regmapTemp = iter->second;
    regmapTemp->addRegister(boardName, blockName, mp, szaArrayMap_);
  }

  // CARMA registers get one validity flag for strings, and one per sample if not

  nValidityFlags_ += mp->getValuetype()==MONITOR_VALUE_TYPE_STRING ? 1 : mp->getNumSamples();
  ++nRegs_;
}

/**.......................................................................
 * Add a register not associated with a CARMA monitor point
 */
void SzaMonitorSystemMap::addRegister(std::string regmapName, std::string boardName, std::string blockName, 
				      unsigned flags, unsigned nEl1, unsigned nEl2, std::string units)
{
  std::map<std::string, SzaRegmapTemp*>::iterator iter = regmapMap_.find(regmapName);
  if(iter == regmapMap_.end()) {
    SzaRegmapTemp* regmapTemp = new SzaRegmapTemp();
    regmapTemp->addRegister(boardName, blockName, flags, nEl1, nEl2, units);
    regmapMap_[regmapName] = regmapTemp;
  } else {
    SzaRegmapTemp* regmapTemp = iter->second;
    regmapTemp->addRegister(boardName, blockName, flags, nEl1, nEl2, units);
  }

  // Non-carma registers get one validity flag each, regardless of
  // length

  ++nValidityFlags_;
  ++nRegs_;
}

/**.......................................................................
 * Take our internal hierarchy of blocks and turn it into an SZA-style
 * array template
 */
void SzaMonitorSystemMap::generateArrayTemplate()
{
  if(!initialized_) {
    ThrowError("Monitor point hierarchy has not been initialized");
  }

  arrayTemplate_.ntemplate = regmapMap_.size();
  arrayTemplate_.templates = (RegTemp*)malloc(sizeof(RegTemp) * arrayTemplate_.ntemplate);

  // Now iterate over regmaps

  unsigned iRegMap=0;
  for(std::map<std::string, SzaRegmapTemp*>::iterator regmapIter=regmapMap_.begin();
      regmapIter != regmapMap_.end(); regmapIter++, iRegMap++) {

    SzaRegmapTemp* regmapTemp = regmapIter->second;

    RegTemp& regTemp = arrayTemplate_.templates[iRegMap];
    strncpy(regTemp.name, (regmapIter->first).c_str(), REG_NAME_LEN);
    regTemp.regtemplate = (RegTemplate*)malloc(sizeof(RegTemplate));
      
    RegTemplate* regTemplate = regTemp.regtemplate;
    regTemplate->nboard = regmapTemp->boardMap_.size();
    regTemplate->boards = (RegBoardTemp*)malloc(sizeof(RegBoardTemp) * regTemplate->nboard);
    
    // For this regmap, iterate over boards

    unsigned iBoard=0;
    for(std::map<std::string, SzaBrdTemp*>::iterator brdIter=regmapTemp->boardMap_.begin();
	brdIter != regmapTemp->boardMap_.end(); brdIter++, iBoard++) {

      // For this Board, iterate over blocks

      SzaBrdTemp* brdTemp = brdIter->second;
      RegBoardTemp& regBoardTemp = regTemplate->boards[iBoard];
      strncpy(regBoardTemp.name_, (brdIter->first).c_str(), REG_NAME_LEN);
      regBoardTemp.nblock_ = brdTemp->blockMap_.size();

      // Set the blocks pointer to the previsouly allocated vector
      // of blocks

      regBoardTemp.blocks_ = &brdTemp->blockVec_[0];
    }
  }

  arrayTemplateIsInitialized_ = true;
};

void SzaMonitorSystemMap::generateArrayMap() 
{
  if(!arrayTemplateIsInitialized_) {
    generateArrayTemplate();
    arrayMap_ = new ArrayMap((void*)(&arrayTemplate_), false, false);
  }
}

ArrayTemplate* SzaMonitorSystemMap::getArrayTemplate() 
{
  if(!arrayTemplateIsInitialized_) {
    generateArrayTemplate();
  }

  return &arrayTemplate_;
};

ArrayMap* SzaMonitorSystemMap::getArrayMap() 
{
  if(!arrayMap_) {
    generateArrayMap();
  }

  return arrayMap_;
};

void SzaMonitorSystemMap::print()
{
  for(std::map<std::string, SzaRegmapTemp*>::iterator regmapIter=regmapMap_.begin();
      regmapIter != regmapMap_.end(); regmapIter++) {

    SzaRegmapTemp* regmapTemp = regmapIter->second;
    for(std::map<std::string, SzaBrdTemp*>::iterator brdIter=regmapTemp->boardMap_.begin();
	brdIter != regmapTemp->boardMap_.end(); brdIter++) {

      SzaBrdTemp* brdTemp = brdIter->second;
      for(std::map<std::string, SzaBlkTemp*>::iterator blkIter=brdTemp->blockMap_.begin();
	  blkIter != brdTemp->blockMap_.end(); blkIter++) {
	COUT(regmapIter->first << "." << brdIter->first << "." << blkIter->first);
      }
    }
  }
}

/**.......................................................................
 * Iterate through our monitor point hierarchy, setting up pointers to
 * the destination data frame for each block in the hierarchy.
 */
void SzaMonitorSystemMap::setupRegisterPointers(ArrayDataFrameManager& dfm)
{
  ArrRegMap* aregmap = 0;

  for(std::map<std::string, SzaRegmapTemp*>::iterator regmapIter=regmapMap_.begin();
      regmapIter != regmapMap_.end(); regmapIter++) {

    aregmap = dfm.getArrReg(regmapIter->first);

    SzaRegmapTemp* regmapTemp = regmapIter->second;
    for(std::map<std::string, SzaBrdTemp*>::iterator brdIter=regmapTemp->boardMap_.begin();
	brdIter != regmapTemp->boardMap_.end(); brdIter++) {

      SzaBrdTemp* brdTemp = brdIter->second;
      for(std::map<std::string, SzaBlkTemp*>::iterator blkIter=brdTemp->blockMap_.begin();
	  blkIter != brdTemp->blockMap_.end(); blkIter++) {

	SzaBlkTemp* blkTemp = blkIter->second;
	RegMapBlock* blk = dfm.findReg(regmapIter->first, brdIter->first, blkIter->first);

	blkTemp->dfm_        = &dfm;
	blkTemp->aregmap_    = aregmap;
	blkTemp->blk_        = blk;
	blkTemp->ptr_        = dfm.frame()->getPtr(dfm.byteOffsetInFrameOf(regmapIter->first, brdIter->first, blkIter->first),
						   DataType::typeOf(blk));
      }
    }
  }

  registerPointersAreInitialized_ = true;
}

/**.......................................................................
 * Read all values out of the CARMA monitor stream, and pack into the
 * data frame
 */
void SzaMonitorSystemMap::packData()
{
  if(!registerPointersAreInitialized_) {
    ThrowError("Register pointers haven't been initialized");
  }

  for(std::map<std::string, SzaRegmapTemp*>::iterator regmapIter=regmapMap_.begin();
      regmapIter != regmapMap_.end(); regmapIter++) {

    SzaRegmapTemp* regmapTemp = regmapIter->second;
    for(std::map<std::string, SzaBrdTemp*>::iterator brdIter=regmapTemp->boardMap_.begin();
	brdIter != regmapTemp->boardMap_.end(); brdIter++) {
      
      SzaBrdTemp* brdTemp = brdIter->second;
      for(std::map<std::string, SzaBlkTemp*>::iterator blkIter=brdTemp->blockMap_.begin();
	  blkIter != brdTemp->blockMap_.end(); blkIter++) {
	
	SzaBlkTemp* blkTemp = blkIter->second;

	//	COUT("Packing: " << blkTemp->aregmap_->name << " " << blkTemp->blk_->name_);
	blkTemp->packData(validityBitMask_);
      }
    }
  }
}

//=======================================================================
// Global functions
//=======================================================================

unsigned SzaMonitorSystemMap::carmaMpToFlags(carma::monitor::MonitorPoint* mp, ArrayMap* szaArrayMap)
{
  unsigned flags = REG_NONE;

  switch (mp->getValuetype()) {
  case MONITOR_VALUE_TYPE_BYTE:
    flags |= REG_UCHAR;
    addSzaFlags(mp, flags, szaArrayMap);
    break;
  case MONITOR_VALUE_TYPE_SHORT:
    flags |= REG_SHORT;
    if(!addSzaFlags(mp, flags, szaArrayMap)) {
      flags |= REG_PREAVG;
    }
    break;
  case MONITOR_VALUE_TYPE_INTEGER:
    flags |= REG_INT;
    if(!addSzaFlags(mp, flags, szaArrayMap)) {
      flags |= REG_PREAVG;
    }
    break;
  case MONITOR_VALUE_TYPE_BOOLEAN:
    flags |= REG_BOOL;
    addSzaFlags(mp, flags, szaArrayMap);
    break;
  case MONITOR_VALUE_TYPE_FLOAT:
    flags |= REG_FLOAT;
    if(!addSzaFlags(mp, flags, szaArrayMap)) {
      flags |= REG_PREAVG;
    }
    break;
  case MONITOR_VALUE_TYPE_DOUBLE:
    flags |= REG_DOUBLE;
    if(!addSzaFlags(mp, flags, szaArrayMap)) {
      flags |= REG_PREAVG;
    }
    break;
  case MONITOR_VALUE_TYPE_COMPLEX:
    flags |= REG_FLOAT|REG_COMPLEX;
    if(!addSzaFlags(mp, flags, szaArrayMap)) {
      flags |= REG_PREAVG;
    }
    break;
  case MONITOR_VALUE_TYPE_STRING:
    flags |= REG_UCHAR|REG_STRING;
    addSzaFlags(mp, flags, szaArrayMap);
    break;
  case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
    flags |= REG_INT;
    addSzaFlags(mp, flags, szaArrayMap);
    break;
  default:
    ThrowError("Unhandled CARMA data type: " << mp->valuetypeToString());
    break;
  }

  // Add any special CARMA-specific flags

  addCarmaFlags(mp, flags);

  return flags;
}

void SzaMonitorSystemMap::addCarmaFlags(carma::monitor::MonitorPoint* mp, unsigned& flags)
{
  std::string str(mp->getCanonicalName());

  if(str == "SlPipeline.IntegratorStageContainer.IntegratorStage.integrationNumber") {
    flags |= REG_FIRST;
  } else if(str == "WbPipeline.IntegratorStageContainer.IntegratorStage.integrationNumber") {
    flags |= REG_FIRST;
  }

}

/**.......................................................................
 * Return the flags descriptor for the matching SZA monitor point, if any.
 *
 * If no match, this method throws
 */
RegMapBlock* SzaMonitorSystemMap::getSzaBlock(ArrayMap* arrayMap, String& regmapName, String& boardName, String& blockName)
{
  ostringstream antName;

  String antNoStr = regmapName.findNextInstanceOf("Sza", true, ".", false, true);
  unsigned antNo = antNoStr.toInt() - 1;      

  antName << "antenna" << antNo;

  // Overwrite the register-map name with the SZA version

  regmapName = antName.str();

  // SZA boards and blocks all start with lower-case letters
    
  boardName = boardName.firstToLower();
  blockName = blockName.firstToLower();

  // Look for a matching register map

  ArrRegMap* arrRegMap = find_ArrRegMap(arrayMap, regmapName.str().c_str());

  if(!arrRegMap)
    ThrowError("No register map matching: " << regmapName.str());

  RegMapBoard* board = find_RegMapBoard(arrRegMap->regmap, boardName.str().c_str());

  if(!board)
    ThrowError("No board matching: " << boardName.str().c_str());

  // Now iterate over all blocks of this board, looking for a match

  for(std::map<std::string, RegMapBlock*>::iterator iter=board->blockMap_.begin(); iter != board->blockMap_.end(); iter++) {
    if(blockName.contains(iter->first)) {
      return iter->second;
    }
  }

  ThrowError("No block matching: " << blockName.str().c_str());
}

/**.......................................................................
 * If this CARMA monitor point has a match in the SZA register map,
 * use the SZA flags, and return true.  Else return false.
 */
bool SzaMonitorSystemMap::addSzaFlags(carma::monitor::MonitorPoint* mp, unsigned& flags, ArrayMap* szaArrayMap)
{
  // If the register map is null, do nothing

  if(szaArrayMap == 0)
    return false;

  // See if this mp has a match in the SZA array map

  String regmapName, boardName, blockName;
  ostringstream antName;

  parseIntoRegisterStrings(mp, regmapName, boardName, blockName);

  if(regmapName.contains("Sza")) {

    try {

      //      COUT("Searching for: " << regmapName << "." << boardName << "." << blockName);

      RegMapBlock* block = getSzaBlock(szaArrayMap, regmapName, boardName, blockName);

      if(block->isSummed()) {
	//COUT("Block is summed");
	flags |= REG_SUM;
      }
      
      if(block->isUnioned()) {
	//	COUT("Block is unioned");
	flags |= REG_UNION;
      }

      if(block->isPreAveraged()) {
	//	COUT("Block is preaveraged");
	flags |= REG_PREAVG;
      }

      if(block->isPostAveraged()) {
	//	COUT("Block is postaveraged");
	flags |= REG_POSTAVG;
      }

      //      COUT("block was found");
      return true;

    } catch(...) {

      //      COUT("NO match found");
      // Deliberate drop-through to end of function

    }
  }

  return false;
}

unsigned SzaMonitorSystemMap::carmaMpToNel(carma::monitor::MonitorPoint* mp)
{
  switch (mp->getValuetype()) {
  case MONITOR_VALUE_TYPE_STRING:
    return MAX_CARMA_STRING_LENGTH+1;
    break;
  default:
    return mp->getNumSamples();
    break;
  }
}

//=======================================================================
// MP pack functions
//=======================================================================

MP_PACK_FN(SzaMonitorSystemMap::packBool)
{
  bool* bval = (bool*)ptr;

  for(unsigned i=0; i < nSamp; i++) {
    bval[i] = mp->getMonitorPointSample(i).getMonitorValue().bo;
  }
}

MP_PACK_FN(SzaMonitorSystemMap::packByte)
{
  unsigned char* cval = (unsigned char*)ptr;

  for(unsigned i=0; i < nSamp; i++) {
    cval[i] = mp->getMonitorPointSample(i).getMonitorValue().byte;
  }
}

MP_PACK_FN(SzaMonitorSystemMap::packShort)
{
  short* sval = (short*)ptr;

  for(unsigned i=0; i < nSamp; i++) {
    sval[i] = mp->getMonitorPointSample(i).getMonitorValue().sh;
  }
}

MP_PACK_FN(SzaMonitorSystemMap::packInt)
{
  int* ival = (int*)ptr;

  for(unsigned i=0; i < nSamp; i++) {
    ival[i] = (int)mp->getMonitorPointSample(i).getMonitorValue().lo;
  }
}

MP_PACK_FN(SzaMonitorSystemMap::packFloat)
{
  float* fval = (float*)ptr;

  for(unsigned i=0; i < nSamp; i++) {
    fval[i] = (float)mp->getMonitorPointSample(i).getMonitorValue().fl;
  }
}

MP_PACK_FN(SzaMonitorSystemMap::packDouble)
{
  double* dval = (double*)ptr;

  for(unsigned i=0; i < nSamp; i++) {
    dval[i] = (double)mp->getMonitorPointSample(i).getMonitorValue().db;
  }
}

MP_PACK_FN(SzaMonitorSystemMap::packComplexFloat)
{
  Complex<float>::Data* cval = (Complex<float>::Data*)ptr;

  MonitorValue val;
  for(unsigned i=0; i < nSamp; i++) {
    val = mp->getMonitorPointSample(i).getMonitorValue();
    cval[i].real_ = val.complex[0];
    cval[i].imag_ = val.complex[1];
  }
}

MP_PACK_FN(SzaMonitorSystemMap::packString)
{
  std::string str = mp->getValueToString(0);
  unsigned char* cval = (unsigned char*)ptr;
  strncpy((char*)cval, str.c_str(), MAX_CARMA_STRING_LENGTH);
}

ArrayTemplate* SzaMonitorSystemMap::getFakeArrayTemplate()
{
  ArrayTemplate* arrayTemplate = (ArrayTemplate*)malloc(sizeof(ArrayTemplate));

  arrayTemplate->templates = (RegTemp*)malloc(sizeof(RegTemp));
  arrayTemplate->ntemplate = 1;

  RegTemp* regtmp = arrayTemplate->templates;
  regtmp->regtemplate = (RegTemplate*)malloc(sizeof(RegTemplate));
  strcpy(regtmp->name, "A template");

  RegTemplate* rtmp = regtmp->regtemplate;
  rtmp->boards = (RegBoardTemp*)malloc(sizeof(RegBoardTemp));
  rtmp->nboard = 1;

  RegBoardTemp* brd = rtmp->boards;
  strcpy(brd->name_, "A board");
  brd->blocks_ = (RegBlockTemp*)malloc(sizeof(RegBlockTemp));
  brd->nblock_ = 1;
  
  RegBlockTemp* blk = brd->blocks_;
  strcpy(blk->name_, "A block");

  blk->axes_         = new CoordAxes(100);
  blk->comment_      = new std::string("A comment");
  blk->carmaUnits_   = new std::string("CARMA units");

  blk->carmaErrors_  = new std::vector<std::pair<std::string, std::string> >;
  blk->carmaErrors_->push_back(std::pair<std::string, std::string>("CARMA low", "CARMA high"));

  blk->base_      = REG_BASE0;
  blk->addr_mode_ = ADDR_DEFAULT;
  blk->flags_     = REG_DEFAULT;

  return arrayTemplate;
}

/**.......................................................................
 * Initialize the validity bitmask
 */
unsigned SzaMonitorSystemMap::initializeValidityBitMask()
{
  // We resize to nValidityFlags_ + 1 since we will subsequently add
  // the validity register itself

  //  COUT("resizeing validity bitmask to: " << nValidityFlags_+1);

  validityBitMask_.resize(nValidityFlags_+1);

  // Validity bits will be low if valid, high if invalid.  This is so
  // that we can trivially integrate frames by ORing the bits
  // together, with the result that if the register was invalid at any
  // point (bit high), the integrated version will be flagged as
  // invalid too.

  validityBitMask_.setAllBitsLow();

  return validityBitMask_.byteVecPtr_->size();
}

/**.......................................................................
 * Initialize validity bit indices
 */
void SzaMonitorSystemMap::initializeValidityBitIndices()
{
  // Now that we've resized, iterate through the register hierarchy to
  // initialize validity flags for registers whose validity can't
  // change, and to initialize the indices into the validity bitmask
  // for each register

  unsigned validityBitIndex=0;

  for(std::map<std::string, SzaRegmapTemp*>::iterator regmapIter=regmapMap_.begin();
      regmapIter != regmapMap_.end(); regmapIter++) {
    
    SzaRegmapTemp* regmapTemp = regmapIter->second;
    for(std::map<std::string, SzaBrdTemp*>::iterator brdIter=regmapTemp->boardMap_.begin();
	brdIter != regmapTemp->boardMap_.end(); brdIter++) {
      
      SzaBrdTemp* brdTemp = brdIter->second;
      unsigned iBlock=0;
      for(std::map<std::string, SzaBlkTemp*>::iterator blkIter=brdTemp->blockMap_.begin();
	  blkIter != brdTemp->blockMap_.end(); blkIter++, iBlock++) {

	SzaBlkTemp*   blkTemp = blkIter->second;
	RegBlockTemp& blk     = brdTemp->blockVec_[iBlock];

	unsigned nSamp = 0;

	blk.carmaValidityBitIndex_ = validityBitIndex;

#if 0
	if(strstr(blk.name_, "loFreq") != 0) {
	  COUT("Just set validity bit index to " << blk.carmaValidityBitIndex_ << " for block " << blk.name_);
	}
#endif

	if(blkTemp->mp_ == 0) {
	  nSamp = 1;
	} else {
	  if(blkTemp->mp_->getValuetype() == MONITOR_VALUE_TYPE_STRING) {
	    nSamp = 1;
	  } else {
	    nSamp = blkTemp->mp_->getNumSamples();
	  }
	}

	// And increment the validity bit index by the number of
	// samples in this monitor point

	validityBitIndex += nSamp;
      }
    }
  }
}

unsigned char* SzaMonitorSystemMap::getValidityPtr()
{
  return &(*validityBitMask_.byteVecPtr_)[0];
}
