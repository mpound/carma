#include "carma/antenna/sza/antenna/corba/MpFileReader.h"

#include "carma/szautil/ArrayDataFrameManager.h"
#include "carma/szautil/Exception.h"

using namespace std;

using namespace sza::antenna::corba;

/**.......................................................................
 * Constructor.
 */
MpFileReader::MpFileReader() 
{
  finished_          = true;
  lastFrameCount_    = 0;
  currentFrameCount_ = 0;
  isNumeric_         = true;
}

/**.......................................................................
 * Destructor.
 */
MpFileReader::~MpFileReader() {}

void MpFileReader::closeFile()
{
  if(ifStr_.is_open()) {
    ifStr_.close();
  }
}

void MpFileReader::setNumeric(bool isNumeric) 
{
  isNumeric_ = isNumeric;
}

void MpFileReader::loadFile(std::string fileName)
{
  ifStr_.open(fileName.c_str(), ios::in);
  ifStr_.clear();

  if(!ifStr_.is_open()) {
    ThrowSimpleError("Couldn't open file: " << fileName);
  }

  // Advance to the first real record in the file

  readNextLine();
  readNextLine();

  // And set the current and last frame count to the first one in the file

  lastFrameCount_ = currentFrameCount_;
}

void MpFileReader::readNextLine()
{
  if(getline(ifStr_, record_.str_)) {
    lastFrameCount_ = currentFrameCount_;

    if(isNumeric_) {
      sscanf(record_.str_.c_str(), "%d%d%d%d%lf%lf%lf%d%d%d", &record_.frameCount_, &record_.tagId_,
	     &record_.blanking_, &record_.validity_, 
	     &record_.avg_, &record_.max_, &record_.min_, // Data
	     &record_.avgIndex_, &record_.numTotalSamps_, &record_.numValidSamps_);

      if(record_.tagId_ == 1507630) {
	COUT("Numeric value read is: " << record_.avg_);
      }

    } else {
      sscanf(record_.str_.c_str(), "%d%d%d%d%s%d%d", &record_.frameCount_, &record_.tagId_,
	     &record_.blanking_, &record_.validity_, 
	     record_.strVal_,  // Data
	     &record_.numTotalSamps_, &record_.numValidSamps_);

      if(record_.tagId_ == 2162780) {
	COUT("Value read is: " << record_.strVal_);
      }
    }

    currentFrameCount_ = record_.frameCount_;
    finished_ = false;
  } else {
    finished_ = true;
  }
}

void MpFileReader::packNextRecord(sza::util::ArrayDataFrameManager* fm, std::map<unsigned, SzaRegister>& tagIdToSzaRegisterMap)
{
  while(!atEnd() && (currentFrameCount_ == lastFrameCount_)) {

    if(record_.tagId_ == 1507630) {
      COUT("Found 1507630");
    }

    SzaRegister* reg = findSzaRegister(tagIdToSzaRegisterMap, record_.tagId_);

    if(record_.tagId_ == 1507630) {
      COUT("reg = " << reg);
    }

    if(reg) {
      writeCarmaReg(fm, reg);
    }

    readNextLine();
  }
}

/**.......................................................................                                                  
 * Find a slot by id number                                                                                                 
 */
SzaRegister* MpFileReader::findSzaRegister(std::map<unsigned, SzaRegister>& tagIdToSzaRegisterMap, unsigned tagId)
{
  std::map<unsigned, SzaRegister>::iterator slot = tagIdToSzaRegisterMap.find(tagId);
  
  if(slot != tagIdToSzaRegisterMap.end())
    return &(slot->second);
  
  return 0;
}

bool MpFileReader::atEnd()
{
  return finished_;
}

std::ostream& sza::antenna::corba::operator<<(std::ostream& os, MpFileReader::MpRecord& record) 
{
  os << record.frameCount_ << " " << record.tagId_ << " " << record.avg_;
  return os;
}

void MpFileReader::writeCarmaReg(sza::util::ArrayDataFrameManager* frame, SzaRegister* reg)
{
  static sza::util::CoordRange range;

  // If the CARMA register is mapped to a specific index of the SZA
  // register, then override the avgIndex from the monitor point file.
  // Otherwise, use the specified index

  if(reg->index_ >= 0) {
    range.setStartIndex(0, reg->index_);
    range.setStopIndex( 0, reg->index_);
  } else {
    range.setStartIndex(0, record_.avgIndex_);
    range.setStopIndex( 0, record_.avgIndex_);
  }

  if(reg->block_->flags_ & REG_DOUBLE) {
    frame->writeReg(reg->arrRegMap_, reg->block_,                  record_.avg_,    &range);
  } else if(reg->block_->flags_ & REG_FLOAT) {
    frame->writeReg(reg->arrRegMap_, reg->block_,           (float)record_.avg_,    &range);
  } else if(reg->block_->flags_ & REG_INT) {
    frame->writeReg(reg->arrRegMap_, reg->block_,             (int)record_.avg_,    &range);
  } else if(reg->block_->flags_ & REG_UINT) {
    frame->writeReg(reg->arrRegMap_, reg->block_,    (unsigned int)record_.avg_,    &range);
  } else if(reg->block_->flags_ & REG_SHORT) {
    frame->writeReg(reg->arrRegMap_, reg->block_,             (short)record_.avg_,  &range);
  } else if(reg->block_->flags_ & REG_USHORT) {
    frame->writeReg(reg->arrRegMap_, reg->block_,    (unsigned short)record_.avg_,  &range);
  } else if(reg->block_->flags_ & REG_UCHAR) {

    if(isNumeric_)
      frame->writeReg(reg->arrRegMap_, reg->block_, (unsigned char)record_.avg_,    &range);
    else {
      frame->writeReg(reg->arrRegMap_, reg->block_,               record_.strVal_);
    }

  }
}
