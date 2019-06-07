#include "carma/szautil/NetArrayTemplate.h"
#include <cstring>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetArrayTemplate::NetArrayTemplate(ArrayTemplate* arrayTemplate) 
{
  setTo(arrayTemplate);
}

/**.......................................................................
 * Constructor.
 */
NetArrayTemplate::NetArrayTemplate() 
{
  external_      = false;
  arrayTemplate_ = 0;
}

void NetArrayTemplate::setTo(ArrayTemplate* arrayTemplate) 
{
  external_      = true;
  arrayTemplate_ = arrayTemplate;
}

/**.......................................................................
 * Destructor.
 */
NetArrayTemplate::~NetArrayTemplate() 
{
  if(arrayTemplate_ && !external_) {
    free(arrayTemplate_);
    arrayTemplate_ = 0;
  } 
}

unsigned NetArrayTemplate::size()
{
  if(!arrayTemplate_) {
    return 0;
  } else {
    return sizeOf(arrayTemplate_);
  }
}

unsigned NetArrayTemplate::sizeOf(ArrayTemplate* arrayTemplate)
{
  unsigned size = 0;

  // Store the number of register map templates

  size += sizeof(arrayTemplate_->ntemplate);

  // The regmaps themselves

  for(unsigned iRegTemp=0; iRegTemp < arrayTemplate_->ntemplate; iRegTemp++) 
    size += sizeOf(arrayTemplate_->templates[iRegTemp]);

  return size;
}

unsigned NetArrayTemplate::sizeOf(RegTemp& regTemp)
{
  unsigned size = 0;

  // The size of name and name of this regmap

  unsigned short nameLen = strlen(regTemp.name);
  size += sizeof(nameLen);
  size += nameLen;

  // The number of boards in this regmap

  size += sizeof(regTemp.regtemplate->nboard);

  // The boards themselves

  for(unsigned iBoard=0; iBoard < regTemp.regtemplate->nboard; iBoard++) 
    size += sizeOf(regTemp.regtemplate->boards[iBoard]);

  // The size of the comment field

  unsigned short commentLen = strlen(regTemp.comment_);
  size += sizeof(commentLen);
  size += commentLen;

  return size;
}

unsigned NetArrayTemplate::sizeOf(RegBoardTemp& boardTemp)
{
  unsigned size = 0;

  // Store the name of this board

  unsigned short nameLen = strlen(boardTemp.name_);
  size += sizeof(nameLen);
  size += nameLen;

  // Store the number of blocks

  size += sizeof(boardTemp.nblock_);

  // Store the array of base addresses

  size += NBASE * sizeof(boardTemp.bases_[0]);

  // Store the blcoks associated with this board

  for(int iBlock = 0; iBlock < boardTemp.nblock_; iBlock++) 
    size += sizeOf(boardTemp.blocks_[iBlock]);

  return size;
}

unsigned NetArrayTemplate::sizeOf(RegBlockTemp& blockTemp)
{
  unsigned size = 0;

  unsigned short nameLen = strlen(blockTemp.name_);
  size += sizeof(nameLen);
  size += nameLen;
  size += sizeof(blockTemp.flags_);
  size += sizeof(blockTemp.addr_mode_);
  size += sizeof(blockTemp.base_);
  size += sizeof(blockTemp.address_);
  size += sizeof(unsigned short) * 2;

  // Add the size of the units string, but if it's currently zero
  // length, resize it to be at least 1 element long

  if(blockTemp.carmaUnits_->size() == 0)
    blockTemp.carmaUnits_->resize(1);

  size += sizeof(unsigned short);
  size +=  blockTemp.carmaUnits_->size();

  // Add the size of the validity bit index too

  size += sizeof(int);

  return size;
}

void NetArrayTemplate::resize()
{
  NetDat::resize(size());
}

void NetArrayTemplate::serialize()
{
  unsigned char* dest = &bytes_[0];
  serialize(dest, arrayTemplate_);
}

/**.......................................................................
 * Serialize an array template
 */
void NetArrayTemplate::serialize(unsigned char*& destPtr, ArrayTemplate* arrayTemplate)
{
  // Serialize the number of register-map templates

  serialize(destPtr, arrayTemplate_->ntemplate);

  // Now iterate over regtemplates, serializing each one

  for(unsigned iRegTemp=0; iRegTemp < arrayTemplate_->ntemplate; iRegTemp++)
    serialize(destPtr, arrayTemplate_->templates[iRegTemp]);
}

/**.......................................................................
 * Serialize a regmap
 */
void NetArrayTemplate::serialize(unsigned char*& destPtr, RegTemp& regTemp)
{
  // First serialize the name of this register map

  unsigned short nameLen = strlen(regTemp.name);
  serialize(destPtr, nameLen);
  serialize(destPtr, regTemp.name, nameLen);

  // Now serialize the number of boards

  serialize(destPtr, regTemp.regtemplate->nboard);

  // Now serialize each board

  for(unsigned iBoard=0; iBoard < regTemp.regtemplate->nboard; iBoard++) 
    serialize(destPtr, regTemp.regtemplate->boards[iBoard]);

  // Now the comment field

  unsigned short commentLen = strlen(regTemp.comment_);
  serialize(destPtr, commentLen);
  serialize(destPtr, regTemp.comment_, commentLen);
}

/**.......................................................................
 * Serialize a board
 */
void NetArrayTemplate::serialize(unsigned char*& destPtr, RegBoardTemp& boardTemp)
{
  // First serialize the name of this board

  unsigned short nameLen = strlen(boardTemp.name_);

  serialize(destPtr, nameLen);
  serialize(destPtr, boardTemp.name_, nameLen);

  // Now serialize the base array

  serialize(destPtr, boardTemp.bases_, NBASE);

  // Now serialize the number of blocks

  serialize(destPtr, boardTemp.nblock_);

  // Now serialize each block
  
  for(int iBlock = 0; iBlock < boardTemp.nblock_; iBlock++) 
    serialize(destPtr, boardTemp.blocks_[iBlock]);
}

/**.......................................................................
 * Serialize a block
 */
void NetArrayTemplate::serialize(unsigned char*& destPtr, RegBlockTemp& blockTemp)
{
  // Serialize the name

  unsigned short nameLen = strlen(blockTemp.name_);
  serialize(destPtr, nameLen);
  serialize(destPtr, blockTemp.name_, nameLen);

  // Serialize the flags

  serialize(destPtr, blockTemp.flags_);

  // Serialize the address mode

  serialize(destPtr, blockTemp.addr_mode_);

  // Serialize the base address

  serialize(destPtr, blockTemp.base_);

  // Serialize the address

  serialize(destPtr, blockTemp.address_);

  // Serialize the axis dimensions

  unsigned short nEl0 = blockTemp.axes_->nEl(0);
  unsigned short nEl1 = blockTemp.axes_->nEl(1);
  serialize(destPtr, nEl0);
  serialize(destPtr, nEl1);

  // Serialize the units string

  unsigned short unitsLen = blockTemp.carmaUnits_->size();
  unsigned char* unitsPtr = (unsigned char*)&blockTemp.carmaUnits_->at(0);
  serialize(destPtr, unitsLen);
  serialize(destPtr, unitsPtr, unitsLen);

  // Serialize the validity bit index too

  serialize(destPtr, blockTemp.carmaValidityBitIndex_);
}

void NetArrayTemplate::initialize(ArrayTemplate*& arrayTemplate)
{
  if(arrayTemplate_) {
    arrayTemplate_ = del_ArrayTemplate(arrayTemplate_);
  }

  arrayTemplate_ = (ArrayTemplate*)malloc(sizeof(ArrayTemplate));

  if(arrayTemplate == 0) {
    ThrowError("Unable to allocate ArrayTemplate");
  }

  arrayTemplate_->ntemplate = 0;
  arrayTemplate_->templates = 0;
}

void NetArrayTemplate::initialize(RegTemp*& regTemps, unsigned nRegTemp)
{
  regTemps = (RegTemp*)malloc(nRegTemp * sizeof(RegTemp));

  for(unsigned i=0; i < nRegTemp; i++) {
    regTemps[i].regtemplate = 0;
    regTemps[i].regtemplate = (RegTemplate*)malloc(sizeof(RegTemplate));

    if(regTemps[i].regtemplate == 0) {
      ThrowError("Unable to allocate RegTemplate");
    }

    regTemps[i].regtemplate->nboard = 0;
    regTemps[i].regtemplate->boards = 0;
  }
  
}

void NetArrayTemplate::initialize(RegBoardTemp*& boardTemps, unsigned nBoard)
{
  boardTemps = (RegBoardTemp*)malloc(nBoard * sizeof(RegBoardTemp));

  if(boardTemps == 0) {
    ThrowError("Unable to allocate board array");
  }

  for(unsigned i=0; i < nBoard; i++) {
    boardTemps[i].blocks_ = 0;
    boardTemps[i].nblock_ = 0;
  }
}

void NetArrayTemplate::initialize(RegBlockTemp*& blockTemps, unsigned nBlock)
{
  blockTemps = (RegBlockTemp*)malloc(nBlock * sizeof(RegBlockTemp));

  if(blockTemps == 0) {
    ThrowError("Unable to allocate block array");
  }

  for(unsigned i=0; i < nBlock; i++) {
    blockTemps[i].axes_         = new CoordAxes();
    blockTemps[i].comment_      = new std::string(" ");
    blockTemps[i].carmaUnits_   = new std::string(" ");

    blockTemps[i].carmaErrors_  = new std::vector<std::pair<std::string, std::string> >;
    blockTemps[i].carmaErrors_->push_back(std::pair<std::string, std::string>("", ""));
  }
}

void NetArrayTemplate::deserialize(const std::vector<unsigned char>& bytes)
{
  checkSize(bytes);

  deserialize(&bytes[0]);
}

void NetArrayTemplate::deserialize(const unsigned char* bytes)
{
  initialize(arrayTemplate_);

  unsigned char* src = (unsigned char*)bytes;

  deserialize(src, arrayTemplate_);
}

/**.......................................................................
 * Deserialize an array template
 */
void NetArrayTemplate::deserialize(unsigned char*& srcPtr, ArrayTemplate* arrayTemplate)
{
  // De-serialize the number of register-map templates

  deserialize(srcPtr, arrayTemplate_->ntemplate);

  initialize(arrayTemplate_->templates, arrayTemplate_->ntemplate);

  // Now iterate over regtemplates, deserializing each one

  for(unsigned iRegTemp=0; iRegTemp < arrayTemplate_->ntemplate; iRegTemp++)
    deserialize(srcPtr, arrayTemplate_->templates[iRegTemp]);
}

/**.......................................................................
 * Serialize a regmap
 */
void NetArrayTemplate::deserialize(unsigned char*& srcPtr, RegTemp& regTemp)
{
  // First serialize the name of this register map

  unsigned short nameLen;
  deserialize(srcPtr, nameLen);
  deserialize(srcPtr, regTemp.name, nameLen);
  regTemp.name[nameLen] = '\0';

  // Now serialize the number of boards

  deserialize(srcPtr, regTemp.regtemplate->nboard);

  initialize(regTemp.regtemplate->boards, regTemp.regtemplate->nboard);

  // Now serialize each board

  for(unsigned iBoard=0; iBoard < regTemp.regtemplate->nboard; iBoard++) 
    deserialize(srcPtr, regTemp.regtemplate->boards[iBoard]);

  // Deserialize the comment

  unsigned short commentLen;
  deserialize(srcPtr, commentLen);
  deserialize(srcPtr, regTemp.comment_, commentLen);
  regTemp.comment_[commentLen] = '\0';
}

/**.......................................................................
 * Serialize a board
 */
void NetArrayTemplate::deserialize(unsigned char*& srcPtr, RegBoardTemp& boardTemp)
{
  // First serialize the name of this board

  unsigned short nameLen;

  deserialize(srcPtr, nameLen);

  deserialize(srcPtr, boardTemp.name_, nameLen);
  boardTemp.name_[nameLen] = '\0';

  // Now serialize the base array

  deserialize(srcPtr, boardTemp.bases_, NBASE);

  // Now serialize the number of blocks

  deserialize(srcPtr, boardTemp.nblock_);

  initialize(boardTemp.blocks_, boardTemp.nblock_);

  // Now serialize each block
  
  for(int iBlock = 0; iBlock < boardTemp.nblock_; iBlock++) 
    deserialize(srcPtr, boardTemp.blocks_[iBlock]);
}

/**.......................................................................
 * Serialize a block
 */
void NetArrayTemplate::deserialize(unsigned char*& srcPtr, RegBlockTemp& blockTemp)
{
  // Deserialize the name

  unsigned short nameLen;
  deserialize(srcPtr, nameLen);
  deserialize(srcPtr, blockTemp.name_, nameLen);
  blockTemp.name_[nameLen] = '\0';

  // Deserialize the flags

  deserialize(srcPtr, blockTemp.flags_);

  // Deserialize the address mode

  deserialize(srcPtr, (unsigned int&)blockTemp.addr_mode_);

  // Deserialize the base address

  deserialize(srcPtr, (unsigned int&)blockTemp.base_);

  // Deserialize the address

  deserialize(srcPtr, blockTemp.address_);

  // Deserialize the axis dimensions

  unsigned short nEl0;
  unsigned short nEl1;
  deserialize(srcPtr, nEl0);
  deserialize(srcPtr, nEl1);

  blockTemp.axes_->setAxis(0, nEl0);
  blockTemp.axes_->setAxis(1, nEl1);

  // Deserialize the units string

  unsigned short unitsLen;
  deserialize(srcPtr, unitsLen);

  blockTemp.carmaUnits_->resize(unitsLen);
  unsigned char* unitsPtr = (unsigned char*)&blockTemp.carmaUnits_->at(0);
  deserialize(srcPtr, unitsPtr, unitsLen);

  // Deserialize the validity bit index too

  deserialize(srcPtr, blockTemp.carmaValidityBitIndex_);
}

//-----------------------------------------------------------------------
// Serialize methods
//-----------------------------------------------------------------------

void NetArrayTemplate::serialize(unsigned char*& destPtr, bool srcVal)
{
  serialize(destPtr, &srcVal, 1);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, bool* src, unsigned nEl)
{
  serialize(destPtr, (unsigned char*)src, DataType::BOOL, nEl);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, unsigned char srcVal)
{
  serialize(destPtr, &srcVal, 1);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, unsigned char* src, unsigned nEl)
{
  serialize(destPtr, (unsigned char*)src, DataType::UCHAR, nEl);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, char srcVal)
{
  serialize(destPtr, &srcVal, 1);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, char* src, unsigned nEl)
{
  serialize(destPtr, (unsigned char*)src, DataType::CHAR, nEl);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, short srcVal)
{
  serialize(destPtr, &srcVal, 1);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, short* srcPtr, unsigned nEl)
{
  serialize(destPtr, (unsigned char*)srcPtr, DataType::SHORT, nEl);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, unsigned short srcVal)
{
  serialize(destPtr, &srcVal, 1);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, unsigned short* src, unsigned nEl)
{
  serialize(destPtr, (unsigned char*)src, DataType::USHORT, nEl);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, int srcVal)
{
  serialize(destPtr, &srcVal, 1);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, int* srcPtr, unsigned nEl)
{
  serialize(destPtr, (unsigned char*)srcPtr, DataType::INT, nEl);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, unsigned int srcVal)
{
  serialize(destPtr, &srcVal, 1);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, unsigned int* src, unsigned nEl)
{
  serialize(destPtr, (unsigned char*)src, DataType::UINT, nEl);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, float srcVal)
{
  serialize(destPtr, &srcVal, 1);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, float* src, unsigned nEl)
{
  serialize(destPtr, (unsigned char*)src, DataType::FLOAT, nEl);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, double srcVal)
{
  serialize(destPtr, &srcVal, 1);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, double* src, unsigned nEl)
{
  serialize(destPtr, (unsigned char*)src, DataType::DOUBLE, nEl);
}

void NetArrayTemplate::serialize(unsigned char*& destPtr, unsigned char* src, DataType::Type type, unsigned nEl)
{
  for(unsigned i=0; i < DataType::sizeOf(type) * nEl; i++) {
    *destPtr++ = *src++;
  }
}

//-----------------------------------------------------------------------
// Deserialize methods
//-----------------------------------------------------------------------

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, bool& destVal)
{
  deserialize(srcPtr, &destVal, 1);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, bool* dest, unsigned nEl)
{
  deserialize(srcPtr, (unsigned char*)dest, DataType::BOOL, nEl);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, unsigned char& destVal)
{
  deserialize(srcPtr, &destVal, 1);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, unsigned char* dest, unsigned nEl)
{
  deserialize(srcPtr, (unsigned char*)dest, DataType::UCHAR, nEl);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, char& destVal)
{
  deserialize(srcPtr, &destVal, 1);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, char* dest, unsigned nEl)
{
  deserialize(srcPtr, (unsigned char*)dest, DataType::CHAR, nEl);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, short& destVal)
{
  deserialize(srcPtr, &destVal, 1);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, short* dest, unsigned nEl)
{
  deserialize(srcPtr, (unsigned char*)dest, DataType::SHORT, nEl);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, unsigned short& destVal)
{
  deserialize(srcPtr, &destVal, 1);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, unsigned short* dest, unsigned nEl)
{
  deserialize(srcPtr, (unsigned char*)dest, DataType::USHORT, nEl);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, int& destVal)
{
  deserialize(srcPtr, &destVal, 1);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, int* dest, unsigned nEl)
{
  deserialize(srcPtr, (unsigned char*)dest, DataType::INT, nEl);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, unsigned int& destVal)
{
  deserialize(srcPtr, &destVal, 1);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, unsigned int* dest, unsigned nEl)
{
  deserialize(srcPtr, (unsigned char*)dest, DataType::UINT, nEl);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, float& destVal)
{
  deserialize(srcPtr, &destVal, 1);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, float* dest, unsigned nEl)
{
  deserialize(srcPtr, (unsigned char*)dest, DataType::FLOAT, nEl);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, double& destVal)
{
  deserialize(srcPtr, &destVal, 1);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, double* dest, unsigned nEl)
{
  deserialize(srcPtr, (unsigned char*)dest, DataType::DOUBLE, nEl);
}

void NetArrayTemplate::deserialize(unsigned char*& srcPtr, unsigned char* dest, DataType::Type type, unsigned nEl)
{
  for(unsigned i=0; i < DataType::sizeOf(type) * nEl; i++) {
    *dest++ = *srcPtr++;
  }
}

ArrayTemplate* NetArrayTemplate::getArrayTemplate()
{
  return arrayTemplate_;
}
