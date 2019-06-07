#include "carma/szautil/ArrayDataFrameManagerIntegrator.h"

using namespace std;

using namespace sza::util;

#define ASSIGN(typeName) \
  {\
    typeName* toTypePtr   = (typeName*) toPtr; \
    typeName* fromTypePtr = (typeName*) fromPtr; \
    for(unsigned i=0; i < nEl; i++) {\
      toTypePtr[i] = fromTypePtr[i];\
    }\
  }

#define ASSIGN_CMPLX(typeName) \
  {\
    typeName* toTypePtr   = (typeName*) toPtr; \
    typeName* fromTypePtr = (typeName*) fromPtr; \
    for(unsigned i=0; i < nEl; i++) {\
      toTypePtr[i].real_ = fromTypePtr[i].real_;\
      toTypePtr[i].imag_ = fromTypePtr[i].imag_;\
    }\
  }

#define SUM(typeName) \
  {\
    typeName* toTypePtr   = (typeName*) toPtr; \
    typeName* fromTypePtr = (typeName*) fromPtr; \
    for(unsigned i=0; i < nEl; i++) {\
      toTypePtr[i] += fromTypePtr[i];\
    }\
  }

#define SUM_CMPLX(typeName) \
  {\
    typeName* toTypePtr   = (typeName*) toPtr; \
    typeName* fromTypePtr = (typeName*) fromPtr; \
    for(unsigned i=0; i < nEl; i++) {\
      toTypePtr[i].real_ += fromTypePtr[i].real_;\
      toTypePtr[i].imag_ += fromTypePtr[i].imag_;\
    }\
  }

#define UNION(typeName) \
  {\
    typeName* toTypePtr   = (typeName*) toPtr; \
    typeName* fromTypePtr = (typeName*) fromPtr; \
    for(unsigned i=0; i < nEl; i++) {\
      toTypePtr[i] |= fromTypePtr[i];\
    }\
  }

#define UNION_CMPLX(typeName) \
  {\
    typeName* toTypePtr   = (typeName*) toPtr; \
    typeName* fromTypePtr = (typeName*) fromPtr; \
    for(unsigned i=0; i < nEl; i++) {\
      toTypePtr[i].real_ |= fromTypePtr[i].real_;\
      toTypePtr[i].imag_ |= fromTypePtr[i].imag_;\
    }\
  }

#define AVG(typeName) \
  {\
    typeName* toTypePtr   = (typeName*) toPtr; \
    typeName* fromTypePtr = (typeName*) fromPtr; \
    for(unsigned i=0; i < nEl; i++) {\
      meanReal = (double) toTypePtr[i];\
      valReal  = (double) fromTypePtr[i];\
      meanReal += (valReal - meanReal) / (nAvg + 1);\
      toTypePtr[i] = meanReal;\
    }\
  }

#define AVG_CMPLX(typeName) \
  {\
    typeName* toTypePtr   = (typeName*) toPtr; \
    typeName* fromTypePtr = (typeName*) fromPtr; \
    for(unsigned i=0; i < nEl; i++) {\
      meanReal = (double) toTypePtr[i].real_;\
      valReal  = (double) fromTypePtr[i].real_;\
      meanReal += (valReal - meanReal) / (nAvg + 1);\
						    \
      meanImag = (double) toTypePtr[i].imag_;\
      valImag  = (double) fromTypePtr[i].imag_;\
      meanImag += (valImag - meanImag) / (nAvg + 1);\
						    \
      toTypePtr[i].real_ = meanReal;\
      toTypePtr[i].imag_ = meanImag;\
    }\
  }


/**.......................................................................
 * Constructor.
 */
ArrayDataFrameManagerIntegrator::ArrayDataFrameManagerIntegrator() 
{
  resetRunningAvgCounter();
}

/**.......................................................................
 * Destructor.
 */
ArrayDataFrameManagerIntegrator::~ArrayDataFrameManagerIntegrator() {}

/**.......................................................................
 * Initialize this integrator from two data frames
 */
void ArrayDataFrameManagerIntegrator::initialize(ArrayMapDataFrameManager* fromFrame, ArrayMapDataFrameManager* toFrame)
{
  COUT("Inside initialize");

  regs_.clear();

  Register reg;

  // Iterate over all register maps 
    
  ArrayMap* arrayMap = fromFrame->arrayMap();
  for(unsigned int iregmap=0; iregmap < arrayMap->nregmap; iregmap++) {
    
    ArrRegMap* aregmap = arrayMap->regmaps[iregmap];
    RegMap* regmap     = aregmap->regmap;
    std::string regmapName = aregmap->name;

    if(!toFrame->regMapIsPresent(regmap)) {
      ThrowError("Data frames don't match");
    }
      
    // Iterate over all boards in this register map.
	
    for(unsigned int iboard=0; iboard < regmap->nboard_; iboard++) {
      RegMapBoard* brd = regmap->boards_[iboard];
      std::string boardName = brd->name;

      // Only increment if this board is present in both source and
      // destination register maps.
      
      if(!toFrame->boardIsPresent(brd)) {
	ThrowError("Data frames don't match");
      }
      
      // Iterate over all register blocks of this board
      
      for(unsigned int iblock=0; iblock < brd->nblock; iblock++) {
	RegMapBlock* blk = brd->blocks[iblock];
	std::string blockName = blk->name_;
	
	// If this block is present in the register maps of both
	// frames, sum them
	
	if(!toFrame->blockIsPresent(blk)) {
	  ThrowError("Data frames don't match");
	}
	  
	reg.fromPtr_ = fromFrame->frame()->getPtr(fromFrame->byteOffsetInFrameOf(regmapName, boardName, blockName), DataType::typeOf(blk));
	reg.toPtr_   =   toFrame->frame()->getPtr(  toFrame->byteOffsetInFrameOf(regmapName, boardName, blockName), DataType::typeOf(blk));
	reg.nEl_     = blk->nEl();
	reg.type_    = DataType::typeOf(blk);

	// Determine which integrate function is appropriate for this
	// register block


#if 1
	bool verbose = false;
	if((regmapName == "Bima8") && (boardName == "AntennaCommon.Drive") && (blockName == "state")) {
	  COUT("Found: " << regmapName << "." << boardName << "." << blockName);
	  verbose = true;
	}
#endif

	if(blk->isSummed() || blk->isPostAveraged()) {
	  if(verbose) {
	    COUT("calling addSum");
	  }
	  reg.packFn_  = addSum;
	} else if(blk->isPreAveraged()) {
	  if(verbose) {
	    COUT("calling addRunningAverage");
	    COUT("Datatype of block = " << DataType::typeOf(blk));
	  }
	  reg.packFn_  = addRunningAverage;
	} else if(blk->isUnioned()) {
	  if(verbose) {
	    COUT("calling addUnion");
	  }
	  reg.packFn_  = addUnion;
	} else if(blk->isFirst()) {
	  if(verbose) {
	    COUT("calling addFirst");
	  }
	  reg.packFn_  = addFirst;
	} else {
	  if(verbose) {
	    COUT("calling addLast");
	  }
	  reg.packFn_  = addLast;
	}

#if 1
	verbose = false;
#endif

	regs_.push_back(reg);

      }; // End iteration over block
    }; // End iteration over boards
  }; // End iteration over regmaps

  resetRunningAvgCounter();
}

ArrayDataFrameManagerIntegrator::Register::Register()
{
  fromPtr_ = 0;
  toPtr_   = 0;
  packFn_  = 0;
  nEl_     = 0;
  type_    = DataType::UNKNOWN;
}

ArrayDataFrameManagerIntegrator::Register::~Register()
{}

ArrayDataFrameManagerIntegrator::Register::Register(const Register& reg)
{
  *this = (Register&) reg;
}

ArrayDataFrameManagerIntegrator::Register::Register(Register& reg)
{
  *this = reg;
}

void ArrayDataFrameManagerIntegrator::Register::operator=(const Register& reg)
{
  *this = (Register&) reg;
}

void ArrayDataFrameManagerIntegrator::Register::operator=(Register& reg)
{
  fromPtr_ = reg.fromPtr_;
  toPtr_   = reg.toPtr_;
  packFn_  = reg.packFn_;
  nEl_     = reg.nEl_;
  type_    = reg.type_;
}

/**.......................................................................
 * Union two register values
 */
ADFM_INTEGRATE_FN(ArrayDataFrameManagerIntegrator::addUnion)
{
  switch(type) {
  case DataType::UCHAR:
    UNION(unsigned char);
    break;
  case DataType::CHAR:
    UNION(char);
    break;
  case DataType::BOOL:
    UNION(bool);
    break;
  case DataType::USHORT:
    UNION(unsigned short);
    break;
  case DataType::SHORT:
    UNION(short);
    break;
  case DataType::UINT:
    UNION(unsigned int);
    break;
  case DataType::INT:
    UNION(int);
    break;
  case DataType::STRING:
    // Do nothing
    break;
  default:
    ThrowError("Type: " << type << " can't be unioned");
    break;
  }
}

/**.......................................................................
 * Take the last value of a register
 */
ADFM_INTEGRATE_FN(ArrayDataFrameManagerIntegrator::addLast)
{
  switch(type) {
  case DataType::UCHAR:
  case DataType::STRING:
    ASSIGN(unsigned char);
    break;
  case DataType::CHAR:
    ASSIGN(char);
    break;
  case DataType::BOOL:
    ASSIGN(bool);
    break;
  case DataType::USHORT:
    ASSIGN(unsigned short);
    break;
  case DataType::SHORT:
    ASSIGN(short);
    break;
  case DataType::UINT:
    ASSIGN(unsigned int);
    break;
  case DataType::INT:
    ASSIGN(int);
    break;
  case DataType::FLOAT:
    ASSIGN(float);
    break;
  case DataType::DOUBLE:
    ASSIGN(double);
    break;
  case DataType::DATE:
    ASSIGN(RegDate::Data);
    break;
  case DataType::COMPLEX_FLOAT:
    ASSIGN_CMPLX(Complex<float>::Data);
    break;
  default:
    ThrowError("Unhandled type: " << type);
    break;
  }
}

/**.......................................................................
 * Take the first value of a register
 */
ADFM_INTEGRATE_FN(ArrayDataFrameManagerIntegrator::addFirst)
{
  // If this is the first sample in the integration, just assign the
  // value.  Else do nothing.

  if(nAvg == 0) {
    addLast(nEl, type, fromPtr, toPtr, nAvg);
  }
}

/**.......................................................................
 * Sum register values
 */
ADFM_INTEGRATE_FN(ArrayDataFrameManagerIntegrator::addSum)
{
  switch(type) {
  case DataType::UCHAR:
    SUM(unsigned char);
    break;
  case DataType::CHAR:
    SUM(char);
    break;
  case DataType::BOOL:
    SUM(bool);
    break;
  case DataType::USHORT:
    SUM(unsigned short);
    break;
  case DataType::SHORT:
    SUM(short);
    break;
  case DataType::UINT:
    SUM(unsigned int);
    break;
  case DataType::INT:
    SUM(int);
    break;
  case DataType::FLOAT:
    SUM(float);
    break;
  case DataType::DOUBLE:
    SUM(double);
    break;
  case DataType::COMPLEX_FLOAT:
    SUM_CMPLX(Complex<float>::Data);
    break;
  case DataType::STRING:
    // DO nothing
    break;
  default:
    ThrowError("Type: " << type << " can't be summed");
    break;
  }
}

/**.......................................................................
 * Sum register values
 */
ADFM_INTEGRATE_FN(ArrayDataFrameManagerIntegrator::addRunningAverage)
{
  double meanReal, valReal;
  double meanImag, valImag;

  switch(type) {
  case DataType::UCHAR:
    AVG(unsigned char);
    break;
  case DataType::CHAR:
    AVG(char);
    break;
  case DataType::BOOL:
    AVG(bool);
    break;
  case DataType::USHORT:
    AVG(unsigned short);
    break;
  case DataType::SHORT:
    AVG(short);
    break;
  case DataType::UINT:
    AVG(unsigned int);
    break;
  case DataType::INT:
    AVG(int);
    break;
  case DataType::FLOAT:
    AVG(float);
    break;
  case DataType::DOUBLE:
    AVG(double);
    break;
  case DataType::COMPLEX_FLOAT:
    AVG_CMPLX(Complex<float>::Data);
    break;
  case DataType::STRING:
    // Do nothing
    break;
  default:
    ThrowError("Type: " << type << " can't be summed");
    break;
  }
}

/**.......................................................................
 * Reset the counter which will be used for running averages
 */
void ArrayDataFrameManagerIntegrator::resetRunningAvgCounter()
{
  nAvg_ = 0;
}

/**.......................................................................
 * Increment the counter which will be used for running averages
 */
void ArrayDataFrameManagerIntegrator::incrementRunningAvgCounter()
{
  nAvg_++;
}

/**.......................................................................
 * Integrate this frame
 */
void ArrayDataFrameManagerIntegrator::integrate()
{
  for(unsigned i=0; i < regs_.size(); i++) {
    Register& reg = regs_[i];
    (*reg.packFn_)(reg.nEl_, reg.type_, reg.fromPtr_, reg.toPtr_, nAvg_);
  }

  incrementRunningAvgCounter();
}

/**.......................................................................
 * Assign this frame
 */
void ArrayDataFrameManagerIntegrator::assign()
{
  resetRunningAvgCounter();

  COUT("Regs is of ize : " << regs_.size());

  for(unsigned i=0; i < regs_.size(); i++) {
    Register& reg = regs_[i];
    addLast(reg.nEl_, reg.type_, reg.fromPtr_, reg.toPtr_, nAvg_);
  }

  incrementRunningAvgCounter();
}

unsigned ArrayDataFrameManagerIntegrator::getNFrameIntegrated()
{
  return nAvg_;
}
