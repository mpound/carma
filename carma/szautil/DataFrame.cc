#include "carma/szautil/AxisRange.h"
#include "carma/szautil/DataFrame.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/szaarrayutils/scanner.h"

using namespace sza::util;

// Constructor/destructor stubs

DataFrame::DataFrame() 
{
  nAvg_ = 1;
};

DataFrame::~DataFrame() {};

// Empty method bodies for pure virtual methods

/**
 * Set the size of the buffer managed by this frame,
 */
void DataFrame::resize(unsigned int) {};

/**
 * Return the size of the buffer managed by this frame,
 */
unsigned int DataFrame::size() {};

/**.......................................................................
 * Return the number of registers in the internal buffer.
 */
unsigned int DataFrame::nReg()
{
  return size() - SCAN_HEADER_DIM;
}

/**.......................................................................
 * Return the number of bytes in the internal buffer.
 */
unsigned int DataFrame::nByte()
{
  return size();
}

/**
 * Empty body for virtual index operator
 */
unsigned char& DataFrame::operator[](unsigned int nBuffer) {};

/**
 * Empty body for virtual assignment operator.
 */
void DataFrame::operator=(DataFrame& frame) 
{
  nAvg_ = frame.nAvg_;
};

/**.......................................................................
 * Return a pointer to our internal data, cast as an unsigned
 * short pointer.
 */
void* DataFrame::getPtr(unsigned int index, DataType::Type type)
{
  switch(type) {
  case DataType::BOOL:
    return getBoolPtr(index);
    break;
  case DataType::UCHAR:
  case DataType::STRING:
    return getUcharPtr(index);
    break;
  case DataType::CHAR:
    return getCharPtr(index);
    break;
  case DataType::USHORT:
    return getUshortPtr(index);
    break;
  case DataType::SHORT:
    return getShortPtr(index);
    break;
  case DataType::UINT:
    return getUintPtr(index);
    break;
  case DataType::INT:
    return getIntPtr(index);
    break;
  case DataType::ULONG:
    return getUlongPtr(index);
    break;
  case DataType::LONG:
    return getLongPtr(index);
    break;
  case DataType::FLOAT:
    return getFloatPtr(index);
    break;
  case DataType::DOUBLE:
    return getDoublePtr(index);
    break;
  case DataType::DATE:
    return getDatePtr(index);
    break;
  case DataType::COMPLEX_FLOAT:
    return getComplexFloatPtr(index);
    break;
  default:
    return 0;
    break;
  }
}

/**.......................................................................
 * Pack an array into internal memory
 */
void DataFrame::pack(void* data, AxisRange& range, DataType::Type type, 
		     unsigned iStart, bool lockFrame)
{
  // If we were requested to lock the frame, do it now
  
  if(lockFrame)
    lock();
  
  try {
    switch(type) {
    case DataType::UCHAR:
    case DataType::STRING:
      {
	unsigned char* ptr = getUcharPtr(iStart);
	unsigned char* dptr = (unsigned char*)data;
	for(range.reset(); !range.isEnd(); ++range) 
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::CHAR:
      {
	char* ptr = getCharPtr(iStart);
	char* dptr = (char*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::BOOL:
      {
	bool* ptr = getBoolPtr(iStart);
	bool* dptr = (bool*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::USHORT:
      {
	unsigned short* ptr = getUshortPtr(iStart);
	unsigned short* dptr = (unsigned short*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::SHORT:
      {
	short* ptr = getShortPtr(iStart);
	short* dptr = (short*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::UINT:
      {
	unsigned int* ptr = getUintPtr(iStart);
	unsigned int* dptr = (unsigned int*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::INT:
      {
	int* ptr = getIntPtr(iStart);
	int* dptr = (int*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::ULONG:
      {
	unsigned long* ptr = getUlongPtr(iStart);
	unsigned long* dptr = (unsigned long*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::LONG:
      {
	long* ptr = getLongPtr(iStart);
	long* dptr = (long*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::FLOAT:
      {
	float* ptr = getFloatPtr(iStart);
	float* dptr = (float*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::DOUBLE:
      {
	double* ptr = getDoublePtr(iStart);
	double* dptr = (double*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::DATE:
      {
	RegDate::Data* ptr = getDatePtr(iStart);
	RegDate::Data* dptr = (RegDate::Data*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    case DataType::COMPLEX_FLOAT:
      {
	Complex<float>::Data* ptr = getComplexFloatPtr(iStart);
	Complex<float>::Data* dptr = (Complex<float>::Data*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = dptr[range.currentIterator()];
      }
      break;
    default:
      break;
    }
    
  } catch(const Exception& err) {
    
    // If the frame was locked in this call, unlock it now
    
    if(lockFrame)
      unlock();
    
    throw err;
    
  } catch(...) {
    
    if(lockFrame)
      unlock();
    
    ThrowError("Caught an unknown error");
  }
  
  if(lockFrame)
    unlock();
}

/**.......................................................................
 * Pack an array into internal memory from consecutive elements of the
 * input data array.
 */
void DataFrame::pack(void* data, unsigned ndata, DataType::Type type, 
		     unsigned iStart, bool lockFrame)
{
  axisRange_.setTo(ndata);
  pack(data, axisRange_, type, iStart, lockFrame);
}

/**.......................................................................
 * Pack an array into internal memory
 */
void DataFrame::packValue(void* data, AxisRange& range, DataType::Type type, 
			  unsigned iStart, bool lockFrame)
{
  // If we were requested to lock the frame, do it now
  
  if(lockFrame)
    lock();
  
  try {
    switch(type) {
    case DataType::UCHAR:
    case DataType::STRING:
      {
	unsigned char* ptr = getUcharPtr(iStart);
	unsigned char* dptr = (unsigned char*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::CHAR:
      {
	char* ptr = getCharPtr(iStart);
	char* dptr = (char*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::BOOL:
      {
	bool* ptr = getBoolPtr(iStart);
	bool* dptr = (bool*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::USHORT:
      {
	unsigned short* ptr = getUshortPtr(iStart);
	unsigned short* dptr = (unsigned short*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::SHORT:
      {
	short* ptr = getShortPtr(iStart);
	short* dptr = (short*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::UINT:
      {
	unsigned int* ptr = getUintPtr(iStart);
	unsigned int* dptr = (unsigned int*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::INT:
      {
	int* ptr = getIntPtr(iStart);
	int* dptr = (int*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::ULONG:
      {
	unsigned long* ptr = getUlongPtr(iStart);
	unsigned long* dptr = (unsigned long*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::LONG:
      {
	long* ptr = getLongPtr(iStart);
	long* dptr = (long*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::FLOAT:
      {
	float* ptr = getFloatPtr(iStart);
	float* dptr = (float*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::DOUBLE:
      {
	double* ptr = getDoublePtr(iStart);
	double* dptr = (double*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::DATE:
      {
	RegDate::Data* ptr = getDatePtr(iStart);
	RegDate::Data* dptr = (RegDate::Data*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    case DataType::COMPLEX_FLOAT:
      {
	Complex<float>::Data* ptr = getComplexFloatPtr(iStart);
	Complex<float>::Data* dptr = (Complex<float>::Data*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  ptr[range.currentElement()] = *dptr;
      }
      break;
    default:
      break;
    }
    
  } catch(const Exception& err) {
    
    // If the frame was locked in this call, unlock it now
    
    if(lockFrame)
      unlock();
    
    throw err;
    
  } catch(...) {
    
    if(lockFrame)
      unlock();
    
    ThrowError("Caught an unknown error");
  }
  
  if(lockFrame)
    unlock();
}

/**.......................................................................
 * Sum an array into internal memory
 */
void DataFrame::addSum(void* data, AxisRange& range, DataType::Type type, 
		       unsigned iStart, bool lockFrame)
{
  // If we were requested to lock the frame, do it now
  
  if(lockFrame)
    lock();
  
  switch(type) {
  case DataType::UCHAR:
    {
      unsigned char* ptr = getUcharPtr(iStart);
      unsigned char* dptr = (unsigned char*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::CHAR:
    {
      char* ptr = getCharPtr(iStart);
      char* dptr = (char*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::BOOL:
    {
      bool* ptr = getBoolPtr(iStart);
      bool* dptr = (bool*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::USHORT:
    {
      unsigned short* ptr = getUshortPtr(iStart);
      unsigned short* dptr = (unsigned short*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::SHORT:
    {
      short* ptr = getShortPtr(iStart);
      short* dptr = (short*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::UINT:
    {
      unsigned int* ptr = getUintPtr(iStart);
      unsigned int* dptr = (unsigned int*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::INT:
    {
      int* ptr = getIntPtr(iStart);
      int* dptr = (int*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::ULONG:
    {
      unsigned long* ptr = getUlongPtr(iStart);
      unsigned long* dptr = (unsigned long*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::LONG:
    {
      long* ptr = getLongPtr(iStart);
      long* dptr = (long*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::FLOAT:
    {
      float* ptr = getFloatPtr(iStart);
      float* dptr = (float*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::DOUBLE:
    {
      double* ptr = getDoublePtr(iStart);
      double* dptr = (double*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] += dptr[range.currentIterator()];
    }
    break;
  case DataType::COMPLEX_FLOAT:
    {
      Complex<float>::Data* ptr = getComplexFloatPtr(iStart);
      Complex<float>::Data* dptr = (Complex<float>::Data*)data;

      for(range.reset(); !range.isEnd(); ++range) {
	ptr[range.currentElement()].real_ += dptr[range.currentIterator()].real_;
	ptr[range.currentElement()].imag_ += dptr[range.currentIterator()].imag_;
      }

    }
    break;
    
  default:
    break;
  }
  
  // If the frame was locked in this call, unlock it now
  
  if(lockFrame)
    unlock();
}

/**.......................................................................
 * Pack an array into internal memory from consecutive elements of the
 * input data array.
 */
void DataFrame::addSum(void* data, unsigned ndata, DataType::Type type, 
		       unsigned iStart, bool lockFrame)
{
  axisRange_.setTo(ndata);
  addSum(data, axisRange_, type, iStart, lockFrame);
}

/**.......................................................................
 * Sum an array into internal memory
 */
void DataFrame::addRunningAverage(void* data, AxisRange& range, DataType::Type type, 
				  unsigned iStart, bool lockFrame)
{
  double meanReal, valReal;
  double meanImag, valImag;
  unsigned iCurrEl, iCurrIter;

  // If we were requested to lock the frame, do it now
  
  if(lockFrame)
    lock();
  
  switch(type) {
  case DataType::UCHAR:
    {
      unsigned char* ptr  = getUcharPtr(iStart);
      unsigned char* dptr = (unsigned char*)data;

      for(range.reset(); !range.isEnd(); ++range) {
	iCurrEl   = range.currentElement();
	valReal   = (double)dptr[range.currentIterator()];
	meanReal  = (double) ptr[iCurrEl];
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl] = (unsigned char)meanReal;
      }
    }
    break;
  case DataType::CHAR:
    {
      char* ptr  = getCharPtr(iStart);
      char* dptr = (char*)data;

      for(range.reset(); !range.isEnd(); ++range) {
	iCurrEl   = range.currentElement();
	valReal   = (double)dptr[range.currentIterator()];
	meanReal  = (double) ptr[iCurrEl];
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl] = (char)meanReal;
      }
    }
    break;
  case DataType::USHORT:
    {
      unsigned short* ptr  = getUshortPtr(iStart);
      unsigned short* dptr = (unsigned short*)data;

      for(range.reset(); !range.isEnd(); ++range) {
	iCurrEl   = range.currentElement();
	valReal   = (double)dptr[range.currentIterator()];
	meanReal  = (double) ptr[iCurrEl];
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl] = (unsigned short)meanReal;
      }
    }
    break;
  case DataType::SHORT:
    {
      short* ptr  = getShortPtr(iStart);
      short* dptr = (short*)data;

      for(range.reset(); !range.isEnd(); ++range) {
	iCurrEl   = range.currentElement();
	valReal   = (double)dptr[range.currentIterator()];
	meanReal  = (double) ptr[iCurrEl];
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl] = (short)meanReal;
      }
    }
    break;
  case DataType::UINT:
    {
      unsigned int* ptr  = getUintPtr(iStart);
      unsigned int* dptr = (unsigned int*)data;

      for(range.reset(); !range.isEnd(); ++range) {
        iCurrEl   = range.currentElement();
	valReal   = (double)dptr[range.currentIterator()];
	meanReal  = (double) ptr[iCurrEl];
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl] = (unsigned int)meanReal;
      }
    }
    break;
  case DataType::INT:
    {
      int* ptr  = getIntPtr(iStart);
      int* dptr = (int*)data;

      for(range.reset(); !range.isEnd(); ++range) {
        iCurrEl   = range.currentElement();
	valReal   = (double)dptr[range.currentIterator()];
	meanReal  = (double) ptr[iCurrEl];
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl] = (int)meanReal;
      }
    }
    break;
  case DataType::ULONG:
    {
      unsigned long* ptr  = getUlongPtr(iStart);
      unsigned long* dptr = (unsigned long*)data;

      for(range.reset(); !range.isEnd(); ++range) {
	iCurrEl   = range.currentElement();
	valReal   = (double)dptr[range.currentIterator()];
	meanReal  = (double) ptr[iCurrEl];
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl] = (unsigned long)meanReal;
      }
    }
    break;
  case DataType::LONG:
    {
      long* ptr  = getLongPtr(iStart);
      long* dptr = (long*)data;

      for(range.reset(); !range.isEnd(); ++range) {
	iCurrEl   = range.currentElement();
	valReal   = (double)dptr[range.currentIterator()];
	meanReal  = (double) ptr[iCurrEl];
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl] = (long)meanReal;
      }
    }
    break;
  case DataType::FLOAT:
    {
      float* ptr  = getFloatPtr(iStart);
      float* dptr = (float*)data;

      for(range.reset(); !range.isEnd(); ++range) {
	iCurrEl   = range.currentElement();
	valReal   = (double)dptr[range.currentIterator()];
	meanReal  = (double) ptr[iCurrEl];
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl] = (float)meanReal;
      }
    }
    break;
  case DataType::DOUBLE:
    {
      double* ptr  = getDoublePtr(iStart);
      double* dptr = (double*)data;

      for(range.reset(); !range.isEnd(); ++range) {
	iCurrEl   = range.currentElement();
	valReal   = (double)dptr[range.currentIterator()];
	meanReal  = (double) ptr[iCurrEl];
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl] = (double)meanReal;
      }
    }
    break;
  case DataType::COMPLEX_FLOAT:
    {
      Complex<float>::Data* ptr  = getComplexFloatPtr(iStart);
      Complex<float>::Data* dptr = (Complex<float>::Data*)data;

      for(range.reset(); !range.isEnd(); ++range) {
	iCurrEl   = range.currentElement();
	iCurrIter = range.currentIterator();

	valReal   = (double) dptr[iCurrIter].real_;
	meanReal  = (double) ptr[iCurrEl].real_;
	meanReal += (valReal - meanReal)/(nAvg_ + 1);
	ptr[iCurrEl].real_ = (float)meanReal;

	valImag   = (double) dptr[iCurrIter].imag_;
	meanImag  = (double) ptr[iCurrEl].imag_;
	meanImag += (valImag - meanImag)/(nAvg_ + 1);
	ptr[iCurrEl].imag_ = (float)meanImag;
      }
    }
    break;
    
  default:
    break;
  }
  
  // If the frame was locked in this call, unlock it now
  
  if(lockFrame)
    unlock();
}

/**.......................................................................
 * Pack an array into internal memory from consecutive elements of the
 * input data array.
 */
void DataFrame::addRunningAverage(void* data, unsigned ndata, DataType::Type type, 
				  unsigned iStart, bool lockFrame)
{
  axisRange_.setTo(ndata);
  addRunningAverage(data, axisRange_, type, iStart, lockFrame);
}

/**.......................................................................
 * Sum an array into internal memory
 */
void DataFrame::addUnion(void* data, AxisRange& range, DataType::Type type, 
			 unsigned iStart, bool lockFrame)
{
  // If we were requested to lock the frame, do it now
  
  if(lockFrame)
    lock();
  
  switch(type) {
  case DataType::UCHAR:
    {
      unsigned char* ptr = getUcharPtr(iStart);
      unsigned char* dptr = (unsigned char*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] |= dptr[range.currentIterator()];
    }
    break;
  case DataType::CHAR:
    {
      char* ptr = getCharPtr(iStart);
      char* dptr = (char*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] |= dptr[range.currentIterator()];
    }
    break;
  case DataType::BOOL:
    {
      bool* ptr = getBoolPtr(iStart);
      bool* dptr = (bool*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] |= dptr[range.currentIterator()];
    }
    break;
  case DataType::USHORT:
    {
      unsigned short* ptr = getUshortPtr(iStart);
      unsigned short* dptr = (unsigned short*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] |= dptr[range.currentIterator()];
    }
    break;
  case DataType::SHORT:
    {
      short* ptr = getShortPtr(iStart);
      short* dptr = (short*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] |= dptr[range.currentIterator()];
    }
    break;
  case DataType::UINT:
    {
      unsigned int* ptr = getUintPtr(iStart);
      unsigned int* dptr = (unsigned int*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] |= dptr[range.currentIterator()];

      //      COUT("Value after UINT union is: " << ptr[0]);
    }
    break;
  case DataType::INT:
    {
      int* ptr = getIntPtr(iStart);
      int* dptr = (int*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] |= dptr[range.currentIterator()];
    }
    break;
  case DataType::ULONG:
    {
      unsigned long* ptr = getUlongPtr(iStart);
      unsigned long* dptr = (unsigned long*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] |= dptr[range.currentIterator()];
    }
    break;
  case DataType::LONG:
    {
      long* ptr = getLongPtr(iStart);
      long* dptr = (long*)data;
      for(range.reset(); !range.isEnd(); ++range)
	ptr[range.currentElement()] |= dptr[range.currentIterator()];
    }
    break;
  default:
    ThrowError("Floating point types can't be unioned");
    break;
  }
  
  // If the frame was locked in this call, unlock it now
  
  if(lockFrame)
    unlock();
}

/**.......................................................................
 * Pack an array into internal memory from consecutive elements of the
 * input data array.
 */
void DataFrame::addUnion(void* data, unsigned ndata, DataType::Type type, 
			 unsigned iStart, bool lockFrame)
{
  axisRange_.setTo(ndata);
  addUnion(data, axisRange_, type, iStart, lockFrame);
}

/**.......................................................................
 * Unpack an array from internal memory
 */
void DataFrame::unpack(void* data, AxisRange& range, DataType::Type type, 
		       unsigned iStart, bool lockFrame)
{
  // If we were requested to lock the frame, do it now
  
  if(lockFrame)
    lock();
  
  try {
    switch(type) {
    case DataType::UCHAR:
    case DataType::STRING:
      {
	unsigned char* ptr = getUcharPtr(iStart);
	unsigned char* dptr = (unsigned char*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::CHAR:
      {
	char* ptr = getCharPtr(iStart);
	char* dptr = (char*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::BOOL:
      {
	bool* ptr = getBoolPtr(iStart);
	bool* dptr = (bool*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::USHORT:
      {
	unsigned short* ptr = getUshortPtr(iStart);
	unsigned short* dptr = (unsigned short*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::SHORT:
      {
	short* ptr = getShortPtr(iStart);
	short* dptr = (short*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::UINT:
      {
	unsigned int* ptr = getUintPtr(iStart);
	unsigned int* dptr = (unsigned int*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::INT:
      {
	int* ptr = getIntPtr(iStart);
	int* dptr = (int*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::ULONG:
      {
	unsigned long* ptr = getUlongPtr(iStart);
	unsigned long* dptr = (unsigned long*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::LONG:
      {
	long* ptr = getLongPtr(iStart);
	long* dptr = (long*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::FLOAT:
      {
	float* ptr = getFloatPtr(iStart);
	float* dptr = (float*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::DOUBLE:
      {
	double* ptr = getDoublePtr(iStart);
	double* dptr = (double*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::DATE:
      {
	RegDate::Data* ptr = getDatePtr(iStart);
	RegDate::Data* dptr = (RegDate::Data*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    case DataType::COMPLEX_FLOAT:
      {
	Complex<float>::Data* ptr = getComplexFloatPtr(iStart);
	Complex<float>::Data* dptr = (Complex<float>::Data*)data;
	for(range.reset(); !range.isEnd(); ++range)
	  dptr[range.currentIterator()] = ptr[range.currentElement()];
      }
      break;
    default:
      break;
    }
  } catch(const Exception& err) {
    if(lockFrame)
      unlock();
    
    throw err;
  } catch(...) {
    if(lockFrame)
      unlock();
    
    ThrowError("Caught an unknown exception");
  }
  
  // If the frame was locked in this call, unlock it now
  
  if(lockFrame)
    unlock();
}

/**.......................................................................
 * Unpack an array from consecutive elements of the internal memory
 */
void DataFrame::unpack(void* data, unsigned ndata, DataType::Type type, 
		       unsigned iStart, bool lockFrame)
{
  // If we were requested to lock the frame, do it now
  
  if(lockFrame)
    lock();
  
  try {
    switch(type) {
    case DataType::UCHAR:
    case DataType::STRING:
      {
	unsigned char* ptr = getUcharPtr(iStart);
	unsigned char* dptr = (unsigned char*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::CHAR:
      {
	char* ptr = getCharPtr(iStart);
	char* dptr = (char*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::BOOL:
      {
	bool* ptr = getBoolPtr(iStart);
	bool* dptr = (bool*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::USHORT:
      {
	unsigned short* ptr = getUshortPtr(iStart);
	unsigned short* dptr = (unsigned short*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::SHORT:
      {
	short* ptr = getShortPtr(iStart);
	short* dptr = (short*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::UINT:
      {
	unsigned int* ptr = getUintPtr(iStart);
	unsigned int* dptr = (unsigned int*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::INT:
      {
	int* ptr = getIntPtr(iStart);
	int* dptr = (int*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::ULONG:
      {
	unsigned long* ptr = getUlongPtr(iStart);
	unsigned long* dptr = (unsigned long*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::LONG:
      {
	long* ptr = getLongPtr(iStart);
	long* dptr = (long*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::FLOAT:
      {
	float* ptr = getFloatPtr(iStart);
	float* dptr = (float*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::DOUBLE:
      {
	double* ptr = getDoublePtr(iStart);
	double* dptr = (double*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::DATE:
      {
	RegDate::Data* ptr = getDatePtr(iStart);
	RegDate::Data* dptr = (RegDate::Data*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    case DataType::COMPLEX_FLOAT:
      {
	Complex<float>::Data* ptr = getComplexFloatPtr(iStart);
	Complex<float>::Data* dptr = (Complex<float>::Data*)data;
	for(unsigned idata=0; idata < ndata; idata++)
	  dptr[idata] = ptr[idata];
      }
      break;
    default:
      break;
    }
  } catch(const Exception& err) {
    if(lockFrame)
      unlock();
    
    throw err;
  } catch(...) {
    if(lockFrame)
      unlock();
    
    ThrowError("Caught an unknown exception");
  }
  
  // If the frame was locked in this call, unlock it now
  
  if(lockFrame)
    unlock();
}

/**.......................................................................
 * Return a pointer to our internal data, cast as a bool
 * pointer.
 */
bool* DataFrame::getBoolPtr(unsigned int index) 
{
  return (bool*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as an unsigned
 * char pointer.
 */
unsigned char* DataFrame::getUcharPtr(unsigned int index) 
{
  return (unsigned char*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as a char
 * pointer.
 */
char* DataFrame::getCharPtr(unsigned int index) 
{
  return (char*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as an unsigned
 * short pointer.
 */
unsigned short* DataFrame::getUshortPtr(unsigned int index) 
{
  return (unsigned short*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as a short
 * pointer.
 */
short* DataFrame::getShortPtr(unsigned int index) 
{
  return (short*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as an unsigned
 * int pointer.
 */
unsigned int* DataFrame::getUintPtr(unsigned int index) 
{
  return (unsigned int*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as an integer
 * pointer.
 */
int* DataFrame::getIntPtr(unsigned int index) 
{
  return (int*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as an unsigned
 * long pointer.
 */
unsigned long* DataFrame::getUlongPtr(unsigned long index) 
{
  return (unsigned long*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as an integer
 * pointer.
 */
long* DataFrame::getLongPtr(unsigned long index) 
{
  return (long*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as a float
 * pointer.
 */
float* DataFrame::getFloatPtr(unsigned int index) 
{
  return (float*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as a double
 * pointer.
 */
double* DataFrame::getDoublePtr(unsigned int index) 
{
  return (double*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as a Date
 * pointer.
 */
RegDate::Data* DataFrame::getDatePtr(unsigned int index) 
{
  return (RegDate::Data*)&((*this)[index]);
}

/**.......................................................................
 * Return a pointer to our internal data, cast as a Complex<float>
 * pointer.
 */
Complex<float>::Data* DataFrame::getComplexFloatPtr(unsigned int index) 
{
  return (Complex<float>::Data*)&((*this)[index]);
}

/**.......................................................................
 * Lock the frame.
 */
void DataFrame::lock()
{
  LogStream errStr;
  
  DBPRINT(false, Debug::DEBUG7, "Inside lock: " << this);
  
  guard_.lock();
}

/**.......................................................................
 * Unlock the frame.
 */
void DataFrame::unlock()
{
  LogStream errStr;
  
  DBPRINT(false, Debug::DEBUG7, "Inside unlock: " << this);
  
  guard_.unlock();
}

/**.......................................................................
 * Reset the counter which will be used for running averages
 */
void DataFrame::resetRunningAvgCounter()
{
  nAvg_ = 1;
}

/**.......................................................................
 * Incremen the counter which will be used for running averages
 */
void DataFrame::incrementRunningAvgCounter()
{
  nAvg_++;
}

