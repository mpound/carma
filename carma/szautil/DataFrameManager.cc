#include "carma/szautil/AntNum.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/DataFrameManager.h"

#include "carma/szaarrayutils/scanner.h"

using namespace sza::util;

/**.......................................................................
 * Initialize the frame manager.
 */
void DataFrameManager::initialize()
{
  nBuffer_           = 0;
  nUsed_             = 0;
  currentIndex_      = 0;
  dataIsInitialized_ = false;
  frame_             = 0;
}

/**.......................................................................
 * Constructor with no resizing of the initially zero-length DataFrame
 * buffer.  This constructor doesn't intialize the antenna number
 * associated with this manager object.
 */
DataFrameManager::DataFrameManager()
{
  initialize();
}


/**.......................................................................
 * Constructor with resizing of the initially zero-length DataFrame
 * buffer
 */
DataFrameManager::DataFrameManager(unsigned nBuffer)
{
  initialize();
  nBuffer_ = nBuffer;
}

/**.......................................................................
 * Copy constructor
 */
DataFrameManager::DataFrameManager(DataFrameManager& fm)
{
  dataIsInitialized_  = fm.dataIsInitialized_;
  currentIndex_       = fm.currentIndex_;
  nBuffer_            = fm.nBuffer_;
  nUsed_              = fm.nUsed_;

  // Initialize the data frame object
  
  if(dataIsInitialized_ && nBuffer_ > 0) 
    *frame_ = *fm.frame_;
}

/**.......................................................................
 * Constructor with initialization from a DataFrame object.
 */
DataFrameManager::DataFrameManager(DataFrame* frame)
{
  // Sanity check

  if(frame == 0)
    throw Error("DataFrameManager::DataFrameManager: "
		"frame argument is NULL.\n");

  if(frame->size() > 0)
    dataIsInitialized_  = true;

  currentIndex_ = 0;
  nBuffer_      = frame->size();
  nUsed_        = 0;
  
  // Copy the passed frame
  
  if(dataIsInitialized_ && nBuffer_ > 0) 
    *frame_ = *frame;
}

/**.......................................................................
 * Destructor
 */
DataFrameManager::~DataFrameManager() 
{
  if(frame_ != 0) {
    delete frame_;
    frame_ = 0;
  }
}

/**.......................................................................
 * Extend the length of the DataFrame buffer
 */
void DataFrameManager::resize(unsigned nBuffer)
{
  LogStream logStr;

  lock();

  try {

    // Initialize the DataFrame object
    
    frame_->resize(nBuffer);
    
    // Initialize the DataFrameManager's bookeeping variables
    
    nUsed_             = 0;
    nBuffer_           = nBuffer;
    dataIsInitialized_ = true;

  } catch(...) {
    logStr.appendMessage(true, "Error resizing the frame buffer");
  }

  unlock();

  if(logStr.isError())
    throw Error(logStr);
}

/**.......................................................................
 * Pack a series of unsigned ints into the Frame
 *
 * @throws Exception
 */
void DataFrameManager::pack(void* data, unsigned ndata, DataType::Type type,
			    int startByteIndex)
{
  LogStream logStr;

  unsigned sizeOfType = DataType::sizeOf(type);

  lock();

  if(startByteIndex < 0 && ndata*sizeOfType > nBuffer_ - nUsed_) 
    logStr.appendMessage(true, "Not enough room in the data buffer");
  else if(startByteIndex >= 0 && startByteIndex + ndata*sizeOfType > nBuffer_)
    logStr.appendMessage(true, "Not enough room in the data buffer");
  else {
    
    try {
      
      // Pack the data into the frame
      
      frame_->pack(data, ndata, type, startByteIndex >= 0 ? 
		   startByteIndex : currentIndex_);

      // Increment the number of use slots in the byte array

      nUsed_ += ndata*sizeOfType;

      // If no start index was passed, increment the current index by
      // the number of bytes just written

      if(startByteIndex < 0)
	currentIndex_ += ndata*sizeOfType;

    } catch(...) {
      logStr.appendMessage(true, "Caught an exception (DFM0)");
    }
  }
  
  unlock();

  if(logStr.isError())
    throw Error(logStr);
}

/**.......................................................................
 * Pack an array of unsigned ints
 *
 * @throws Exception
 */
void DataFrameManager::pack(unsigned int* data, unsigned ndata, 
			    int startIndex)
{
  pack((void*)data, ndata, DataType::UINT, startIndex);
}

/**.......................................................................
 * Pack an array of unsigned longs
 *
 * @throws Exception
 */
void DataFrameManager::pack(unsigned long* data, unsigned ndata, 
			    int startIndex)
{
  pack((void*)data, ndata, DataType::ULONG, startIndex);
}

/**.......................................................................
 * Pack an array of floats
 *
 * @throws Exception
 */
void DataFrameManager::pack(float* data, unsigned ndata, 
			    int startIndex)
{
  pack((void*)data, ndata, DataType::FLOAT, startIndex);
}

/**.......................................................................
 * Incremental unpack.
 * 
 * @throws Exception
 */
void DataFrameManager::unpack(unsigned char* data, unsigned ndata)
{
  LogStream logStr;

  lock();

  if(ndata > nBuffer_) 
    logStr.appendMessage(true, "Not enough data in buffer");
  else {
   
    try {

      // Copy the next ndata elements of the frame into the data buffer
      
      for(unsigned idata=0; idata < ndata; idata++) 
	data[idata] = (*frame_)[currentIndex_++];

    } catch(...) {
      logStr.appendMessage(true, "Caught an exception (DFM1)");
    }
  }
  
  unlock();

  if(logStr.isError())
    throw Error(logStr);
}

/**.......................................................................
 * Unpack all the Frame data into an array of unsigned ints.  Doesn't
 * modify the currentIndex_ counter.
 * 
 * @throws Exception
 */
void DataFrameManager::unpack(unsigned char* data)
{
  LogStream logStr;
  unsigned char* ptr = frame_->getUcharPtr();

  lock();

  try {
    
    // Copy ALL the data in the frame buffer.  We 
    
    for(unsigned idata=0; idata < nBuffer_; idata++) 
      data[idata] = ptr[idata];
    
  } catch(...) {
    logStr.appendMessage(true, "Caught an exception (DFM2)");
  }

  unlock();
}

/**.......................................................................
 * Advance the internal index counter
 *
 * @throws Exception
 */
void DataFrameManager::advance(unsigned ndata)
{
  LogStream logStr;

  lock();

  if(currentIndex_ + ndata > nBuffer_-1)
    logStr.appendMessage(true, "Resulting index exceeds the array dimension");
  else {
    
    // Advance the index
    
    currentIndex_ += ndata;
  }

  unlock();

  if(logStr.isError())
    throw Error(logStr);
}

/**.......................................................................
 * Fill the next nData registers with a constant value
 *
 * @throws Exception
 */
void DataFrameManager::fillBuffer(unsigned char val, unsigned ndata)
{
  LogStream logStr;

  lock();

  if(ndata > nBuffer_ - nUsed_)
    logStr.appendMessage(true, "Not enough room in the data buffer");
  else {
  
    try {

      // Copy the data into the frame buffer
      
      for(unsigned idata=0; idata < ndata; idata++) {
	(*frame_)[currentIndex_++] = val;
	nUsed_++;
      }
    } catch(...) {
      logStr.appendMessage(true, "Caught an exception (DFM3)");
    }
  }

  unlock();

  if(logStr.isError())
    throw Error(logStr);
}

/**.......................................................................
 * Fill the all registers with a constant value
 *
 * @throws Exception
 */
void DataFrameManager::fillBuffer(unsigned char val)
{
  LogStream logStr;

  lock();

  try {
    
    // Copy the data into the frame buffer
    
    for(unsigned idata=0; idata < frame_->size(); idata++) {
      (*frame_)[idata] = val;
      nUsed_++;
    }
  } catch(...) {
    logStr.appendMessage(true, "Caught an exception (DFM4)");
  }

  unlock();

  if(logStr.isError())
    throw Error(logStr);
}

/**.......................................................................
 * Set the error-flag status member
 */
void DataFrameManager::setErrorStatus(bool wasError)
{
  LogStream logStr;

  lock();

  try { // operator[] could throw an exception
    *(frame_->getBoolPtr(0)) = wasError;
  } catch(...) {
    logStr.appendMessage(true, "Caught an exception (DFM5)");
  }

  unlock();

  if(logStr.isError())
    throw Error(logStr);
}

/**.......................................................................
 * Return a handle to the raw data frame managed by this object.
 * Warning: you must call lock() and unlock() explicitly if you use
 * this!
 */
DataFrame* DataFrameManager::frame()
{
  return frame_;
}

/**.......................................................................
 * Reset the bookkeeping parameters of a DataFrameManager object
 */
void DataFrameManager::reinitialize()
{
  lock();

  nUsed_        = 0;
  currentIndex_ = 0;
  fillBuffer(0);

  unlock();
}

/**.......................................................................
 * Lock the frame.
 */
void DataFrameManager::lock()
{
  frame_->lock();
}

/**.......................................................................
 * Unlock the frame.
 */
void DataFrameManager::unlock()
{
  frame_->unlock();
}

/**.......................................................................
 * Get a unique frame id based on integral MJD half-seconds.
 */
unsigned int getId() 
{
  DBPRINT(true, Debug::DEBUG10, "Here");

  return 0;
};

/**.......................................................................
 * Pack an entire data frame
 *
 * @throws Exception
 */
void DataFrameManager::pack(DataFrame* frame, int startIndex)
{
  // And pack the data portion into the location specified by startIndex

  pack((void*)frame->getUcharPtr(byteOffsetInFrameOfData()), sizeInBytesOfData(),
       DataType::UCHAR, startIndex);
}

/**.......................................................................
 * The data portion of the buffer will be offset by the size in bytes
 * of the header.
 */
unsigned DataFrameManager::
byteOffsetInFrameOfData()
{
  return SCAN_HEADER_DIM * sizeof(unsigned);
}

/**.......................................................................
 * The size in bytes is just the size of the frame
 */
unsigned DataFrameManager::
sizeInBytes()
{
  return frame_->size();
}

/**.......................................................................
 * The size of the data is the size in bytes, less the offset from the
 * head of the frame of the data.
 */
unsigned DataFrameManager::
sizeInBytesOfData()
{
  return sizeInBytes() - byteOffsetInFrameOfData();
}

/**.......................................................................
 * Overloadable base class assigment operator
 */
void DataFrameManager::operator=(DataFrameManager& fm)
{
  // Resize our data frame to match the passed one

  resize(fm.nBuffer_);

  // Assign our members

  reinitialize();

  lock();
  fm.lock();

  try {

    // Assign the underlying data frame
    
    *frame_ = *fm.frame_;

    // Unlock the frames if an exception was thrown

  } catch (const Exception& err) {

    unlock();
    fm.unlock();

    throw(err);
  } catch (...) {
    unlock();
    fm.unlock();
  }
}
