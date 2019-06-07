#include "carma/szautil/ArchiveFileHandler.h"
#include "carma/szautil/Date.h"
#include "carma/szautil/RegDate.h"

#include "carma/szaarrayutils/arraytemplate.h"

#include<iostream>
#include<iomanip>

using namespace std;

using namespace sza::util;
using namespace sza::array;

#define UNSAFE 1

/**.......................................................................
 * Constructor.
 */
ArchiveFileHandler::ArchiveFileHandler() 
{
  arrayMap_             = 0;
  nrs_                  = 0;
  nrsSize_              = 0;
  lastArrayMapSize_     = 0;
  nBytesInMsgHeader_    = 8;

  offsetInBytesOfFrameUtcInArrayMap_ = 0;
}

/**.......................................................................
 * Const Copy Constructor.
 */
ArchiveFileHandler::ArchiveFileHandler(const ArchiveFileHandler& objToBeCopied)
{
  *this = (ArchiveFileHandler&)objToBeCopied;
};

/**.......................................................................
 * Copy Constructor.
 */
ArchiveFileHandler::ArchiveFileHandler(ArchiveFileHandler& objToBeCopied)
{
  *this = objToBeCopied;
};

/**.......................................................................
 * Const Assignment Operator.
 */
void ArchiveFileHandler::operator=(const ArchiveFileHandler& objToBeAssigned)
{
  *this = (ArchiveFileHandler&)objToBeAssigned;
};

/**.......................................................................
 * Assignment Operator.
 */
void ArchiveFileHandler::operator=(ArchiveFileHandler& objToBeAssigned)
{
  std::cout << "Calling default assignment operator for class: ArchiveFileHandler" << std::endl;
};

/**.......................................................................
 * Output Operator.
 */
std::ostream& sza::util::operator<<(std::ostream& os, ArchiveFileHandler& obj)
{
  os << "Default output operator for class: ArchiveFileHandler" << std::endl;
  return os;
};

/**.......................................................................
 * Destructor.
 */
ArchiveFileHandler::~ArchiveFileHandler() 
{
  if(nrs_) {
    nrs_ = del_NetReadStr(nrs_);
    nrsSize_  = 0;
  }

  // Delete arrayMap_ with del_ArrayMap() for reference counting
  // correctness

  if(arrayMap_) {
    arrayMap_ = del_ArrayMap(arrayMap_);
  }
}

/**.......................................................................
 * Overloaded open function from the base class that also initializes the 
 * read buffer
 */
void ArchiveFileHandler::openForRead(bool memMap)
{
  FileHandler::openForRead(false);

  initializeNetReadStr();

#if UNSAFE

  if(readSize() || arrayMap_ == 0) {
    unsigned prevOffset = currentOffset_;

    // Now try to read the array map.  If we fail (because the file
    // was truncated presumably) delete any current arraymap and set
    // the last array map size to zero, since we won't actually know
    // the size of this array map.

    try {
      readArrayMap();
    } catch(Exception& err) {
      arrayMap_ = del_ArrayMap(arrayMap_);
      lastArrayMapSize_ = 0;
      throw err;
    }

    lastArrayMapSize_ = currentOffset_ - prevOffset;

  } else {
    if(lastArrayMapSize_ == 0) {
      ThrowError("No array map has been read");
    }

    advanceByNbytes(lastArrayMapSize_);
  }
#else
  readSize();
  readArrayMap();
#endif

  // The number of bytes in the file header is just the current offset

  nBytesInFileHeader_ = currentOffset_;

  // Number of bytes per frame in the file is the size of the array
  // map, plus the 8-byte header

  nBytesInFrameMsg_ = arrayMap_->nByte(true) + nBytesInMsgHeader_;
  nFramesInFile_    = (sizeInBytes_ - currentOffset_) / nBytesInFrameMsg_;

  // Precompute the offset of the array.frame.utc register

  offsetInBytesOfFrameUtcInArrayMap_ = 
    arrayMap_->byteOffsetOf(true, "array", "frame", "utc");

  if(memMap) {
    memoryMap();
  }

}

void ArchiveFileHandler::readTimestamps()
{
  sza::util::RegDate date;

  TimeVal start, stop, diff;

  start.setToCurrentTime();

  for(unsigned iFrame=0; iFrame < nFramesInFile_; iFrame++) {
    advanceToFrame(iFrame, offsetInBytesOfFrameUtcInArrayMap_);
    read((void*)date.data(), 8);
  }

  stop.setToCurrentTime();
  diff = stop - start;
}

/**.......................................................................
 * Initialize the net read stream
 */
void ArchiveFileHandler::initializeNetReadStr()
{
  // If the net read stream hasn't been allocated yet, do it now

  if(nrs_ == 0) {

    nrs_ = new_NetReadStr(fd_, 100);

    if(nrs_ == 0) {
      ThrowError("Unable to initialize the NetReadStr object");
    }

  }

  // Attach the fd of this file to the net read stream object

  attach_NetReadStr(nrs_, fd_);
}

/**.......................................................................
 * Read the arraymap from the file
 */
void ArchiveFileHandler::readArrayMap()
{
  int opcode;                 // The type of message that has been read
  NetBuf* net=0;              // The network input buffer 
  ArrayMap* newArrayMap=0;

  // Read the next message from the control program.

  switch(readNextMsg()) {
  case MS_READ_DONE:
    break;
  default:
    ThrowError("Error reading array map from " << path_);
    break;
  };

  // Get an alias to the input buffer.

  net = nrs_->net;
  
  // Unpack the array template from the input buffer.

  if(net_start_get(net, &opcode) ||
     opcode != ARC_ARRAYMAP_RECORD ||
     (newArrayMap = net_get_ArrayMap(net)) == NULL ||
     net_end_get(net)) {
    ThrowError("The array-map in file " << path_ << " is corrupt");
  };

  // If we successfully allocated the new array map, decrement the
  // reference count of the old one (and potentially delete it), and
  // reassign it

  arrayMap_ = del_ArrayMap(arrayMap_);
  arrayMap_ = newArrayMap;
}

/**.......................................................................
 * Get the maximum buffer size from the file
 */
bool ArchiveFileHandler::readSize()
{
  int nrsSize; /* The new size extracted from the file */
  int opcode;    /* The type of message that has been read */
  NetBuf *net;   /* The network input buffer */
  
  // Get an alias to the input buffer.

  net = nrs_->net;
  
  // Read the next message from the current file.

  switch(readNextMsg()) {
  case MS_READ_DONE:
    break;
  default:
    ThrowError("Error reading frame size from " << path_);
    break;
  };
  
  // Unpack the message.

  if(net_start_get(net, &opcode) ||
     opcode != ARC_SIZE_RECORD ||
     net_get_long(net, 1, (unsigned long* )&nrsSize) ||
     net_end_get(net)) {
    ThrowError("Error reading " << path_);
  };

  // If ths size is greater than the length of this file, this means
  // the file is truncated

  if(nrsSize > sizeInBytes_) {
    ThrowError("File " << path_ << " appears to be truncated");
  }

  // If the size requirements have changed or haven't been set before,
  // replace the current network buffer with a new one of the required
  // size.

  if(nrsSize_ != nrsSize) {

    nrsSize_ = nrsSize;
    nrs_ = del_NetReadStr(nrs_);
    nrs_ = new_NetReadStr(fd_, nrsSize_);

    if(!nrs_) {
      ThrowError("Unable to allocate new NetReadStr object");
    }

    // Return true if the size has changed

    return true;
  }

  // Return false if the size hasn't changed

  return false;
}

/**.......................................................................
 * Get the next message from the file
 */
sza::array::MsReadState ArchiveFileHandler::readNextMsg()
{
  // Read the next message.

  for(;;) {
    switch(nrs_read_msg(nrs_)) {
    case NetReadStr::NET_READ_SIZE:
    case NetReadStr::NET_READ_DATA:
      break;
    case NetReadStr::NET_READ_DONE:
      currentOffset_ += nrs_->net->nput;
      return MS_READ_DONE;
      break;
    case NetReadStr::NET_READ_ERROR:
      ReportError("Skipping the rest of archive file: " << path_);
      currentOffset_ += nrs_->net->nput;
      return MS_READ_ENDED;
      break;
    case NetReadStr::NET_READ_CLOSED:
      return MS_READ_ENDED;
      break;
    };
  };

}

/**.......................................................................
 * Advance to the requested frame (plus any additional offset)
 */
void ArchiveFileHandler::advanceToFrame(unsigned iFrame, off_t regOffset)
{
  if(iFrame > nFramesInFile_ - 1) {
    ThrowError("Invalid frame number: " << iFrame << ". Should be < " 
	       << nFramesInFile_ - 1)
  }

  // Calculate the byte offset from the start of the file to get to
  // the head of the requested frame

  off_t offset = nBytesInFileHeader_ // Advance past the file header
    + iFrame * nBytesInFrameMsg_     // Plus the number of frames to skip
    + nBytesInMsgHeader_             // Skip the msg header of the frame we want 
    + regOffset;                     // And skip to the start of the register we want

  // Convert to offset from the current offset

  offset -= currentOffset_;

  advanceByNbytes(offset);
}

/**.......................................................................
 * Get the MJD of the specified frame
 */
double ArchiveFileHandler::getMjd(unsigned iFrame)
{
  RegDate date;

  advanceToFrame(iFrame, offsetInBytesOfFrameUtcInArrayMap_);
  read((void*)date.data(), 8);

  return date.mjd();
}

/**.......................................................................
 * Binary search for the first frame in the file before (or equal to)
 * the specified date
 */
unsigned ArchiveFileHandler::findFirstFrameBefore(std::string date)
{
  Date targetDate;
  targetDate.setToDateAndTime(date);
  double mjd = targetDate.getMjd();

  unsigned iLow=0, iHi=nFramesInFile_-1, iMid;
  double mjdLow, mjdHi, mjdMid;

  do {

    iMid = (iHi + iLow)/2;

    mjdMid = getMjd(iMid);
    mjdLow = getMjd(iLow);
    mjdHi  = getMjd(iHi);

    // Take care of the case where the mjd is not bracketed in this
    // file

    if(mjdLow > mjd) {  
      return iLow;
    } else if(mjdHi < mjd) {
      return iHi;
      
      // Now do the binary search proper

    } else if(mjd >= mjdMid) {
      iLow = iMid;
    } else if(mjd < mjdMid) {
      iHi = iMid;
    }

  } while(iHi - iLow > 1);

  return mjdHi == mjd ? iHi : iLow;
}

/**.......................................................................
 * Binary search for the first frame in the file after (or equal to)
 * the specified date
 */
unsigned ArchiveFileHandler::findFirstFrameAfter(std::string date)
{
  Date targetDate;
  targetDate.setToDateAndTime(date);
  double mjd = targetDate.getMjd();

  unsigned iLow=0, iHi=nFramesInFile_-1, iMid;
  double mjdLow, mjdHi, mjdMid;

  do {

    iMid = (iHi + iLow)/2;

    mjdMid = getMjd(iMid);
    mjdLow = getMjd(iLow);
    mjdHi  = getMjd(iHi);

    // Take care of the case where the mjd is not bracketed in this
    // file

    if(mjdLow > mjd) {  
      return iLow;
    } else if(mjdHi < mjd) {
      return iHi;
      
      // Now do the binary search proper

    } else if(mjd >= mjdMid) {
      iLow = iMid;
    } else if(mjd < mjdMid) {
      iHi = iMid;
    }

  } while(iHi - iLow > 1);

  return mjdLow == mjd ? iLow : iHi;
}


unsigned ArchiveFileHandler::nFrame()
{
  return nFramesInFile_;
}
