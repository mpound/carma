#include "carma/szautil/RtdArrayTemplate.h"
#include <cstring>

#include "lzma.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RtdArrayTemplate::RtdArrayTemplate(ArrayTemplate* arrayTemplate) 
{
  setTo(arrayTemplate);
}

/**.......................................................................
 * Constructor.
 */
RtdArrayTemplate::RtdArrayTemplate() 
{
  external_      = false;
  arrayTemplate_ = 0;
}

void RtdArrayTemplate::setTo(ArrayTemplate* arrayTemplate) 
{
  external_      = true;
  arrayTemplate_ = arrayTemplate;

  constructAsciiTemplate();
  compressAsciiTemplate();
}

/**.......................................................................
 * Destructor.
 */
RtdArrayTemplate::~RtdArrayTemplate() 
{
  if(arrayTemplate_ && !external_) {
    free(arrayTemplate_);
    arrayTemplate_ = 0;
  } 
}

ArrayTemplate* RtdArrayTemplate::getArrayTemplate()
{
  return arrayTemplate_;
}

/**.......................................................................
 * Convert an array template to an ascii listing of registers
 */
void RtdArrayTemplate::constructAsciiTemplate()
{
  std::ostringstream os;
  unsigned iReg=0;

  // Now iterate over regtemplates, serializing each one

  //  COUTCOLOR("array template has " << arrayTemplate_->ntemplate << " templates", "blue");
  for(unsigned iRegTemp=0; iRegTemp < arrayTemplate_->ntemplate; iRegTemp++) {
    RegTemp& regTemp = arrayTemplate_->templates[iRegTemp];

    //    COUTCOLOR("template has " << regTemp.regtemplate->nboard << " boards", "blue");
    for(unsigned iBoard=0; iBoard < regTemp.regtemplate->nboard; iBoard++) {
      RegBoardTemp& brdTemp = regTemp.regtemplate->boards[iBoard];

      //      COUTCOLOR("board has " << brdTemp.nblock_ << " blocks", "blue");
      for(int iBlock = 0; iBlock < brdTemp.nblock_; iBlock++) {
	RegBlockTemp& blkTemp = brdTemp.blocks_[iBlock];
	regTempMap_[iReg] = &blkTemp;

	os << iReg << " " << regTemp.name << "." << brdTemp.name_ << "." << blkTemp.name_ << *blkTemp.axes_ 
	   << " " << DataType::typeOf(&blkTemp) << std::endl;
	++iReg;
      }
    }
  }

  asciiTemplate_ = os.str();
}

/**.......................................................................
 * Print the ascii version of this template
 */
void RtdArrayTemplate::print()
{
  COUT(asciiTemplate_);
}

/**.......................................................................
 * Take an ascii template and compress it in xz format
 */
void RtdArrayTemplate::compressAsciiTemplate()
{
  //------------------------------------------------------------
  // Start by setting the byte array to zero size
  //------------------------------------------------------------

  bytes_.resize(0);

  //------------------------------------------------------------
  // Now initialize lzma stuff.  We pick a buffer size and initialize
  // the pertinent lzma stream variables to point to the beginning of
  // the input buffer and the beginning of the output buffer.
  // avail_out is supposed to be the number of bytes still available
  // in the output stream so that at any time nBufMax - strm.avail_out
  // should represent the number of bytes that have been placed there
  // by the coder
  //------------------------------------------------------------

  lzma_stream strm = LZMA_STREAM_INIT;

  unsigned nBufMax = 10000;
  unsigned char inBuf[nBufMax];
  unsigned char outBuf[nBufMax];

  strm.next_in   = inBuf;
  strm.avail_in  = 0;

  strm.next_out  = outBuf;
  strm.avail_out = nBufMax;

  lzma_ret retVal = lzma_easy_encoder(&strm, 0, LZMA_CHECK_CRC64);

  if(retVal != LZMA_OK) {
    ThrowError("easy_encoder returtned with error");
  }

  //-----------------------------------------------------------------------
  // Now process all bytes from the asciiTemplate, writing the
  // compressed bytes to a storage array as the output buffer becomes full
  //-----------------------------------------------------------------------

  int nread = 0;

  std::ostringstream os;

  // Iterate through the whole string until the number of processed
  // bytes equals the string length

  unsigned strLen = asciiTemplate_.size();
  unsigned nProcessed = 0;
  bool eof = false;
  unsigned nCurr = 0;
  lzma_action action;

  while(retVal != LZMA_STREAM_END) {
    
    //------------------------------------------------------------
    // If there is room in the buffer, copy the next chunk of data
    // into the input buffer
    //------------------------------------------------------------
    
    if(strm.avail_in == 0) {

      //------------------------------------------------------------
      // If the size of the remaining string to be processed is larger
      // than the buffer, read nBufMax only
      //------------------------------------------------------------
      
      if(strLen - nProcessed > nBufMax) {
	
	for(unsigned i=0; i < nBufMax; i++)
	  inBuf[i] = asciiTemplate_[nProcessed + i];
	
	nCurr = nBufMax;
	
	//------------------------------------------------------------
	// Else just read the rest of the string
	//------------------------------------------------------------
	
      } else {
	
	for(unsigned i=0; i < (strLen - nProcessed); i++)
	  inBuf[i] = asciiTemplate_[nProcessed + i];
	
	nCurr = strLen - nProcessed;
      }

      strm.next_in   = inBuf;
      strm.avail_in  = nCurr;

      nProcessed += nCurr;

      // We stop when all the bytes have been placed into the input
      // buffer

      eof = (nProcessed == strLen);
      action = eof ? LZMA_FINISH : LZMA_RUN;
    }
    
    //    COUT("Beforeavail_in = " << strm.avail_in << " avail_out = " << strm.avail_out);

    retVal = lzma_code(&strm, action);

    //    COUT("After avail_in = " << strm.avail_in << " avail_out = " << strm.avail_out);
    //    COUT("retVal was " << retVal << " END = " << LZMA_STREAM_END << " avail = " << strm.avail_out);
      
    if(retVal == LZMA_OK || retVal == LZMA_STREAM_END) {
      
      // If the buffer is now full(avail_out = 0), write out the
      // compressed data
      
      if(strm.avail_out == 0 || retVal == LZMA_STREAM_END) {
	//	COUT("Writing " << (nBufMax - strm.avail_out) << " bytes");
	addTo(bytes_, outBuf, nBufMax - strm.avail_out);
	strm.next_out   = outBuf;
	strm.avail_out  = nBufMax;
      }
      
    } else {
      //      COUT("Throwing error because retVal = " << retVal);
      ThrowError("lzma_code returned with error");
    }
  }
}

void RtdArrayTemplate::addTo(std::vector<unsigned char>& arr, unsigned char* ptr, unsigned nbyte)
{
  unsigned prevSize = arr.size();
  arr.resize(prevSize + nbyte);
  for(unsigned i=0; i < nbyte; i++)
    arr[prevSize + i] = ptr[i];
}

void RtdArrayTemplate::deserialize(const std::vector<unsigned char>& bytes)
{
  bytes_ = bytes;
}

void RtdArrayTemplate::deserialize(const unsigned char* bytes)
{
  for(unsigned i=0; i < bytes_.size(); i++) {
    bytes_[i] = bytes[i];
  }
}

void RtdArrayTemplate::serialize()
{
  // Do nothing.  Compressed template will already be contained in bytes_ array
}

void RtdArrayTemplate::writeToFile()
{
  int fd = -1;
  fd = ::open("emlTest.xz", O_RDWR | O_CREAT, S_IRWXU);

  if(fd < 0) {
    ThrowError("Unable to open file: emlTest.xz");
  }

  COUT("Writing " << bytes_.size() << " to the file");

  ::write(fd, &bytes_[0], bytes_.size());

  ::close(fd);
}
