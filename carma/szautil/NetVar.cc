#include "carma/szautil/NetVar.h"

#include <arpa/inet.h>

using namespace std;

using namespace sza::util;
using namespace sza::util;

/**
 * Constructor.
 */
NetVar::NetVar(sza::util::DataType::Type type, void* vPtr, unsigned nEl, bool convert)
{
  type_       = type;
  vPtr_       = vPtr;
  resizeable_ = false;
  nBytePerEl_ = DataType::sizeOf(type_);
  convert_ = convert;

  setNel(nEl);
}

NetVar::NetVar(sza::util::DataType::Type type, void* vPtr, bool convert)
{
  type_       = type;
  vPtr_       = vPtr;
  resizeable_ = true;
  nBytePerEl_ = DataType::sizeOf(type_);
  convert_    = convert;

  setNel(nEl());
}

/**.......................................................................
 * Return the number of elements in this variable
 */
unsigned NetVar::nEl()
{
  if(!resizeable_) 
    return nEl_;
  else {
    switch(type_) {
    case DataType::STRING:
      {
	std::string* strPtr = (std::string*) vPtr_;
	
	if(strPtr->size() == 0)
	  strPtr->resize(1);

	return strPtr->size();

      }
      break;

    case DataType::UCHAR:
      {
	std::vector<unsigned char>* ucharVecPtr = (std::vector<unsigned char>*) vPtr_;

	// Always set the size of resizeable objects to something
	// finite.  Otherwise, attetmpts to access the uderlying
	// memory will fail.

	if(ucharVecPtr->size() == 0)
	  ucharVecPtr->resize(1);

	return ucharVecPtr->size();
      }
      break;
    default:    
      ThrowError("Unhandled vector type: " << type_);
      break;
    }
  }
}

/**.......................................................................
 * Set the number of elements in this variable, for a fixed-length
 * variable
 */
void NetVar::setNel(unsigned nEl)
{
  nEl_ = nEl;
  NetDat::resize(nBytePerEl_ * nEl_ + sizeOfPrefix());
  
  if(resizeable_) {
    switch(type_) {
    case DataType::UCHAR:
      {
	std::vector<unsigned char>* ucharVecPtr = (std::vector<unsigned char>*) vPtr_;
	ucharVecPtr->resize(nEl_);
      }
      break;
    case DataType::STRING:
      {
	std::string* strPtr = (std::string*) vPtr_;
	strPtr->resize(nEl_);
      }
      break;
    default:    
      ThrowError("Unhandled vector type: " << type_);
      break;
    }
  }
}

/**.......................................................................
 * Copy constructor
 */
NetVar::NetVar(const NetVar& netVar)
{
  type_ = netVar.type_;
  nEl_  = netVar.nEl_;
}

/**.......................................................................
 * Copy constructor
 */
NetVar::NetVar(NetVar& netVar)
{
  type_ = netVar.type_;
  nEl_  = netVar.nEl_;
}

/**.......................................................................
 * Destructor.
 */
NetVar::~NetVar() {}

/**.......................................................................
 * Recalculate size and resize the underlying byte array
 */
void NetVar::resize()
{
  // Resize the underlying byte array, if necessary

  setNel(nEl());
}

/**.......................................................................
 * Serialize the data
 */
void NetVar::serialize()
{
  // Copy from the source pointer to the byte array, either 1-1 (if no
  // network byte order conversion was requested), or converting to
  // network byte order if requested

  if(size() > 0) {
    unsigned char* src  = getPtr();
    unsigned char* dest = &bytes_[0];

    // Write the number of elements, if relevant.  If requested to
    // convert to network byte order, convert the number of elements,
    // regardless of whether or not the type requires conversion
    
    if(resizeable_) {

      unsigned nEl = nEl_;
      if(convert_)
	nEl = htonl(nEl_);

      unsigned char* tmp = (unsigned char*) &nEl;
      for(unsigned i=0; i < sizeof(unsigned int); i++) {
	*dest++ = *tmp++;
      }
    }
    
    // Now write the data.  If converting, convert each element to
    // network byte order.  If not converting, just copy the bytes 1-1
    
    if(convert()) {

      if(nBytePerEl_ == 2) {
	unsigned short* hptr = (unsigned short*)src;
	for(unsigned i=0; i < nEl(); i++) {
	  unsigned short ns = htons(hptr[i]);
	  unsigned char* sptr = (unsigned char*) &ns;
	  *dest++ = *sptr++;
	  *dest++ = *sptr++;
	}
      } else if(nBytePerEl_ == 4) {
	unsigned int* hptr = (unsigned int*)src;
	for(unsigned i=0; i < nEl(); i++) {
	  unsigned int nl = htonl(hptr[i]);
	  unsigned char* sptr = (unsigned char*) &nl;
	  *dest++ = *sptr++;
	  *dest++ = *sptr++;
	  *dest++ = *sptr++;
	  *dest++ = *sptr++;
	}
      }

      // Else not converting

    } else {
      unsigned nDataByte = nEl() * nBytePerEl_;
      for(unsigned i=0; i < nDataByte; i++) {
	*dest++ = *src++;
      }
    }
  }
}

/**.......................................................................
 * If this is a resizeable variable, write the number of elements
 *
 * Else do nothing
 */
void NetVar::prependNel(unsigned char* dest)
{
  if(resizeable_) {
    unsigned char* src = (unsigned char*) &nEl_;
    for(unsigned i=0; i < sizeof(unsigned int); i++) {
      *dest++ = *src++;
    }

    unsigned nElTest;
    unsigned char* ret = (unsigned char*) &nElTest;

    *(ret+0) = *(src-4);
    *(ret+1) = *(src-3);
    *(ret+2) = *(src-2);
    *(ret+3) = *(src-1);
  }
}

/**.......................................................................
 * If this is a resizeable variable, parse the number of elements.
 *
 * Else do nothing
 */
void NetVar::parseNel(const unsigned char* src)
{
  if(resizeable_) {
    unsigned char* dest = (unsigned char*) &nEl_;
    for(unsigned i=0; i < sizeof(unsigned int); i++) {
      *dest++ = *src++;
    }

    setNel(nEl_);
  }
}

/**.......................................................................
 * Method for deserialization of variables
 */
void NetVar::deserialize(const std::vector<unsigned char>& bytes) 
{
  deserialize(&bytes[0]);
}

/**.......................................................................
 * Private deserialization method
 */
void NetVar::deserialize(const unsigned char* src) 
{
  // Read the number of elements in the source vector, if present

  if(resizeable_) {

    unsigned nEl;
    unsigned char* tmp = (unsigned char*) &nEl;
    for(unsigned i=0; i < sizeof(unsigned int); i++) {
      *tmp++ = *src++;
    }

    if(convert_)
      nEl_ = ntohl(nEl);
    else
      nEl_ = nEl;

    setNel(nEl_);
  }

  // Now extract the data from it

  unsigned char* dest = getPtr();

  // If we are converting from network byte order to host order, see
  // if this type requires conversion

  if(convert()) {

      if(nBytePerEl_ == 2) {
	unsigned short* hptr = (unsigned short*)src;
	for(unsigned i=0; i < nEl(); i++) {
	  unsigned short ns = ntohs(hptr[i]);
	  unsigned char* sptr = (unsigned char*) &ns;
	  *dest++ = *sptr++;
	  *dest++ = *sptr++;
	}
      } else if(nBytePerEl_ == 4) {

	unsigned int* hptr = (unsigned int*)src;
	for(unsigned i=0; i < nEl(); i++) {
	  unsigned int hl = ntohl(hptr[i]);

	  unsigned char* sptr = (unsigned char*) &hl;
	  *dest++ = *sptr++;
	  *dest++ = *sptr++;
	  *dest++ = *sptr++;
	  *dest++ = *sptr++;
	}
      }

    // Else not converting

  } else {
    unsigned nDataByte = nEl() * nBytePerEl_;
    for(unsigned i=0; i < nDataByte; i++) {
      *dest++ = *src++;
    }
  }
}

unsigned NetVar::sizeOfPrefix()
{
  return resizeable_ ? sizeof(unsigned int) : 0;
}

void NetVar::checkSize(const std::vector<unsigned char>& bytes)
{
  if(!resizeable_) {
    return NetDat::checkSize(bytes);
  } else {

    unsigned nEl = (bytes.size() - sizeOfPrefix()) / nBytePerEl_;
    setNel(nEl);
  }
}

unsigned char* NetVar::getPtr()
{
  if(!resizeable_) {
    return (unsigned char*)vPtr_;
  } else {
    switch(type_) {
    case DataType::UCHAR:
      {
	std::vector<unsigned char>* ucharVecPtr = (std::vector<unsigned char>*) vPtr_;
	return &(ucharVecPtr->at(0));
      }
      break;
    case DataType::STRING:
      {
	std::string* strPtr = (std::string*) vPtr_;
	return (unsigned char*)(&(strPtr->at(0)));
      }
      break;
    default:    
      ThrowError("Unhandled vector type: " << type_);
      break;
    }
  }
}

unsigned NetVar::size()
{
  return nEl() * nBytePerEl_ + sizeOfPrefix();
}

bool NetVar::isResizeable()
{
  return resizeable_;
}

/**.......................................................................
 * Return whether or not we need to convert to/from network byte order
 */
bool NetVar::convert()
{
  return convert_ && 
    (type_ == DataType::UINT   || 
     type_ == DataType::INT    ||
     type_ == DataType::USHORT ||
     type_ == DataType::SHORT);
}
