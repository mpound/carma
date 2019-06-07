#include "carma/szautil/NetBuf.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include <stdlib.h>
#include <unistd.h>
#include <netinet/in.h>
#include <float.h>
#include <errno.h>
#include <math.h>
#include <cstring>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructors.
 */
NetBuf::NetBuf() 
{
  privateConstructor(-1, 0, 0);
}

/**.......................................................................
 * Constructor with buffer
 */
NetBuf::NetBuf(unsigned int size, unsigned char* extBuf)
{
  privateConstructor(-1, size, extBuf);
}

/**.......................................................................
 * Constructor with buffer and file descriptor
 */
NetBuf::NetBuf(int fd, unsigned int size, unsigned char* extBuf)
{
  privateConstructor(fd, size, extBuf);
}

/**.......................................................................
 * A private constructor for initialization
 */
void NetBuf::privateConstructor(int fd, unsigned int size, unsigned char* extBuf)
{
  attach(fd);
  maxMsgSize_ =  0;
  external_   =  false;
  buffer_     =  0;
  nPut_       =  0;
  nGet_       =  0;

  setBuffer(size, extBuf);
}

/**.......................................................................
 * Destructor.
 */
NetBuf::~NetBuf() 
{
  if(!external_) {
    if(buffer_ != 0) {
      free(buffer_);
      buffer_ = 0;
    }
  }
}

/**.......................................................................
 * Allocate an internal buffer, or set it to point to external
 * memory
 */
void NetBuf::setBuffer(unsigned int size, unsigned char* extBuf)
{
  buffer_ = 0;
  maxMsgSize_ = size;
  
  if(extBuf==0) {
    
    external_ = false;
    buffer_ = (unsigned char*)malloc(maxMsgSize_ + msgPrefixSize());
    
    if(buffer_ == 0) 
      ThrowError("Uunable to allocate buffer");
    
  } else {
    external_ = true;
    buffer_ = extBuf;
  }
}

/**.......................................................................
 * Attach this buffer to a file descriptor
 */
void NetBuf::attach(int fd) 
{
  fd_ = fd;

  lastReadState_ = (fd_ < 0) ? NET_READ_CLOSED : NET_READ_DONE;
  lastSendState_ = (fd_ < 0) ? NET_SEND_CLOSED : NET_SEND_DONE;
}

/**.......................................................................
 * Read data into the buffer
 */
NetBuf::NetReadState NetBuf::read(int fd)
{
  // Determine how to respond from the I/O status last recorded.

  switch(lastReadState_) {
  case NET_READ_PREFIX:         // Read the message prefix
  case NET_READ_DATA:           // Continue reading an incompletely
				// read message
    break;

  case NET_READ_DONE:              // Prepare to read the byte-count prefix 
    setReadState(NET_READ_PREFIX); // of a new message 
    msgLen_ = msgPrefixSize();
    nPut_   = 0;
    nGet_   = 0;

    break;

  case NET_READ_CLOSED:           // Don't report errors more than once 
  case NET_READ_ERROR:
    return lastReadState_;
    break;
  };

  // Check the file descriptor

  int readFd = (fd < 0 ? fd_ : fd);

  if(readFd < 0) {
    ReportError("Invalid file dsscriptor");
    return setReadState(NET_READ_ERROR);
  }

  // And check the buffer 

  if(!buffer_) {
    ReportError("Buffer not provided.");
    return setReadState(NET_READ_ERROR);
  };

  // Read as many bytes of the message as possible.

  while(nPut_ < msgLen_) {
    int nread;  // The latest number of bytes returned by read() 
    
    // Read as many bytes as possible up to the required size.

    errno = 0;
    nread = ::read(readFd, buffer_ + nPut_, msgLen_ - nPut_);

    // Did we manage to read anything?

    if(nread > 0) {
      nPut_ += nread;
      
      // If we just completed reading the message-size prefix then
      // substitute the real value of the target message size for the
      // previous dummy size.

      if(nPut_ >= msgLen_ && lastReadState_ == NET_READ_PREFIX) {

	setReadState(NET_READ_DATA);

	try {
	  msgLen_ = parseMsgPrefix();
	  nPut_ = 0;
	  nGet_ = 0;
	} catch(...) {
	  return setReadState(NET_READ_ERROR);
	}

	if(msgLen_ > maxMsgSize_) {
	  ReportError("Bad message length: message length: " << msgLen_ << ", should be < " << maxMsgSize_);
	  return setReadState(NET_READ_ERROR);
	};
      };
    } else {
      
      // Check for read errors.

      switch(errno) {
	
	// Connection closed?

      case 0:

	if(nPut_ == 0) {
	  return setReadState(NET_READ_CLOSED);
	} else {
	  ReportError("End of stream reached prematurely");
	  return setReadState(NET_READ_ERROR);
	};
	break;
	
	// No data currently available for non-blocking I/O?

      case EWOULDBLOCK:     // BSD non-blocking I/O 
	return lastReadState_;
	break;

	// Continue after interrupts.

      case EINTR:
	break;
	
	// Report unexpected I/O errors.

      default:
	ReportSysError("read()");
	return setReadState(NET_READ_ERROR);
      };
    };
  };

  return setReadState(NET_READ_DONE);
}


/**.......................................................................
 * Write a previously composed network message.
 */
NetBuf::NetSendState NetBuf::send(int fd)
{
  // Determine how to respond from the I/O status last recorded.

  switch(lastSendState_) {
  case NET_SEND_DATA:             // Continue sending an incompletely 
    break;                        // sent message 

  case NET_SEND_DONE:             // Send a new message 
    lastSendState_ = NET_SEND_DATA;
    nGet_          = 0;
    
    // Check the message length encompasses at least the message prefix

    if(nPut_ < msgPrefixSize()) {

      ReportError("Message too short");
      return setSendState(NET_SEND_ERROR);

    } else if(nPut_ > maxMsgSize_ + msgPrefixSize()) {
      ReportError("Message longer than buffer");
      return setSendState(NET_SEND_ERROR);
    };
    break;
  case NET_SEND_CLOSED:           // Don't report errors more than once 
  case NET_SEND_ERROR:
    return lastSendState_;
    break;
  };
  
  // Check the file descriptor

  int sendFd = (fd < 0 ? fd_ : fd);

  if(sendFd < 0) {
    ReportError("Invalid file descriptor");
    return setSendState(NET_SEND_ERROR);
  }

  // And check the buffer 

  if(!buffer_) {
    ReportError("Buffer not provided.");
    return setSendState(NET_SEND_ERROR);
  };

  // Write as many bytes of the message as possible.

  while(nGet_ < nPut_) {
    int nsent;    // The latest number of bytes sent by write() 

    errno = 0;
    nsent = ::write(sendFd, buffer_ + nGet_, nPut_ - nGet_);

    // Did we manage to send anything?

    if(nsent > 0) {
      nGet_ += nsent;
    } else {
      
      // Handle write errors.

      switch(errno) {
	
	// No error reported in errno even though no bytes were
	// transferred.

      case 0:
	ReportError("write() returned " << nsent << " without an explanation.");
	return setSendState(NET_SEND_ERROR);
	break;
	
	// Insufficient room currently available for non-blocking I/O?

      case EWOULDBLOCK:     // BSD non-blocking I/O 
	return lastSendState_;
	break;

	// Continue after interrupts.

      case EINTR:
	break;
	
	// Report unexpected I/O errors.

      default:
	ReportSysError("write()");
	return setSendState(NET_SEND_ERROR);
      };
    };
  };
  
  // The message has been successfully sent.

  return setSendState(NET_SEND_DONE);
}

/**.......................................................................
 * The size of the message prefix
 */
unsigned int NetBuf::msgPrefixSize()
{
  return sizeof(unsigned int);
}

/**.......................................................................
 * Parse the message prefix, and return the byte count
 */
unsigned int NetBuf::parseMsgPrefix()
{
  unsigned int msgLen;

  getUint(1, &msgLen);

  return msgLen;
}

/**.......................................................................
 * Write a message prefix
 */
void NetBuf::putMsgPrefix(unsigned int size)
{
  putUint(1, &size);
}

#if 0
/**.......................................................................
 * Return an arbitrary data type from the buffer
 */
void NetBuf::getDataType(DataType::Type type, unsigned n, void* ptr)
{
  unsigned size = DataType::sizeof(type);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nGet_ + size * n > nPut_) 
    ThrowError("Insufficient data in buffer");

  // Copy one element at a time from the network buffer, convert it to
  // host byte order and copy the result to the output array.

  DataType dataType(type);
  memcpy(dataType.data(), buffer_ + nGet_, n * size);
  nGet_ += n * size;
}
#endif

/**.......................................................................
 * Return one or more chars from the buffer
 */
void NetBuf::getUchar(unsigned n, unsigned char* ptr)
{
  unsigned size = sizeof(unsigned char);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nGet_ + size * n > nPut_) 
    ThrowError("Insufficient data in buffer");

  memcpy(ptr, buffer_ + nGet_, n*size);
  nGet_ += n*size;
}

/**.......................................................................
 * Return one or more chars from the buffer
 */
void NetBuf::getChar(unsigned n, char* ptr)
{
  unsigned size = sizeof(char);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nGet_ + size * n > nPut_) 
    ThrowError("Insufficient data in buffer");

  memcpy(ptr, buffer_ + nGet_, n*size);
  nGet_ += n*size;
}

/**.......................................................................
 * Return an arbitrary data type from the buffer
 */
void NetBuf::getUshort(unsigned n, unsigned short* ptr)
{
  unsigned size = sizeof(unsigned short);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nGet_ + size * n > nPut_) 
    ThrowError("Insufficient data in buffer");

  // Copy one element at a time from the network buffer, convert it to
  // host byte order and copy the result to the output array.

  unsigned short sTemp;

  for(unsigned i=0; i < n; i++) {
    memcpy(&sTemp, buffer_ + nGet_, size);
    nGet_ += size;
    ptr[i] = ntohs(sTemp);
  }
}

/**.......................................................................
 * Return an arbitrary data type from the buffer
 */
void NetBuf::getShort(unsigned n, short* ptr)
{
  unsigned size = sizeof(short);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nGet_ + size * n > nPut_) 
    ThrowError("Insufficient data in buffer");

  // Copy one element at a time from the network buffer, convert it to
  // host byte order and copy the result to the output array.

  short sTemp;

  for(unsigned i=0; i < n; i++) {
    memcpy(&sTemp, buffer_ + nGet_, size);
    nGet_ += size;
    ptr[i] = ntohs(sTemp);
  }
}

/**.......................................................................
 * Return an unsigned int from the buffer
 */
void NetBuf::getUint(unsigned n, unsigned int* ptr)
{
  unsigned size = sizeof(unsigned int);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nGet_ + size * n > nPut_) 
    ThrowError("Insufficient data in buffer");

  // Copy one element at a time from the network buffer, convert it to
  // host byte order and copy the result to the output array.

  unsigned int iTemp;

  for(unsigned i=0; i < n; i++) {
    memcpy(&iTemp, buffer_ + nGet_, size);
    nGet_ += size;
    ptr[i] = ntohl(iTemp);
  }
}

/**.......................................................................
 * Return an int from the buffer
 */
void NetBuf::getInt(unsigned n, int* ptr)
{
  unsigned size = sizeof(int);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nGet_ + size * n > nPut_) 
    ThrowError("Insufficient data in buffer");

  // Copy one element at a time from the network buffer, convert it to
  // host byte order and copy the result to the output array.

  int iTemp;

  for(unsigned i=0; i < n; i++) {
    memcpy(&iTemp, buffer_ + nGet_, size);
    nGet_ += size;
    ptr[i] = ntohl(iTemp);
  }
}

/**.......................................................................
 * Return a double
 */
void NetBuf::getDouble(unsigned n, double* ptr)
{
  unsigned size = sizeof(double);
  
  // Do we have sufficient data in the buffer to satisfy the request?

  if(nGet_ + n*size > nPut_) 
    ThrowError("Insufficient data in buffer");

  // Copy one element at a time from the network buffer, convert it to
  // host byte order and copy the result to the output array.

  double dTemp;
  for(unsigned i=0; i < n; i++) {
    memcpy(&dTemp, buffer_ + nGet_, size);
    nGet_ += size;
    ptr[i] = netToHost(dTemp);
  }
}

/**.......................................................................
 * Return a float
 */
void NetBuf::getFloat(unsigned n, float* ptr)
{
  unsigned size = sizeof(float);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nGet_ + n * sizeof(float) > nPut_) 
    ThrowError("Insufficient data in buffer");

  // For hosts that support IEEE-754 floats simply copy from the
  // network buffer to the output array.

#if CPU==MC68040 || CPU==MC68060 || defined sparc
  memcpy(ptr, buffer_ + nGet_, n * sizeof(float));
  nGet_ += n * sizeof(float);
  
  // For other architectures convert from IEEE-754 to the host type.
  // The following algorithm should work on any architecture, albeit
  // probably not very efficiently.
  //

#else
  
  unsigned int net_long;
  for(unsigned i=0; i < n; i++) {

    // Extract the IEEE-754 float into a local 32-bit network long.

    memcpy(&net_long, buffer_ + nGet_, sizeof(float));
    nGet_ += size;
    ptr[i] = ieeeToFloat(net_long);
  }
#endif
}

/**.......................................................................
 * An IEEE 754 number can be decoded as follows, where [a..b] means
 * an integer formed from bits a to b, with bit 1 being the lsb.
 *
 * value = pow(-1,[32]) * pow(2,[31..24]-127) * [23..1]/pow(2,24)
 */
float NetBuf::ieeeToFloat(unsigned int net_long)
{
  float host_float; // A float converted to host format 
  int sign_bit;     // Non-zero if the number is negative 
  unsigned iexp;    // The power-of-two exponent 
  double mantissa;  // The mantissa of the number 

  // Convert to host byte-order.
  
  net_long = ntohl(net_long);
  
  // Get the sign bit (bit 32).
  
  sign_bit = net_long>>31U & 1U;
  
  // Get the exponent (bits 31-24).

  iexp = (net_long >> 23U & 0xffU) - 126U;
  
  // Get the mantissa (bits 23-1).

  mantissa = net_long & 0x7fffffU;
  
  // Compute the host-specific float.

  return ldexp(mantissa / 0x1000000 + 0.5, iexp) * (sign_bit ? -1 : 1);
}

/**.......................................................................
 * Extract one or more double elements from a network buffer.  The
 * network buffered double must be in IEEE 754 format, except that
 * IEEE special values such as NaN and Inf are not supported.
 */
double NetBuf::ieeeToDouble(double ieee)
{
  double retval;
  unsigned char *bytes = (unsigned char*)&ieee;
    
  // Get the sign bit.
  
  int sign_bit = bytes[0]>>7U & 1U;
  
  // Get the exponent (bits 63-53).
  
  unsigned iexp = (bytes[0] & 0x7FU) << 4U | bytes[1] >> 4U;
  
  // Get the mantissa (bits 52-1).
  
  double mantissa = ((bytes[1] & 0xFU) << 24U | bytes[2] << 16U |
		     bytes[3] << 8U | bytes[4]) * (double) (1U<<24U) +
    (bytes[5] << 16U | bytes[6] << 8U | bytes[7]);
  
  // Compose the host-format double in the output array.
  
  retval = ldexp(mantissa/ldexp(1.0,53)+0.5, iexp-1022U) * (sign_bit ? -1:1);

  return retval;
};

double NetBuf::netToHost(double netDouble)
{
  return netDouble;
}

/**.......................................................................
 * Put one or more chars from the buffer
 */
void NetBuf::putUchar(unsigned n, unsigned char* ptr)
{
  unsigned size = sizeof(unsigned char);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nPut_ + size * n > maxMsgSize_) 
    ThrowError("Insufficient room in buffer");

  memcpy(buffer_ + nPut_, ptr, n*size);
  nPut_ += n*size;
}

/**.......................................................................
 * Put one or more chars from the buffer
 */
void NetBuf::putChar(unsigned n, char* ptr)
{
  unsigned size = sizeof(char);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nPut_ + size * n > maxMsgSize_) 
    ThrowError("Insufficient room in buffer");

  memcpy(buffer_ + nPut_, ptr, n*size);
  nPut_ += n*size;
}

/**.......................................................................
 * Put one or more shorts into the buffer
 */
void NetBuf::putUshort(unsigned n, unsigned short* ptr)
{
  unsigned size = sizeof(unsigned short);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nPut_ + size * n > maxMsgSize_) 
    ThrowError("Insufficient room in buffer");

  // Copy one element at a time from the network buffer, convert it to
  // host byte order and copy the result to the output array.

  unsigned short iTemp;
  for(unsigned i=0; i < n; i++) {
    iTemp = htons(ptr[i]);
    memcpy(buffer_ + nPut_, &iTemp, size);
    nPut_ += size;
  }
}

/**.......................................................................
 * Put one or more shorts into the buffer
 */
void NetBuf::putShort(unsigned n, short* ptr)
{
  unsigned size = sizeof(short);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nPut_ + size * n > maxMsgSize_) 
    ThrowError("Insufficient room in buffer");

  // Copy one element at a time from the network buffer, convert it to
  // host byte order and copy the result to the output array.

  unsigned short iTemp;
  for(unsigned i=0; i < n; i++) {
    iTemp = htons(ptr[i]);
    memcpy(buffer_ + nPut_, &iTemp, size);
    nPut_ += size;
  }
}

/**.......................................................................
 * Put one or more ints into the buffer
 */
void NetBuf::putUint(unsigned n, unsigned int* ptr)
{
  unsigned size = sizeof(unsigned int);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nPut_ + size * n > maxMsgSize_) 
    ThrowError("Insufficient room in buffer");

  // Copy one element at a time from the network buffer, convert it to
  // host byte order and copy the result to the output array.

  unsigned int iTemp;
  for(unsigned i=0; i < n; i++) {
    iTemp = htonl(ptr[i]);
    memcpy(buffer_ + nPut_, &iTemp, size);
    nPut_ += size;
  }
}

/**.......................................................................
 * Put one or more ints into the buffer
 */
void NetBuf::putInt(unsigned n, int* ptr)
{
  unsigned size = sizeof(int);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nPut_ + size * n > maxMsgSize_) 
    ThrowError("Insufficient room in buffer");

  // Copy one element at a time from the network buffer, convert it to
  // host byte order and copy the result to the output array.

  unsigned int iTemp;
  for(unsigned i=0; i < n; i++) {
    iTemp = htonl(ptr[i]);
    memcpy(buffer_ + nPut_, &iTemp, size);
    nPut_ += size;
  }
}

/**.......................................................................
 * Return a double
 */
void NetBuf::putDouble(unsigned n, double* ptr)
{
  unsigned size = sizeof(double);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nPut_ + size * n > maxMsgSize_) 
    ThrowError("Insufficient room in buffer");

  // For hosts that support big-endian IEEE-754 64-bit doubles, simply
  // copy the input data into the output buffer.

#if CPU==MC68040 || CPU==MC68060 || defined sparc
  memcpy(buffer_ + nPut_, ptr, n * sizeof(double));
  nPut_ += n * sizeof(double);
#else

  // For other architectures convert each element from host double
  // format to IEEE-754 double format. The following algorithm should
  // work on any architecture, albeit slowly.

  double dTemp;
  for(unsigned i=0; i < n; i++) {
    dTemp = doubleToIeee(ptr[i]);
    memcpy(buffer_ + nPut_, &dTemp, size);
    nPut_ += size;
  }
#endif
}

/**.......................................................................
 * Return a float
 */
void NetBuf::putFloat(unsigned n, float* ptr)
{
  unsigned size = sizeof(unsigned int);

  // Do we have sufficient data in the buffer to satisfy the request?

  if(nPut_ + size * n > maxMsgSize_) 
    ThrowError("Insufficient room in buffer");

  // For hosts that support big-endian IEEE-754 64-bit floats, simply
  // copy the input data into the output buffer.

#if CPU==MC68040 || CPU==MC68060 || defined sparc
  memcpy(buffer_ + nPut_, ptr, n * sizeof(float));
  nPut_ += n * sizeof(float);
#else

  // For other architectures convert each element from host float
  // format to IEEE-754 float format. The following algorithm should
  // work on any architecture, albeit slowly.

  unsigned int iTemp;
  for(unsigned i=0; i < n; i++) {
    iTemp = floatToIeee(ptr[i]);
    memcpy(buffer_ + nPut_, &dTemp, size);
    nPut_ += size;
  }
#endif
}

/**.......................................................................
 * Convert from host float to IEEE
 */
float NetBuf::floatToIeee(float host_float)
{
  unsigned int net_long;       /* The output float packed into a network long */
  unsigned int mantissa;    /* Normalized mantissa scaled by 0x1000000 */
  float abs_float = fabs(host_float); /* Avoid sign extension problems */
  long sign_bit = host_float<0;       /* Sign bit (0 or 1) */
  int iexp;                           /* Power of two exponent */
  
  // NaN and Inf have architecture-depended representations, so
  // transmute them to FLT_MAX.

  if(abs_float > FLT_MAX || !(abs_float > 0.0))
	abs_float = FLT_MAX;
  
  // Compute the mantissa and the exponent.
  
  mantissa = (unsigned long)((frexp(abs_float, &iexp)-0.5) * 0x1000000);
  iexp += 126U;
  
  // Compose the IEEE float in the 32-bit network long.
  
  net_long = mantissa |  iexp << 23 | sign_bit << 31;
  
  // Convert the host-byte-order long to network byte order and
  // copy it to the network buffer.
  
  net_long = htonl(net_long);
  
  return net_long;
};

/**.......................................................................
 * Convert from host double to IEEE
 */
double NetBuf::doubleToIeee(double host_double)
{
  double mantissa;                  /* Normalized mantissa scaled by 2^53 */
  unsigned int msbs, lsbs;                /* The most/least significant bytes */
  double abs_double = fabs(host_double);/* Avoid sign extension problems */
  double two_pow_32 = ldexp(1.0,32);  /* 2^32 */
  unsigned sign_bit = host_double<0;    /* Sign bit (0 or 1) */
  int iexp;                             /* Power of two exponent */
  
  // NaN and Inf have architecture-depended representations, so
  // transmute them to DBL_MAX.

  if(abs_double > DBL_MAX || !(abs_double > 0.0))
	abs_double = DBL_MAX;
  
  // Compute the mantissa and the exponent.

  mantissa = (frexp(abs_double, &iexp)-0.5) * ldexp(1.0,53);
  iexp += 1022U;
      
  // IEEE says the msb is a sign bit, the next 11 bits encode the exponent
  // and the final 52 bits encode the mantissa. These components are
  // derived such that the original number is
  //
  // value = (-1)^sign x 2^(exponent-1023) x mantissa
  //
  // Form the 4 most and least significant bytes in two long's.
  
  lsbs = (unsigned int)(modf(mantissa / two_pow_32, &mantissa) * two_pow_32);
  msbs = ((unsigned int)mantissa) | (iexp << 20U) | (sign_bit << 31U);
  
  // Convert the 2 4-byte words into network byte order.

  lsbs = htonl(lsbs);
  msbs = htonl(msbs);

  double retval;
  unsigned int* iPtr = (unsigned int*)&retval;

  iPtr[0] = msbs;
  iPtr[1] = lsbs;

  return retval;
}

/**.......................................................................
 * Initialize the message by writing a dummy message size
 */
void NetBuf::startPut()
{
  if(buffer_ == 0)
    ThrowError("No buffer has been provided");

  // Initialize the counts

  nPut_ = 0;
  nGet_ = 0;

  // And write a dummy prefix to increment the buffer.  This will be
  // updated with the real size when endPut() is called

  putMsgPrefix(msgPrefixSize());
}

/**.......................................................................
 * Finalize the message by writing the real message size, exclusive of
 * the prefix length
 */
void NetBuf::endPut()
{
  if(buffer_ == 0)
    ThrowError("No buffer has been provided");

  unsigned nput = nPut_;

  nPut_ = 0;
  putMsgPrefix(nput - msgPrefixSize());
  nPut_ = nput - msgPrefixSize();
}

/**.......................................................................
 * Initialize the message by writing a dummy message size
 */
void NetBuf::startGet()
{
  if(nPut_ < msgPrefixSize())
    ThrowError("Message header missing");

  // Position the extraction pointer at the start of the message

  nGet_ = msgPrefixSize();
}

/**.......................................................................
 * Terminate the extraction of a message from the buffer
 */
void NetBuf::endGet()
{
  if(nGet_ < nPut_) 
    ThrowError("Unexpected bytes at end of message");
}

NetBuf::NetReadState NetBuf::setReadState(NetReadState state)
{
  lastReadState_ = state;
  return lastReadState_;
}

NetBuf::NetSendState NetBuf::setSendState(NetSendState state)
{
  lastSendState_ = state;
  return lastSendState_;
}
