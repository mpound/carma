#include "carma/szautil/Debug.h"
#include "carma/szautil/Port.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/StringUtils.h"
#include "carma/szautil/Vector.h"

#include <sys/ioctl.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Port::Port(int fd) 
{
  fd_ = fd;
}

/**.......................................................................
 * Destructor.
 */
Port::~Port() {}

/**.......................................................................
 * Write a message to the port
 */
void Port::writeString(std::string& message, int fd)
{
  LogStream errStr;
  ostringstream os;
  std::string copy = message;
  
  // Strip off any newlines
  
  sza::util::strip(copy, '\n');
  
  // And construct the output message.  NB: We do NOT want to append
  // std::ends here, or outStr.size() will include the NULL terminator
  
  os << copy << appendStr_.str();
  
  std::string outStr = os.str();
  
  // Be very careful here!  std::string::size() returns a count which
  // includes the terminal '\0', but most applications (like write())
  // expect a size which is the equivalent of std::string::size()-1

  /*
    COUT(outStr.c_str() << " size = " << outStr.size());
    
    int i;
    char outString[100];
    strcpy(outString, outStr.c_str());
    for (i=0; i<outStr.size(); i++)
    {
    //   if(isalnum(*(outString+i)) || isprint(*outString+i) || isspace(*outString+i)) {
    if(!iscntrl(*(outString+i))) { 
    COUT("string[i] = " << *(outString+i));
    } else if(*(outString+i) == '\r') {
    COUT("string[i] = CR");
    } else if(*(outString+i) == '\n') {
    COUT("string[i] = LF");
    } else if(*(outString+i) == '\0') {
    COUT("string[i] = NULL");
    } else {
    COUT("string[i] = JUNK");
    }
    
    }

    COUT("fd_ : " << fd_);
  */

  DBPRINT(true, Debug::DEBUG7, "Writing: " << outStr.c_str() << " size = " << outStr.size());

  if(fd_ >= 0) {
    if(write(fd < 0 ? fd_ : fd, (char*)outStr.c_str(), outStr.size()) != outStr.size()) {
      errStr.appendSysError(true, "write()");
      throw Error(errStr);
    }
  }
}

/**.......................................................................
 * Write a message to the port
 */
void Port::writeBytes(Vector<unsigned char>& message)
{
  LogStream errStr;
  ostringstream os;
  Vector<unsigned char> copy = message;
  
  // And construct the output message
  
  cout << "Append string is of size: " << appendStr_.str().size() << endl;
  cout << "message string is of size: " << copy.size() << endl;
  for(unsigned i=0; i < appendStr_.str().size(); i++)
    copy.push_back(appendStr_.str().at(i));
  cout << "message string is of size: " << copy.size() << endl;
  cout << "message string is of size: " << copy.size() << endl;
  
  // Be very careful here!  std::string::size() returns a count which
  // includes the terminal '\0', but most applications (like write())
  // expect a size which is the equivalent of std::string::size()-1
  
#if 1
  unsigned char* cPtr = &copy[0];
  for(unsigned i=0; i < copy.size(); i++)
    fprintf(stdout, "array[%d] = 0x%02x (%c)\n", i, cPtr[i], isprint(cPtr[i]) ? cPtr[i] : ' ');
#endif

#if 1
  if(fd_ >= 0) {
    if(write(fd_, (char*)&message[0], message.size()) != message.size()) {
      errStr.appendSysError(true, "write()");
      throw Error(errStr);
    }
  }
#endif
}

/*.......................................................................
 * Read bytes from the serial port and return them as a string
 */
std::string Port::readString(int fd)
{
  int iByte, nByte, waserr=0;
  ostringstream os;
  LogStream errStr;
  
  // See how many bytes are waiting to be read
  
  do {
    
    nByte = getNbyte(fd);
    
    // If a non-zero number of bytes was found, read them
    
    if(nByte > 0) {
      
      char c;
      
      // Read the bytes one at a time, so we can check each one.
      
      for(iByte=0; iByte < nByte; iByte++) {
	
	if(read((fd < 0 ? fd_ : fd), &c, 1) < 0) {
	  errStr.appendSysError(true, "read()");
	  throw Error(errStr);
	}
	
	// Only concatenate this char if no match was found in the
	// strip string
	
	if(!stripStr_.contains(c) || dontStripStr_.contains(c)) {
	  
	  // Only print this char if it is printable, or if we are
	  // printing all chars
	  
	  if(dontStripStr_.contains(c) || !(stripUnprintable_ && !isprint(c)))
	    os << c;
	}
      }
    }
  } while(nByte > 0);
  
  return os.str();
}

/*.......................................................................
 * Read bytes from the serial port and return them as a Vector of unsigned char
 */
unsigned char Port::getNextByte()
{
  if(getNbyte() == 0)
    ThrowError("Nothing to read");

  unsigned char c;

  if(::read(fd_, &c, 1) != 1)
    ThrowSysError("read()");

  return c;
}

/*.......................................................................
 * Read bytes from the serial port and return them as a Vector of unsigned char
 */
unsigned int Port::readBytes(unsigned char *message, int fd)
{
  int iByte, nByte, bytesRead, waserr=0;
  LogStream errStr;
  
  // See how many bytes are waiting to be read
  
    
  nByte = getNbyte(fd);
  if(nByte > MAX_RCV_BUFFER)
  {
    nByte = MAX_RCV_BUFFER;
  }
  
  bytesRead = 0;
    
  // If a non-zero number of bytes was found, read them
    
  if(nByte > 0) 
  {
    unsigned char c;
      
    DBPRINT(true, Debug::DEBUG9, "Found " << nByte << " bytes ready");
    
    if((bytesRead = read((fd < 0 ? fd_ : fd), &message[0], nByte)) < 0) 
    {
      std::cout << "ERROR read returned -1" << std::endl;
      errStr.appendSysError(true, "read()");
      //portTrace_.push_back(0xDEAD);
      throw Error(errStr);
    }
  }

  return bytesRead;
}

/*.......................................................................
 * Read bytes from the serial port and return them as a Vector of unsigned char
 */
unsigned int Port::readBytes(Vector<unsigned char>& buffer)
{
  int iByte, nByte, bytesRead=0;
  
  // See how many bytes are waiting to be read
    
  nByte = getNbyte(fd_);
  buffer.resize(nByte);

  // If a non-zero number of bytes was found, read them
    
  if(nByte > 0) {
    bytesRead = ::read(fd_, &buffer[0], nByte);

    if(bytesRead < 0)
      ThrowSysError("read()");
  }

  return bytesRead;
}

int Port::getNbyte(int fd) 
{
  int request = FIONREAD;
  int nByte=0;
  
  if(ioctl((fd < 0 ? fd_ : fd), request, &nByte) != 0) {
    LogStream errStr;
    errStr.appendSysError(true, "ioctl()");
    throw Error(errStr);
  }
  
  return nByte;
}


/*.......................................................................
 * Read bytes from the serial port up to a terminator char and return
 * them as a string
 */
bool Port::concatenateString(std::ostringstream& os, int fd, bool cont)
{
  int iByte, nByte, waserr=0;
  LogStream errStr;
  
  // See how many bytes are waiting to be read
  
  do {
    
    nByte = getNbyte(fd);
      
    // If a non-zero number of bytes was found, read them
    
    if(nByte > 0) {
      
      // Read the bytes one at a time, so we can check each one.
      
      for(iByte=0; iByte < nByte; iByte++)
	concatenateChar(os, fd);
    }

  } while(nByte > 0 && cont);
  
  return false;
}


void Port::concatenateChar(std::ostringstream& os, int fd)
{
  char c;
  
  if(read((fd < 0 ? fd_ : fd), &c, 1) < 0) {
    LogStream errStr;
    errStr.appendSysError(true, "read()");
    throw Error(errStr);
  }
  
  // If we have encountered a terminator char, discard it, and
  // return
  
  if(termStr_.contains(c)) 
    return;
  
  // Only concatenate this char if no match was found in the
  // strip string
  
  if(!stripStr_.contains(c) || dontStripStr_.contains(c)) {
    
    // Only print this char if it is printable, or if we are
    // printing all chars
    
    if(dontStripStr_.contains(c) || !(stripUnprintable_ && !isprint(c)))
      os << c;
  }
}

/**.......................................................................
 * Terminate a read at any of the characters in the passed string
 */
void Port::terminateAt(std::string term)
{
  termStr_ = term;
}

/**.......................................................................
 * Strip
 */
void Port::strip(std::string strip)
{
  stripStr_ = strip;
}

/**.......................................................................
 * Strip
 */
void Port::dontStrip(std::string dontStrip)
{
  dontStripStr_ = dontStrip;
}

/**.......................................................................
 * Strip unprintable characters
 */
void Port::stripUnprintable(bool strip)
{
  stripUnprintable_ = strip;
}

/**.......................................................................
 * A string to append to the end of each line
 */
void Port::append(std::string append)
{
  appendStr_ = append;
}

void Port::setLineBuf()
{
  if(fd_ == 1)
    setvbuf(stdout, (char *)NULL, _IOLBF, 0);
  if(fd_ == 2)
    setvbuf(stderr, (char *)NULL, _IOLBF, 0);
}

void Port::setNoBuf()
{
  if(fd_ == 1)
    setvbuf(stdout, (char *)NULL, _IONBF, 0);
  if(fd_ == 2)
    setvbuf(stderr, (char *)NULL, _IONBF, 0);
}
