#include "carma/szautil/Exception.h"
#include "carma/szautil/FileHandler.h"

#include<iostream>

#include <cstdlib>
#include <cstring>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
FileHandler::FileHandler() 
{
  fd_            = -1;
  pathIsSet_     = false;
  currentOffset_ = 0;
  memMap_        = false;
  mptrHead_      = 0;
  loadFile_      = 0;
}

/**.......................................................................
 * Const Copy Constructor.
 */
FileHandler::FileHandler(const FileHandler& objToBeCopied)
{
  *this = (FileHandler&)objToBeCopied;
};

/**.......................................................................
 * Copy Constructor.
 */
FileHandler::FileHandler(FileHandler& objToBeCopied)
{
  *this = objToBeCopied;
};

/**.......................................................................
 * Const Assignment Operator.
 */
void FileHandler::operator=(const FileHandler& objToBeAssigned)
{
  *this = (FileHandler&)objToBeAssigned;
};

/**.......................................................................
 * Assignment Operator.
 */
void FileHandler::operator=(FileHandler& objToBeAssigned)
{
  std::cout << "Calling default assignment operator for class: FileHandler" 
	    << std::endl;
};

/**.......................................................................
 * Output Operator.
 */
std::ostream& sza::util::operator<<(std::ostream& os, FileHandler& obj)
{
  os << "Default output operator for class: FileHandler" << std::endl;
  return os;
};

/**.......................................................................
 * Destructor.
 */
FileHandler::~FileHandler() 
{
  // Make sure that any opened file descriptor is closed before
  // exiting

  close();
}

void FileHandler::setTo(std::string filePath)
{
  path_ = filePath;
  pathIsSet_ = true;
  close();
}

void FileHandler::openForRead(bool memMap)
{
  if(!pathIsSet_) {
    ThrowError("No file has been specified");
  }

  fd_ = ::open(path_.c_str(), O_RDONLY);

  if(fd_ < 0) {
    ThrowSysError("Unable to open file: " << path_);
  }

  currentOffset_ = 0;
  memMap_ = memMap;

  // Always get the file size when we open 

  getFileSizeInBytes();

  // If we are memory-mapping this file, do it now

  if(memMap_) 
    memoryMap();
}

void FileHandler::memoryMap()
{
  // Reset the file descriptor to point to the head of the file

  setToBeginning();

  // If we are memory-mapping this file, do it now


  mptrHead_ = (unsigned char*) mmap(0, sizeInBytes_, PROT_READ, 
				    MAP_PRIVATE, fd_, 0);

  memMap_ = true;
}

void FileHandler::loadFile()
{
  // Reset the file descriptor to point to the head of the file

  setToBeginning();

  // Now load this file into memory

  mptrHead_ = (unsigned char*) malloc(sizeInBytes_);
  read(mptrHead_, sizeInBytes_);
  loadFile_ = true;
}

void FileHandler::close()
{
  if(memMap_) {
    munmap((void*)mptrHead_, sizeInBytes_);
    mptrHead_ = 0;
    memMap_   = false;
  }

  if(loadFile_) {
    free(mptrHead_);
    mptrHead_ = 0;
  }

  if(fd_ > 0) {
    if(::close(fd_) < 0) {
      ThrowSysError("Error closing file: " << path_);
    }
  }

  fd_ = -1;
  currentOffset_ = 0;
}

void FileHandler::advanceByNbytes(off_t bytes)
{
  checkFd();

  off_t offset=0;

  if(memMap_ || loadFile_) {
    currentOffset_ += bytes;

  } else {
    offset = lseek(fd_, bytes, SEEK_CUR);
    currentOffset_ = offset;
  }

  if(currentOffset_ < 0) {
    ThrowSysError("Error advancing file descriptor");
  }

}

void FileHandler::setToBeginning()
{
  checkFd();

  off_t offset=0;

  if(memMap_ || loadFile_) {
    offset = 0;
  } else {
    offset = lseek(fd_, 0, SEEK_SET);
  }

  if(offset < 0) {
    ThrowSysError("Error advancing file descriptor");
  }

  currentOffset_ = 0;
}

off_t FileHandler::setToEnd()
{
  checkFd();

  off_t offset = lseek(fd_, 0, SEEK_END);

  if(offset < 0) {
    ThrowSysError("Error advancing file descriptor");
  }

  currentOffset_ = offset;

  return offset;
}

unsigned FileHandler::getFileSizeInBytes()
{
  checkFd();

  sizeInBytes_ = (size_t)setToEnd();

  setToBeginning();

  return sizeInBytes_;
}

void FileHandler::checkFd()
{
  if(fd_ < 0) {
    ThrowError("Invalid file descriptor");
  }
}

off_t FileHandler::getCurrentOffset()
{
  return currentOffset_;
}

void FileHandler::read(void* buf, size_t nByte)
{
  ssize_t nread=0;

  // If this file has been memory mapped, read from the m-pointer

  if(memMap_ || loadFile_) {

    void* from = (void*)(mptrHead_ + currentOffset_);
    memcpy(buf, from, nByte);
    nread = nByte;

    // Else read normally

  } else {
    nread = ::read(fd_, buf, nByte);
  }

  if(nread < 0) {
    ThrowSysError("Unable to read");
  } else if(nread != nByte) {
    currentOffset_ += nread;
    ThrowError(path_ << ": Failed to read " << nByte << " bytes (only read: " << nread << " bytes)");
  }

  currentOffset_ += nread;
}
