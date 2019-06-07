#include "carma/szautil/Exception.h"
#include "carma/szautil/LogFile.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/TimeVal.h"

#include <iostream>

using namespace std;

using namespace sza::util;

const int LogFile::MAX_VERSION=999; // Max postfixed version number

/**.......................................................................
 * Constructor.
 */
LogFile::LogFile() 
{
  // Initialize our file descriptor to NULL
  
  file_ = 0;
  datePrefix_ = false;
  
  // Initialize the line buffer
  
  initBuffer();
  
  // Initialize the prefix and dir to something
  
  setPrefix("logfile");
  setDirectory(".");
}

/**.......................................................................
 * Destructor.
 */
LogFile::~LogFile() 
{
  // Close the logfile is it was sucessfully opened
  
  close();
}

/**.......................................................................
 * Set the destination directory for logfiles created by this
 * object.
 */
void LogFile::setDirectory(const std::string& directory)
{
  directory_ = directory;
}

/**.......................................................................
 * Set the prefix for logfiles created by this object.
 */
void LogFile::setPrefix(const std::string& prefix)
{
  prefix_ = prefix;
  datePrefix_ = false;
}

/**.......................................................................
 * Tell this class to create names based on the date
 */
void LogFile::setDatePrefix()
{
  datePrefix_ = true;
}

/**.......................................................................
 * Flush a logfile
 */
void LogFile::flush()
{
  // Write any characters in our line buffer to the file descriptor

  if(lineBuffer_.str().length() > 0)
    putLine();

  // Flush any connection that may already be open

  if(file_ != 0)
    fflush(file_);
}  

/**.......................................................................
 * Close a logfile
 */
void LogFile::close()
{
  // Close any connection that may already be open
  
  flush();

  if(file_ != 0)
    fclose(file_);
}  

/**.......................................................................
 * Open a logfile
 */
void LogFile::open()
{
  LogStream errStr;
  
  // Close any connection that may already be open
  
  close();
  
  // Get the next file name
  
  std::string fileName = newFileName();
  
  // And attempt to open it for writing
  
  if((file_= fopen(fileName.c_str(),"w"))==0) {
    errStr.appendMessage(true, "Unable to open file");
    errStr.appendSysError(true, "fopen()");
    throw Error(errStr);
  }
  
  // Set line buffering for this stream
  
  setlinebuf(file_);
  
  // Get the current time
  
  TimeVal timeVal;
  timeVal.setToCurrentTime();
  
  // Log a message that we successfully opened the file
  
  errStr.initMessage(false);
  errStr << "Opened logfile: " << fileName << " on " 
	 << timeVal.getUtcString();
  
  errStr.report();
}

/**.......................................................................
 * Return true if a file exists
 */
bool LogFile::fileExists(std::string fileName)
{
  FILE *fp = 0;
  
  fp = fopen(fileName.c_str(), "r");
  
  if(fp)
    fclose(fp);
  
  return fp != 0;
}

/**.......................................................................
 * Create a unique file name based on the prefix and files already in
 * the log directory
 */
std::string LogFile::newFileName()
{
  // Valid arguments?
  
  if(!datePrefix_ && prefix_.size()==0) {
    ThrowError("Zero-length prefix");
  };
  
  /*
   * Append incrementally higher version numbers until a unique file
   * name is found.
   */
  ostringstream os;

  TimeVal tVal;
  tVal.setToCurrentTime();

  std::string prefix = datePrefix_ ? tVal.dateString() : prefix_;

  // If this is a date prefix, don't append a version number unless a
  // file with the current name already exists

  if(datePrefix_) {
    os.str("");
    os << directory_ << "/" << prefix;

    if(!fileExists(os.str()))
      return os.str();
  }

  // Else append version numbers until we find a unique one

  for(unsigned iver=0; iver < MAX_VERSION; iver++) {
    
    os.str("");
    os << directory_ << "/" << prefix << "_" << iver;
    
    if(!fileExists(os.str()))
      return os.str();
  };
  
  // Max version number exceeded.
  
  ThrowError("Max version number exceeded");
}

/**.......................................................................
 * Write to a log file
 *
 * A subtlety here is that we want to write timestamps for every line,
 * but don't necessarily want to write a time stamp every time
 * characters get written to the file.  For instance, if the data
 * originate from a terminal interface, a serial interface may return
 * a prompt, and a newline will only be delivered when the user hits
 * newline after typing something.
 *
 * This routine fills a char buffer intended for writing to the
 * logfile, but only writes it when a newline is encountered, OR when
 * the buffer would otherwise overflow.  Thus, a timestamp will in
 * general correspond to when a newline was delivered, and only in
 * exceptional cases, to when a buffer overflow occurred.
 */
void LogFile::append(std::string message)
{
  // Incrementally add characters to the line buffer until we
  // encounter a newline char.  
  
  for(unsigned i=0; i < message.size(); i++) {
    
    // If a newline was encountered, write the line to the file.  On
    // return, the line buffer will have been reinitialized.
    
    if(message[i] == '\n') {
      putLine();
    } else if(message[i] != '\0') {
      lineBuffer_ << message[i];
    }
  }
}

void LogFile::prepend(std::string message)
{
  prependStr_ = message;
}

/**.......................................................................
 * Write a line to the logfile
 */
void LogFile::putLine() 
{
  ostringstream os;

  // Get the current time and write it to the file
  
  TimeVal timeVal;
  timeVal.setToCurrentTime();
  
  os << timeVal.getUtcString() << ": " << prependStr_ << lineBuffer_.str() << endl;
  
  // Now write the line
  
  write(os.str());
  
  // And reset the buffer
  
  initBuffer();
}

/**.......................................................................
 * Initialize the line buffer
 */
void LogFile::initBuffer()
{
  // Place an intial space onto the string stream
  
  lineBuffer_.str("");
}

/**.......................................................................
 * Write a string to the file
 */
void LogFile::write(std::string line)
{
  LogStream errStr;
  
  if(file_ == 0)
    return;
  
  // And print the line
  
  if(fputs((char*)line.c_str(), file_) < 0) {
    errStr.appendSysError(true, "fputs");
    throw Error(errStr);
  }
}

