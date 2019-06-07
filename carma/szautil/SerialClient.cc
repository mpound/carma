#include <fcntl.h>
#include <unistd.h>

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/SerialClient.h"

using namespace std;
using namespace sza::util;

//------------------------------------------------------------
// Recognized baud rates
//------------------------------------------------------------

/**
 * Recognized baud rates
 */
SerialClient::BaudRate SerialClient::baudRates_[] = {
  {  B1200,   1200},
  {  B2400,   2400},
  {  B4800,   4800},
  {  B9600,   9600},
  { B38400,  38400},
  { B57600,  57600},
  {B115200, 115200}
};

/**
 * The number of elements in the above array
 */
unsigned SerialClient::nBaudRates_ = sizeof(baudRates_)/sizeof(SerialClient::BaudRate);

/**.......................................................................
 * Constructor.
 */
SerialClient::SerialClient(std::string portName, int baudRate, bool canonical, bool sevenBit) :
  portName_(portName), canonical_(canonical), sevenBit_(sevenBit)
{
  baudRate_ = 0; // Uninitialized state

  setBaudRate(baudRate);

  stripUnprintable(false);

  ownFd_ = false;
}

/**.......................................................................
 * Destructor.
 */
SerialClient::~SerialClient() 
{
  disconnect();
}

/**.......................................................................
 * Set the baudrate
 */
void SerialClient::setBaudRate(int baudRate)
{
  LogStream errStr;

  // Parse the baudrate

  for(unsigned ibaud = 0; ibaud < nBaudRates_; ibaud++) {
    if(baudRate == baudRates_[ibaud].baudRate) {
      baudRate_ = &baudRates_[ibaud];
      return;
    }
  }

  // Else error

  errStr.initMessage(true);

  errStr << "Unsupported baud rate: " << baudRate << endl
	 << " Should be one of: " << endl;
    
  for(unsigned ibaud = 0; ibaud < nBaudRates_; ibaud++) 
    errStr << baudRates_[ibaud].baudRate << endl;
  
  throw Error(errStr);
}

/*.......................................................................
 * Initialize the serial port connection to the TERM
 */
int SerialClient::connect()
{
  struct termios termio,termio_save;
  LogStream errStr;

  /**
   * Open a read-write connection to the serial port.
   */
  fd_ = -1;

  fflush(stdin);
  fflush(stdout);

  fd_ = open(portName_.c_str(), O_RDWR);
  if(fd_ < 0) {
    errStr.appendSysError(true, "open()");
    errStr.report();
    return -1;
  }

  /*
   * Set up for 8-bit terminal-I/O at the requested baud rate
   *
   *
   * Store the current state for restoration when we exit.
   */
  if(tcgetattr(fd_, &termio_save) < 0) {
    errStr.appendSysError(true, "tcgetattr()");
    throw Error(errStr);
  }

  termio = termio_save;

  termio.c_lflag = canonical_ ? ICANON : 0;
  termio.c_iflag = IGNBRK;  // Ignore break on input
  termio.c_cflag = CLOCAL | // Ignore modem control lines
                   PARODD | // Odd parity
		   PARENB | // Enable parity bit generation
		   CREAD |  // Enable receiver
		   (sevenBit_ ? CS7 : CS8) |    // 8 bit characters
		   B57600;  // Baud rate
  termio.c_oflag = 0;

 /* 
  * Set input/output speed
  */
  if(cfsetispeed(&termio, baudRate_->speed) < 0 || 
     cfsetospeed(&termio, baudRate_->speed) < 0) {
    errStr.appendMessage(true, "Error setting port speed.");
    throw Error(errStr);
  }
  /*
   * Set the new attributes.
   */
  if(tcsetattr(fd_, TCSAFLUSH, &termio) < 0) {
    errStr.appendSysError(true, "tcsetattr()");
    throw Error(errStr);
  }

#if 0
  std::cout << "Termio: lflag = " << termio.c_lflag << std::endl;
  std::cout << "Termio: iflag = " << termio.c_iflag << std::endl;
  std::cout << "Termio: cflag = " << termio.c_cflag << std::endl;
  std::cout << "Termio: oflag = " << termio.c_oflag << std::endl;
  std::cout << "Termio: c_line = " << static_cast<unsigned int>(termio.c_line) << std::endl;

#endif

  ownFd_ = true;

  return fd_;
}

/*.......................................................................
 * Initialize the serial port connection to the TERM
 */
int SerialClient::connectWx()
{
  struct termios termio,termio_save;
  LogStream errStr;

  /**
   * Open a read-write connection to the serial port.
   */
  fd_ = -1;

  fflush(stdin);
  fflush(stdout);

  fd_ = open(portName_.c_str(), O_RDONLY);
  if(fd_ < 0) {
    errStr.appendSysError(true, "open()");
    errStr.report();
    return -1;
  }

  /*
   * Set up for 8-bit terminal-I/O at the requested baud rate
   *
   *
   * Store the current state for restoration when we exit.
   */
  if(tcgetattr(fd_, &termio_save) < 0) {
    errStr.appendSysError(true, "tcgetattr()");
    throw Error(errStr);
  }

  termio = termio_save;

  termio.c_iflag = 0;
  termio.c_oflag = 0;
  termio.c_lflag = 0;
  termio.c_cflag = baudRate_->speed | CS8 | CLOCAL | CREAD;

  if (tcsetattr(fd_, TCSANOW, &termio) < 0) {
    errStr.appendSysError(true, "tcsetattr()");
    throw Error(errStr);
  }

  return fd_;
}

/*.......................................................................
 * Terminate the serial port connection to the TERM
 */
void SerialClient::disconnect()
{
  if(fd_ >= 0 && ownFd_) {
    if(close(fd_) < 0) {
      LogStream errStr;
      errStr.appendSysError(true, "close()");
      throw Error(errStr);
    }

    fd_ = -1;
  }
}

void SerialClient::setPort(std::string portName)
{
  portName_ = portName;
}

void SerialClient::setFd(int fd)
{
  fd_ = fd;
}

std::string SerialClient::portName()
{
  return portName_;
}
