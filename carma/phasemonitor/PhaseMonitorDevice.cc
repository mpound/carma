/*
 * $Id: PhaseMonitorDevice.cc,v 1.39 2013/02/20 17:04:10 abeard Exp $
 * vim: set ts=2 sts=2 sw=2 et:
 */

#include "carma/phasemonitor/PhaseMonitorDevice.h"

#include "carma/phasemonitor/exceptions.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"
#include "carma/util/Time.h"

#include <log4cpp/Category.hh>

#include <errno.h>
#include <math.h>
#include <poll.h>
#include <sys/time.h>
#include <termios.h>

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::phasemonitor;

#ifndef PMDTHROW
#define PMDTHROW( a ) \
do { \
    ostringstream _errs; \
    _errs << a; \
    throw CARMA_EXCEPTION( PhaseMonitorDeviceException, _errs ); \
} while (0)
#else
#error "PMDTHROW is already defined, it will probably break this code."
#endif

static ssize_t serial_read(int fd, char *buf, size_t len, int timeout)
{
	struct pollfd fds[1];
	int ret;

	fds[0].fd = fd;
	fds[0].events = POLLIN;

	while (true) {
		// poll for data to read: return no bytes on timeout
		ret = poll(fds, 1, timeout);
		if (ret == 0) {
			errno = ETIMEDOUT;
			return 0;
		}

		/* read the bytes */
		const ssize_t bytes = read(fd, buf, len);
		return bytes;
	}
}

static ssize_t serial_write(int fd, const char *buf, size_t len, int timeout)
{
	struct pollfd fds[1];
	int ret;

	fds[0].fd = fd;
	fds[0].events = POLLOUT;

	while (true) {
		// poll for data to write: return no bytes on timeout
		ret = poll(fds, 1, timeout);
		if (ret == 0) {
			errno = ETIMEDOUT;
			return 0;
		}

		/* read the bytes */
		const ssize_t bytes = write(fd, buf, len);
		return bytes;
	}
}


namespace {


const string kQueryVoltagesContext = "queryVoltages";
const string queryTemperatureCContext = "queryTemperatureC";


void
sleepSomeNanos( const long nanos )
{
    struct timespec req;
    
    req.tv_sec = 0;
    req.tv_nsec = nanos;

    while ( true ) {
        struct timespec rem;
        
        if ( ::nanosleep( &req, &rem ) == 0 )
            return;
            
        if ( errno != EINTR )
            break;
            
        req = rem;
    }

    PMDTHROW( "nanosleep failure" );
}


} // namespace < anonymous >


PhaseMonitorDevice::PhaseMonitorDevice( const string & device,
                                        const bool     emulate,
                                        const string & record, 
                                        const bool  testBadVolts, 
                                        const ::std::string replayFilename ) : 
  _log( Program::getLogger() ),
  _logQuestionsAndReplies( false ),
  _devFD( -1 ),
  _devFileName( device ),
  _replayFileName( replayFilename ),
  _emulate( emulate || ( replayFilename.length() > 0 ) ),
  _testBadVolts( testBadVolts ),
  _replay( replayFilename.length() > 0 ),
  _replayFile(),
  _useRstream( false ),
  _rstream()
{
    CPTRACE( Trace::TRACE1, "PhaseMonitorDevice( '" << device
        << "', " << (boolalpha) << emulate << " )" );

  if ( record.compare( "" ) != 0 ) {
    _rstream.open( record.c_str(), ofstream::out|ofstream::app );
    _useRstream = true;
    _recordName = record;
  }

  if ( _emulate == true )
  {
    _masterPTfd = getpt();
    if ( _masterPTfd < 0 )
      PMDTHROW( "EMULATE getpt(): " << strerror( errno ) );

    if ( grantpt( _masterPTfd ) != 0 )
      PMDTHROW( "EMULATE grantpt(): " << strerror( errno ) );

    if ( unlockpt( _masterPTfd ) != 0 )
      PMDTHROW( "EMULATE unlockpt(): " << strerror( errno ) );

    _slavePTName = ptsname( _masterPTfd );
    if ( _slavePTName == NULL )
      PMDTHROW( "EMULATE ptsname(): " << strerror( errno ) );

    _devFileName = string( _slavePTName );
  } 

  devopen( _devFileName );
  AtoDSetup();

  programLogInfoIfPossible( "Opened serial port and communicating with A/D device!" );

  if ( _replayFileName.length() > 0 ) {
    _replayFile.open( _replayFileName.c_str() );
    if ( !_replayFile.good() ) 
      throw CARMA_ERROR( "Unable to open " + _replayFileName + " for replay." );
  }

  if ( record != "" && record == replayFilename ) 
    throw CARMA_ERROR( "Record and replay filenames are the same!" );
}

string PhaseMonitorDevice::getDeviceFileName()
{
  return _devFileName;
}

bool PhaseMonitorDevice::isEmulating()
{
  return _emulate;
}

void PhaseMonitorDevice::testSleepSomeNanos()
{
  sleepSomeNanos( 1000L );
}

bool PhaseMonitorDevice::testBadStartOfReply()
{
  string badreply = "bad";
  bool caught = false;
  try
  {
    startOfReply( badreply );
    cout << "  Not caught!" << endl;
  }
  catch ( InvalidResponseException & sornfe )
  {
    cout << "  Caught! what() - " << sornfe.what() << endl;
    caught = true;
  }

  return caught;
}

void PhaseMonitorDevice::AtoDSetup()
{
  CPTRACE( Trace::TRACE2, "  Setting up A/D" );
  string reply;

  int errors = 1;

  while ( errors != 0 )
  {
    reply = inquire( "$012" );

    if ( reply.compare( "!01080600" ) != 0 )
    {
      errors++;
    }
    else
      errors = 0;

    if ( errors > 5 )
      PMDTHROW( "Bad A/D Configuration: '" << reply << "'" );

    sleepSomeNanos( 30L * 1000L * 1000L );
  }

  /* Enable multiplexing on the 5 A/D channels that measure voltages */
  /* See the Advantech Adam 4000 User's Manual for details */
  reply = inquire( "$0151F" );
  
  // Give it a second to setup multiplexing...
  sleepSomeNanos( 999L * 1000L * 1000L );

  // Now verify that it's setup setting. 
  errors = 1;
  while ( errors != 0 )
  {
    reply = inquire( "$016" );

    if ( reply.compare( "!011F" ) != 0 ) 
      ++errors;
    else
      errors = 0;
    
    if ( errors > 5 )
      PMDTHROW( "Bad A/D Configuration: '" << reply << "'" );

    sleepSomeNanos( 30L * 1000L * 1000L );
  }

  CPTRACE( Trace::TRACE3, "Communication with A/D established!" );
}


  void
PhaseMonitorDevice::devopen( const string & device )
{
  _devFD = open( device.c_str(), (O_RDWR | O_NOCTTY), 0 );

  if ( _devFD == -1 )
    PMDTHROW( "Unable to open " << device << ": " << strerror( errno ) );

  struct termios tios;
  struct termios *tio = &tios;

	// clear termios
	memset(tio, 0, sizeof(*tio));

	tio->c_cflag = B9600 | CS8 | CLOCAL | CREAD | HUPCL;
	tio->c_iflag = IGNPAR;
	tio->c_oflag = IGNPAR;

	/* set input mode (canonical mode, no echo, ...) */
	tio->c_lflag = NOFLSH | ICANON;

	/* end of line character is '\r' (canonical mode splits lines with this) */
	tio->c_cc[VEOL] = '\r';

	/* timeout on 2 seconds of inactivity */
	tio->c_cc[VTIME] = 20;

	/* no minimum number of characters must be present */
	tio->c_cc[VMIN] = 0;

	/* set the new termios */
	if (tcsetattr(_devFD, TCSAFLUSH, tio) < 0)
    PMDTHROW("error tcsetattr: " << strerror(errno));

	/* flush the input and output buffers */
	if (tcflush(_devFD, TCIOFLUSH) < 0)
    PMDTHROW("error tcflush(TCIOFLUSH): " << strerror(errno));

  // Do a little sleep to avoid overwhelming the device
  sleepSomeNanos( 50L * 1000L * 1000L );  // 50 millisecond
}


void
PhaseMonitorDevice::command( const string & question )
{
	/* write a log message with the command we are sending */
	{
		std::ostringstream oss;
		if (_emulate)
			oss << "EMULATED ";
		oss << "command('" << question << "')";
		CPTRACE(Trace::TRACE7, oss.str());
	}

	/* fixups for people who forget '\r' at the end */
	std::string buf = question;
	if (question.at(question.size() - 1) != '\r')
		buf += '\r';

	size_t offset = 0;
	size_t avail = buf.size();

	/* keep writing until we've written the entire buffer */
	while (avail > 0) {
		const ssize_t bytes = serial_write(_devFD, buf.c_str() + offset, avail, 1000);
		if (bytes <= 0) {
      ThrowCarmaException( SerialCommException, 
                           "error writing to port: " << strerror( errno ) );
		}

		offset += bytes;
		avail -= bytes;
	}

	/* force the serial port to drain (unless in emulated mode) */
	int ret = _emulate ? 0 : tcdrain(_devFD);
	if (ret < 0) {
    ThrowCarmaException( SerialCommException,
                         "error draining port: " << strerror( errno ) );
	}
}

// Now read the 5th A/D input, which is the temperature of
// of the correlator box in C expressed as DegC/100 in volts
// Thermistor is LM35DT
float PhaseMonitorDevice::queryTemperatureC()
{
  string reply;
  string::size_type gts = 0;

  CPTRACE( Trace::TRACE4, " queryTemperatureC()" );

  bool done = false;
  int errors = 0;

  while ( done == false )
  {
    try
    {
      reply = inquire( "#014" );
      gts = startOfReply( reply );
      done = true;
    }
    catch ( const InvalidResponseException & )
    {
      ++errors;

      if ( errors > 5 )
	throw;

      // Just stifle the exception and try again
    }

    if ( done == false )
      sleepSomeNanos( 10L * 1000L * 1000L );
  } // while

  const string floatstring = reply.substr(gts+1, 7);

  float answer = convertStringToFloat( floatstring, queryTemperatureCContext );

  answer = answer * 100.0;

  return answer;
}


string::size_type 
PhaseMonitorDevice::startOfReply( const string & reply )
{
  const string::size_type gts = reply.find_first_of( ">" );

  if ( gts == string::npos )
  {
    ostringstream oss;

    oss << "Error in response from PhaseMonitorDevice::inquire: "
      "Expected '>' not found! reply: '" << reply << "'";

    throw CARMA_EXCEPTION( InvalidResponseException, oss.str() );
  }

  return gts;
}

/*
 * The "#01" command requests voltages from all channels that have been
 * configured on the A/D card (which has address 01). 
 * 4 channels were configured in PhaseMonitorDevice::devopen() 
 * A typical reply string would be something like:
 * ">+1.6510-0.5630-1.2230+0.3210"
 */
void PhaseMonitorDevice::queryVoltages( float *rawVolts )
{
  static float lastVolts[4] = {(float)0.0, (float)0.0, (float)0.0, (float)0.0};
  string reply;
  string::size_type gts = string::npos;

  CPTRACE( Trace::TRACE7, "  queryVoltages(...), emulate = "
      << (boolalpha) << _emulate << " replay = " << (boolalpha) << _replay );

  if ( _replay )
  {
    CPTRACE( Trace::TRACE5, "Replaying data from file..." );
    reply = replay();
    gts = startOfReply( reply );
  }
  else // not replaying...
  {
    bool done = false;
    int errors = 0;

    // Try five times to get an answer back...
    while ( done == false )
    {
      try
      {
	reply = inquire( "#01" );
	gts = startOfReply( reply );
	done = true;
      }
      catch ( const InvalidResponseException & )
      {
	++errors;

	if ( errors > 5)
	  throw;

	// Just stifle the exception and try again
      }

      if ( done == false )
	sleepSomeNanos( 10L * 1000L * 1000L );
    }
  }

  if ( _useRstream && _rstream.good() )
    _rstream << reply << endl;

  string::size_type replyOffsets[4] = { 1, 8, 15, 22 };

  for ( int i = 0; i < 4; i++ )
  {
    string floatstring = reply.substr(gts+replyOffsets[i], 7);
    rawVolts[i] = convertStringToFloat( floatstring, kQueryVoltagesContext );

    // Check if voltage looks reasonable
    if ( rawVolts[i] < -5.0 || rawVolts[i] > 5.0 )
    {
      ostringstream err;
      err << " Bad Voltage, channel " << i << " = " << rawVolts[i];
      throw CARMA_EXCEPTION( BadVoltageException, err.str() );
    }

  }

  lastVolts[0] = rawVolts[0];
  lastVolts[1] = rawVolts[1];
  lastVolts[2] = rawVolts[2];
  lastVolts[3] = rawVolts[3];
}

  float
PhaseMonitorDevice::convertStringToFloat( const string & valueString,
    const string & context )
{
  const char * const convstr = valueString.c_str();
  char * endptr = 0;
  const float answer = strtof( convstr, &endptr );
  const int savedErrno = errno;

  if ( (answer == 0) && (endptr == convstr) )
  {
    ThrowCarmaException( InvalidResponseException, 
                         "  error converting '" << valueString << 
                         "' to float in " << context << " (unknown reason)" );
  }

  if ( savedErrno == ERANGE )
  {
    ostringstream oss;

    oss << "  error converting '" << valueString
      << "' to float in " << context;

    if ( answer == 0 )
      oss << " (underflow)";
    else if ( answer == HUGE_VALF )
      oss << " (overflow)";
    else
      oss << " ERANGE (unknown reason)";

    throw CARMA_EXCEPTION( InvalidResponseException, oss.str() );
  }

  CPTRACE( Trace::TRACE7, "   convertStringToFloat( '" << valueString << "' ): "
      << answer );

  return answer;

}


string
PhaseMonitorDevice::inquire( const string & question )
{
  std::string replyString;
  char reply[256];
  ssize_t bytes;

  if ( _logQuestionsAndReplies )
    programLogInfoIfPossible( "Question=\"" + question + "\"" );

  // send the command to the serial device (or the slave PTY in emulated mode)
  command( question );

  // Need to clear off the written question if we're in emulate
  // as it is now sitting on the master pseudo terminal
  // Then write something sensible to the slave terminal
  // that will be picked up as a response to the command
  if ( _emulate ) {
    /*------------------------------------------------------------------------*/
    /* EMULATED MODE                                                          */
    /*------------------------------------------------------------------------*/

    // read the data on the master PTY (what was written in command(), earlier)
    memset(reply, 0, sizeof(reply));
    ssize_t bytes = serial_read(_masterPTfd, reply, sizeof(reply), 1000);

    // if the last char is a '\r', remove it
    if (bytes > 0 && reply[bytes - 1] == '\r')
      bytes -= 1;

    // careful about non-NUL terminated strings
    replyString = std::string(reply, bytes);

    // double check that we got the correct command back
    if (replyString != question) {
      std::ostringstream oss;
      oss << "EMULATED inquire read question='" << question << "'"
          << " does not match reply='" << replyString << "'"
          << " error: " << strerror(errno);
      PMDTHROW(oss.str());
    }

    CPTRACE( Trace::TRACE5, "EMULATED inquire( '" << question << "' )" );
    std::string cannedResponse;

    if ( question.compare( "$012" ) == 0 )
      cannedResponse = "!01080600\r";
    else if ( question.compare( "$0151F" ) == 0 )
      cannedResponse = "\r";
    else if ( question.compare( "$016" ) == 0 )
      cannedResponse = "!011F\r";
    else if ( question.compare( "#014" ) == 0 )
      cannedResponse = ">+09.9900\r";
    else if ( question.compare( "#01" ) == 0 ) {
      // queryVolts, return something that looks right
      if ( _testBadVolts == true )
        cannedResponse = ">-11.000-0.5630-1.223+0.3210\r";
      else
        cannedResponse = ">+1.6510-0.5630-1.2230+0.3210\r";
    } else {
      PMDTHROW( "EMULATED Unknown inquire question: '" << question << "'" );
    }

    // write the data out to the master PTY (shows up as readable on the slave)
    bytes = serial_write(_masterPTfd, cannedResponse.c_str(), cannedResponse.size(), 1000);
    if (bytes <= 0) {
      PMDTHROW( "EMULATED write: " << strerror(errno) );
    }
  } else {
    /*------------------------------------------------------------------------*/
    /* REAL OPERATION MODE (NOT EMULATED)                                     */
    /*------------------------------------------------------------------------*/
    CPTRACE( Trace::TRACE5, "   inquire( '" << question << "' )" );
  }

  // Do a little sleep to avoid overwhelming the device
  sleepSomeNanos( 1L * 1000L * 1000L );  // 1 millisecond

  // make sure the buffer is zeroed
  memset(reply, 0, sizeof(reply));

  // 5 retries on reading the command (500 ms each)
  for (int i = 0; i < 5; i++) {
    CPTRACE(Trace::TRACE5, "inquire: read command reply i=" << i);
    bytes = serial_read(_devFD, reply, sizeof(reply), 500);
    if (bytes > 0)
      break;
  }

  if (bytes <= 0) {
    ThrowCarmaException( SerialCommException, 
                         "error during serial port read: " << strerror(errno) );
  }

  // if the last char is a '\r', remove it
  if (bytes > 0 && reply[bytes - 1] == '\r')
    bytes -= 1;

  // careful about non-NUL terminated strings
  replyString = std::string(reply, bytes);
  CPTRACE( Trace::TRACE5, "    inquire reply: '" << replyString << "'" );

  if ( _logQuestionsAndReplies )
    programLogInfoIfPossible( "Reply=\"" + replyString + "\"" );

  // Do a little sleep to avoid overwhelming the device
  sleepSomeNanos( 1L * 1000L * 1000L );  // 1 millisecond

  return replyString;
}


void PhaseMonitorDevice::stopReplay( )
{
    if ( _replayFileName.size() == 0 )
      throw CARMA_EXCEPTION( PhaseMonitorDeviceException, 
        "Not replaying but stopReplay called." );

    _replayFile.close();

    _replay = false;
}

  string
PhaseMonitorDevice::replay()
{
  const size_t kMaxAnswer = 300;
  char answer[ kMaxAnswer ];

  if ( _replay && _replayFile.good() ) {
    _replayFile.getline( answer, kMaxAnswer - 1 );
    answer[ kMaxAnswer - 1 ] = '\0';

    if ( _replayFile.eof() )
      throw CARMA_EXCEPTION( PhaseMonitorDeviceReplayException, "EOF" );

  } else
  {
    ostringstream err;

    err << "Replay error: " << ( _replay ? strerror( errno )
	: " replay not true! setReplayActive() was not called first?" );

    throw CARMA_EXCEPTION( PhaseMonitorDeviceException, err.str() );
  }

  return string( answer );
}

ostream& operator<<( ostream& os, PhaseMonitorDevice &dev )
{
  os << "PhaseMonitorDevice( " << dev.getDeviceFileName()
    << ", emulate=" << (boolalpha) << dev.isEmulating() << " )" << endl
    << "   testBadVolts: " << dev.testBadVolts();

  return os;
}

