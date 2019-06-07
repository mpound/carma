//
// @file	syslog2carmalog.cc
// @version	$Id: syslog2carmalog.cc,v 1.6 2007/12/14 09:10:58 colby Exp $
// @author	Colby Gutierrez-Kraybill
//
// @usage Read the carma syslog UNIX socket and provide log messages to
// 	  RTD clients in real-time and to the DBMS feeder system on a
//        queuing basis.
//
// @description
// This program reads the carma syslog UNIX domain socket and provides
// a ring buffer of syslog messages in an MMAP'd file allowing for a
// real-time feed through to RTD processes and slower DBMS
// related processes for loading and then searching via the DBMS.
// 
// The flat file created for bulk loading into the DBMS is written in
// the format that syslog2Db expects (see that program for more info).
// 
// The syslog2carmalog program is design to be launched at system
// boot time using an init.d script.  This allows the system to survive
// carma restarts.  It does mean that a generic carmaSystem stop|start
// will not provide you with a new instance of this program running.
// To do that, you must use /etc/init.d/syslog2carmalog restart
//
// The system /etc/syslog.conf must have entries of the sort:
//
// ..
// local5.info;local5.!crit                /var/carma/log/interferometry.log
// local5.info;local5.!crit                |/var/carma/log/syslog.pipe
// ...
//
// @key    daemon   true    b     Daemonize (background) process.
// @key    conffile   dbms/dbms.conf    string     File from which to 
// get database configuration info, e.g. where to read/write processed files.
// The conffile location is interpreted by Program::getConfFile(). 
// @key pipe @noDefault s Pipe to read from (a FIFO or named pipe')
//    If not specified, defaults to pipeFileNames in dbms.conf
// @key inspectmmap false b Print out all information about the mmap file (verbose)
// @key test false b Test acts as a syslog message generator for
//   testing/benchmarking.  When in test mode, this program will not
//   attempt to attach to the FIFO.  This will send all messages to the
//   LOG_LOCAL2 facility, which for CARMA is the default or carma.log
// @key testnum 10000 i Number of syslog messages to generate for test.
// @key testdelay 100 i Number of ms to sleep between sending messages.
//   A value of 0 will cause no delay.  Any timing less than 1-50ms will
//   be limited to 50ms, as anything lower is approaching the jitter
//   of the usleep() used for the delay (which is 20-30ms).
// @key testsize 100 i Number of bytes to include in message.
// @key filter @noDefault s See what a filter returns.
//
// @logger DEFAULT_FACILITY carma.dbms.syslog2carmalog
//

// Generic CARMA includes
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/Time.h"
#include "carma/util/ErrorException.h"
#include "carma/util/loggingUtils.h"
#include "carma/util/StartPthread.h"

// CARMA includes specific to this program
#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/LogProcessor.h"
#include "carma/dbms/MalformedSyslogLineException.h"
#include "carma/dbms/SyslogListenerManager.h"
#include "carma/dbms/SyslogMessage.h"
#include "carma/dbms/SyslogMMAPFile.h"
#include "carma/dbms/SyslogMMAPOperator.h"
#include "carma/util/NotFoundException.h"



// C++ includes
#include <iostream>
#include <sstream>
#include <fstream>

// UNIX System includes
#include <unistd.h>
#include <syslog.h>


using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::dbms;

int carma::util::Program::main()
{

  log4cpp::Category &log( Program::getLogger() );

  try
  {
    CPTRACE( Trace::TRACE1, " syslog2carmalog program starting up" );


    string conffile = getConfFile(getStringParameter("conffile"));
    auto_ptr<DBConfigurator> dbconf;
    try
    {
      auto_ptr<DBConfigurator> dbconftmp
	(new DBConfigurator(conffile));
      dbconf = dbconftmp;
    }
    catch ( const carma::util::NotFoundException &exc )
    {
      stringstream msg;
      msg << "Unable to read configuration file " << conffile << endl;
      CARMA_CPTRACE (carma::util::Trace::TRACE1, msg.str());
      log.crit(msg.str());
      return EXIT_FAILURE;
    }

    if ( parameterWasSpecified( "filter" ) )
    {
      string filter = getStringParameter( "filter" );
      cout << " Will now filter based on filter spec: '" << filter << "'" << endl;

      SyslogMMAPFile smapf( dbconf->getSyslogMMAPFileName(), false );
      SyslogMMAPOperator soper( smapf );

      soper.setFilter( filter );

      cout << "  parsed: " << soper.toString() << endl;

      string::size_type resultSize = soper.instantSetSize();

      cout << "  Filter set size: " << resultSize << endl;
      cout << "  Will filter 10 messages" << endl;

      int count = 0;

      while ( count < 10 )
      {
	SyslogMessage *sm = soper.nextMessage();

	if ( sm != (SyslogMessage *)NULL )
	{
	  count++;
	  cout << " PASSED: " << sm->toString() << endl;
	  delete sm;
	}

      }

      return EXIT_SUCCESS;
    }

    bool inspectmmap = getBoolParameter( "inspectmmap" );

    if ( inspectmmap ) 
    {
      cout << " Inspecting MMAP file: " << dbconf->getSyslogMMAPFileName() << endl;

      cout << "  opening...";
      cout.flush();
      // Open as reader
      SyslogMMAPFile smapf( dbconf->getSyslogMMAPFileName(), false );
      cout << "done." << endl;

      cout << "  Header info: " << endl;
      cout << smapf.toVerboseString() << endl;
      cout.flush();

      cout << "Current number of messages: " << smapf.getNumMessages() << endl;
      cout << "Scanning messages." << endl;
      cout.flush();

      cout << "  Monitoring position of current/last positions for 5 messages: " << endl;

      long mon = 0;
      /*
      long nextcur;
      long cur = -1, last;
      while ( mon < 5 )
      {
	if ( (nextcur = smapf.getCurrentPos()) != cur )
	{
	  cur = nextcur;
	  last = smapf.getLastPos();
	  cout << "     cur: " << cur << "(" << smapf.getPosLoc(cur) <<  ") last:" << last << "("
	    << smapf.getPosLoc(last) << ")     " << endl;
	  mon++;
	}

	usleep(250000);
      }

      cout << endl;
      cout << "Watching the next 5 messages:" << endl;
      mon = 0;
      cur = -1;
      while ( mon < 5 )
      {
	if ( (nextcur = smapf.getCurrentPos()) != cur )
	{
	  cout << "    '" << smapf.getCurrentMessage() << "'" << endl;
	  cur = nextcur;
	  mon++;
	}

	usleep(250000);
      }

      cout << endl;
      */
      cout << "Getting all messages:" << endl;
      mon = smapf.getCurrentPos();
      SyslogMessage *msg;
      while ( mon > -1 )
      {
	msg = smapf.getMessageAt(mon);

	cout << msg->toString() << endl;

	delete msg;

	mon--;
      }

      cout << endl;

      return EXIT_SUCCESS;
    }

    bool testMode = getBoolParameter( "test" );

    if ( testMode )
    {
      // Go into syslog message generating mode
      int testNum = getIntParameter( "testnum" );
      int numSent = 0;

      CPTRACE( Trace::TRACE1, " Entering test mode, opening syslog" );

      const char *ident = "syslog2carmalog.test";
      // Open the log with identity, do not wait to open until the first syslog
      // and the proper facility (DEFAULT in CARMA e.g. carma.log)
      openlog( ident, LOG_NDELAY, LOG_LOCAL2 );
      CPTRACE( Trace::TRACE1, " Opened log" );

      useconds_t sleepTime = (useconds_t)(getIntParameter( "testdelay" ) * 1000L);

      int testSize = getIntParameter( "testsize" );
      char outBound[testSize+1];
      outBound[testSize] = '\0';

      while ( numSent < testNum )
      {
	memset( outBound, ((numSent%26)+65), testSize );
	syslog( LOG_INFO, "%9d %s", numSent, outBound );
	CPTRACE( Trace::TRACE3, "   Sent: '" << numSent << " " << outBound << "'" );

	numSent++;

	if ( sleepTime > 50000 )
	  usleep( sleepTime );
	else if ( sleepTime > 0 )
	  usleep( (useconds_t)50000 );

      }

      cout << "Total number of test messages sent: " << numSent << endl;
      cout << "someday, I'll have a checksum..." << endl;
    }
    else
    {
      ostringstream oss;

      if ( getBoolParameter( "daemon" ) )
      {
	CPTRACE( Trace::TRACE1, " daemonizing" );
	// Do not set current working dir to /
	// redirect stdin, stdout, stderr to /dev/null
	daemon( 1, 0 );
      }

      // TODO, move this into getting info from dbconf
      // and make this parameter an override
      //       string pipeFileName = getStringParameter( "pipe" );
      //     CPTRACE( Trace::TRACE2, " pipeFileName: " << pipeFileName );
      SyslogListenerManager slm( dbconf->getPipeFileNames() );
      CPTRACE( Trace::TRACE2, " syslogListenerManager object created" );

      //      SyslogListenerThread syslogListener( pipeFileName );
      //      CPTRACE( Trace::TRACE2, " syslogListener object created" );

      // StartPthreadWithRef will throw std::runtime_error on errors
      pthread_t listener = StartPthreadWithRef( SyslogListenerManager::thread,
	  slm, "syslogListenerManager" );
      CPTRACE( Trace::TRACE1, " SyslogListenerManager pthreadid: " << listener );

      string *rawMessage;
      SyslogMessage *syslogMessage;

      SyslogMMAPFile smapf( dbconf->getSyslogMMAPFileName(), true );

      while ( true )
      {
	// Now we block on new messages coming in and
	// when one is available, construct a SyslogMessage
	// and copy it into the mmap'd ring buffer
	// TODO, may need to have separate FIFO pipe for
	// each facility, as there's no other way to
	// disambiguate where a "last message repeated"
	// came from or any other non-CARMA conformal message 
	rawMessage = slm.waitForNextMessage();

	CPTRACE( Trace::TRACE6, "      msg:'" << *rawMessage << "'" );

	try
	{
	  syslogMessage = LogProcessor::fromString( rawMessage );

	  CPTRACE( Trace::TRACE2, "  got:" << syslogMessage );

	  smapf.atomicUpdate( syslogMessage );

	  delete syslogMessage;
	}
	catch ( const MalformedSyslogLineException &msle )
	{
	  cerr << "***** MALFORMED SYSLOG ENTRY****: " << msle.what() << endl;
	}

	// getMessateCount() and getTotalBytes() are not
	// atomically linked when called like this, so
	// do not rely on this for an always accurate
	// number (you could end up reading them in the
	// middle of their being updated and one being
	// updated while the other not).  In this case
	// that's okay, we just want a general idea of
	// what's going on in the trace...
	CPTRACE( Trace::TRACE3, "   " << Time::getTimeString(2) << " "  );
	//    << slm.getMessageCount() << " " 
	//   << slm.getTotalBytes() );

	// clean up memory
	delete rawMessage;
      }
    }

  }
  catch (const ErrorException & e)
  {
    std::ostringstream os;
    os << "Caught exception: " << e;

    Program::getLogger().crit(os.str());

    return(EXIT_FAILURE);
  }
  catch ( ... )
  {
    Backtrace bt;

    bt.captureNoThrow();
    string bto = bt.formattedAsString( "    ", "\n" );

    cerr << bto << endl;
    log << Priority::ERROR << "Caught unknown exception in syslog2carmalog"
      << bto;
    CPTRACE( Trace::TRACE1, "Caught unknown exception in syslog2carmalog "
	<< bto );

    return( EXIT_FAILURE );
  }

  return( EXIT_SUCCESS );
}

