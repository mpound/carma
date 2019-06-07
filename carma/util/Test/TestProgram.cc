#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "carma/corba/corba.h"
#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/util/BaseException.h"

//
// @version	$Revision: 1.6 $ $Date: 2010/07/12 21:06:33 $ 
//
// @usage	test signal catching and daemonizing capabilities
// 		of Program.
//
// @description
//	Test program that allows the user to send any signal to Program. Also
//	allows the user to ask Program to daemonize this test program, which 
//	will then write to a log file output showing that is a daemon.
//
//	There could be more description here, but not sure if the parser will find it.
//
// @key daemonize	f	b   to daemonize or not to daemonize
// @key	sendsignal	0	i   signal to be sent to self
// @key	throwexception	noexception	s exception to be thrown (and caught) by self
//
// @logger TEST_FACILITY carma.test.util.TestProgram
//

using namespace std;
using namespace log4cpp;
using namespace CORBA;
using namespace carma;
using namespace carma::util;

namespace carma  {
    namespace TestProgram  {
	class UnknownExceptionRequested : public std::exception  {
	};
    }
}

using namespace carma::TestProgram;

static bool
ValidException (string exceptionName)
{
    if (exceptionName == "BaseException")  {
	return true;
    } else if (exceptionName == "SystemException")  {
	return true;
    } else if (exceptionName == "CORBAexception")  {
	return true;
    } else if (exceptionName == "stdException")  {
	return true;
    } else if (exceptionName == "anyException")  {
	return true;
    } else
	return false;
	
}



static void
ThrowException (string exceptionName)
{
    if (exceptionName == "BaseException")  {
	throw BaseException ("BaseException requested");
    } else if (exceptionName == "SystemException")  {
	throw SystemException ("SystemException requested");
    } else if (exceptionName == "CORBAexception")  {
	throw BaseException ("Sorry, cannot throw requested CORBAexception.");
    } else if (exceptionName == "stdException")  {
	throw std::exception ();
    } else if (exceptionName == "anyException")  {
	throw "any exception requested";
    } else
	throw TestProgram::UnknownExceptionRequested ();
}



int
carma::util::Program::main( ) {
    const bool daemonized = getBoolParameter( "daemonize" );
    const int sendSignal = getIntParameter( "sendsignal" );
    const ::std::string throwException = getStringParameter( "throwexception" );
    const ::std::string noException = "noexception";

    Category&	logger = Program::getLogger();

    pid_t	myPid = getpid();
    pid_t	myParentPid = getppid();

    if (daemonized)  {
	struct stat	buf;
	// get some stuff and log it
	logger << Priority::EMERG
	       << "cout is open : "
	       << (fstat (1, &buf) == 0)
	       << " and cin is open : "
	       << (fstat (0, &buf) == 0)
	       << " and cerr is open : "
	       << (fstat (2, &buf) == 0)
	       << " and my parent's pid is "
	       << myParentPid;
    }

    if (0 < sendSignal  &&  sendSignal <= SIGUNUSED)  {
	// tell the world that we're sending signal to self
	logger << Priority::EMERG
	       << "Sending signal "
	       << sendSignal
	       << " to process "
	       << myPid
	       << " child of "
	       << myParentPid;

	// send signal to self
	if (kill (myPid, sendSignal) == -1)  {
	    ostringstream errStream;
	    errStream << "Failed to send signal "
		      << sendSignal
		      << endl;
	    logger << Priority::DEBUG
	           << "Failed to send signal "
		   << sendSignal
		   << "\n";
	    throw carma::util::SystemException (errStream);
	}
    }

    if (throwException != noException)  {
	try  {
	    ThrowException (throwException);
	}  catch ( const TestProgram::UnknownExceptionRequested & )  {
	    logger << Priority::EMERG
		   << "Unknown exception type "
		   << throwException
		   << " requested.\n" 
		   << "Only supported exceptions are "
		   << "carma::util::SystemException, "
		   << "carma::util::BaseException, "
		   << "CORBA Exception, "
		   << "std::exception and, finally, the "
		   << "any, or unspecified exception. ";
	}
    }

    return 0;
}



