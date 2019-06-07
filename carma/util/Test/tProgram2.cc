#include <cerrno>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <signal.h>

#include "carma/corba/corba.h"

#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/util/SystemException.h"

//
// @version     $Revision: 1.18 $ $Date: 2012/01/13 17:04:19 $ 
//
// @usage       test signal catching and daemonizing capabilities
//              of Program.
//
// @description
//      Test program that allows the user to send any signal to Program. Also
//      allows the user to ask Program to daemonize this test program, which 
//      will then write to a log file output showing that is a daemon.
//      An interesting test here is to grab 100 keywords a number of times
//
//      There could be more description here, but not sure if the parser will find it.
//
// @key sendsignal      0       int   signal to be sent to self
// @key throwexception  noexception     string exception to be thrown (and caught) by self
// @key  atest  0 int   Test speed access to 100 keywords
// @key  a00    1 double   Number to test
// @key  a01    1 double   Number to test
// @key  a02    1 double   Number to test
// @key  a03    1 double   Number to test
// @key  a04    1 double   Number to test
// @key  a05    1 double   Number to test
// @key  a06    1 double   Number to test
// @key  a07    1 double   Number to test
// @key  a08    1 double   Number to test
// @key  a09    1 double   Number to test
// @key  a10    1 double   Number to test
// @key  a11    1 double   Number to test
// @key  a12    1 double   Number to test
// @key  a13    1 double   Number to test
// @key  a14    1 double   Number to test
// @key  a15    1 double   Number to test
// @key  a16    1 double   Number to test
// @key  a17    1 double   Number to test
// @key  a18    1 double   Number to test
// @key  a19    1 double   Number to test
// @key  a20    1 double   Number to test
// @key  a21    1 double   Number to test
// @key  a22    1 double   Number to test
// @key  a23    1 double   Number to test
// @key  a24    1 double   Number to test
// @key  a25    1 double   Number to test
// @key  a26    1 double   Number to test
// @key  a27    1 double   Number to test
// @key  a28    1 double   Number to test
// @key  a29    1 double   Number to test
// @key  a30    1 double   Number to test
// @key  a31    1 double   Number to test
// @key  a32    1 double   Number to test
// @key  a33    1 double   Number to test
// @key  a34    1 double   Number to test
// @key  a35    1 double   Number to test
// @key  a36    1 double   Number to test
// @key  a37    1 double   Number to test
// @key  a38    1 double   Number to test
// @key  a39    1 double   Number to test
// @key  a40    1 double   Number to test
// @key  a41    1 double   Number to test
// @key  a42    1 double   Number to test
// @key  a43    1 double   Number to test
// @key  a44    1 double   Number to test
// @key  a45    1 double   Number to test
// @key  a46    1 double   Number to test
// @key  a47    1 double   Number to test
// @key  a48    1 double   Number to test
// @key  a49    1 double   Number to test
// @key  a50    1 double   Number to test
// @key  a51    1 double   Number to test
// @key  a52    1 double   Number to test
// @key  a53    1 double   Number to test
// @key  a54    1 double   Number to test
// @key  a55    1 double   Number to test
// @key  a56    1 double   Number to test
// @key  a57    1 double   Number to test
// @key  a58    1 double   Number to test
// @key  a59    1 double   Number to test
// @key  a60    1 double   Number to test
// @key  a61    1 double   Number to test
// @key  a62    1 double   Number to test
// @key  a63    1 double   Number to test
// @key  a64    1 double   Number to test
// @key  a65    1 double   Number to test
// @key  a66    1 double   Number to test
// @key  a67    1 double   Number to test
// @key  a68    1 double   Number to test
// @key  a69    1 double   Number to test
// @key  a70    1 double   Number to test
// @key  a71    1 double   Number to test
// @key  a72    1 double   Number to test
// @key  a73    1 double   Number to test
// @key  a74    1 double   Number to test
// @key  a75    1 double   Number to test
// @key  a76    1 double   Number to test
// @key  a77    1 double   Number to test
// @key  a78    1 double   Number to test
// @key  a79    1 double   Number to test
// @key  a80    1 double   Number to test
// @key  a81    1 double   Number to test
// @key  a82    1 double   Number to test
// @key  a83    1 double   Number to test
// @key  a84    1 double   Number to test
// @key  a85    1 double   Number to test
// @key  a86    1 double   Number to test
// @key  a87    1 double   Number to test
// @key  a88    1 double   Number to test
// @key  a89    1 double   Number to test
// @key  a90    1 double   Number to test
// @key  a91    1 double   Number to test
// @key  a92    1 double   Number to test
// @key  a93    1 double   Number to test
// @key  a94    1 double   Number to test
// @key  a95    1 double   Number to test
// @key  a96    1 double   Number to test
// @key  a97    1 double   Number to test
// @key  a98    1 double   Number to test
// @key  a99    1 double   Number to test
// 
// @logger TEST_FACILITY carma.test.util.tProgram2
//

//
// OLD:
// -key daemonize       f       b   to daemonize or not to daemonize
//

using namespace ::std;
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


namespace {


bool
ValidException( const string & exceptionName )
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


void
ThrowException (string exceptionName)
{
    if (exceptionName == "BaseException")  {
        throw CARMA_EXCEPTION (carma::util::ErrorException, 
                               "BaseException requested");
    } else if (exceptionName == "SystemException")  {
        throw CARMA_SYSTEM_EXCEPTION ("SystemException requested");
    } else if (exceptionName == "CORBAexception")  {
        throw CARMA_EXCEPTION (carma::util::ErrorException, 
                               "Sorry, cannot throw requested CORBAexception.");
    } else if (exceptionName == "stdException")  {
        throw std::exception ();
    } else if (exceptionName == "anyException")  {
        throw "any exception requested";
    } else
        throw TestProgram::UnknownExceptionRequested ();
}

const ::size_t kKeyCount = 100;

void
access_test( const int ntest )
{
  char key[kKeyCount][6];

  Program & p = Program::getProgram();

  for (::size_t i=0; i<kKeyCount; i++) {
    sprintf(key[i],"a%02zu",i);
  }

  double sum  = 0.0;

  for (int j=0; j<ntest; j++) {
    for (::size_t i=0; i<kKeyCount; i++) {
      sum += p.getDoubleParameter(key[i]);
    }
  }

  cout << sum << endl;
}


} // namespace < anonymous >


int
carma::util::Program::main()
{
    int         atest  = getIntParameter("atest");
    //bool      daemonized = getBoolParameter ("daemonize");
    bool        daemonized = false;
    int         sendSignal = getIntParameter ("sendsignal");
    string      throwException = getStringParameter ("throwexception");
    const string        noException = "noexception";
    Category&   logger = Program::getLogger();

    pid_t       myPid = getpid();
    pid_t       myParentPid = getppid();

    // 0 is kind of useless and the same as 1, so don't use 0, use 1..6
    CARMA_CPTRACE(::carma::util::Trace::TRACE0, "tProgram2:: useless testing trace 0" << endl);
    CARMA_CPTRACE(::carma::util::Trace::TRACE1, "tProgram2:: testing trace 1" << endl);
    CARMA_CPTRACE(::carma::util::Trace::TRACE2, "tProgram2:: testing trace 2" << endl);
    CARMA_CPTRACE(::carma::util::Trace::TRACE4, "tProgram2:: testing trace 4" << endl);

    
    if (atest) {
      access_test(atest);
      return 0;
    }


      

    if (daemonized)  {
        struct stat     buf;
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
            throw CARMA_SYSTEM_EXCEPTION (errStream.str().c_str());
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



