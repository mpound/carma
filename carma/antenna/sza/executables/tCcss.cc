#include "carma/antenna/sza/antenna/corba/AstroBandGatherer.h"
#include "carma/antenna/sza/antenna/corba/SzaMonitorSystemMap.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/NetMonitorFrameServer.h"

#include "carma/szautil/Program.h"

#include "carma/util/ErrorException.h"

using namespace std;
using namespace carma::util;
using namespace sza::util;
using namespace sza::antenna::corba;

//=======================================================================
// Main starts here
//=======================================================================

PROGRAM_KEYWORDS = {
  { "corr",             "wb",   "s", USAGE "Correlator to listen to (wb | sl | both)"},
  { "nframeavg",        "0",    "i", USAGE "If > 0 the number of frames over which to calculate coherence.  If == 0, no coherence is calculated"},
  { "port",             "6665", "i", USAGE "Port on which to serve correlator data"},
  { "thresholdLevel",   "0.5",  "d", USAGE "The threshold above which a baseline is considered coherent"},
  { "thresholdMinutes", "20",   "d", USAGE "The time threshold (in minutes) above which a baseline is considered coherent"},
  { END_OF_KEYWORDS}
  };

PROGRAM_INITIALIZE_USAGE {}

int Program::main()
{
  sigset_t allSignals;
  sigfillset(&allSignals);
  pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

  try {
    std::string imr = Program::getImrHostname();

    // Create a net monitor frame
  
    NetMonitorFrame nmf;

    int fdPair[2];
    int fdRead;
    int fdWrite;

    if(::socketpair(AF_LOCAL, SOCK_STREAM, 0, fdPair)) {
      ThrowSysError("socketpair()");
    }

    fdRead  = fdPair[0];
    fdWrite = fdPair[1];

    NetMonitorFrameServer server(true, 6665, &nmf, fdRead);
    server.spawn();

    // Instantiate the AstroBandGatherer object now

    COUT("Instantiating with imr = " << imr);
    
    AstroBandGatherer* gatherer = 0;

    std::string corr        = Program::getParameter("corr");
    unsigned nFrameAvg      = Program::getIntParameter("nframeavg");
    double thresholdLevel   = Program::getDoubleParameter("thresholdLevel");
    double thresholdMinutes = Program::getDoubleParameter("thresholdMinutes");
    double thresholdMjd = thresholdMinutes/(24*60);

    if(corr == "wb") {
      gatherer = new AstroBandGatherer(imr, CORR_WB, &nmf, fdWrite, 10, nFrameAvg, thresholdLevel, thresholdMjd);
    } else if(corr == "sl") { 
      gatherer = new AstroBandGatherer(imr, CORR_SL, &nmf, fdWrite, 10, nFrameAvg, thresholdLevel, thresholdMjd);
    } else {
      COUT("Instantiating new ABG");
      gatherer = new AstroBandGatherer(imr, CORR_SL|CORR_WB, &nmf, fdWrite, 10, nFrameAvg, thresholdLevel, thresholdMjd);
    }
    
    sza::util::Runnable::blockForever();

  } catch(carma::util::ErrorException& err) {
    COUT("Caught an error: " << err.what());
    return 1;
  } catch(sza::util::Exception& err) {
    COUT("Caught an error: " << err.what());
    return 1;
  }

  return 0;
}
