#include <iostream>

#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/monitorPointSpecializations.h"
#include "carma/monitor/WeatherSubsystem.h"

#include "carma/szautil/Program.h"

#include "carma/szautil/ArchiverWriterFrame.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Mutex.h"
#include "carma/szautil/NetArrayTemplate.h"
#include "carma/szautil/NetArrayDataFrameManager.h"
#include "carma/szautil/RegDate.h"
#include "carma/szautil/Runnable.h"
#include "carma/szautil/Server.h"
#include "carma/szautil/String.h"

#include "carma/antenna/sza/antenna/corba/SzaMonitorSystemMap.h"

#include "carma/szaarrayutils/arraytemplate.h"
#include "carma/szaarrayutils/arraymap.h"

#include "carma/szautil/ArrayDataFrameManager.h"
#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/NetMonitorFrameServer.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::corba;

PROGRAM_KEYWORDS = {
  { "nframe",        "0",  "i", USAGE "Number of frames"},
  { "serverPort", "5665",  "i", USAGE "Port on which we will serve data"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace carma::monitor;
using namespace carma::util;

void storeChild(const MonitorContainer::Child& child, std::vector<carma::monitor::MonitorPoint*>& mpVec);
void storeContainer(MonitorContainer& mc, std::vector<carma::monitor::MonitorPoint*>& mpVec);
void fillMonitorPointVector(MonitorSystem& ms, std::vector<carma::monitor::MonitorPoint*>& mpVec);

//=======================================================================
// Reader reads the monitor system and packs it into an SZA-style
// register map
//=======================================================================

class CarmaMonitorSystemReader : public sza::util::Runnable 
{
public:
  
  CarmaMonitorSystemReader(NetMonitorFrame* ndf, int serverFd);
  ~CarmaMonitorSystemReader();
  void run();
  static RUN_FN(runFn);
  void notifyServer();

private:
  
  MonitorSystem*      carmaMs_;
  SzaMonitorSystemMap szaMsMap_;
  NetMonitorFrame*    ndf_;
  int                 serverFd_;
  RegMapBlock*        masterClockTimestampBlock_;
  RegMapBlock*        arrayFrameUtcBlock_;
};
  
//-----------------------------------------------------------------------
// Stub to call our run method
//-----------------------------------------------------------------------

RUN_FN(CarmaMonitorSystemReader::runFn)
{
  CarmaMonitorSystemReader* runnable = (CarmaMonitorSystemReader*) arg;
  runnable->run();
  return 0;
}

//-----------------------------------------------------------------------
// Constructor instantiates a new monitor system, and constructs an
// SZA-style array map from it,
//-----------------------------------------------------------------------

CarmaMonitorSystemReader::CarmaMonitorSystemReader(NetMonitorFrame* ndf, int serverFd) :
  Runnable(true, CarmaMonitorSystemReader::runFn)
{
  // Store a pointer to the data frame object

  ndf_      = ndf;
  serverFd_ = serverFd;
  
  // Now read the CARMA monitor stream to construct the register map

  std::vector<carma::monitor::MonitorPoint*> mpVec;
  carmaMs_ = new FinalCarmaMonitorSystem();

  fillMonitorPointVector(*carmaMs_, mpVec);

  szaMsMap_.constructArrayMapFromCarmaMonitorSystem(mpVec);

  COUT("Longest string length is now: " << szaMsMap_.longestStringLen_);

  // Add registers which my reader/viewer software requires

  szaMsMap_.addRegister("array", "frame", "utc",      REG_UTC,          1, 0);
  szaMsMap_.addRegister("array", "frame", "nsnap",    REG_UINT|REG_SUM, 1, 0);
  szaMsMap_.addRegister("array", "frame", "record",   REG_UINT,         1, 0);

  // Now resize the internal validity bitmask to be large enough to
  // accomodate the current register map (+1 because we will also add
  // the validity register to the register map)

  unsigned validitySizeInBytes = szaMsMap_.initializeValidityBitMask();
  COUT("Validity reg size in bytes is now: " << validitySizeInBytes);
  // And add a register that will store validity flags for the whole
  // register map

  szaMsMap_.addRegister("array", "frame", "validity", REG_UCHAR|REG_UNION,  validitySizeInBytes);

  // Now initialize the validity flags

  szaMsMap_.initializeValidityBitIndices();

  // Finally, map SZA register pointers to CARMA monitor points

#if 0 // Doesn't work for some reason I will have to look into
  ArrayMap* arrayMap = szaMsMap_.getArrayMap();
  ndf_->nadfm_.initialize(arrayMap, false);
  szaMsMap_.setupRegisterPointers(ndf_->nadfm_);
#else
  (void)szaMsMap_.getArrayMap();
  ndf_->nadfm_.setTo(szaMsMap_.getArrayTemplate());
  szaMsMap_.setupRegisterPointers(ndf_->nadfm_);
#endif

  // Just write nsnap once -- the value will be fixed for all frames.
  // This is needed to keep track of integration of frames downstream
  // by archivers

  ndf_->nadfm_.writeReg("array", "frame", "nsnap", (unsigned int)1);
}

//-----------------------------------------------------------------------
// Destructor just deletes the reference to the CMS
//-----------------------------------------------------------------------

CarmaMonitorSystemReader::~CarmaMonitorSystemReader()
{
  if(carmaMs_) {
    delete carmaMs_;
    carmaMs_ = 0;
  }
}

//-----------------------------------------------------------------------
// Run method of this class.  This is where all the work is done.  The
// reader thread just sits in a loop, reading from the CMS, packing
// the half-second MS data into the SZA-style data frame, and
// notifying the server thread that new data are ready
//-----------------------------------------------------------------------

void CarmaMonitorSystemReader::run()
{
  static RegDate date;
  static RegDate::Data* datePtr = date.data();

  // Reset the queue so that we will perform blocking reads on the
  // carma monitor system
  
  carmaMs_->resetQueue();

  do {

    COUT("About to read...");
    carmaMs_->read();
    COUT("About to read... done");

    ndf_->guard_.lock();

    szaMsMap_.packData();

    // Copy the date into array.frame.utc and write the record counter

    unsigned int frameCount = carmaMs_->getFrameCount();
    double frameMjd = carma::util::Time::MJD(frameCount);

    date.setMjd(frameMjd);
    ndf_->nadfm_.writeReg("array", "frame", "utc",    datePtr);
    ndf_->nadfm_.writeReg("array", "frame", "record", &frameCount);

    // Lastly, write the validity flags for this frames

    unsigned char* validityPtr = szaMsMap_.getValidityPtr();

    ndf_->nadfm_.writeReg("array", "frame", "validity", validityPtr);
    
#if 0
    unsigned char src[] = "3c345.451";
    ndf_->nadfm_.writeReg("Sza1", "Tracker", "source", src);
#endif

    ndf_->guard_.unlock();

    notifyServer();

  } while(true);
}

//-----------------------------------------------------------------------
// Notify the server thread that new data are present
//-----------------------------------------------------------------------

void CarmaMonitorSystemReader::notifyServer() 
{
  // Notify server that data have arrived
  
  unsigned byte = 0x0;
  ::write(serverFd_, &byte, 1);
}

//=======================================================================
// Entry point of this program.  We instantiate a monitor system
// reader, to acquire data from the CARMA monitor system, and a
// server, to republish data in mpstore format.
//=======================================================================

int Program::main(void)
{
  sigset_t allSignals;
  sigfillset(&allSignals);
  pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

  NetMonitorFrameServer*    server = 0;
  CarmaMonitorSystemReader* reader = 0;

  bool stop = false;
  while(!stop) {

    try {
      NetMonitorFrame ndf;
      
      int fdPair[2];
      int fdRead;
      int fdWrite;
      
      if(::socketpair(AF_LOCAL, SOCK_STREAM, 0, fdPair)) {
	ThrowSysError("socketpair()");
      }
      
      fdRead  = fdPair[0];
      fdWrite = fdPair[1];
      
      server = new NetMonitorFrameServer(false, Program::getIntParameter("serverPort"), &ndf, fdRead);
      
      reader = new CarmaMonitorSystemReader(&ndf, fdWrite);
      reader->spawn();
      
      server->run();
      stop = true;
      
    } catch(carma::util::ErrorException& err) {
      CARMALOGINFO("Caught an error: " << err.what());
      COUT("Caught an error: " << err.what());
    } catch(sza::util::Exception& err) {
      CARMALOGINFO("Caught an error: " << err.what());
      COUT("Caught an error: " << err.what());
    } catch(...) {
      CARMALOGINFO("Caught an unknown error");
      COUT("Caught an unknown error");
    }

    // Explicitly call the destructors to make sure any socket
    // connections are properly shut down

    if(server) {
      delete server;
      server = 0;
    }

    if(reader) {
      delete reader;
      reader = 0;
    }

    // Sleep 10 seconds and try again

    if(!stop)
      sleep(10);
  }

  return 0;
}


//-----------------------------------------------------------------------
// Utility function to create a vector of monitor points from the CMS
//-----------------------------------------------------------------------

void fillMonitorPointVector(MonitorSystem& ms, std::vector<carma::monitor::MonitorPoint*>& mpVec) 
{
  storeContainer(ms, mpVec);
}

//-----------------------------------------------------------------------
// Utility function to write a container into the vector of monitor
// points
//-----------------------------------------------------------------------

void storeContainer(MonitorContainer& mc, std::vector<carma::monitor::MonitorPoint*>& mpVec)
{
  for (int i=0; i < mc.getNumChildren(); i++) {
    const MonitorContainer::Child& child = mc.getChild(i);
    storeChild(child, mpVec);
  }
}

//-----------------------------------------------------------------------
// Utility function to write the child of a container into the vector
// of monitor points
//-----------------------------------------------------------------------

void storeChild(const MonitorContainer::Child& child, std::vector<carma::monitor::MonitorPoint*>& mpVec)
{
  if(child.isMp()) {
    mpVec.push_back(child.mpPtr());
  } else if(child.isContainer()) {
    storeContainer(child.containerRef(), mpVec);
  } else if(child.isSubsystem()) {
    storeContainer(child.containerRef(), mpVec);
  }
}
