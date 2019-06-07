#include "carma/antenna/sza/antenna/corba/AstroBandGatherer.h"
#include "carma/antenna/sza/antenna/corba/AstroBandListener.h"
#include "carma/antenna/sza/antenna/corba/SzaMonitorSystemMap.h"

#include "carma/szautil/Complex.h"
#include "carma/szautil/FrameFlags.h"

using namespace std;
using namespace sza::antenna::corba;
using namespace sza::util;

#define DATAFRAME_NSEC 500000000

AstroBandGatherer::AstroBandGatherer(std::string imr, unsigned corrType, NetMonitorFrame* nmf, 
				     int serverFd, unsigned nFrameMax, unsigned nFrameAvg, double thresholdLevel, double thresholdMjd)
{
  //  COUT("Inside ABG with nFrameAvg = " << nFrameAvg);
  nFrameAvg_        = nFrameAvg;
  initialize(imr, corrType, nmf, serverFd, nFrameMax);
}

void AstroBandGatherer::initialize(std::string imr, unsigned corrType, NetMonitorFrame* nmf, 
				   int serverFd, unsigned nFrameMax, double thresholdLevel, double thresholdMjd)
{
  corrType_     = corrType;
  serverFd_     = serverFd;
  imr_          = imr;
  nmf_          = nmf;
  frameBuffer_  = 0;
  nFrameMax_    = nFrameMax;
  thresholdLevel_ = thresholdLevel;
  thresholdMjd_   = thresholdMjd;

  recordNumber_ = 0;
  rcvd_         = (unsigned char) FrameFlags::NOT_RECEIVED;
  nsnap_        = 1;
  
  if(corrType_ == CORR_NONE)
    ThrowError("Received correlator type NONE");

  // Create registers for the wideband correlator

  if(corrType_ & CORR_WB)
    szaMsMap_.addWbCorrelatorRegisters(nFrameAvg_ > 0);

  if(corrType_ & CORR_SL)
    szaMsMap_.addSlCorrelatorRegisters(nFrameAvg_ > 0);

  // Finally, add register that my monitor system requires

  szaMsMap_.addRegister("array", "frame", "utc",    REG_UTC,          1, 0);
  szaMsMap_.addRegister("array", "frame", "nsnap",  REG_UINT|REG_SUM, 1, 0);
  szaMsMap_.addRegister("array", "frame", "record", REG_UINT,         1, 0);
  
  // Now initialize the NetMonitorFrame object to the register map we
  // just created

  (void)szaMsMap_.getArrayMap();
  nmf_->nadfm_.setTo(szaMsMap_.getArrayTemplate());

  // Create the frame buffer too.  We will maintain a circular buffer
  // of up to 4 frames, which means a delay of 2 seconds for
  // half-second frames.

  frameBuffer_ = new ArrayFrameBuffer(nFrameMax_, false, szaMsMap_.getArrayMap());

  // Now create listener threads

  createAstroBandListeners();

  // And set up pointers to pertinent registers in the data frame

  setupRegisterPointers();

  // Finally, set a timer to expire on the half-second

  timer_.addHandler(startNewFrame, this);
  timer_.enableTimer(true, 
		     0, 0, // start timer right away -- no delay
		     0,    // Offset from absolute second boundary = 0
		     0, 500000000); // Interval is half-second (5e8 ns)

  timer_.spawn();

  // Finally, spawn the listeners

  spawnListeners();
}

AstroBandGatherer::~AstroBandGatherer()
{
  if(frameBuffer_) {
    delete frameBuffer_;
  }
}

ABSOLUTE_TIMER_HANDLER(AstroBandGatherer::startNewFrame)
{
  AstroBandGatherer* gatherer = (AstroBandGatherer*) args;
  gatherer->startNewFrame();
}

void AstroBandGatherer::startNewFrame()
{
  // Get the timestamp corresponding to the frame we just created

  static TimeVal timeVal;
  timeVal.setToCurrentTime();
  timeVal.incrementNanoSeconds(DATAFRAME_NSEC);

  // Increment the record number

  ++recordNumber_;

  // Force the frame buffer to return a new frame for this timestamp

  static ArrayDataFrameManager* frame=0;

  frame = 
    dynamic_cast<ArrayDataFrameManager*>(frameBuffer_->getFrame(timeVal.getMjdId(DATAFRAME_NSEC),true));

  //  COUT("Just created frame for id = " << timeVal.getMjdId(DATAFRAME_NSEC));

  date_ = timeVal;

  for(unsigned i=0; i < regs_.size(); i++) {
    regs_[i].pack(frame);
  }
  
  // If there are multiple frames waiting in the queue, keep sending
  // until the buffer is drained.  But don't send the next frame until
  // at least a full half-second period after its timestamp.  
  // 
  // If we were creating frames for the current half-second, we would
  // have to wait until the count is at least 2, since the number of
  // frames in the queue is not decremented until the next call to
  // FrameBuffer::dispatchNextFrame().  But since we are always
  // creating frames for the next half-second, we must now wait until
  // the count is at least 3.  This means that we have the frame we
  // just created (for the next half-second), a frame for the current
  // half-second, and a frame for the last half-second, which should
  // be the next one sent.

  if(frameBuffer_->getNframesInQueue() > nFrameMax_ - 1)
    dispatchDataFrame();
}

void AstroBandGatherer::dispatchDataFrame()
{
  ArrayDataFrameManager* adfm = 
    dynamic_cast<ArrayDataFrameManager*>(frameBuffer_->dispatchNextFrame());

  if(adfm) {
    *nmf_->nadfm_.frame_ = *adfm->frame_;
  }

  // Notify server that data have arrived
  
  notifyServer();
}

void AstroBandGatherer::notifyServer()
{
  unsigned byte = 0x0;
  ::write(serverFd_, &byte, 1);
}

/**.......................................................................
 * Create astro band listeners
 */
void AstroBandGatherer::createAstroBandListeners()
{
  std::ostringstream objectName;

  if(corrType_ == CORR_SL) {
    astroBandNoStart_ = 1;
    astroBandNoStop_  = 8;
  } else if(corrType_ == CORR_WB) {
    astroBandNoStart_ = 9;
    astroBandNoStop_  = 24;
  } else {
    astroBandNoStart_ = 1;
    astroBandNoStop_  = 24;
  }

  // For now, we only listen to wideband astrobands

  for(unsigned astroBandNo = astroBandNoStart_; astroBandNo <= astroBandNoStop_; astroBandNo++) {
    COUT("Creating new listener for astroband: " << astroBandNo);
    AstroBandListener* listener = new AstroBandListener(this, imr_, astroBandNo, nmf_, nFrameAvg_, thresholdLevel_, thresholdMjd_);
    listeners_.push_back(listener);
  }
}
  
/**.......................................................................
 * Spawn listeners once resources are all in place
 */
void AstroBandGatherer::spawnListeners()
{
  unsigned iStop = listeners_.size();

  for(unsigned i=0; i < iStop; i++) {
    listeners_[i]->spawn();
  }
}

ArrayDataFrameManager* AstroBandGatherer::getFrame(double mjd)
{
  TimeVal tVal;
  tVal.setMjd(mjd);

  //  COUT("Calling getframe for id = " << tVal.getMjdId(DATAFRAME_NSEC));

  return dynamic_cast<ArrayDataFrameManager*>(frameBuffer_->getFrame(tVal.getMjdId(DATAFRAME_NSEC), false));
}

void AstroBandGatherer::setupRegisterPointers()
{
  std::ostringstream os;

  for(unsigned astroBandNo = astroBandNoStart_; astroBandNo < astroBandNoStop_; astroBandNo++) {
    os.str("");
    os << "Astroband" << astroBandNo;
    regs_.push_back(SzaMonitorSystemReg("corr", os.str(), "received", &rcvd_));
  }
  
  regs_.push_back(SzaMonitorSystemReg("array", "frame", "utc",    date_.data()));
  regs_.push_back(SzaMonitorSystemReg("array", "frame", "record", &recordNumber_));
  regs_.push_back(SzaMonitorSystemReg("array", "frame", "nsnap",  &nsnap_));
}
