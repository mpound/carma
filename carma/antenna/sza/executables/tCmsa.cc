#include <iostream>
#include <signal.h>

#include <sys/types.h>
#include <sys/socket.h>

#include "carma/szautil/Program.h"

#if 1
#include "carma/szautil/BitMask.h"
#endif

#include "carma/szautil/ArchiverWriterFrame.h"
#include "carma/szautil/ArrayDataFrameManagerIntegrator.h"

#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/NetMonitorFrameClient.h"
#include "carma/szautil/NetMonitorFrameServer.h"

#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "host",       "localhost", "s", USAGE "Server host"},
  { "clientPort", "5666",      "i", USAGE "Client port to connect to on host (5666 is the default port of tCmsr)"},
  { "nframe",     "120",       "i", USAGE "Number of frames to integrate before archiving (120 = 120 half-sec frames, or 1 minute)"},
  { "nmax",       "800",       "i", USAGE "Maximum number of frames to write before starting a new file (0 = no max)"},
  { "corr",       "none",      "s", USAGE "Correlator pipeline with which to integrate synchronously (none|wb|sl)"},
  { "arcdir",     ".",         "s", USAGE "Archive directory"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

//=======================================================================
// Client class to read frames from the server, integrate and archive
// them
//=======================================================================

class MonitorFrameArchiver : public sza::util::NetMonitorFrameClient {
public:

  enum CorrType {
    CORR_NONE,
    CORR_ANY,
    CORR_WB,
    CORR_SL,
  };

  MonitorFrameArchiver(bool spawnThread, 
		       std::string host, unsigned port, NetMonitorFrame* nmf);

  virtual ~MonitorFrameArchiver();

  void setNFrameToIntegrate(unsigned nFrame);
  void setArchiveDirectory(std::string archiveDirectory);
  void setNMax(unsigned nFrame);
  void setCorrelator(CorrType corrType);
  void setCorrelator(std::string corrName);

  static CorrType corrType(std::string corrName);

private:
  
  CorrType corrType_;
  std::string archiveDirectory_;
  unsigned nFrame_;
  unsigned nMax_;
  ArchiverWriterFrame* writer_;
  ArrayDataFrameManagerIntegrator integrator_;
  bool* integratingPtr_;
  int*  integrationNumberPtr_;
  bool  integrating_;
  int   integrationNumber_;
  bool  nextFrameStartsIntegration_;

  // Overload processServerData() so we can modify it to write data
  // frames as they are received

  void processServerData();
  bool integrationJustFinished();
  bool startingNewIntegration();
  bool integrating();
  void initializeIntegratingPtr(ArrayDataFrameManager& adfm);

};

MonitorFrameArchiver::MonitorFrameArchiver(bool spawnThread, 
					   std::string host, unsigned port, NetMonitorFrame* nmf) :
  NetMonitorFrameClient(spawnThread, host, port, nmf, -1, true)
{
  writer_ = 0;
  setNFrameToIntegrate(1);
  setNMax(0);
  setCorrelator(CORR_NONE);
  integratingPtr_       = 0;
  integrationNumberPtr_ = 0;
}

MonitorFrameArchiver::~MonitorFrameArchiver()
{
  if(writer_) {
    delete writer_;
    writer_ = 0;
  }
}

MonitorFrameArchiver::CorrType MonitorFrameArchiver::corrType(std::string corrName)
{
  if(corrName == "wb") {
    return CORR_WB;
  } else if(corrName == "sl") {
    return CORR_SL;
  } else if(corrName == "none") {
    return CORR_NONE;
  }

  ThrowError("Correlator type: " << corrName << " is invalid.  Must be either 'wb', 'sl', or 'none'");

  return CORR_NONE;
}

/**.......................................................................
 * If integrating synchronously with a correlator pipeline, use this
 * method to set which correlator we are synchronized with
 */
void MonitorFrameArchiver::setCorrelator(CorrType corrType)
{
  corrType_       = corrType;
}

void MonitorFrameArchiver::setCorrelator(std::string corrName)
{
  corrType_ = corrType(corrName);
}

void MonitorFrameArchiver::setNMax(unsigned nFrame)
{
  nMax_ = nFrame;
}

void MonitorFrameArchiver::setNFrameToIntegrate(unsigned nFrame)
{
  nFrame_ = nFrame;
}

void MonitorFrameArchiver::setArchiveDirectory(std::string archiveDirectory)
{
  archiveDirectory_ = archiveDirectory;
}
  
/**.......................................................................
 * What to do when a monitor frame is received.
 */
void MonitorFrameArchiver::processServerData()
{
  // Call the base-class method to deserialize the data

  NetMonitorFrameClient::processServerData();

  //-----------------------------------------------------------------------
  // If a new template was received, (re)initialize the writer, and
  // remap the read and integrate data frames to each other
  //-----------------------------------------------------------------------

  if(nmf_->nadfm_.getType() == NetArrayDataFrameManager::MEM_TEMPLATE) {

    COUT("Got a template");
    if(writer_) {
      COUT("Deleting writer_");
      delete writer_;
      writer_ = 0;
    }

    COUT("Allocating new writer_");
    writer_ = new ArchiverWriterFrame(nmf_->getArrayTemplate(), false, false);
    COUT("Writer is now: " << writer_);

    ArrayDataFrameManager* wdfm = writer_->frame();

    // Map these two frames together

    integrator_.initialize(&nmf_->nadfm_, wdfm);

    COUT("Here 0");
    initializeIntegratingPtr(nmf_->nadfm_);

    // Open the archive

    COUT("Here 1");
    if(writer_->openArcfile(archiveDirectory_)) 
      ThrowError("Unable to open archive file in directory: " << archiveDirectory_);

    // Set the maximum number of frames to write before starting a new file

    COUT("Here 2");
    writer_->setFileSize(nMax_);

    COUT("Here 3");
    //-----------------------------------------------------------------------
    // Else if a data frame was recieved, integrate it
    //-----------------------------------------------------------------------

  } else if(nmf_->nadfm_.getType() == NetArrayDataFrameManager::MEM_FRAME) {

    CTOUT("Got a frame... size = " << nmf_->nadfm_.sizeInBytes());

    //-----------------------------------------------------------------------
    // Have we integrated as many frames as were requested?  If so, we
    // should write the data frame, and then assign the new frame into
    // it
    //-----------------------------------------------------------------------

    if(integrationJustFinished()) {
      CTOUT("WRITING OLD INTEGRATION****************************");

#if 0
      BitMask bitMask;
      std::valarray<unsigned char> validityArr;
      validityArr.resize(11973);
      writer_->frame()->readReg("array", "frame", "validity", &validityArr[0]);
      bitMask.setTo(&validityArr);
      COUT("About to write Bit 10625 = " << bitMask.bitVal(10625));
      COUT("About to write Bit 10626 = " << bitMask.bitVal(10626));
#endif

      if(writer_->saveIntegration())
	ThrowError("Error saving integration in directory: " << archiveDirectory_);

    } 

    //-----------------------------------------------------------------------
    // If we are now starting a new integration, assign the current
    // frame
    //-----------------------------------------------------------------------

    if(startingNewIntegration()) {
      CTOUT("ASSIGNING NEW FRAME****************************");
      integrator_.assign();
      
      //-----------------------------------------------------------------------
      // Else if we are continuing an on-going integration, itegrate
      // the current frame
      //-----------------------------------------------------------------------

    } else if(integrating()) {

      CTOUT("INTEGRATING NEW FRAME****************************");

      integrator_.integrate();
   
#if 0
      BitMask bitMask;
      std::valarray<unsigned char> validityArr;
      validityArr.resize(11973);
      writer_->frame()->readReg("array", "frame", "validity", &validityArr[0]);
      bitMask.setTo(&validityArr);
      COUT("Bit 10625 = " << bitMask.bitVal(10625));
      COUT("Bit 10626 = " << bitMask.bitVal(10626));
#endif

    }

  }
}

/**.......................................................................
 * Get a pointer to the monitor point which determines whether or not
 * a correlator pipeline is currently integrating
 */
void MonitorFrameArchiver::initializeIntegratingPtr(ArrayDataFrameManager& adfm)
{
  integrating_                = false;
  nextFrameStartsIntegration_ = true;

  if(corrType_ == CORR_SL) {
    integratingPtr_       = (bool*)adfm.frame()->getPtr(adfm.byteOffsetInFrameOf("SlPipeline", "IntegratorStageContainer.IntegratorStage", "integrating"),       DataType::BOOL);
    integrationNumberPtr_ = (int*) adfm.frame()->getPtr(adfm.byteOffsetInFrameOf("SlPipeline", "IntegratorStageContainer.IntegratorStage", "integrationNumber"), DataType::INT);
  } else if(corrType_ == CORR_WB) {
    integratingPtr_       = (bool*)adfm.frame()->getPtr(adfm.byteOffsetInFrameOf("WbPipeline", "IntegratorStageContainer.IntegratorStage", "integrating"),       DataType::BOOL);
    integrationNumberPtr_ = (int*) adfm.frame()->getPtr(adfm.byteOffsetInFrameOf("WbPipeline", "IntegratorStageContainer.IntegratorStage", "integrationNumber"), DataType::INT);
  } else {
    integratingPtr_       = 0;
    integrationNumberPtr_ = 0;
    integrating_          = true;
  }
}

/**.......................................................................
 * Return true if an integration is complete.  
 *
 * If we are not synchronizing with a correlator pipeline, then this
 * is whenever the frame count reaches the requested number of frames
 *
 * If we are synchronizing, then this is whenever the pipeline stops
 * integrating.
 */
bool MonitorFrameArchiver::integrationJustFinished()
{
  //------------------------------------------------------------
  // If not synchronizing with any correlator, the integration is done
  // when we reach the requested frame count
  //------------------------------------------------------------

  if(corrType_ == CORR_NONE) {

    CTOUT("Inside IJF: nframe = " << integrator_.getNFrameIntegrated() <<
	  " returning " << (integrator_.getNFrameIntegrated() == nFrame_));

    return integrator_.getNFrameIntegrated() == nFrame_;

    //------------------------------------------------------------
    // Else we are synchronizing with a correlator
    //------------------------------------------------------------

  } else {

    // If we are currently integrating, and the integrating flag
    // changes state to false, then we are done with the current
    // integration

    if((integrating_ && (*integratingPtr_) == false)) {
      CTOUT("Inside IJF: Integrating and integrating pointer reports false: returning true");
      integrating_ = false;
      return true;
    }

    if(integrating_ && (*integrationNumberPtr_) != integrationNumber_) {
      CTOUT("Inside IJF: Integrating and integrationg number " << integrationNumber_ << " doesn't match " << (*integrationNumberPtr_) << ": returning true");
      return true;
    }

    // Else we are either not currently integrating, in which case we
    // have already written the last integration, or we are still
    // adding to the current integration, in which case we are also
    // not finished

    CTOUT("Inside IJF: returning false");

    return false;
  }
}

/**.......................................................................
 * Return true if we should integrate the current frame
 */
bool MonitorFrameArchiver::integrating()
{
  // If we are not synchronizing with any correlator, always return
  // true

  if(corrType_ == CORR_NONE) {
    CTOUT("Inside I: returning true");

    return true;

    // Else check the integration flag is true

  } else {

    // If we should integrate the current frame, set the integrating_
    // flag and return true

    if((*integratingPtr_) == true && (integrationNumber_ == (*integrationNumberPtr_))) {
	 CTOUT("Inside I: integration ptr reads true and integration number " << integrationNumber_ << " matches " << (*integrationNumberPtr_) << ": returning true");
      integrating_ = true;
      return true;
    }

    // Else we are not currently integrating

    CTOUT("Inside I: integration ptr reads false: returning false");
    return false;

  }
}

bool MonitorFrameArchiver::startingNewIntegration()
{
  // If we are not synchronizing with any correlator, return true if
  // an integration just finished

  if(corrType_ == CORR_NONE) {

    CTOUT("Inside SNI: nframe = " << integrator_.getNFrameIntegrated() <<
	  " returning " << (integrator_.getNFrameIntegrated() == nFrame_));

    return integrator_.getNFrameIntegrated() == nFrame_ || integrator_.getNFrameIntegrated() == 0;

    // Else we are synchronizing with a correlator

  } else {

    // We are starting a new integration if we are not currently
    // integrating, and the integration mp changes state to true

    if(!integrating_ && (*integratingPtr_) == true) {
      CTOUT("Inside SNI: NOT integrating and integration ptr reads true: returning true");
      integrating_       = true;
      integrationNumber_ = (*integrationNumberPtr_);
      return true;
    }

    if(integrating_ && (integrationNumber_ != (*integrationNumberPtr_))) {
      CTOUT("Inside SNI: integrating and integration number has incremented: returning true");
      integrationNumber_ = (*integrationNumberPtr_);
      return true;
    }
    
    // Else we are either still not integrating, or still working on
    // the current integration

    return false;
  }
}

//=======================================================================
// Get monitor frames from the server, integrate and archive them
//=======================================================================

int Program::main(void)
{
  sigset_t allSignals;
  sigfillset(&allSignals);
  pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

  MonitorFrameArchiver* archiver = 0;

  bool stop = false;
  while(!stop) {

    try {

      NetMonitorFrame nmf;

      archiver = new MonitorFrameArchiver(false, 
					  Program::getParameter("host"), 
					  Program::getIntParameter("clientPort"), 
					  &nmf);

      archiver->setNFrameToIntegrate(Program::getIntParameter("nframe"));
      archiver->setNMax(Program::getIntParameter("nmax"));
      archiver->setArchiveDirectory(Program::getParameter("arcdir"));
      archiver->setCorrelator(Program::getParameter("corr"));
      
      archiver->run();
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

    if(archiver) {
      delete archiver;
      archiver = 0;
    }

    // Sleep 10 seconds and try again

    if(!stop)
      sleep(10);
  }

  return 0;
}
