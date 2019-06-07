#include <math.h>

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/PointingParameter.h"

#include "carma/antenna/sza/antenna/control/AntennaDrive.h"
#include "carma/antenna/sza/antenna/control/AntennaException.h"
#include "carma/antenna/sza/antenna/control/Date.h"
#include "carma/antenna/sza/antenna/control/Tracker.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

using namespace carma::util;

using namespace std;
using namespace sza::array;
using namespace sza::util;

using namespace sza::antenna::control;

/**.......................................................................
 * Allocate all of the resources needed by the tracker task.
 * Note that no further resource allocation or re-allocation should
 * be peformed while the task is running. This means that the instrument
 * can be left unattended after start-up without the fear of resource
 * depletion causing unexpected failures.
 *
 * Input:
 *
 *  parent     AntennaDrive *  Resources of the parent object.
 */
Tracker::Tracker(AntennaDrive* parent, bool simPmac, bool ignoreWrapLogic) :
  SzaTask(), sza::util::GenericTask<TrackerMsg>::GenericTask(), 
  simPmac_(simPmac), ignoreWrapLogic_(ignoreWrapLogic)
{
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which ~Tracker() can be
  // safely called.

  // Set pointers to NULL

  share_   = 0;
  tracker_ = 0;
  pmac_    = 0;

  // Sanity check arguments

  if(parent == 0) {
    ThrowError("Tracker::Tracker: Received NULL parent argument");
  }

  parent_  = parent;

  share_   = parent_->getShare();

  if(share_ == 0) {
    ThrowError("Tracker::Tracker: share argument is NULL.");
  }

  // Get the object which will manage PMAC resources
  
  pmac_    = new PmacBoard(share_, "pmac", simPmac);
  
  // Lookup the register-map entry of the tracker statistics board.
  
  tracker_ = new TrackerBoard(share_, (char*)("tracker"));
  
  // And call our initialization method

  initialize();
}

/**.......................................................................
 * Delete the resources of a tracker-task.
 */
Tracker::~Tracker()
{
  if(pmac_ != 0)
    delete pmac_;

  if(tracker_ != 0)
    delete tracker_;
}

/**.......................................................................
 * Reset non-pointer members of the Tracker object
 */
void Tracker::initialize()
{
  shutdownPending_ = false;
  year_            = 0;
  whatNext_        = IGNORE;
  archived_        = PointingParameter::NONE;
  lacking_         = PointingParameter::ALL;
  lastReq_         = 0;
  lastAck_         = 0;
  oldStatus_       = TrackingStatus::LACKING; 
  newStatus_       = TrackingStatus::LACKING;
  paramsUpdated_   = false;
  pmacTracking_    = false;

  // Initialize CARMA sequence numbers

  lastAckCarmaSeqNo_[sza::util::GenericTaskMsg::NONE]       = 0;
  lastAckCarmaSeqNo_[sza::util::GenericTaskMsg::DRIVE]      = 0;
  lastAckCarmaSeqNo_[sza::util::GenericTaskMsg::RX]         = 0;
  lastAckCarmaSeqNo_[sza::util::GenericTaskMsg::CAL]        = 0;
  lastAckCarmaSeqNo_[sza::util::GenericTaskMsg::OPTICS]     = 0;
  lastAckCarmaSeqNo_[sza::util::GenericTaskMsg::OPTICALTEL] = 0;

  lastReqCarmaSeqNo_[sza::util::GenericTaskMsg::NONE]       = 0;
  lastReqCarmaSeqNo_[sza::util::GenericTaskMsg::DRIVE]      = 0;
  lastReqCarmaSeqNo_[sza::util::GenericTaskMsg::RX]         = 0;
  lastReqCarmaSeqNo_[sza::util::GenericTaskMsg::CAL]        = 0;
  lastReqCarmaSeqNo_[sza::util::GenericTaskMsg::OPTICS]     = 0;
  lastReqCarmaSeqNo_[sza::util::GenericTaskMsg::OPTICALTEL] = 0;

  // Connect to the pmac. During development we need to be able to
  // operate without the pmac, so ignore setup errors.

  if(!pmac_->connect()) 
    parent_->sendPmacConnectedMsg(false);

  // And arrange for the halt command to be sent to the pmac on the
  // next one-second tick

  haltTelescope(NULL);
}

/**.......................................................................
 * Reset non-pointer members of the Tracker object
 */
void Tracker::resetMembers()
{
  shutdownPending_ = false;
  year_            = 0;
  whatNext_        = IGNORE;
  archived_        = PointingParameter::NONE;
  lacking_         = PointingParameter::ALL;
  lastReq_         = 0;
  lastAck_         = 0;
  oldStatus_       = TrackingStatus::LACKING; 
  newStatus_       = TrackingStatus::LACKING;
  paramsUpdated_   = false;
  pmacTracking_    = false;
}

/**.......................................................................
 * This function is called by new_Tracker() and ini_Tracker() to clear
 * legacy pointing model terms before the thread is next run. It assumes
 * that all pointers either point at something valid or have been set
 * to NULL.
 */
void Tracker::reset()
{
  resetMembers();

  // Reset source parameters
  
  src_.reset();
  
  // Reset requested offset parameters  
  
  offset_.reset();
  
  // Reset pointing model parameters
  
  model_.reset();
  
  // Reset parameters of the refraction calculation
  
  atmos_.reset();
  
  // Rest site parameters
  
  site_.reset();
  
  // Reset commanded positions

  lastCommanded_.reset();
  prevCommanded_.reset();

  nextTarget_.reset();
}

/**.......................................................................
 * Respond to a 1-second tick from the time-code reader ISR by
 * recording the new clock offset. If a resynchronization of the clock
 * has been requested, also check the time against the time-code
 * reader and correct it if necessary.
 *
 * Input:
 *
 *   msg  TrackerMsg*  The message received on the Tracker message 
 *                          queue.
 */
void Tracker::addTick(TrackerMsg* msg)
{
  unsigned long tick = msg->body.tick;
  long lastMjd, lastSec;
  long currMjd, currSec;
  TimeVal* currentMjd=0;
  LogStream errStr;

  // Return without doing anything if the pmac is disconnected

  if(!pmac_->pmacIsConnected())
    return;

  // Get the timestamp of the last call to this function.

  lastMjd = lastTick_.getMjdDays();
  lastSec = lastTick_.getMjdSeconds();

  try {
    
    currentMjd = &lastTick_;

    // Determine the current MJD UTC from the the host machine
    // (synched via NTP to GPS time)

    currentMjd->setToCurrentTime();

    currMjd = currentMjd->getMjdDays();
    currSec = currentMjd->getMjdSeconds();

    // The new time should be 1 second later than the previous time.

    if((currMjd == lastMjd   && currSec == lastSec+1) ||
       (currMjd == lastMjd+1 && lastSec == 86399 && currSec == 0)) {

      // Record the new clock tick offset in the database.

      share_->setClock(*currentMjd);

      // Do we have enough information to roughly point the telescope?

      if(!okToPoint()) {
	newStatus_ = TrackingStatus::LACKING;

      } else {
	
	// Find out where the telescope is currently located and
	// record this in the archive.
	
	AxisPositions current;

	// Read the current position and set the pmac tracking status

	pmacTracking_ = pmac_->readPosition(&current, &model_);

	// Store the current position

	nextTarget_.setCurrentPosition(current);

	// The sequence is as follows: updates to the pmac are made
	// once a second, on the half-second.  The positions written
	// to the pmac each second are for the integral second tick
	// 1.5 seconds in the future.  Reads of the PMAC DPRAM are
	// made every half second, at the 0.25 and 0.75 second
	// boundaries.  When the current and commanded positions are
	// compared on the half-second, the current position for the
	// pmac will be the position read 0.25 seconds ago, which
	// should reflect the position commanded not 1 second ago, but
	// two seconds ago.
	//
	//       Commanded       Commanded       Commanded    
	//        position        position        position    
	//       for second    	 for second      for second
	//           2               3               4
	//           |               |               |       
	//      Read |  Read    Read |  Read    Read |  Read 
	//      PMAC |  PMAC    PMAC |  PMAC    PMAC |  PMAC 
	//       |   |   |       |   |   |       |   |   |   
	//
	//   |               |               |               | 
	//   |___|___|___|___|___|___|___|___|___|___|___|___|
	//
	//   0               1               2               3
	//                   
	//                   |               |	             |	     
	//                   |               |	             |	     
	//              PMAC asserts    PMAC asserts    PMAC asserts 
	//               position 1      position 2      position 3  
	//
	//

	// Now archive the current (should reflect the commanded
	// values from two ticks ago) and the commanded position from
	// two ticks ago

	try {
	  tracker_->archivePosition(&current, &prevCommanded_);
	} catch(Exception& err) {
	  cout << err.what() << endl;
	  err.report();
	  throw err;
	}

	// If the pmac is ready for a new command, execute the next
	// state of the pmac state machine.

	if(!pmac_->isBusy()) {
	  
	  // Use the (implicit) TimeVal copy constructor to create a
	  // copy of currentMjd, since updatePmac may modify the
	  // timestamp.

	  TimeVal mjd = *currentMjd;

	  // Send the updated position to the pmac.  Here we use
	  // nextTarget_.currentPosition_ instead of current.  
	  //
	  // The reason for this is that if the axis can slip
	  // mechanically, we want to be commanding a fixed position.
	  // Otherwise, the current position as reported by the pmac
	  // might be changing with time, and we will continuously be
	  // commanding changing positions.  This can lead to runaway
	  // drifting of the telescope.
	  // 
	  // Instead, nextTarget_.currentPosition_ only gets modified
	  // by the first call to setCurrentPosition() after a
	  // reset(), so we can safely pass this to updatePmac().

	  try {
	    updatePmac(mjd, &nextTarget_.currentPosition_);
	  } catch (Exception& err) {

	    cout << err.what() << endl;
	    err.report();

	    // An error may be thrown if the UMAC interface does not
	    // respond in time, which has been seen to happen
	    // infrequently.  If we are tracking a source when such a
	    // timeout occurs, we should just queue a resync and
	    // continue tracking.  If not, we will just halt the
	    // telescope.

	    errStr.initMessage(true);
	    errStr << "No response from the UMAC." <<
	      " whatNext_ = " << whatNext_ << ", newStatus_ = " << newStatus_;
	  }

	} else {
	}
      };

      // If the time hasn't changed at all then this implies that a
      // spurious interrupt was received.
      
    } else if(currMjd == lastMjd && currSec == lastSec) {
      errStr.initMessage(true);
      errStr << "Discarding spurious time-code reader interrupt.";
    } else {

      // Did the clock jump? 

      if(labs(currMjd - lastMjd) < 2) {

	long seconds = (currMjd - lastMjd) * 86400 + (currSec - lastSec);

	errStr.initMessage(true);
	errStr << "Time appears to have gone "
	       << (seconds < 0 ? "back":"forward")
	       << " by "
	       << labs(seconds)
	       << "seconds.";
      } else {

	errStr.initMessage(true);
	errStr << "Time appears to have gone "
	       << (currMjd < lastMjd ? "back":"forward")
	       << " by "
	       << labs(currMjd - lastMjd)
	       << " days.";
      };
      
      // Record the time error for inclusion in the archive database.
      
      newStatus_ = TrackingStatus::TIME_ERROR;
    };
    
  } catch(Exception& err) {
    err.report();
    newStatus_ = TrackingStatus::TIME_ERROR;
  };

  // Archive the current tracking status.
  
  archiveStatus();

  // And simply log any errors that occurred.

  if(errStr.isError())
    errStr.log();
}

/**.......................................................................
 * This is a private function of Tracker::addTick() used to update the
 * pmac on each one second tick when PmacBoard::isBusy() returns false.
 *
 * Input:
 *
 *  mjd      TimeVal&        The Julian Date (utc).
 *  current  AxisPositions*  The current position of the telescope.
 */
void Tracker::updatePmac(TimeVal& mjd, AxisPositions *current)
{
  LogStream logStr;
  const ScopedLogNdc ndc( "Tracker::updatePmac" );

  // Send the pmac the desired position of the track 1.5 seconds
  // from now.

  mjd.incrementSeconds(1.5);
  sourcePosition(mjd);
  pmacNewPosition(PmacMode::TRACK, current);
  
  // Whenever parameters of the track get updated, we have to wait
  // until the next second tick to see whether the pmac accepted
  // the modifications.
  
  if(paramsUpdated_) {
    paramsUpdated_ = false;
    newStatus_ = TrackingStatus::UPDATING;
  } else {
    
    // Get the above-commanded elevation position and rate.
    
    double el_rate  = nextTarget_.Position(Pointing::MOUNT_RATES)->
      get(Axis::EL);

    double el_angle = nextTarget_.Position(Pointing::MOUNT_ANGLES)->
      get(Axis::EL);
    
    // Use the elevation rate to extrapolate back to the current elevation
    // of the source and its elevation one second from now. Note that
    // the above elevation was computed for 1.5 seconds in the future.
    
    double el_next = el_angle - el_rate;
    double el_now  = el_next  - el_rate;
    
    // Get the current min/max elevation limits.
    
    double el_min = model_.Encoder(Axis::EL)->getMountMin();
    double el_max = model_.Encoder(Axis::EL)->getMountMax();
    
    // Is the source going to exceed the lower elevation limit
    // within the next second? Note that the above position is for
    // 1.5 seconds in the future, so we have to use the elevation
    // rate to extrapolate back in time.
    
    if(el_now <= el_min || el_next <= el_min) {
      
      if(oldStatus_ != TrackingStatus::TOO_LOW) {
        ReportSimpleError("The source is below "
			  "the lower elevation limit.");
        parent_->sendSourceSetMsg(lastReq_);
      }
      
      newStatus_ = TrackingStatus::TOO_LOW;
      
      // Acknowledge failure to track to the CARMA monitor stream

      sendCarmaSeqNoMsg(false);

      // Is the source going to exceed the upper elevation limit
      // within the next second? Note that the above position is
      // for 1.5 seconds in the future, so we have to use the
      // elevation rate to extrapolate back in time.
      
    } else if(el_now >= el_max || el_next >= el_max) {
      
      if(oldStatus_ != TrackingStatus::TOO_HIGH) 
	ReportSimpleError("The source is above "
			  "the upper elevation limit.");

      newStatus_ = TrackingStatus::TOO_HIGH;
      
      // Acknowledge failure to track to the CARMA monitor stream

      sendCarmaSeqNoMsg(false);

      // Is the pmac tracking within reasonable limits?
      
    } else if(pmacTracking_) {
      
      // Now that we are successfully on source report the
      // completion of any control-program commands that led to
      // this point.
      
      if(lastAck_ < lastReq_) {
        parent_->sendPmacDoneMsg(lastReq_);
        lastAck_ = lastReq_;
      };

      // Write the sequence number to the CARMA monitor stream

      sendCarmaSeqNoMsg(true);
      
      // Report the successful acquisition of the source?
      
      if(oldStatus_ != TrackingStatus::TRACKING) {

        if(nextTarget_.isFixed()) {
          ReportMessage("The telescope is now halted");
        } else {
          ReportMessage("The telescope is now tracking source: "  << src_.getName());
        }

      }
      
      newStatus_ = TrackingStatus::TRACKING;

    };
  };

  // Report any messages we chose to log.
  
  if(logStr.isError()) 
    ErrorDef(err, logStr);
}

/**.......................................................................
 * Write a given pmac command into the pmac dual-port-ram. Before invoking
 * this function, the caller must call PmacBoard::isBusy() to see whether the
 * pmac is ready for a new command.
 *
 * Input:
 *
 *  mode         PmacMode    The command-type to send.
 *  current AxisPositions *  The current position of the telescope, as
 */
void Tracker::pmacNewPosition(PmacMode::Mode mode, 
			      AxisPositions *current) 
{
  const ScopedLogNdc ndc( "Tracker::pmacNewPosition" );
  std::ostringstream o;
  o << "pmacNewPosition(): ignoreWrapLogic=" << boolalpha << ignoreWrapLogic_
    << ", azWrapMode=" << model_.Encoder(Axis::AZ)->getWrapModeString()
    << ")" ;
  // This will log once per second, so disable unless needed
  //programLogInfo(o.str());
  unsigned new_position = 1;    // When the new command has been written to 
                                // the dual-port-ram, this value will be 
                                // assigned to the PMAC new_position 
                                // flag. 
  PmacTarget pmac;              // The target encoder positions and rates 
                                // for each axis 
  Pointing* target = &nextTarget_;

  // Convert angles and rates to encoder positions and rates.  If we
  // have been asked to keep any axes unmoved, set the corresponding
  // target positions to the current positions of the axes and assign
  // target rates of zero.
  
  //------------------------------------------------------------
  // Check if the AZ axis is to be moved
  //------------------------------------------------------------
  
  if(target->includesAxis(Axis::AZ)) {
    target->convertMountToEncoder(model_.Encoder(Axis::AZ), 
				  pmac.PmacAxis(Axis::AZ), 
				  current->az_.count_,
				  ignoreWrapLogic_);
    if(mode == PmacMode::SLEW)
      pmac.PmacAxis(Axis::AZ)->
	setRate(model_.Encoder(Axis::AZ)->getSlewRate());
    
  } else {
    
    target->Position(Pointing::MOUNT_ANGLES)->
      set(Axis::AZ, current->az_.topo_);

    target->Position(Pointing::MOUNT_RATES)->
      set(Axis::AZ, 0);
    
    pmac.PmacAxis(Axis::AZ)->setCount(current->az_.count_);
    pmac.PmacAxis(Axis::AZ)->setRate(0);
  };
  
  //------------------------------------------------------------
  // Check if the EL axis is to be moved
  //------------------------------------------------------------
  
  if(target->includesAxis(Axis::EL)) {
    target->convertMountToEncoder(model_.Encoder(Axis::EL), 
				  pmac.PmacAxis(Axis::EL), 
				  current->el_.count_,
				  ignoreWrapLogic_);
    
    if(mode == PmacMode::SLEW)
      pmac.PmacAxis(Axis::EL)->
	setRate(model_.Encoder(Axis::EL)->getSlewRate());
    
  } else {
    
    target->Position(Pointing::MOUNT_ANGLES)->
      set(Axis::EL, current->el_.topo_);

    target->Position(Pointing::MOUNT_RATES)->
      set(Axis::EL, 0);
    
    pmac.PmacAxis(Axis::EL)->setCount(current->el_.count_);
    pmac.PmacAxis(Axis::EL)->setRate(0);
  };
  
  //------------------------------------------------------------
  // Check the PA axis - not used for the SZA
  //------------------------------------------------------------
  
  if(target->includesAxis(Axis::PA)) {
    target->convertMountToEncoder(model_.Encoder(Axis::PA), 
				  pmac.PmacAxis(Axis::PA), 
				  current->pa_.count_,
				  ignoreWrapLogic_);
    if(mode == PmacMode::SLEW)
      pmac.PmacAxis(Axis::PA)->
	setRate(model_.Encoder(Axis::PA)->getSlewRate());
    
  } else {
    
    target->Position(Pointing::MOUNT_ANGLES)->
      set(Axis::PA, current->pa_.topo_);

    target->Position(Pointing::MOUNT_RATES)->
      set(Axis::PA, 0);
    
    pmac.PmacAxis(Axis::PA)->setCount(current->pa_.count_);
    pmac.PmacAxis(Axis::PA)->setRate(0);
  };
  
  // Record the new pmac operating mode.
  
  pmac.setMode(nextTarget_.mode_);
  
  // Convert the commanded encoder counts back to topocentric mount
  // coordinates and record them for later comparison with the actual
  // telescope position. Note that the results can differ from
  // target->mount_angle because of the wrap regions.

  // First copy the last commanded position into the container for the
  // previous commanded position

  prevCommanded_ = lastCommanded_;

  // Now update the last commanded position

  lastCommanded_.az_ = model_.Encoder(Axis::AZ)->
    convertCountsToSky(pmac.az_.count_);

  lastCommanded_.el_ = model_.Encoder(Axis::EL)->
    convertCountsToSky(pmac.el_.count_);

  lastCommanded_.pa_ = model_.Encoder(Axis::PA)->
    convertCountsToSky(pmac.pa_.count_);

  // If this command initiates a slew or a halt (ie, a 'fixed'
  // position), set the prevCommand position to the new expected
  // position

  if(target->isFixed())
    prevCommanded_ = lastCommanded_;

  // Write the new values into the dual-port ram, then tell the PMAC
  // to read them by setting its new_position flag.

  pmac_->commandNewPosition(&pmac);

  // Record details of the pointing in the archive.

  tracker_->archivePointing(&archived_, 
			    &atmos_, 
			    &model_, 
			    &pmac, 
			    &nextTarget_, 
			    &site_, 
			    &offset_);
}

/**.......................................................................
 * Arrange to reboot the pmac. If the pmac new_position flag is set
 * this will be done immediately, otherwise it will be postponed to
 * the next 1-second tick.
 *
 * Input:
 *  msg    TrackerMsg *   If the halt command was received from the
 *                          control program then this must contain
 *                          a transaction sequence number. Otherwise
 *                          send NULL.
 */
void Tracker::rebootPmac(TrackerMsg* msg)
{
  // If this command was received from the control program, record its
  // transaction number.
  
  if(msg != 0)
    registerRequest(msg->body.rebootPmac.seq);
  
  // Set up pointing in preparation for the reboot
  
  nextTarget_.setupForReboot(share_);
  
  // Queue the halt for the next 1-second boundary.
  
  whatNext_ = REBOOT;
}

/**.......................................................................
 * Arrange to slew the telescope to a given az,el,dk coordinate.
 *
 * Input:
 *   msg  TrackerMsg*  The message received on the Tracker message queue.
 */
void Tracker::slewTelescope(TrackerMsg* msg)
{
  const ScopedLogNdc ndc( "Tracker::slewTelescope" );
  std::ostringstream o;
  o << "slewTelescope(): currently ignoreWrapLogic_=" 
    << boolalpha << ignoreWrapLogic_ << ",will set to false" ;
  programLogInfo(o.str());

  // Record the control program transaction number.
  
  registerRequest(msg->body.slew.seq);
  
  // Set up the pointing parameters for the slew
  
  nextTarget_.setupForSlew(share_, msg);
  // The next two lines allow use of the full azimuth range
  ignoreWrapLogic_ = false;
  model_.Encoder(Axis::AZ)->setWrapMode(WrapMode::NONE);
  
  // Queue the slew for the next 1-second boundary.
  
  whatNext_ = SLEW;

#if 1
  // Test the encoder wrapping calculation:

  double azRad = msg->body.slew.az;

  PmacAxis axis;
  int currentCount = 22130881;

  model_.Encoder(Axis::AZ)->convertMountToEncoder(azRad, 0.0, &axis, 
        currentCount, ignoreWrapLogic_);

#endif
}

/**.......................................................................
 * Arrange to halt the telescope. If the pmac new_position flag is set
 * this will be done immediately, otherwise it will be postponed to
 * the next 1-second tick.
 *
 * Input:
 *  msg   TrackerMsg  *  If the halt command was received from the
 *                         control program then this must contain
 *                         a transaction sequence number. Otherwise
 *                         send NULL.
 */
void Tracker::haltTelescope(TrackerMsg* msg)
{
  // If this command was received from the control program, record the
  // sequence
  
  if(msg != 0)
    registerRequest(msg->body.halt.seq);
  
  // Zero the pointing flow parameters to start
  
  nextTarget_.setupForHalt(share_);
  
  // Queue the halt for the next 1-second boundary.
  
  whatNext_ = HALT;
}

/**.......................................................................
 * Extend the ephemeris of the current source.
 *
 * Input:
 *   msg  TrackerMsg*  The message received on the Tracker message queue.
 */
void Tracker::extendTrack(TrackerMsg* msg)
{
  const ScopedLogNdc ndc( "Tracker::extendTrack" );
  std::ostringstream o;
  o << "extendTrack(source=" << msg->body.track.source
    << ", seq=" << msg->body.track.seq 
    << ", azWrapMode=" << msg->body.track.wrapMode
    << ")" ;
  o << " currently ignoreWrapLogic=" << boolalpha << ignoreWrapLogic_
    << " (will be set to false)";
  programLogInfo(o.str());
  
  // Convert to internal units.
  
  double mjd  = msg->body.track.mjd;

  HourAngle ra;
  ra.setRadians(msg->body.track.ra);

  DecAngle dec;
  dec.setRadians(msg->body.track.dec);

  double dist = msg->body.track.dist;

  // If this is the start of a new observation, discard legacy
  // positions of the previous source and arrange for the next call to
  // trk_update_pmac() to slew to the next source. Also arrange for
  // the start of the track to be reported.

  if(msg->body.track.seq >= 0) {
    
    src_.reset();
    src_.setName(msg->body.track.source);

    // The exact type of the source is not important for the antenna
    // (ie, SRC_J2000 vs. SRC_EPHEM), just that it knows this is not
    // an AZ/EL source

    src_.setType(sza::array::SRC_EPHEM);
    
    whatNext_ = TARGET;
    
    // Record the control program transaction number.

    registerRequest(msg->body.track.seq);
    
  };
  
    // Install the new wrap mode in the AZ Encoder object

    model_.Encoder(Axis::AZ)->setWrapMode(msg->body.track.wrapMode);
    // And make sure that it is being used...
    ignoreWrapLogic_ = false;

  // Extend the track of the current source.

  src_.extend(mjd, ra, dec, dist);

  // Also set up the next 'target' so we know that this is a real
  // source down the road, and not a slew or halt.

  nextTarget_.setupForTrack();
}

/**.......................................................................
 * Adjust the tracking offsets of specified drive axes, and round them
 * into the range -pi..pi.
 *
 * Input:
 *   msg  TrackerMsg*  The message received on the Tracker message queue.
 */
void Tracker::setOffset(TrackerMsg* msg)
{
  // Record the sequence number of the incomplete transaction, for
  // subsequent use when reporting completion to the control program.

  registerRequest(msg->body.offset.seq);
  
  offset_.Offset(msg->body.offset.offset.type)->
    set(msg->body.offset.offset);
}

/**.......................................................................
 * Record the specified angle as the deck angle at which the vertical
 * direction on the TV corresponds to the local vertical of the telescope.
 *
 * Input:
 *   msg  TrackerMsg*  The message received on the Tracker message 
 *                          queue. 
 */
void Tracker::setTvAngle(TrackerMsg* msg)
{
  signed value;  // A signed register value 
  
  // Record the value locally in floating-point radians.
  
  offset_.Offset(OffsetMsg::TV)->
    setAngle(msg->body.tvAngle.angle);
}

/**.......................................................................
 * Install new encoder offsets.
 * 
 * Input:
 *   msg  TrackerMsg*  The message received on the Tracker message 
 *                          queue. 
 */
void Tracker::setEncoderZero(TrackerMsg* msg)
{
  // This operation could break an ongoing track, so to give the user
  // an opportunity to wait to get back on source, the tracker will
  // report the completion of this update to the control program when
  // we next get on source. Record the sequence number of this update
  // to allow it to be sent with the completion message.

  registerRequest(msg->body.encoderZeros.seq);

  // Record the new values.
  
  model_.Encoder(Axis::AZ)->setZero(msg->body.encoderZeros.az);
  model_.Encoder(Axis::EL)->setZero(msg->body.encoderZeros.el);
  model_.Encoder(Axis::PA)->setZero(msg->body.encoderZeros.dk);
  
  lacking_  &= ~PointingParameter::ZEROS;
  archived_ &= ~PointingParameter::ZEROS;
  
  // Update the mount-angle limits recorded in trk->model.az,el,dk.
  
  updateMountLimits();
}

/**.......................................................................
 * Record new refraction coefficients received from the weather
 * station task.
 *
 * Input:
 *
 *   msg  TrackerMsg*  The message received on the Tracker message 
 *                          queue.
 */
void Tracker::updateRefraction(TrackerMsg* msg)
{

  // Get the appropriate refraction container.

  Refraction *r = atmos_.Refraction(msg->body.refraction.mode);

  // Record the new terms and mark them as usable.

  r->setUsable(true);
  r->setA(msg->body.refraction.a);
  r->setB(msg->body.refraction.b);

  // If the modified refraction coefficients are those that are
  // currently being used, mark them as available, but so far
  // un-archived.

  if(atmos_.isCurrent(r)) {
    lacking_  &= ~PointingParameter::ATMOSPHERE;
    archived_ &= ~PointingParameter::ATMOSPHERE;
  };
}

/**.......................................................................
 * Update the refraction
 */
void Tracker::updateRefraction()
{
  if(refracCalculator_.canComputeRefraction()) {

    sza::util::Atmosphere::RefractionCoefficients coeff = 
      refracCalculator_.refractionCoefficients();
    
    // Get the appropriate refraction container.
    
    Refraction* r = atmos_.Refraction(sza::util::PointingMode::RADIO);

    // Record the new terms and mark them as usable.

    r->setUsable(true);
    r->setA(coeff.a);
    r->setB(coeff.b);

    // If the modified refraction coefficients are those that are
    // currently being used, mark them as available, but so far
    // un-archived.
    
    if(atmos_.isCurrent(r)) {
      lacking_  &= ~PointingParameter::ATMOSPHERE;
      archived_ &= ~PointingParameter::ATMOSPHERE;
    };
  }

  if(refracCalculator_.canComputeOpticalRefraction()) {

    sza::util::Atmosphere::RefractionCoefficients coeff = 
      refracCalculator_.opticalRefractionCoefficients();
    
    // Get the appropriate refraction container.
    
    Refraction* r = atmos_.Refraction(sza::util::PointingMode::OPTICAL);

    // Record the new terms and mark them as usable.

    r->setUsable(true);
    r->setA(coeff.a);
    r->setB(coeff.b);

    // If the modified refraction coefficients are those that are
    // currently being used, mark them as available, but so far
    // un-archived.
    
    if(atmos_.isCurrent(r)) {
      lacking_  &= ~PointingParameter::ATMOSPHERE;
      archived_ &= ~PointingParameter::ATMOSPHERE;
    };
  }
}

/**.......................................................................
 * Update the equation-of-the-equinoxes interpolator.
 *
 * Input:
 *   msg  TrackerMsg*  The message received on the Tracker message 
 *                          queue.
 */
void Tracker::extendEqnEqx(TrackerMsg* msg)
{
  // Convert to internal units.

  double tt     = msg->body.extendEqnEqx.mjd;
  double eqneqx = msg->body.extendEqnEqx.eqneqx;

  // Append the new parameters to the associated quadratic interpolation
  // tables.

  share_->extendEqnEqx(tt, eqneqx);
  
  // Mark the EQN-EQX parameters as available, but so far un-archived.
  
  lacking_  &= ~PointingParameter::EQNEQX;
  archived_ &= ~PointingParameter::EQNEQX;

  archiveStatus();
}

/**.......................................................................
 * Install new slew rates.
 *
 * Input:
 *   msg  TrackerMsg*  The message received on the Tracker message 
 *                          queue.
 */
void Tracker::setSlewRate(TrackerMsg* msg)
{
  // This operation could break an ongoing track (eg if the pmac
  // rejects the rates), so to give the user an opportunity to wait to
  // get back on source, the tracker will report the completion of
  // this update to the control program when we next get on
  // source. Record the sequence number of this update to allow it to
  // be sent with the completion message.

  registerRequest(msg->body.slewRate.seq);

  // Record the new values.
  
  if(msg->body.slewRate.axes & Axis::AZ)
    model_.Encoder(Axis::AZ)->setSlewRate(msg->body.slewRate.az);

  if(msg->body.slewRate.axes & Axis::EL)
    model_.Encoder(Axis::EL)->setSlewRate(msg->body.slewRate.el);
}

/**.......................................................................
 * Record new encoder offsets and multipliers.
 *
 * Input:
 *  msg  TrackerMsg* The message received on the Tracker message 
 *                   queue.
 */
void Tracker::calEncoders(TrackerMsg* msg)
{
  // This operation could break an ongoing track, so to give the user
  // an opportunity to wait to get back on source, the tracker will
  // report the completion of this update to the control program when
  // we next get on source. Record the sequence number of this update
  // to allow it to be sent with the completion message.

  registerRequest(msg->body.encoderCountsPerTurn.seq);
  
  // Record the new values
  
  model_.Encoder(Axis::AZ)->
    setCountsPerTurn(abs(msg->body.encoderCountsPerTurn.az));
  model_.Encoder(Axis::EL)->	   				     
    setCountsPerTurn(abs(msg->body.encoderCountsPerTurn.el));
  
  model_.Encoder(Axis::AZ)->
    setCountsPerRadian(msg->body.encoderCountsPerTurn.az / twopi);
  model_.Encoder(Axis::EL)->	 				     
    setCountsPerRadian(msg->body.encoderCountsPerTurn.el / twopi);
  
  // Mark the encoder calibration parameters as available, but so far
  // un-archived.
  
  lacking_  &= ~PointingParameter::ENCODERS;
  archived_ &= ~PointingParameter::ENCODERS;
  
  // Update the mount-angle limits recorded in model.az,el,dk.
  
  updateMountLimits();
}

/**.......................................................................
 * Calibrate the axis tilts of the telescope.
 *
 * Input:
 *   msg TrackerMsg* The message received on the Tracker 
 *                        message queue.
 */
void Tracker::calTilts(TrackerMsg* msg)
{
  // This operation could break an ongoing track, so to give the user
  // an opportunity to wait to get back on source, the tracker will
  // report the completion of this update to the control program when
  // we next get on source. Record the sequence number of this update
  // to allow it to be sent with the completion message.

  registerRequest(msg->body.tilts.seq);

  // Record the new parameters for subsequent use.

  AxisTilt* az = model_.AxisTilt(Axis::AZ);
  
  az->setHaTilt(msg->body.tilts.ha);
  az->setLatTilt(msg->body.tilts.lat);
  
  model_.AxisTilt(Axis::EL)->setTilt(msg->body.tilts.el);

  // Mark the tilt parameters as available, but so far un-archived.

  lacking_  &= ~PointingParameter::TILTS;
  archived_ &= ~PointingParameter::TILTS;
}

/**.......................................................................
 * Calibrate the gravitational flexure of the telescope.
 *
 * Input:
 *   msg TrackerMsg* The message received on the Tracker 
 *                        message queue.
 */
void Tracker::calFlexure(TrackerMsg* msg)
{
  // This operation could break an ongoing track, so to give the user
  // an opportunity to wait to get back on source, the tracker will
  // report the completion of this update to the control program when
  // we next get on source. Record the sequence number of this update
  // to allow it to be sent with the completion message.

  registerRequest(msg->body.flexure.seq);
  
  // Record the new flexure parameters for subsequent use.
  
  Flexure* flexure = model_.Flexure(msg->body.flexure.mode);

  flexure->setSineElFlexure(msg->body.flexure.sFlexure);
  flexure->setCosElFlexure(msg->body.flexure.cFlexure);
  flexure->setUsable(true);
  
  // Mark the flexure parameters as available, but so far un-archived.
  
  lacking_  &= ~PointingParameter::FLEXURE;
  archived_ &= ~PointingParameter::FLEXURE;
}

/**.......................................................................
 * Calibrate the collimation of the telescope.
 *
 * Input:
 *   msg TrackerMsg* The message received on the Tracker 
 *                        message queue.
 */
void Tracker::calCollimation(TrackerMsg* msg)
{
  // Get a pointer to the container of the specified collimation model.

  Collimation* c = model_.Collimation(msg->body.collimation.mode);

  // This operation could break an ongoing track, so to give the user
  // an opportunity to wait to get back on source, the tracker will
  // report the completion of this update to the control program when
  // we next get on source. Record the sequence number of this update
  // to allow it to be sent with the completion message.

  registerRequest(msg->body.collimation.seq);

  // Record the new parameters and mark them as usable.

  Angle x = Angle(Angle::Radians(), msg->body.collimation.x);
  Angle y = Angle(Angle::Radians(), msg->body.collimation.y);

  if(msg->body.collimation.addMode == sza::util::OffsetMsg::SET) {
    c->setXOffset(x);
    c->setYOffset(y);
  } else {
    c->incrXOffset(x);
    c->incrYOffset(y);
  }

  // Mark the collimation for the current model as usable

  c->setUsable(true);

  // If the modified collimation terms are the ones that are currently
  // being used, mark them as available, but so far un-archived.

  if(model_.isCurrent(c)) {
    lacking_  &= ~PointingParameter::COLLIMATION;
    archived_ &= ~PointingParameter::COLLIMATION;
  }
}

/**.......................................................................
 * Record new encoder limits.
 *
 * Input:
 *
 *   msg TrackerMsg* The message received on the Tracker 
 *                        message queue.
 */
void Tracker::recordEncoderLimits(TrackerMsg* msg)
{
  // This operation could break an ongoing track, so to give the user
  // an opportunity to wait to get back on source, the tracker will
  // report the completion of this update to the control program when
  // we next get on source. Record the sequence number of this update
  // to allow it to be sent with the completion message.

  registerRequest(msg->body.encoderLimits.seq);
  
  model_.Encoder(Axis::AZ)->
    setLimits(msg->body.encoderLimits.az_min, 
	      msg->body.encoderLimits.az_max);

  model_.Encoder(Axis::EL)->
    setLimits(msg->body.encoderLimits.el_min, 
	      msg->body.encoderLimits.el_max);
  
  // Mark the limits as available, but so far un-archived.
  
  lacking_  &= ~PointingParameter::LIMITS;
  archived_ &= ~PointingParameter::LIMITS;
  
  // Update the mount-angle limits recorded in trk->model.az,el,dk.
  
  updateMountLimits();
}

/**.......................................................................
 * Select between the optical and radio pointing models.
 *
 * Input:
 *   msg TrackerMsg* The message received on the Tracker 
 *                        message queue.
 */
void Tracker::selectModel(TrackerMsg* msg)
{

  // This operation could break an ongoing track, so to give the user
  // an opportunity to wait to get back on source, the tracker will
  // report the completion of this update to the control program when
  // we next get on source. Record the sequence number of this update
  // to allow it to be sent with the completion message.

  registerRequest(msg->body.selectModel.seq);
  
  // Install the associated collimation, flexure and refraction
  // parameters, all of which are model-dependent (optical or radio)
  
  model_.setCurrentCollimation(msg->body.selectModel.mode);
  model_.setCurrentFlexure(msg->body.selectModel.mode);
  atmos_.setCurrentRefraction(msg->body.selectModel.mode);
  
  // The new parameters need to be archived.
  
  archived_ &= ~(PointingParameter::COLLIMATION | 
		 PointingParameter::FLEXURE |
		 PointingParameter::ATMOSPHERE);
  
  // Are the new refraction terms usable?
  
  Refraction* refrac = atmos_.currentRefraction();
  
  if(refrac->isUsable())
    lacking_ &= ~PointingParameter::ATMOSPHERE;
  else
    lacking_ |= PointingParameter::ATMOSPHERE;
  
  // Are the new collimation terms usable?
  
  Collimation* collim = model_.currentCollimation();
  
  if(collim->isUsable())
    lacking_ &= ~PointingParameter::COLLIMATION;
  else
    lacking_ |= PointingParameter::COLLIMATION;

  // Are the new flexure terms usable?
  
  Flexure* flexure = model_.currentFlexure();
  
  if(flexure->isUsable())
    lacking_ &= ~PointingParameter::FLEXURE;
  else
    lacking_ |= PointingParameter::FLEXURE;

}

/**.......................................................................
 * Update the year. Note that the time code reader doesn't supply the
 * year, so the year has to be provided by the control program.
 *
 * Input:
 *
 *   msg TrackerMsg* The message received on the Tracker 
 *                        message queue.
 */
void Tracker::changeYear(TrackerMsg* msg)
{
 
  // Record a specified new year?

  if(msg != 0) {
    year_ = msg->body.year.year;

    // At startup this function is called with msg==NULL. At that
    // point the control system clock has been initialized from the
    // Sun's clock, so the year can be extracted from it.

  } else {
    Date date;        // The broken down Gregorian version of 'utc' 
    date.convertMjdUtcToDate(share_->getUtc());
    year_ = date.getYear();
  };
}

/**.......................................................................
 * Update the local and system-wide site-location parameters.
 *
 * Input:
 *
 *   msg TrackerMsg* The message received on the Tracker 
 *                        message queue.
 */
void Tracker::locateSite(TrackerMsg* msg)
{
  Angle lon, lat;
  lon.setRadians(msg->body.site.lon);
  lat.setRadians(msg->body.site.lat);
  double altitude  = msg->body.site.alt;

  site_.setFiducial(lon, lat, altitude);

  // Update relevant site parameters in the refraction calculator

  refracCalculator_.setAltitude(site_.altitude());
  refracCalculator_.setLatitude(site_.latitude());

  share_->setSite(site_.getLongitude(), site_.getLatitude(), 
		  site_.getAltitude());
  
  // Mark the site-info as available, but so far un-archived.
  
  lacking_  &= ~PointingParameter::SITE;
  archived_ &= ~PointingParameter::SITE;
}

/**.......................................................................
 * Update the local and system-wide site-location parameters.
 *
 * Input:
 *
 *   msg TrackerMsg* The message received on the Tracker 
 *                        message queue.
 */
void Tracker::locateAntenna(TrackerMsg* msg)
{
  double up    = msg->body.location.up;
  double east  = msg->body.location.east;
  double north = msg->body.location.north;

  site_.setOffset(up, east, north);

  // Update relevant site parameters in the refraction calculator

  refracCalculator_.setAltitude(site_.altitude());
  refracCalculator_.setLatitude(site_.latitude());

  // Mark the location-info as available, but so far un-archived.
  
  lacking_  &= ~PointingParameter::LOCATION;
  archived_ &= ~PointingParameter::LOCATION;

  // Mark the site as out of date too

  lacking_  &= ~PointingParameter::SITE;
  archived_ &= ~PointingParameter::SITE;
}

/**.......................................................................
 * Update the UT1-UTC interpolator.
 *
 * Input:
 *   msg TrackerMsg* The message received on the Tracker 
 *                        message queue.
 */
void Tracker::extendUt1Utc(TrackerMsg* msg)
{
  // Convert to internal units.

  double utc    = msg->body.extendUt1Utc.mjd;
  double ut1utc = msg->body.extendUt1Utc.ut1utc;

  // Append the new parameters to the associated quadratic
  // interpolation tables.

  share_->extendUt1Utc(utc, ut1utc);
  
  // Mark the UT1-UTC parameters as available, but so far un-archived.
  
  lacking_  &= ~PointingParameter::UT1UTC;
  archived_ &= ~PointingParameter::UT1UTC;

  archiveStatus();
}

/**.......................................................................
 * Update the mount-angle limits that correspond to revised values of
 * model.{az,el,dk}.{per_turn,per_radian,zero,min,max}, and mark them
 * as unarchived.
 */
void Tracker::updateMountLimits()
{
  // Do we have enough information to do this yet?

  if(!(lacking_ & (PointingParameter::ENCODERS | 
		   PointingParameter::ZEROS | 
		   PointingParameter::LIMITS)))
    model_.updateMountLimits();
  
  // Mark the archived limits as stale.
  
  archived_ &= ~PointingParameter::LIMITS;
}

/**.......................................................................
 * Compute the encoder positions and rates needed to track a given
 * source at a given utc.
 *
 * Input:
 *  mjd               int    The day component of the MJD utc at which
 *                           coordinates are required.
 *  sec               int    The MJD utc time of day, in seconds.
 */
void Tracker::sourcePosition(TimeVal& mjd)
{
  if(nextTarget_.isFixed())
    finalizePointing();
  else
    sourcePositionTrack(mjd);
}

/**.......................................................................
 * Compute the encoder positions and rates needed to track a given
 * source at a given utc.
 *
 * Input:
 *  mjd               int    The day component of the MJD utc at which
 *                           coordinates are required.
 *  sec               int    The MJD utc time of day, in seconds.
 */
void Tracker::sourcePositionTrack(TimeVal& mjd)
{
  PointingCorrections f;  // The current apparent az/el pointing 
  double pmra;    // The proper motion in Right Ascension (radians/sec) 
  double pmdec;   // The proper motion in Declination (radians/sec) 
  
  // Compute the target time as a Modified Julian Date.

  double utc = mjd.getTimeInMjdDays();

  // Get the Terrestrial time that corresponds to 'utc'.

  double tt = share_->getTt(utc);
  
  // Determine the local apparent sidereal time.

  double lst = share_->getLst(utc);

  if(tt < 0 || lst < 0) {
    ThrowError("Tracker::sourcePosition: Illegal time received.\n");
  }
  
  // Store pointers to our internal objects
  
  sza::util::Source* src = &src_;
  Pointing* p = &nextTarget_;
  
  // Record the time stamp of the coordinates.
  
  p->setTime(utc);
  
  // We want all axes to be commanded.
  
  p->setAxes(Axis::BOTH);
  
  // Get the location of this antenna.
  
  site_.updateLatitude(&f);
  
  // Record the name of the source.
  
  p->setName(src->getName());
  
  // Interpolate for the geocentric apparent ra,dec and distance of
  // the source. Also add any equatorial tracking offsets that the
  // user has requested.

  p->setRa(  src->getRa(tt).radians() + offset_.EquatOffset()->ra_);
  p->setDec(src->getDec(tt).radians() + offset_.EquatOffset()->dec_);

  double dist = src->getDist(tt);
  p->setDist(dist);
 
  // Estimate the proper motion of the source.
  
  pmra  = src->getGradRa(tt).radians() / daysec;
  pmdec = src->getGradDec(tt).radians() / daysec;
  
  // Compute the geocentric azimuth and elevation and record them in p.
  
  p->computeGeocentricPosition(lst, &f);
  
  // Account for horizontal parallax.
  
  site_.applyParallax(dist, &f);
  
  // Account for atmospheric refraction.
  
  double refraction = atmos_.applyRefraction(&f);
  p->setRefraction(refraction);
  
  // Account for diurnal aberration.
  
  site_.applyDiurnalAberration(&f);
  
  Position* position = p->Position(Pointing::TOPOCENTRIC);
  position->set(f.az, f.el, f.pa);
  
  // Correct for telescope flexure.
  
  model_.applyFlexure(&f);
  
  // Correct for the inevitable misalignment of the azimuth axis.

  model_.AxisTilt(Axis::AZ)->apply(&f);
  
  // Correct for misalignment of the elevation axis.
  
  model_.AxisTilt(Axis::EL)->apply(&f);
  
  // Correct for collimation errors.
  
  model_.applyCollimation(&f);
  
  // Add in any sky offset.
  
  offset_.SkyOffset()->apply(&f);
  
  // Get the final az,el,pa and associated drive rates.
  
  finalizePointing(pmra, pmdec, &f);
}

/**.......................................................................
 * Compute the final az,el,pa and associated drive rates.
 *
 * Input:
 * 
 *  pmra     double                 The proper motion in Right Ascension 
 *                                  (radians/sec).
 *  pmdec    double                 The proper motion in Declination 
 *                                  (radians/sec).
 *  f        PointingCorrections *  The corrected az,el and latitude.
 */
void Tracker::finalizePointing(double pmra, double pmdec, 
			       PointingCorrections *f)
{

  // Get pointers to the mount angles and rates
  
  Position* mount = nextTarget_.Position(Pointing::MOUNT_ANGLES);
  Position* rates = nextTarget_.Position(Pointing::MOUNT_RATES);
  
  // Get the pointing model.
  
  Model* model = &model_;
  
  // Get user-supplied tracking offsets.
  
  TrackerOffset* offset = &offset_;
  
  // Get local copies of cached values.
  
  double cos_el  = f->cos_el;
  double sin_el  = f->sin_el;
  double cos_az  = f->cos_az;
  double sin_az  = f->sin_az;
  double cos_lat = f->cos_lat;
  double sin_lat = f->sin_lat;
  
  // Compute the rate of change of hour angle wrt sidereal time in
  // radians of sidereal time per second of UT1.
  
  double dhdt = rot_to_ut * twopi / daysec - pmra;
  
  // Precompute pertinent spherical trig equations.
  
  double cos_dec_cos_pa = sin_lat * cos_el - cos_lat * sin_el * cos_az;
  double sin_az_cos_lat = sin_az * cos_lat;
  double sin_dec = sin_lat * sin_el + cos_lat * cos_el * cos_az;
  
  // Record the already computed azimuth, elevation and parallactic
  // values.
  
  mount->set(f->az, f->el, f->pa);

  // Compute the azimuth, elevation and parallactic angle rotation
  // rates.  If we are at the zenith the az and pa rates will be
  // infinite, so set the rates to 0 to tell pmac_new_position() that
  // a slew will be needed to reacquire the source position.

  if(cos_el != 0.0) {
    double cos_pa = cos(mount->pa_);
    double sin_pa = sin(mount->pa_);
    rates->set(Axis::EL, dhdt * sin_az_cos_lat + pmdec * cos_pa);
    rates->set(Axis::AZ, (dhdt * cos_dec_cos_pa + pmdec * sin_pa) / cos_el);
    rates->set(Axis::PA, (dhdt * -cos_lat * cos_az + pmdec*sin_pa*sin_el) / 
	       cos_el - pmdec * sin_dec * sin_pa * sin_pa);
  } else {
    rates->set(0.0, 0.0, 0.0);
  };
  
  // Update the az and el pointing offsets to include any new offsets
  // measured by the user from the TV monitor of the optical-pointing
  // telescope.

  offset_.mergeTvOffset(f);
  
  // Apply user-supplied tracking offsets.

  mount->increment(offset_.MountOffset());
}

/**.......................................................................
 * Compute the final az,el,pa and associated drive rates.
 *
 * Input:
 * 
 *  pmra     double                 The proper motion in Right Ascension 
 *                                  (radians/sec).
 *  pmdec    double                 The proper motion in Declination 
 *                                  (radians/sec).
 *  f        PointingCorrections *  The corrected az,el and latitude.
 */
void Tracker::finalizePointing()
{
  // Get pointers to the mount angles and rates
  
  Position* mount = nextTarget_.Position(Pointing::MOUNT_ANGLES);
  Position* rates = nextTarget_.Position(Pointing::MOUNT_RATES);
  
  // Get the pointing model.
  
  Model* model = &model_;
  
  // Get user-supplied tracking offsets.
  
  TrackerOffset* offset = &offset_;
  
  // Update the az and el pointing offsets to include any new offsets
  // measured by the user from the TV monitor of the optical-pointing
  // telescope.

  offset_.mergeTvOffset(&nextTarget_);
  
  // Apply user-supplied tracking offsets.

  mount->increment(offset_.MountOffset());
}

/**.......................................................................
 * Respond to an unflag-board request for the gps or pmac boards.
 *
 * Input:
 *   msg TrackerMsg* The message received on the Tracker 
 *                        message queue.
 */
void Tracker::unflagBoard(TrackerMsg* msg)
{
  // Identify and attempt to re-initialize the offending board.  If
  // the initialization fails, return without unflagging the board.

  if(msg->body.flagBoard.board != pmac_->getIndex()) {
    ThrowError("Unknown board.");
  }
  
  // Unflag the sucessfully initialized board.
  
  share_->unflagBoard(msg->body.flagBoard.board);
}

/**.......................................................................
 * Register the receipt of a control-program command that needs to be
 * acknowledged when has taken effect. Also mark the tracking parameters
 * as modified.
 *
 * Input:
 *  seq   unsigned    The sequence number assigned to the command by
 *                    the control program.
 */
void Tracker::registerRequest(unsigned seq)
{
  // We ignore the sequence number if it is zero or negative.  The
  // former are only received from interactive commands and we don't
  // want those to interfere with a running script.  The latater are
  // only received as ephemeris updates.

  if(seq > 0) {
    lastReq_ = seq;
  }

  paramsUpdated_ = true;
}

/**.......................................................................
 * Register the receipt of a CARMA control-program command that needs to be
 * acknowledged when has taken effect.
 */
void Tracker::registerCarmaRequest(TrackerMsg* msg)
{
  if(msg->carmaSeqNoType_ != sza::util::GenericTaskMsg::NONE) {

    carmaSeqNoType_                     = msg->carmaSeqNoType_;
    lastReqCarmaSeqNo_[carmaSeqNoType_] = msg->carmaSeqNo_;

    // Now that we can have multiple subarray controllers talking to
    // us, the sequence numbers are no longer guaranteed to be
    // monotonic!  
    //
    // As long as the sequence number received is greater than the
    // last one we acknowledged, it's ok.  But if the sequence numbers
    // step backwards, then when we check if lastAckSeqNo <
    // lastReqSeqNo, it will fail, and we will not acknowledge the
    // request.  
    //
    // Therefore, if this sequence number is less than the last one we
    // acknowledged, we now reset the last acknowledged seq number so
    // that the check will succeed:

    if((msg->carmaSeqNo_ > 0) && (msg->carmaSeqNo_ < lastAckCarmaSeqNo_[carmaSeqNoType_])) {
      lastAckCarmaSeqNo_[carmaSeqNoType_] = msg->carmaSeqNo_-1;
    }

  }

#if 0
  // Just as a test - write these now

  sendCarmaSeqNoMsg(true);
#endif
}

/**.......................................................................
 * Record the current tracking status (trk->new_status) in the archive
 * database and in trk->old_status.
 */
void Tracker::archiveStatus()
{
  unsigned state = (int) newStatus_;
  unsigned off_source =
    (newStatus_ != TrackingStatus::TRACKING || !pmacTracking_) ? 1 : 0;
  
  // Record the new tracking status.
  
  tracker_->archiveStatus(state, off_source, lacking_);
  
  // Keep a record of the status for comparison with that of the next
  // 1-second tick.
  
  oldStatus_ = newStatus_;
}

/**.......................................................................
 * Process a message received on the Tracker message queue
 *
 * Input:
 *
 *   msg TrackerMsg* The message received on the Tracker 
 *                   message queue.
 */
void Tracker::processMsg(TrackerMsg* msg)
{

  if(msg->type != TrackerMsg::TICK && msg->type != TrackerMsg::STROBE_PMAC && msg->type != TrackerMsg::CONNECT_PMAC) {
    DBPRINT(true, Debug::DEBUG7, msg);
  }

  registerCarmaRequest(msg);

  switch (msg->type) {
    
  case TrackerMsg::COLLIMATION:
    calCollimation(msg);
    break;
  case TrackerMsg::CONNECT_PMAC:
    connectPmac();
    break;
  case TrackerMsg::ENCODER_CALS:
    calEncoders(msg);
    break;
  case TrackerMsg::ENCODER_LIMITS:
    recordEncoderLimits(msg);
    break;
  case TrackerMsg::ENCODER_ZEROS:
    setEncoderZero(msg);
    break;
  case TrackerMsg::EXTEND_EQNEQX:
    extendEqnEqx(msg);
    break;
  case TrackerMsg::EXTEND_UT1UTC:
    extendUt1Utc(msg);
    break;
  case TrackerMsg::FLAG_BOARD:
    unflagBoard(msg);
    break;
  case TrackerMsg::FLEXURE:
    calFlexure(msg);
    break;
  case TrackerMsg::HALT:
    haltTelescope(msg);
    break;
  case TrackerMsg::OFFSET:
    setOffset(msg);
    break;
  case TrackerMsg::REBOOT_PMAC:
    rebootPmac(msg);
    break;
  case TrackerMsg::SELECT_MODEL:
    selectModel(msg);
    break;
  case TrackerMsg::REFRACTION:
    updateRefraction(msg);
    break;    
  case TrackerMsg::LOCATION:
    locateAntenna(msg);
    break;
  case TrackerMsg::SITE:
    locateSite(msg);
    break;
  case TrackerMsg::SLEW:
    slewTelescope(msg);
    break;
  case TrackerMsg::SLEWRATE:
    setSlewRate(msg);
    break;
  case TrackerMsg::STROBE_PMAC:
    strobePmac();
    break;
  case TrackerMsg::TICK:
    try {
      addTick(msg);
    } catch(...) {
      disconnectPmac();
    }
    break;    
  case TrackerMsg::TILTS:
    calTilts(msg);
    break;
  case TrackerMsg::TRACK:
    extendTrack(msg);
    break;
  case TrackerMsg::TV_ANGLE:
    setTvAngle(msg);
    break;
  case TrackerMsg::YEAR:
    changeYear(msg);
    break;
  case TrackerMsg::WEATHER:
    refracCalculator_.
      setAirTemperature(sza::util::Temperature(sza::util::Temperature::Kelvin(), (msg->body.weather.airTemperatureInK)));
    refracCalculator_.setHumidity(msg->body.weather.relativeHumidity);
    refracCalculator_.setPressure(msg->body.weather.pressureInMbar);
    updateRefraction();
    break;
  case TrackerMsg::RX:
    refracCalculator_.setRx((sza::util::Rx::Id)msg->body.rx.id);
    updateRefraction();
    break;
  case TrackerMsg::WRAPMODE:
    COUT("Setting wrap mode to: " << msg->body.wrapMode.wrapMode);
    setWrapMode(msg->body.wrapMode.wrapMode);
    break;
  default:
    {
      ThrowError("Tracker::processMsg: Unrecognized message type");
    }
    break;
  }
}

/**.......................................................................
 * Return a list of Boards we want the parent task to adopt
 */
list<Board*> Tracker::listBoards()
{
  list<Board*> boards;

  boards.insert(boards.begin(), pmac_);

  return boards;
}

/**.......................................................................
 * Method to strobe the pmac, causing it to readout its DPRAM and copy
 * it to shared memory.
 */
void Tracker::strobePmac()
{
  if(pmac_->pmacIsConnected()) {
    try {
      pmac_->mirrorDpramToSharedMemory();
    } catch(Exception& err) {
      cout << err.what() << endl;

      // If an error occured while reading/writing the DPRAM, we
      // disconnect from the pmac, and send a message to enable the
      // connect timer.
      
      disconnectPmac();
    }
  }
}

/**.......................................................................
 * Attempt to connect to the pmac.
 */
void Tracker::connectPmac()
{
  // If we successfully connected, disable the connect timer.

  if(simPmac_ || pmac_->connect())
    parent_->sendPmacConnectedMsg(true);
}

/**.......................................................................
 * This function will be called if an error occurs talking to the
 * pmac.
 */
void Tracker::disconnectPmac()
{
  pmac_->disconnect();
  parent_->sendPmacConnectedMsg(false);
}

/**.......................................................................
 * Return true if we have enough information to roughly point the
 * telescope.
 */
bool Tracker::okToPoint() 
{
  if(simPmac_) 
    return true;

  DBPRINT(false, Debug::DEBUG3, "SITE "     << ((lacking_ & PointingParameter::SITE) ?     "is " : "is not ") << "lacking" << endl);
  DBPRINT(false, Debug::DEBUG3, "UT1UTC "   << ((lacking_ & PointingParameter::UT1UTC) ?   "is " : "is not ") << "lacking" << endl);
  DBPRINT(false, Debug::DEBUG3, "EQNEQX "   << ((lacking_ & PointingParameter::EQNEQX) ?   "is " : "is not ") << "lacking" << endl);
  DBPRINT(false, Debug::DEBUG3, "ENCODERS " << ((lacking_ & PointingParameter::ENCODERS) ? "is " : "is not ") << "lacking" << endl);
  DBPRINT(false, Debug::DEBUG3, "ZEROS "    << ((lacking_ & PointingParameter::ZEROS) ?    "is " : "is not ") << "lacking" << endl);
  DBPRINT(false, Debug::DEBUG3, "LIMITS "   << ((lacking_ & PointingParameter::LIMITS) ?   "is " : "is not ") << "lacking" << endl);
  
  return !(lacking_ &
	   (PointingParameter::SITE   | PointingParameter::UT1UTC   | 
	    PointingParameter::EQNEQX | PointingParameter::ENCODERS | 
	    PointingParameter::ZEROS  | PointingParameter::LIMITS));
}

/**.......................................................................
 * Send a message to write a CARMA sequence number
 */
void Tracker::sendCarmaSeqNoMsg(bool success)
{
  if(carmaSeqNoType_ != sza::util::GenericTaskMsg::NONE) {

    if(lastAckCarmaSeqNo_[carmaSeqNoType_] < lastReqCarmaSeqNo_[carmaSeqNoType_]) {
      parent_->sendCarmaSeqNoMsg(lastReqCarmaSeqNo_[carmaSeqNoType_], carmaSeqNoType_, success);
      lastAckCarmaSeqNo_[carmaSeqNoType_] = lastReqCarmaSeqNo_[carmaSeqNoType_];
    }
  }
}

void Tracker::setWrapMode(WrapMode::Mode wrapMode)
{
  // Install the new wrap mode in the AZ Encoder object

  model_.Encoder(Axis::AZ)->setWrapMode(wrapMode);
}
