#ifndef TRACKER_H
#define TRACKER_H

/**
 * @file Tracker.h
 * 
 * Tagged: Thu Nov 13 16:53:55 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <list>
#include <map>

// SZA class includes

#include "carma/szautil/Atmosphere.h"
#include "carma/szautil/PmacMode.h"
#include "carma/szautil/QuadPath.h"
#include "carma/szautil/Source.h"
#include "carma/szautil/TrackingStatus.h"

#include "carma/szautil/TimeVal.h"

#include "carma/antenna/sza/antenna/control/Atmosphere.h"
#include "carma/antenna/sza/antenna/control/CameraBoard.h"
#include "carma/antenna/sza/antenna/control/Site.h"
#include "carma/antenna/sza/antenna/control/SzaTask.h"
#include "carma/antenna/sza/antenna/control/PmacBoard.h"
#include "carma/antenna/sza/antenna/control/Pointing.h"
#include "carma/antenna/sza/antenna/control/PointingCorrections.h"
#include "carma/antenna/sza/antenna/control/Position.h"
#include "carma/antenna/sza/antenna/control/TrackerBoard.h"
#include "carma/antenna/sza/antenna/control/TrackerMsg.h"
#include "carma/antenna/sza/antenna/control/TrackerOffset.h"
#include "carma/antenna/sza/antenna/control/WrapMode.h"

// Needed for various sza::array enumerators

#include "carma/szaarrayutils/szaregs.h"

namespace sza {
  namespace antenna {
    namespace control {

      class AntennaDrive;

      /**
       * Define a class that will handle pointing and tracking for
       * this antenna.
       */
      class Tracker :  
	public SzaTask,
	public sza::util::GenericTask<TrackerMsg> {
	
	//============================================================
	// Public members
	//============================================================
	
	public:
	
	/**
	 * Enumerate the boards controlled by this task
	 */
	enum BoardType {
	  CAMERA,
	  TRACKER,
	  PMAC
	};
	
	/**
	 * Return a list of boards controlled by this task
	 */
	std::list<Board*> listBoards();
	
	//============================================================
	// Private members
	//============================================================
	
	private:
	
	// The receiver we're currently using

	sza::util::Rx::Id rxId_;

	bool simPmac_;
	bool ignoreWrapLogic_;

	/**
	 * The following identifiers define stages in the state
	 * machine that controls the pmac.
	 */
	enum NextPmacState {
	  IGNORE,   // No further action needed (pmac halted)
	  HALT,     // Tell the pmac to stop the telescope drives
	  SLEW,     // Tell the pmac to slew
	  WAIT,     // Waiting for the pmac to signal completion of a
		    // slew
	  TARGET,   // Prepare to change source
	  SYNC,     // Prepare the pmac to start a new track
	  SLAVE,    // Send the next 1-second position of an ongoing
		    // track
	  REBOOT    // Tell the pmac to reboot itself 
	};
	
	//------------------------------------------------------------
	// Private methods & members
	//------------------------------------------------------------
	
	friend class AntennaDrive;
	
	/**
	 * Pointer to the resources of the parent task.
	 */
	AntennaDrive* parent_;
	
	/**
	 * Constructor function requires the shared resource object
	 * which will maintain pertinent registers of this antenna's
	 * state.
	 *
	 * @throws Exception
	 */
	Tracker(AntennaDrive* parent, bool simPmac=false, bool ignoreWrapLogic=true);
	
	/**
	 * Destructor.
	 */
	~Tracker();
	
	/**
	 * Public method to initate a response to a message received on the
	 * AntennaDrive message queue
	 *
	 * @throws Exception
	 */
	void processMsg(TrackerMsg* msg);
	
	//------------------------------------------------------------
	// Private member declarations
	//------------------------------------------------------------
	
	/**
	 * True when a shutdown request has been received.
	 */
	bool shutdownPending_; 
	
	/**
	 * The current year - received from the control program.
	 */
	int year_;              
	
	/**
	 * The next PMAC action to perform.
	 */
	NextPmacState whatNext_;
	
	/**
	 * The set of externally provided pointing parameters that
	 * have been recorded in the archive since their last
	 * change. This is a bit set of Pointing::Parameter values
	 * above
	 */
	unsigned archived_; 
	
	/**
	 * The set of externally provided pointing parameters which
	 * have not been received yet. This is a bitwise union of
	 * Pointing::Parameter values
	 */
	unsigned lacking_;     
	
	/**
	 * The sequence number of the last telescope positioning
	 * request received from the control program.
	 */
	long lastReq_;       
	
	/**
	 * The last sequence number that was acknowledged to the control
	 * program on completion.
	 */
	long lastAck_;        

	std::map<sza::util::GenericTaskMsg::CarmaSeqNoType, unsigned long> lastReqCarmaSeqNo_;
	std::map<sza::util::GenericTaskMsg::CarmaSeqNoType, unsigned long> lastAckCarmaSeqNo_;
	sza::util::GenericTaskMsg::CarmaSeqNoType carmaSeqNoType_;
	
	/**
	 * The tracking status at the previous 1-second tick.
	 */
	sza::util::TrackingStatus::Status oldStatus_; 
	
	/**
	 * The current tracking status 
	 */
	sza::util::TrackingStatus::Status newStatus_; 
	
	/**
	 * This should be set to non-zero whenever any parameters that
	 * could break the current track are changed.
	 */
	bool paramsUpdated_;   
	/**
	 * The last value of the "acquired" bit of the pmac status
	 * register that was read by PmacBoard::readPosition() on the
	 * last 1-second tick.
	 */
	int pmacTracking_;     
	/**
	 * The latest atmospheric refraction terms
	 */
	Atmosphere atmos_;      
	
	/**
	 * A virtual board of camera state registers
	 */
	CameraBoard* camera_;   
	
	/**
	 * The telescope pointing model
	 */
	Model model_;           
	
	/**
	 * Registers of the PMAC motion-controller board the control
	 * host.
	 */
	PmacBoard* pmac_;       
	
	/**
	 * The position to request on the next 1-sec pulse
	 */
	Pointing nextTarget_;  
	
	/**
	 * The last two commanded mount positions
	 */
	Position lastCommanded_;    // Position commanded on the last tick
	Position prevCommanded_;    // Position commanded two ticks ago
	
	/**
	 * The location of the SZA
	 */
	Site site_;             
	
	// An object for handling atmospheric calculations

	sza::util::Atmosphere refracCalculator_;

	/**
	 * The current source
	 */
	sza::util::Source src_;            
	
	/**
	 * The time of the last time-code-reader interrupt
	 */
	sza::util::TimeVal lastTick_;   
	
	/**
	 * A virtual board of tracker registers
	 */
	TrackerBoard* tracker_; 
	
	/**
	 * The current mount tracking offsets
	 */
	TrackerOffset offset_;  
	
	//------------------------------------------------------------
	// Private Methods
	//------------------------------------------------------------
	
	void resetMembers();
	
	/**
	 * This function is called by new_Tracker() and ini_Tracker() to clear
	 * legacy pointing model terms before the thread is next run. It assumes
	 * that all pointers either point at something valid or have been set
	 * to NULL.
	 */
	void reset();
	
	/**
	 * Reset non-pointer members of the Tracker object
	 *
	 * @throws Exception
	 */
	void initialize();
	
	/**
	 * This is a private function of addTick used to update the
	 * pmac on each one second tick when PmacBoard::isBusy returns
	 * 0.
	 *
	 * @param  mjd     long    The Julian Date (utc).
	 * @param  sec     long    The number of seconds into 'mjd'.
	 * @param  current AxisPositions *  The current position of the 
	 *                                  telescope.
	 * @throws Exception
	 */
	void updatePmac(sza::util::TimeVal& mjd, 
			AxisPositions* current);
	
	/**
	 * Record the current tracking status (newStatus_) in the
	 * archive database and in oldStatus_.
	 */
	void archiveStatus();
	
	/**
	 * Update the mount-angle limits that correspond to revised
	 * values of
	 * model.{az,el,dk}.{per_turn,per_radian,zero,min,max}, and
	 * mark them as unarchived.
	 */
	void updateMountLimits();
	
	/**
	 * Compute the encoder positions and rates needed to track a given
	 * source at a given utc.
	 *
	 * @param mjd int    The day component of the MJD utc at which
	 *                           coordinates are required.
	 * @param sec int    The MJD utc time of day, in seconds.
	 */
	void sourcePosition(sza::util::TimeVal& mjd);

	// Version for fixed sources

	void sourcePositionTrack(sza::util::TimeVal& mjd);
	
	/**
	 * Write a given pmac command into the pmac
	 * dual-port-ram. Before invoking this function, the caller
	 * must call PmacBoard::isBusy() to see whether the pmac is
	 * ready for a new command.
	 *
	 * @param mode    PmacMode         The command-type to send.
	 * @param current AxisPositions *  The current position of the 
	 *                                 telescope, as readPosition().
	 * @throws Exception
	 */
	void pmacNewPosition(sza::util::PmacMode::Mode mode, 
			     AxisPositions *current);
	
	/**
	 * Prototype the functions that implement the various stages
	 * of pointing calculations
	 */
	void finalizePointing(double pmra, double pmdec, 
			      PointingCorrections *f);

	// Version for fixed sources

	void finalizePointing();
	
	/**
	 * Register the receipt of a control-program command that
	 * needs to be acknowledged when has taken effect. Also mark
	 * the tracking parameters as modified.
	 *
	 * Input:
	 *  seq   unsigned    The sequence number assigned to the command by
	 *                    the control program.
	 */
	void registerRequest(unsigned seq);

	void registerCarmaRequest(TrackerMsg* msg);
	void sendCarmaSeqNoMsg(bool success);
	
	/**
	 * Send a message back to the control program that the Pmac
	 * has completed the last request.
	 */
	void sendPmacDoneMessage(unsigned seq);
	/**
	 * Send a message that the source has set.
	 */
	void sendSourceSetMessage(unsigned seq);
	
	//............................................................
	// Methods which are called in response to messages from the
	// Drive Task
	//............................................................
	
	/**
	 * Respond to a 1-second tick from the time-code reader ISR by
	 * recording the new clock offset. If a resynchronization of
	 * the clock has been requested, also check the time against
	 * the time-code reader and correct it if necessary.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void addTick(TrackerMsg* msg);
	
	/**
	 * Calibrate the collimation of the telescope.
	 *
	 * @throws Exception
	 */	
	void calCollimation(TrackerMsg* msg);
	
	/**
	 * Record new encoder offsets and multipliers.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void calEncoders(TrackerMsg* msg);
	
	/**
	 * Calibrate the gravitational flexure of the telescope.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void calFlexure(TrackerMsg* msg);
	
	/**
	 * Calibrate the axis tilts of the telescope.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void calTilts(TrackerMsg* msg);
	
	/**
	 * Update the year. Note that the time code reader doesn't supply the
	 * year, so the year has to be provided by the control program.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void changeYear(TrackerMsg* msg);
	
	/**
	 * Update the equation-of-the-equinoxes interpolator.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 *
	 * @throws Exception
	 */
	void extendEqnEqx(TrackerMsg* msg);
	
	/**
	 * Extend the ephemeris of the current source.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void extendTrack(TrackerMsg* msg);
	
	/**
	 * Update the UT1-UTC interpolator.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 *
	 * @throws Exception
	 */
	void extendUt1Utc(TrackerMsg* msg);
	
	/**
	 * Arrange to halt the telescope. If the pmac new_position flag is set
	 * this will be done immediately, otherwise it will be postponed to
	 * the next 1-second tick.
	 *
	 * @param msg TrackerMsg * If the halt command was received
	 * from the control program then this must contain a
	 * transaction sequence number. Otherwise send NULL.
	 */
	void haltTelescope(TrackerMsg* msg);
	
	/**
	 * Update the local and system-wide site-location parameters.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void locateSite(TrackerMsg* msg);
	
	/**
	 * Update the antenna-specific offset
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void locateAntenna(TrackerMsg* msg);

	/**
	 * Arrange to reboot the pmac. If the pmac new_position flag is set
	 * this will be done immediately, otherwise it will be postponed to
	 * the next 1-second tick.
	 *
	 * @param msg TrackerMsg * If the halt command was received
	 * from the control program then this must contain a
	 * transaction sequence number. Otherwise send NULL.
	 */
	void rebootPmac(TrackerMsg* msg);
	
	/**
	 * Record new encoder limits.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void recordEncoderLimits(TrackerMsg* msg);
	
	/**
	 * Record the specified angle as the deck angle at which the
	 * vertical direction on the TV corresponds to the local
	 * vertical of the telescope.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 *
	 * @throws Exception
	 */	
	void setTvAngle(TrackerMsg* msg);
	
	/**
	 * Select between the optical and radio pointing models.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void selectModel(TrackerMsg* msg);
	
	/**
	 * Install new encoder offsets.
	 * 
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */	
	void setEncoderZero(TrackerMsg* msg);
	
	/**
	 * Adjust the tracking offsets of specified drive axes, and round them
	 * into the range -pi..pi.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */	
	void setOffset(TrackerMsg* msg);
	
	/**
	 * Install new slew rates.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */	
	void setSlewRate(TrackerMsg* msg);
	
	/**
	 * Arrange to slew the telescope to a given az,el,dk coordinate.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */	
	void slewTelescope(TrackerMsg* msg);
	
	/**
	 * Respond to an unflag-board request for the pmac boards.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 *
	 * @throws Exception
	 */	
	void unflagBoard(TrackerMsg* msg);
	
	/**
	 * Record new refraction coefficients received from the weather
	 * station task.
	 *
	 * @param msg TrackerMsg* The message received on the
	 * AntennaDrive message queue.
	 */
	void updateRefraction(TrackerMsg* msg);
	void updateRefraction();
	
	/**
	 * Respond to a message to read out the pmac.
	 */
	void strobePmac();
	
	/**
	 * Attempt to connect to the pmac.
	 */
	void connectPmac();
	
	/**
	 * A function called when an error occurs talking to the pmac.
	 */
	void disconnectPmac();

	/**
	 * Return true if we have enough information to roughly point
	 * the telescope.
	 */
	bool okToPoint();

	void setWrapMode(WrapMode::Mode wrapMode);

      }; // End class Tracker
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif
