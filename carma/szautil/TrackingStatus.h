#ifndef TRACKINGSTATUS_H
#define TRACKINGSTATUS_H

/**
 * @file TrackingStatus.h
 * 
 * Started: Wed Dec 17 19:50:04 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    /**
     * Class to enumerate the current telescope tracking status.
     */
    class TrackingStatus {
    public:
      
      enum Status {
	
	/**
	 * At least one critical tracker configuration parameter
	 * hasn't been received from the control program yet.
	 */
	LACKING,
	
	/**
	 * The last time received from the GPS time-code reader was
	 * invalid.
	 */
	TIME_ERROR,
	
	/**
	 * The tracker is responding to a change in conditions such
	 * as a new command or configuration parameter received from
	 * the control program, or an error condition such as a loss
	 * of time sync.
	 */
	UPDATING,
	
	/**
	 * The telescope is currently halted
	 */
	HALTED,
	
	/**
	 * The telescope is currently slewing
	 */
	SLEWING,
	
	/**
	 * The telescope is currently tracking a source 
	 */
	TRACKING,
	
	/**
	 * The telescope is trying to track a source that is below
	 * the lower elevation limit of the telescope, so the
	 * elevation is being held at this limit.
	 */
	TOO_LOW,
	
	/**
	 * The telescope is trying to track a source that is above
	 * the upper elevation limit of the telescope, so the
	 * elevation is being held at this limit.
	 */
	TOO_HIGH
      };

    }; // End class TrackingStatus
    
  }; // End namespace util
}; // End namespace sza

#endif
