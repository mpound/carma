#ifndef TRACKERMSG_H
#define TRACKERMSG_H

/**
 * @file TrackerMsg.h
 * 
 * Tagged: Thu Nov 13 16:53:56 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szaarrayutils/szaregs.h" // SRC_LEN

#include "carma/szautil/Exception.h"
#include "carma/szautil/GenericTaskMsg.h"

#include "carma/szautil/Axis.h"
#include "carma/szautil/OffsetMsg.h"
#include "carma/szautil/PointingMode.h"
#include "carma/szautil/Rx.h"

#include "carma/antenna/sza/antenna/control/Collimation.h"
#include "carma/antenna/sza/antenna/control/Refraction.h"
#include "carma/antenna/sza/antenna/control/WrapMode.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      class TrackerMsg :
	public sza::util::GenericTaskMsg {
	
	public:
	
	/**
	 * Enumerate supported message types.
	 */
	enum MsgType {
	  COLLIMATION,
	  CONNECT_PMAC,      // Attempt to connect to the pmac.
	  DISCONNECT_PMAC,   // Disconnect from the pmac.
	  ENCODER_CALS,
	  ENCODER_LIMITS,
	  ENCODER_ZEROS,
	  EXTEND_EQNEQX,
	  EXTEND_UT1UTC,
	  FLAG_BOARD,
	  FLEXURE,
	  HALT,
	  LOCATION,
	  SITE,
	  OFFSET,
	  REBOOT_PMAC,
	  REFRACTION,
	  RESTART,
	  SELECT_MODEL,
	  SLEW,
	  SLEWRATE,
	  START_TIMER,         // Start 1pps timer
	  STOP_TIMER,          // Stop 1pps timer
	  STROBE_PMAC,
	  TICK,                // 1-pps tick has arrived
	  TILTS,
	  TRACK,
	  TV_ANGLE,
	  YEAR,
	  WEATHER,
	  RX,
	  WRAPMODE
	};
	
	/**
	 * A type for this message
	 */
	MsgType type;
	
	/**
	 * A message body.
	 */
	union {
	  
	  /**------------------------------------------------------------
	   * Reboot the Pmac
	   */
	  struct {
	    unsigned long seq;
	  } rebootPmac;
	  
	  /**------------------------------------------------------------
	   * Slew the telescope to a demanded position
	   */
	  struct {
	    unsigned long seq;
	    sza::util::Axis::Type axes;
	    char source[SRC_LEN];
	    double az;
	    double el;
	    double pa;
	  } slew;
	  
	  /**------------------------------------------------------------
	   * Halt the telescope immediately
	   */
	  struct {
	    unsigned long seq;
	  } halt;
	  
	  /**------------------------------------------------------------
	   * A 1-pps tick
	   */
	  unsigned long tick;
	  
	  /**------------------------------------------------------------
	   * Extend the track of a source
	   */
	  struct {
	    signed int seq;
	    char source[SRC_LEN];
	    double mjd;  // The Terrestrial Time at which ra,dec are
			 // valid, expressed as a Modified Julian Day
			 // number
	    double ra;   // The desired apparent Right Ascension
			 // (0...360 degrees in radians)
	    double dec;  // The desired apparent Declination
			 // (-180..180 degrees in radians)
	    double dist; // The distance to the source if it is near
			 // enough for parallax to be
			 // significant. Specify the distance in
			 // AU. Send 0 for distant sources.
	    WrapMode::Mode wrapMode;
	  } track;
	  
	  /**------------------------------------------------------------
	   * Adjust the tracking offsets
	   */
	  struct {
	    unsigned long seq; // The sequence number of this request
	    sza::util::OffsetMsg offset;  // A message containg the
					  // offset (see OffsetMsg.h)
	  } offset;
	  
	  /**------------------------------------------------------------
	   * Set the PA angle at which the vertical direction on the TV
	   * monitor of the optical telescope matches the direction of
	   * increasing topocentric elevation
	   */
	  struct {
	    double angle; // The PA angle at which the camera image is
			  // upright (radians)
	  } tvAngle;
	  
	  /**------------------------------------------------------------
	   * Update the refraction correction
	   *
	   */
	  struct {
	    sza::util::PointingMode::Type mode; // Which refraction mode are we
	    // updating? (radio | optical)
	    double a; // The A term of the refraction correction
	    double b; // The B term of the refraction correction
	  } refraction;
	  
	  /**------------------------------------------------------------
	   * Commands used to send occasional updates of variable earth
	   * orientation parameters.
	   *
	   * For each command the control system retains a table of
	   * the 3 most recently received updates. These three values
	   * are quadratically interpolated to yield orientation
	   * parameters for the current time.  On connection to the
	   * control system, the control program is expected to send
	   * values for the start of the current day, the start of the
	   * following day and the start of the day after
	   * that. Thereafter, at the start of each new day, it should
	   * send parameters for a time two days in the future.
	   *
	   * On startup of the control system, requests for ut1utc and
	   * eqeqx will return zero. On receipt of the first
	   * earth-orientation command, requests for orientation
	   * parameters will return the received values.  On the
	   * receipt of the second, requesters will receive a linear
	   * interpolation of the parameters. On receipt of the third
	   * and subsequent commands, requesters will receive
	   * quadratically interpolated values using the parameters of
	   * the three most recently received commands.
	   */
	  /**------------------------------------------------------------
	   * The UT1-UTC correction
	   */
	  struct {
	    double mjd;     // The UTC to which this command refers as
			    // a Modified Julian Day number
	    double ut1utc;  // The value of ut1 - utc (seconds) 
	  } extendUt1Utc;
	  
	  /**------------------------------------------------------------
	   * The equation of the equinoxes.
	   */
	  struct {
	    double mjd;     // The Terrestrial Time to which this
			    // command refers, as a Modified Julian
			    // day
	    double eqneqx;  // The equation of the equinoxes (radians)
	  } extendEqnEqx;
	  
	  /**------------------------------------------------------------
	   * The slew_rate is used to set the slew speeds of each of
	   * the telescope axes. The speed is specified as a
	   * percentage of the maximum speed available.
	   */
	  struct {
	    unsigned long seq; // The tracker sequence number of this command 
	    sza::util::Axis::Type axes;   // A bitwise union of
					  // Axis::Type enumerated
					  // bits, used to specify
					  // which of the following
					  // axis rates are to be
					  // applied.
	    long az;           // Azimuth slew rate (0-100) 
	    long el;           // Elevation slew rate (0-100)
	    long dk;           // Deck slew rate (0-100)
	  } slewRate;
	  
	  /**------------------------------------------------------------
	   * Calibrate the axis tilts of the telescope.
	   */
	  struct {
	    unsigned long seq;  // The tracker sequence number of this command 
	    double ha;     // The hour-angle component of the
	    // azimuth-axis tilt (mas) 
	    double lat;    // The latitude component of the azimuth-axis
	    // tilt (mas) 
	    double el;     // The tilt of the elevation axis
	    // perpendicular to the azimuth ring,
	    // measured clockwise around the direction of
	    // the azimuth std::vector (mas)
	  } tilts;
	  
	  /**------------------------------------------------------------
	   * Set the gravitational flexure of the telescope.
	   */
	  struct {
	    unsigned long seq;  // The tracker sequence number of this
				// command
	    sza::util::PointingMode::Type mode; // A PointingMode
						// enumeration
						// (optical|radio)

	    double sFlexure;    // Gravitational flexure (radians per
				// sine elevation)
	    double cFlexure;    // Gravitational flexure (radians per
				// cosine elevation)
	  } flexure;
	  
	  /**------------------------------------------------------------
	   * Calibrate the collimation of the optical or radio axes.
	   */
	  struct {
	    unsigned long seq; // The tracker sequence number of this
			       // command
	    sza::util::PointingMode::Type mode; // Which collimation?
						// (optical|radio)
	    double x;        // The magnitude of the horizonatl
			      // collimation offset
	    double y;        // The magnitude of the vertical
			      // collimation offset
	    sza::util::OffsetMsg::Mode addMode; // ADD or SET?

	  } collimation;
	  
	  /**------------------------------------------------------------
	   * Set the calibation factors of the telescope encoders. 
	   */
	  struct {
	    unsigned long seq; // The sequence number of this command
	    long az;
	    long el;
	    long dk;
	  } encoderCountsPerTurn;

	  /**------------------------------------------------------------
	   * Tell the drive task what the limits on encoder values
	   * are.
	   */
	  struct {
	    unsigned long seq; // The tracker sequence number of this command 
	    long az_min;      // The lower azimuth limit (encoder counts) 
	    long az_max;      // The upper azimuth limit (encoder counts) 
	    long el_min;      // The lower elevation limit (encoder counts)
	    long el_max;      // The upper elevation limit (encoder counts)
	    long pa_min;      // The lower deck limit (encoder counts)
	    long pa_max;      // The upper deck limit (encoder counts)
	  } encoderLimits;
	  
	  /**------------------------------------------------------------
	   * Set the zero points of the telescope encoders. The angles
	   * are measured relative to the position at which the
	   * encoders show zero counts.
	   */
	  struct {
	    unsigned long seq; // The sequence number of this command
	    double az;  // Azimuth encoder angle at zero azimuth,
			// measured in the direction of increasing
			// azimuth (radians)
	    double el;  // Elevation encoder angle at zero elevation,
			// measured in the direction of increasing
			// elevation (radians)
	    double dk;  // Deck encoder angle at the deck reference
			// position, measured clockwise when looking
			// towards the sky (radians)
	  } encoderZeros;
	  
	  /**------------------------------------------------------------
	   * Select between the optical and radio pointing models.
	   */
	  struct {
	    unsigned long seq;  // The tracker sequence number of this
	    // command
	    sza::util::PointingMode::Type mode; // A PointingMode enumeration 
	  } selectModel;
	  
	  /**------------------------------------------------------------
	   * Tell the control system what the current year is. This is
	   * necessary because the gps time-code reader doesn't supply
	   * year information.
	   */
	  struct {
	    short year;   // The current Gregorian year 
	  } year;
	  
	  /**------------------------------------------------------------
	   * The following command is used to inform the control system of the
	   * site of this antenna
	   */
	  struct {
	    double lon;  // The SZA longitude (east +ve) [-pi..pi] (radians)
	    double lat;  // The SZA latitude [-pi/2..pi/2] (radians)
	    double alt;  // The SZA altitude (meters) 
	  } site;
	  
	  /**------------------------------------------------------------
	   * The following command is used to inform the control system of the
	   * site of this antenna
	   */
	  struct {
	    double north; // The antenna offset N (meters)
	    double east;  // The antenna offset E (meters)
	    double up;    // The antenna offset Up (meters)
	  } location;

	  /**------------------------------------------------------------
	   * Flag a board.
	   */
	  struct {
	    unsigned short board; // The register map index of the
	    // board to un/flag
	    bool flag;            // True to flag, false to unflag
	  } flagBoard;
	  
	  struct {
	    double airTemperatureInK;
	    double relativeHumidity;
	    double pressureInMbar;
	  } weather;

	  struct {
	    sza::util::Rx::Id id;
	  } rx;

	  struct {
	    WrapMode::Mode wrapMode;
	  } wrapMode;

	} body;
	
	//------------------------------------------------------------
	// Methods for packing message to the tracker task.
	//
	// We explicitly initialize genericMsgType_ in each method,
	// since we cannot do this in a constructor, since objects
	// with explicit constructors apparently can't be union
	// members.
	//------------------------------------------------------------
	
	/**
	 * Pack a message to connect to the pmac.
	 */
	inline void packConnectPmacMsg() {
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  setCarmaSequenceNumber();
	  type = CONNECT_PMAC;
	}
	
	/**
	 * Pack a message to strobe the pmac.
	 */
	inline void packStrobePmacMsg() {
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  setCarmaSequenceNumber();
	  type = STROBE_PMAC;
	}
	
	/**
	 * Pack a message to disconnect from the pmac.
	 */
	inline void packDisconnectPmacMsg() {
	  genericMsgType_ = 
	    sza::util::GenericTaskMsg::TASK_SPECIFIC;
	  
	  setCarmaSequenceNumber();
	  type = DISCONNECT_PMAC;
	}
	
	inline void packRebootPmacMsg(unsigned long seq)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = REBOOT_PMAC;
	    body.rebootPmac.seq = seq;
	  }
	
	inline void packSlewMsg(unsigned long seq, 
				std::string source, 
				sza::util::Axis::Type axes, 
				double az, double el, double pa)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = SLEW;
	    body.slew.seq  = seq;
	    body.slew.axes = axes;
	    
	    if(source.size() > SRC_LEN)
	      ThrowError("source name is too long.\n");
	    
	    strncpy(body.slew.source, source.c_str(), SRC_LEN);
	    
	    body.slew.az = az;
	    body.slew.el = el;
	    body.slew.pa = pa;

	    setCarmaSequenceNumber();
	  }
	
	inline void packHaltMsg(unsigned long seq)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type = HALT;
	    body.halt.seq = seq;

	    setCarmaSequenceNumber();
	  }
	
	inline void packTrackMsg(signed long seq, 
				 std::string source, 
				 double mjd, double ra,  
				 double dec, double dist,
				 WrapMode::Mode wrapMode)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = TRACK;
	    body.track.seq = seq;
	    
	    if(source.size() > SRC_LEN)
	      ThrowError("source name is too long.\n");
	    
	    strncpy(body.track.source, source.c_str(), SRC_LEN);
	    
	    body.track.mjd      = mjd;
	    body.track.ra       = ra;
	    body.track.dec      = dec;
	    body.track.dist     = dist;
	    body.track.wrapMode = wrapMode;
	  }
	
	inline void packOffsetMsg(unsigned long seq, 
				  sza::util::OffsetMsg offset)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = OFFSET;
	    body.offset.seq    = seq;
	    body.offset.offset = offset;
	  }
	
	inline void packTvAngleMsg(double angle)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type               = TV_ANGLE;
	    body.tvAngle.angle = angle;
	  }
	
	inline void packEncoderZerosMsg(unsigned long seq, 
					double az, double el, double dk)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = ENCODER_ZEROS;

	    body.encoderZeros.seq = seq;
	    body.encoderZeros.az  = az;
	    body.encoderZeros.el  = el;
	    body.encoderZeros.dk  = dk;
	  }
	
	inline void packEncoderCountsPerTurnMsg(unsigned long seq, 
						long az, long el, long dk)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = ENCODER_CALS;
	    body.encoderCountsPerTurn.seq = seq;
	    body.encoderCountsPerTurn.az  = az;
	    body.encoderCountsPerTurn.el  = el;
	    body.encoderCountsPerTurn.dk  = dk;
	  }
	
	inline void packRefractionMsg(sza::util::PointingMode::Type mode, double a, 
				      double b)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = REFRACTION;
	    body.refraction.mode = mode;
	    body.refraction.a    = a;
	    body.refraction.b    = b;
	  }
	
	inline void packRestartMsg()
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = RESTART;
	  }
	
	inline void packExtendUt1UtcMsg(double mjd, double ut1utc)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = EXTEND_UT1UTC;
	    body.extendUt1Utc.mjd    = mjd;
	    body.extendUt1Utc.ut1utc = ut1utc;
	  }
	
	inline void packExtendEqnEqxMsg(double mjd, double eqneqx)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = EXTEND_EQNEQX;
	    body.extendEqnEqx.mjd    = mjd;
	    body.extendEqnEqx.eqneqx = eqneqx;
	  }
	
	inline void packSlewRateMsg(unsigned long seq, 
				    sza::util::Axis::Type axes, 
				    long az, long el, long dk)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = SLEWRATE;
	    body.slewRate.seq  = seq;
	    body.slewRate.axes = axes;
	    body.slewRate.az   = az;
	    body.slewRate.el   = el;
	    body.slewRate.dk   = dk;
	  }
	
	inline void packTickMsg(unsigned long tick)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = TICK;
	    body.tick = tick;
	  }
	
	inline void packTiltsMsg(unsigned long seq, double ha, double lat, 
				 double el)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = TILTS;
	    body.tilts.seq = seq;
	    body.tilts.ha  = ha;
	    body.tilts.lat = lat;
	    body.tilts.el  = el;
	  }
	
	inline void packFlexureMsg(unsigned long seq, 
				   sza::util::PointingMode::Type mode, 
				   double sFlexure,
				   double cFlexure)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = FLEXURE;
	    body.flexure.seq     = seq;
	    body.flexure.mode    = mode;
	    body.flexure.sFlexure = sFlexure;
	    body.flexure.cFlexure = cFlexure;
	  }
	
	inline void packCollimationMsg(unsigned long seq, 
				       sza::util::PointingMode::Type mode,
				       double x, double y,
				       sza::util::OffsetMsg::Mode addMode)

	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = COLLIMATION;
	    body.collimation.seq     = seq;
	    body.collimation.mode    = mode;
	    body.collimation.x       = x;
	    body.collimation.y       = y;
	    body.collimation.addMode = addMode;
	  }
	
	inline void packEncoderLimitsMsg(unsigned long seq, 
					 long az_min, long az_max, 
					 long el_min, long el_max, 
					 long pa_min, long pa_max)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = ENCODER_LIMITS;
	    body.encoderLimits.seq    = seq;
	    body.encoderLimits.az_min = az_min;
	    body.encoderLimits.az_max = az_max;
	    body.encoderLimits.el_min = el_min;
	    body.encoderLimits.el_max = el_max;
	    body.encoderLimits.pa_min = pa_min;
	    body.encoderLimits.pa_max = pa_max;
	  }
	
	inline void packSelectModelMsg(unsigned long seq, 
				       sza::util::PointingMode::Type mode)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = SELECT_MODEL;
	    body.selectModel.seq  = seq;
	    body.selectModel.mode = mode;
	  }
	
	inline void packYearMsg(short year)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = YEAR;
	    body.year.year = year;
	  }
	
	inline void packSiteMsg(double lon, double lat, double alt)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = SITE;
	    body.site.lon = lon;
	    body.site.lat = lat;
	    body.site.alt = alt;
	  }
	
	inline void packLocationMsg(double north, double east, double up)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = LOCATION;
	    body.location.north = north;
	    body.location.east = east;
	    body.location.up = up;
	  }
	
	inline void packFlagBoardMsg(unsigned short board, bool flag)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = FLAG_BOARD;
	    body.flagBoard.board = board;
	    body.flagBoard.flag  = flag;
	  }
	
	/**
	 * A method for packing a message to flag an antenna as
	 * un/reachable
	 */
	inline void packWeatherMsg(double airTempInK, double relHumidity, double pressureInMbar) 
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = WEATHER;
	    
	    body.weather.airTemperatureInK = airTempInK;
	    body.weather.relativeHumidity  = relHumidity;
	    body.weather.pressureInMbar    = pressureInMbar;
	  }

	/**
	 * A method for packing a message to flag an antenna as
	 * un/reachable
	 */
	inline void packRxMsg(sza::util::Rx::Id rxId)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = RX;
	    
	    body.rx.id = rxId;
	  }

	inline void packWrapModeMsg(WrapMode::Mode wrapMode)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = WRAPMODE;
	    
	    body.wrapMode.wrapMode = wrapMode;
	  }
	
	/**
	 * Allows cout << TrackerMsg
	 */
	friend std::ostream& operator<<(std::ostream& os, TrackerMsg* msg);

      }; // End class TrackerMsg
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
