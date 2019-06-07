#include "carma/szaarrayutils/astrom.h"

#include "carma/antenna/sza/antenna/control/GpsBoard.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"

using namespace std;
using namespace std;
using namespace sza::util;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor for GPS board class.  This calls the base-class
 * constructor with the board as the first argument.
 */
GpsBoard::GpsBoard(SzaShare* share, string name) : Board(share, name)
{
  // Initialize these to NULL, in case we want to test for failure
  // later on

  config_   = 0;
  freeze_   = 0;
  release_  = 0;
  utc_reg0_ = 0;
  utc_regs_ = 0;
  period_   = 0;
  intset_   = 0;
  intvec_   = 0;
  start_    = 0;
  stop_     = 0;

  // Now look up useful registers on the time-code-reader board.

  config_   = findReg("config");
  freeze_   = findReg("freeze");
  release_  = findReg("release");
  utc_reg0_ = findReg("time_reg0");
  utc_regs_ = findReg("time_regs");
  period_   = findReg("period");
  intset_   = findReg("intset");
  intvec_   = findReg("intvec");
  start_    = findReg("start");
  stop_     = findReg("stop");

  // And initialize

  reset();
}

/**.......................................................................
 * Called to intialize.
 */
void GpsBoard::reset()
{
  reported_error_       = false;
  reported_unlock_      = false;
  reported_badday_      = false;
  reported_unreachable_ = false;
}

/**.......................................................................
 * Activate the time-code-reader board.
 */
void GpsBoard::connect(int year)
{
  // Set up to receive interrupts from the time-code-reader's external
  // event input. This will be connected to the 1PPS output.

  try {

    // Disable first.

    disconnect();
#ifdef DASI
    unsigned intvec[4] = {GPS_ISR_VECTOR, 0, 0, 0};
    unsigned intcnf[4] = {GPS_ISR_LEVEL | 16U, 0, 0, 0};
    
    writeReg(intvec_, 0, 4, intvec);
    writeReg(intset_, 0, 4, intcnf);
#endif
    // Set the period of the generator output to 8KHz.
    // This equates to a period of 125us (0x7d).

    unsigned period[2] = {0x7d ,0};  // LSB, MSB 
    writeReg(period_, 0, 2, period);

    // Set up the general configuration registers.

    unsigned config[4];

    // Select Synchronized Generator Mode (bit 0), Leap-year-enable if
    // we are in a leap year (bit 6), and enable interrupts (bit 7).

    config[0] = 2U | (isLeapYear(year) ? (1U<<6U) : 0) | (1U<<7U);

    // Arrange for the generator output period to be loaded (bit 4),
    // and for the output rate to be synchronized with the 1PPS signal (bit 5).

    config[1] = (1U<<4U) | (1U<<5U);
    config[2] = 0;         // Defaults 
    config[3] = (1U<<5U);  // Use IRIG-B input 
    writeReg(config_, 0, 4, config);
  } catch(...) {

    // But will ignore errors   

    failedConnect();
    return;
  }

  // Enable the receipt of interrupts from the time-code-reader board.

#ifdef DASI
  sysIntEnable(GPS_ISR_LEVEL);
#endif
}

/*.......................................................................
 * This is a private error return function of GpsBoard::connect().
 */
void GpsBoard::failedConnect()
{
  try {
    disconnect();
  } catch(...) {
  }
}

/**.......................................................................
 * Shutdown the time-code-reader board.
 */
void GpsBoard::disconnect()
{
  /*
   * Disable interrupts and stop the generator.  We ignore an
   * exceptional return from verifyBoard()
   */
  try {
    if(isReachable()) {
      unsigned config = (1U<<2U);
      writeReg(config_, 0, 1, &config);
    } 
  } catch (Exception& err) {
#ifdef DASI
    sysIntDisable(GPS_ISR_LEVEL);
#endif
    throw(err);
  };
#ifdef DASI
  sysIntDisable(GPS_ISR_LEVEL);
#endif
}

/**.......................................................................
 * Read the current date and time from the time code reader card, and
 * return it as a Modified Julian Date. Be aware that this function is
 * expensive and that it can fail.
 *
 * Input:
 *
 *  year        int      The current year.
 * 
 * Input/Output:
 *
 *  mjd         long *   On output the Modified Julian Day 
 *                       number will be assigned to *mjd.
 *  ms          long *   On output the time of day (milli-
 *                       seconds) will be assigned to *ms.
 */
void GpsBoard::readTime(int year, long *mjd, long *ms)
{
  unsigned dummy;   // A register value to be discarded 
  unsigned bcd[4];  // The four bcd time-readout registers 
  int nsec;         // The number of nanoseconds after second. (0-1000000000) 
  int sec;          // The number of seconds after the minute. (0-60) 
  int min;          // The number of minutes after the hour. (0-59) 
  int hour;         // The number of hours since midnight. (0-23) 
  int dayno;        // The day number in the year (1-366) 
  int waserr = 0;   // True if an error occurs while the database is locked 

  // Do nothing unless the time code reader is marked as reachable.

  if(!isReachable()) {
    if(!reported_unreachable_) {
      reported_unreachable_ = true;
      throw Error("GpsBoard::readTime: Gps board is unreachable.\n");
    }
    return;
  }

  // Temporarily prevent other tasks from accessing the time-code reader board.

  share_->grabRegs(SzaShare::WAIT_FOREVER);

  // Freeze the time readout of the time-code-reader, read its time
  // readout registers, then unfreeze readout.

  try {  
    readReg(freeze_,   0, 1, &dummy);
    readReg(utc_reg0_, 0, 1, &bcd[0]);
    readReg(utc_regs_, 0, 3, &bcd[1]);
    readReg(release_,  0, 1, &dummy);
  } catch (...) {

    // Relinquish exclusive access to the time-code-reader.
    // ungrabRegs() might throw an exception too, but at this point,
    // we don't care about which error we propagate.

    share_->ungrabRegs();

    throw Error("GpsBoard::readTime: Error.\n");
  }

  // Relinquish exclusive access to the time-code-reader.

  share_->ungrabRegs();

  // Check for time-code reader errors. Report changes in the error
  // status, using flags to record the last status encountered.

  if((bcd[3] >> 12U) & 1U) {

    if(!reported_error_) 
      reported_error_ = true;

    throw Error("Time code reader reports error in IRIG-B signal.\n");

  } else if(reported_error_) {

    reported_error_ = false;
    ErrorDef(err, "Time code reader reports IRIG-B signal is now OK.\n");
  };

  // On power-up the GPS receiver sometimes starts with an illegal day
  // number of zero.

  if((bcd[3] & 0xfff) == 0) {
    if(!reported_badday_) 
      reported_badday_ = true;

    throw Error("Time code reader reports error in IRIG-B signal.\n");

  } else if(reported_badday_) {

    reported_badday_ = false;
    ErrorDef(err, "The GPS day-number is now back in range.\n");
  };

  // Warn about loss of lock, but continue to read the time from the
  // time-code reader.

  if(~(bcd[3] >> 13U) & 1U) {
    if(!reported_unlock_) {
      reported_unlock_ = true;
      ErrorDef(err, "Warning: The time code reader has lost phase lock.\n");
    };
  } else if(reported_unlock_) {
    reported_unlock_ = 0;
    ErrorDef(err, "The time code reader has recovered phase lock.\n");
  };

  // Convert from BCD components to the current date and time, in
  // Gregorian components.

  nsec = 1000U * ((bcd[0] & 0xf) + 10 *                      // x1 us 
		  (((bcd[0] >> 4U) & 0xf) + 10 *             // x10 us 
		   (((bcd[0] >> 8U) & 0xf) + 10 *            // x100 us 
		    (((bcd[0] >> 12U) & 0xf) + 10 *          // x1 ms 
		     ((bcd[1] & 0xf)          + 10 *         // x10 ms 
		      ((bcd[1] >> 4U)  & 0xf))))));          // x100 ms 
  sec = (((bcd[1] >> 8U) & 0xf) +                            // x1 sec 
	 (((bcd[1] >> 12U) & 0xf) * 10U));                   // x10 sec 
  min = ((bcd[2] & 0xf) +                                    // x1 min 
	 (((bcd[2] >> 4U) & 0xf) * 10U));                    // x10 min 
  hour= (((bcd[2] >> 8U) & 0xf) +                            // x1 hour 
	 (((bcd[2] >> 12U) & 0xf) * 10U));                   // x10 hour 

  // Get the day number within the current year.

  dayno = ((bcd[3] & 0xf) + 10 *                             // x1 day 
	   (((bcd[3] >> 4U) & 0xf) + 10U *                   // x10 day 
	    ((bcd[3] >> 8U) & 0xf)));                        // x100 day 

  // Compute the returned Modified Julian Day number and the time of
  // day.
  
  *mjd = sza::array::mjd_of_year(year) + dayno - 1;
  *ms = ((hour * 60 + min) * 60 + sec) * 1000 + nsec / 1000000;
}

/**.......................................................................
 * Arm the gps time-code reader to output a start pulse at a given
 * time.
 *
 * Input:
 *  mjd        int     The Modified Julian Day number at which to
 *                     output the start pulse.
 *  sec        int     The number of seconds into the day at which to
 *                     output the start pulse.
 */
void GpsBoard::arm(int mjd, int sec)
{
  int year;        /* The current year */
  int dayno;       /* The day number within the target year */
  int hour, min;   /* Hours, minutes and seconds */
  unsigned bcd[8]; /* The start/stop times as an array of BCD */
                   /*  pairs (see below) */
  /*
   * Get the day-number in the year.
   */
  if(sza::array::mjd_utc_to_year_day(mjd, &year, &dayno)==1)
    throw Error("GpsBoard::arm: Error in mjd_utc_to_year_day().\n");
  /*
   * Get the time of day.
   */
  hour = sec / 3600;
  sec -= hour * 3600;
  min  = sec / 60;
  sec -= min * 60;
  /*
   * Translate the start date into BCD components.
   */
  bcd[0] = 0;                                       /* [x10 us]  [x1 us] */
  bcd[1] = 0;                                       /* [x1 ms]   [x100 us] */
  bcd[2] = 0;                                       /* [x100 ms] [x10 ms] */
  bcd[3] = ((sec/10) << 4U) + (sec%10);             /* [x10 sec] [x1 sec] */
  bcd[4] = ((min/10) << 4U) + (min%10);             /* [x10 min] [x1 min] */
  bcd[5] = ((hour/10) << 4U) + (hour%10);           /* [x10 hr]  [x1 hr] */
  bcd[6] = (((dayno%100)/10) << 4U) + (dayno%10);   /* [x10 day] [x1 day] */
  bcd[7] =  dayno/100;                              /*   N/A     [x100 day] */
  /*
   * Set both the start and stop times to the same time.
   */
  writeReg(start_, 0, 8, bcd);
  writeReg(stop_,  0, 8, bcd);
}

/**.......................................................................
 * Enable the leap-year flag of the GPS if the year is a leap year.
 */
void GpsBoard::enableLeapYear(int year)
{
  unsigned config;  // The value of the time-code-reader config register 

  // Read the current configuration from the GPS card
  readReg(config_, 0, 1, &config);

  // If this is a leap year, set the configuration flag accordingly
  if(isLeapYear(year))
    config |= (1U<<6U);
  else
    config &= ~(1U<<6U);

  // And write it back to the board
  writeReg(config_, 0, 1, &config);
}
/**
 * Return true if the passed year is a leap year
 */
bool GpsBoard::isLeapYear(int year)
{
  return sza::array::is_leap_year(year)==1;
}
