// -*- c++ -*-
/**
 *
 * @file   
 * Common time functions. Also support for time
 * of half-second frames.
 *
 * @author Original: Steve Scott  
 * @reviewer Original: Colby Kraybill, Peter Teuben
 * @inspector Original: Peter Teuben
 *
 * $Id: Time.h,v 1.46 2012/07/10 16:16:54 abeard Exp $
 * $CarmaCopyright$
 *
 */


#ifndef CARMA_UTIL_TIME_H
#define CARMA_UTIL_TIME_H

#include <iosfwd>
#include <string>
#include <ctime>
#include "carma/util/types.h"

struct timeval;

namespace carma {
  /**
     Utility classes
  */
  namespace util {

    /**
     * This class deals primarily with Frames and Times.
     * Half-second Frames are used for monitor and data collection and are
     * aligned with absolute time. The time of a Frame refers to the start time 
     * of the data contained in the frame. Frames are counted as integers 
     * starting from 00:00:00 UTC, Jan 1, 2000.
     * Times are expressed as doubles in units of days with a reference system  
     * of Modified Julian Daynumber (MJD), which is aligned with UTC.
     * When any of the "get" methods are called without an explicit time or
     * frame argument, the current time is gotten with the accuracy determined
     * by the OS.
     *
     * Note: <BR>
     * 1) Frames are integers and our Time is a double. As with any integer/real
     * conversion, care *must* be taken when converting. There are methods for
     * converting with rounding and with truncation. The programmer must make
     * an intelligent choice based on the circumstances of the conversion.
     * 2) Frames, along with civil time, have a discontinuity at leap seconds.
     * When these events occur (approx every year and half), the suggested 
     * action is to stop observing shortly before the leap second, have a 
     * celebratory drink to the God of Time, reset your clock if necessary, 
     * and then resume observing. <BR>
     * 3) The string output of time is essential for debugging this class but 
     * can be used whenever the format is suitable. It does not include the 
     * century and is not Y2K compliant, a defect that is accepted in exchange 
     * for brevity. The FITS formatted time and date strings are Y2K compliant.
     * The FITS format is also known as ISO 8601. 
     * Formatted time output is a broad topic and this class does not attempt
     * to provide a general solution. If you develop a new format you are
     * encouraged to incorporate it into this class.If a number of new formats 
     * are developed simultaneously, then a new class (FormattedTime?) 
     * might be useful.
     * 4) The abbreviations "MJD" and "FITS" will be used in the code as a 
     * commonly accepted term in our field.
     * 5) This class contains no state. 
     *
     * @todo Write method to interpret date/time string back to an MJD 
     * (see STL time_get).
     *
     */
    class Time 
    {
    public:
      /**
       * enumeration for defining timezones
       */
      typedef enum {
        LOCAL =   -1, 
        GMT   =    0, 
        UTC   =  GMT,
        ADT   =    3,
        AST   =    4,
        EDT   =  AST,
        EST   =    5,
        CDT   =  EST,
        CST   =    6,
        MDT   =  CST,
        MST   =    7,
        PDT   =  MST,
        PST   =    8,
        AKDT  =  PST,
        AKST  =    9,
        HADT  = AKST,
        HAST  =   10      
      } TimeZone;

      /**
       ** Default constructor
       */
      Time();

      /**
       *  Create a Time object with given mjd
       */
      Time(double mjd);

      /**
       ** Destructor
       */
      virtual ~Time();

      /**
       *  Return stored MJD
       */
      double getStoredMJD() const;

      /**
       ** Get current MJD
       ** @return current MJD
       */
      static double MJD() ;

      /**
       ** Get MJD for a given frame count.
       ** Note that is a real representation of a linear conversion 
       ** of an integer. It will be very close, but subject to the limitations
       ** of real numbers.
       ** @param frame count to convert to MJD 
       ** @return an MJD for the input frame
       */
      static double MJD(frameType frame) ;       

      /**
       ** Get integral half-second frame that is closest to current time
       ** @return half-second frame count, since 2000, closest to current time
       ** @see computeCurrentFrame 
       */
      static frameType computeClosestFrame() ;

      /**
       ** Get closest integral half-second frame since 2000 to requested time
       ** @param mjd requested date/time as Modified Julian Daynumber
       ** @see computeCurrentFrame 
       */
      static frameType computeClosestFrame(double mjd) ;

      /**
       * Get the closest integral half-second frame since 2000 to
       * the input date string.  The timezone is
       * defaulted to be the local timezone
       * @param date The date string specified in a format identifiable
       * with a Unix date format string. 
       * @param format The date string in a Unix format. Default is format used
       * by syslog plus the year (%Y %b %2d %H:%M:%S).
       * @param tz The time zone, defined by the enum Time::TimeZone,
       that "date" is in (not the time zone that you're currently in)
       * @see <tt>date(1)</tt>, <tt>strptime(3)</tt>
       * @return the frame count closest to the input date, or zero if the
       * date was not parsable for some reason.
       */
      static frameType computeClosestFrame(
                                           const std::string& date, 
                                           const std::string& format="%Y %b %d %H:%M:%S",
                                           const TimeZone tz=LOCAL
                                           );

      /**
       ** Get integral half-second frame since 2000, rounded down.
       ** BEWARE: this truncation will probably not give what is desired if
       ** the MJD is just a smidgen below the frame (which is the case if it 
       ** originally came from a frame).
       ** @see computeClosestFrame 
       */
      static frameType computeCurrentFrame() ;

      /**
       ** Get integral half-second frame since 2000 for requested time, 
       ** rounded down. 
       ** BEWARE: this truncation will probably not give what is desired if
       ** the MJD is just a smidgen below the frame (which is the case if it 
       ** originally came from a frame).
       ** @param mjd requested date/time as Modified Julian Daynumber
       ** @see computeCurrentFrame 
       ** @see computeClosestFrame 
       */
      static frameType computeFrame(double mjd) ;

      /**
       ** Get time to next frame (half-second) with a delay after the frame.
       ** This uses the current time from the OS.
       ** @param delay in seconds after next half-second frame; default=0
       ** @return time until next frame + delay, in seconds 
       */
      static double computeTimeToNextFrame(double delay=0) ;

      /**
       ** Get time from now to a specific frame with a delay after the frame.
       ** @param frame number (number of half-seconds since 2000)
       ** @param delay in seconds after next half-second frame; default=0
       ** @return time until next frame + delay, in seconds 
       */
      static double computeTimeToFrame(frameType frame, double delay=0) ;

      /**
       ** Get time difference of current time to the delayed frame.
       ** @param frame number (number of half-seconds since J2000)
       ** @param delay in seconds after next half-second frame; default=0
       ** @return time difference (now -[frame + delay]), in seconds 
       */
      static double computeFrameTimeDiff(frameType frame, double delay=0) ;

      /**
       ** Compute the MJD of the Unix-style seconds since 1970.
       ** @param seconds the seconds since January 1, 1970, as from ctime().
       ** @return the MJD equivalent of seconds.
       */
      static double computeMJD (time_t seconds) ; 

      /**
       * Get the MJD given the input date string.  The timezone is
       * assumed to be the local timezone.
       * @param date The date string specified in a format identifiable
       * with a Unix date format string. 
       * @param format The date string in a Unix format. Default is format used
       * by syslog plus the year (%Y %b %2d %H:%M:%S).
       * @param tz The time zone, defined by the enum Time::TimeZone,
       * that "date" is in (not the time zone that you're currently in)
       * @see <tt>date(1)</tt>, <tt>strptime(3)</tt>
       * @return the MJD closest to the input date, or zero if the
       * date was not parsable for some reason.
       */
      static double computeMJD(
                               const std::string& date, 
                               const std::string& format="%Y %b %d %H:%M:%S",
                               const TimeZone tz=LOCAL
                               );

      // older version
      static double computeMJD1(
                               const std::string& date, 
                               const std::string& format="%Y %b %d %H:%M:%S",
                               const TimeZone tz=LOCAL
                               );

      /**
       ** Converts absolute time specified as a ::timespec to an 
       ** MJD. NOTE - timespec assumed to contain absolute time, not some
       ** difference.
       ** @param ts const struct timespec structure with abs time specified 
       **        in seconds and nanoseconds from the Epoch
       ** @return the MJD equivalent of abs time specified in timespec struct.
       */
      static double timespec2MJD (const struct timespec ts);
      
      /**
       ** Converts absolute time specified as a ::timeval to an MJD. 
       ** @param tv const struct timeval structure with abs time specified 
       **        in seconds and nanoseconds from the Epoch.
       ** @return the MJD equivalent of abs time specified in timespec struct.
       */
      static double timeval2MJD (const struct ::timeval & abstimeval);

      /**
       * Converts a frame number to a time_t in UTC
       */
      static time_t gettime_t(const frameType frame);

      /**
       ** Get time of day string for current time
       ** @param precision digits to the right of the decimal for seconds
       ** @return string containing the UT time of day (no date)
       **         format is hh:mm:ss.ss
       ** using a 24 hour clock for the hours.
       ** @throws ErrorException if precision < 0 or > 10
       */
      static std::string getTimeString(int precision=0) ;

      /**
       ** Get time of day string for specified time
       ** @param mjd specified time as a Modified Julian Daynumber
       ** @param precision digits to the right of the decimal for seconds
       ** @return string containing the UT time of day (no date)
       **         format is hh:mm:ss.ss
       ** using a 24 hour clock for the hours.
       ** @throws ErrorException if precision < 0 or > 10
       */
      static std::string getTimeString(double mjd, int precision=0) ;

      /**
       ** Get time of day for given frame as a string
       ** @param frame number (number of half-seconds since J2000)
       ** @param precision digits to the right of the decimal for seconds
       ** @return string containing the MJD time of day (no date)
       **         format is hh:mm:ss.ss
       ** using a 24 hour clock for the hours.
       ** @throws ErrorException if precision < 0 or > 10
       */
      static std::string getTimeString(frameType frame, int precision=0) ;

      /**
       ** Get date string for current time 
       ** @param dateFormat format that the date will be returned (see <tt>date(1)</tt>)
       ** @return string containing the date in specified dateFormat
       */                                                   
      static std::string getDateString(
            const std::string &dateFormat = "%d%b%y") ;

      /**
       ** Get date string for specified time
       ** @param mjd specified time as a Modified Julian Daynumber
       ** @param dateFormat format that the date will be returned (see <tt>date(1)</tt>)
       ** @return string containing the date in specified dateFormat
       */
      static std::string getDateString(double mjd,
            const std::string &dateFormat = "%d%b%y") ;

      /**
       ** Get date for given frame as a string
       ** @param frame number (number of half-seconds since J2000)
       ** @param dateFormat format that the date will be returned (see <tt>date(1)</tt>)
       ** @return string containing the date in specified dateFormat
       */
      static std::string getDateString(frameType frame,
             const std::string &dateFormat = "%d%b%y") ;

      /**
       ** Get date for given MJD day as a string
       ** @param imjd day number
       ** @param dateFormat format that the date will be returned (see <tt>date(1)</tt>)
       ** @return string containing the date in specified dateFormat
       */
      static std::string getDateString(int imjd,
            const std::string &dateFormat = "%d%b%y") ;

      /**
       ** Get date string for current time in FITS notation                    
       ** @return string containing the date, format yyyy-mm-dd, 
       ** e.g. 2003-03-27
       */                                                   
      static std::string getFITSdateString() ;

      /**
       ** Get date string for specified day
       ** @param imjd specified time as an integer Modified Julian Daynumber
       ** @return string containing the date, format yyyy-mm-dd
       */
      static std::string getFITSdateString(int imjd) ;

      /**
       ** Get date string for specified time
       ** @param mjd specified time as a Modified Julian Daynumber
       ** @return string containing the date, format yyyy-mm-dd
       */
      static std::string getFITSdateString(double mjd) ;

      /**
       ** Get date for given frame as a string
       ** @param frame number (number of half-seconds since J2000)
       ** @return string containing the date, format yyyy-mm-dd
       */
      static std::string getFITSdateString(frameType frame) ;

      /**
       ** Get date and time of day string for current time.
       ** @param dateFormat format that the date will be returned (see <tt>date(1)</tt>)
       ** @return string containing the MJD date and time
       **         format is what ever is specifyed in strftime format
       */
      static ::std::string getNonBuggyByDesignDateTimeString(
	  double mjd, const ::std::string &dateFormat );

      /**
       ** Get date and time of day string for current time.
       ** @param precision digits to the right of the decimal for seconds
       ** @param dateFormat format that the date will be returned (see <tt>date(1)</tt>)
       ** @return string containing the MJD date and time
       **         format is dateFormat hh:mm:ss.ss
       ** using a 24 hour clock for the hours.
       */
      static std::string getDateTimeString(int precision=0, 
            const std::string &dateFormat = "%d%b%y") ;

      /**
       ** Get date and time of day string for given frame.
       ** @param frame number (number of half-seconds since J2000)
       ** @param precision digits to the right of the decimal for seconds
       ** @param dateFormat format that the date will be returned (see <tt>date(1)</tt>)
       ** @return string containing the MJD date and time
       **         format is dateFormat hh:mm:ss.ss
       ** using a 24 hour clock for the hours.
       ** @throws ErrorException if precision < 0 or > 10
       */
      static std::string getDateTimeString(frameType frame, 
             int precision=0,
             const std::string &dateFormat = "%d%b%y") ;
 
      /**
       ** Get date and time of day string for specified time.
       ** @param mjd specified time as a Modified Julian Daynumber
       ** @param precision digits to the right of the decimal for seconds
       ** @param dateFormat format that the date will be returned (see <tt>date(1)</tt>)
       ** @return string containing the MJD date and time
       **         format is dateFormat hh:mm:ss.ss
       ** using a 24 hour clock for the hours.
       ** @throws ErrorException if precision < 0 or > 10
       */
      static std::string getDateTimeString(double mjd,
             int precision=0,
             const std::string &dateFormat = "%d%b%y") ;


      /**
       ** Get FITS style date and time of day string for current time.
       ** @param precision digits to the right of the decimal for seconds
       ** @return string containing the MJD date and time
       **         format is yyyy-dd-mmThh:mm:ss.ss
       ** using a 24 hour clock for the hours.
       */
      static std::string getFITSdateTimeString(int precision=0);

      /**
       ** Get FITS style date and time of day string for specified time.
       ** @param mjd specified time as a Modified Julian Daynumber
       ** @param precision digits to the right of the decimal for seconds
       ** @return string containing the MJD date and time
       **         format is yyyy-mm-ddThh:mm:ss.ss
       ** using a 24 hour clock for the hours.
       ** @throws ErrorException if precision < 0 or > 10
       */
      static std::string getFITSdateTimeString(double mjd, int precision=0) ;

      /**
       ** Get FITS style date and time of day string for given frame.
       ** @param frame number (number of half-seconds since J2000)
       ** @param precision digits to the right of the decimal for seconds
       ** @return string containing the MJD date and time
       **         format is yyyy-mm-ddThh:mm:ss.ss
       ** using a 24 hour clock for the hours.
       ** @throws ErrorException if precision < 0 or > 10
       */
      static std::string getFITSdateTimeString(frameType frame, 
            int precision=0) ;

      /**
       * Utility for obtaining GMT from a local time
       * Note:This is expensive. Use diffHrsFromGmt if performance
       * is an issue.
       */
      static struct tm getGmtFromLmt(const std::string &date,
				     const std::string &format,
				     const Time::TimeZone tz);

      /**
       * Utility for obtaining GMT from a local time with
       * faster performance. About a 3x speedup over getGmtFromLmt,
       * which adds up in a loop over thousands calls (e.g. filling
       * a FluxCatalog from FluxSource.cat)
       * @param myTime tm struct giving time in local time zone.
       * @param secondsSince1970 - return from a call to mktime(&myTime)
       * Note the call is not made from within this method for
       * performance reasons as methods which call this method
       * have to call mktime anyway.
       * @return the number of hours between local time and gmt
       */
      static int  diffHrsFromGmt(const struct tm myTime,
	                         const time_t secondsSince1970);



      /**
       **  The MJD for 1970.0
       **/
      static const double MJD1970 = 40587.0;

      /**
       **  The MJD for 2000.0
       **/
      static const double MJD2000 = 51544.0;

      /**
       **  The JD (Julian Data) for 2000.0
       **  as a convenience.
       **/
      static const double JD2000 = 2451544.5;

      /**
       **  The number of seconds per day
       **/
      static const double SECONDS_PER_DAY = 86400.0;

      /**
       ** The number of minutes per day
       **/ 
      static const double MINUTES_PER_DAY;

      /**
       ** The number of microseconds per day
       **/
      // value is initialized with the storage declaration in the .cc file
      static const double MILLISECONDS_PER_DAY;

      /**
       ** The number of milliseconds per day
       **/
      // value is initialized with the storage declaration in the .cc file
      static const double MICROSECONDS_PER_DAY;

      /**
       **  The number of half-second frames per day
       **/
      // value is initialized with the storage declaration in the .cc file
      static const double FRAMES_PER_DAY;

    private:
      /**
       ** Get current time as days since 1 Jan 2000, 00 UT
       */
      static double  get2kd() ;
      static int     computeIntegerMJD(double mjd, int precision=0) ;
      static double  roundMJD(double mjd, int precision=0) ;
      double mjd_;
    };

  } }  // End namespace carma::util  


/**
 * @relatesalso carma::util::Time
 * @brief Insert (i.e. output) a presentation of the current time from an
 *        instance of carma::util::Time into an output stream.
 *
 * The precision with which the seconds field is presented is determined by the
 * precision of the stream. Hence, usage like this:
 * @code
 * ::std::cout << ::std::setprecision( 2 ) << myTime << ::std::endl;
 * @endcode
 * would produce output that looks like this:
 * @code
 * 12:04:23.43
 * @endcode
 *
 * @param os
 *        The output stream to insert the presentation into.
 *
 * @param time
 *        The carma::util::Time instance to get the current time from.
 *
 * @return The @p os output stream parameter so that stream insertions
 *         can be chained in the usual C++ way (as shown in the example).
 */
::std::ostream& operator<<( ::std::ostream&           os,
                             const carma::util::Time& time );


#endif  // CARMA_UTIL_TIME_H
















