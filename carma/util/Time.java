// vim: set ts=4 sts=4 sw=4 et:
// $Id: Time.java,v 1.9 2013/07/17 20:27:55 iws Exp $

/**
 * Utilities for converting frameCounts to human-readable form
 */

package carma.util;

import java.lang.Math;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

public class Time {
    public static final double MJD1970 = 40587.0;
    public static final double MJD2000 = 51544.0;
    public static final double SECONDS_PER_DAY = 86400.0;
    public static final double FRAMES_PER_DAY = 2 * SECONDS_PER_DAY;
    public static final double SECONDS_PER_HOUR = 3600.0;
    public static final double FRAMES_PER_HOUR = 2 * SECONDS_PER_HOUR;
    public static final double FRAMES_PER_MIN = 2 * 60.0;

    public Time() {}

    // return a Date object with the number of milliseconds since 1970
    // for a given frame count
    public static Date getDate(int frameCount) {
        // milliseconds 1970 - 2000
        double millis = (MJD2000 - MJD1970) * SECONDS_PER_DAY * 1000.0;

        // add in milliseconds 2000 - framecount
        millis += frameCount * 500.0;

        return new Date(new Double(millis).longValue());
    }

    /**
     * get a String converting a frame count to a date and time
     * - default format for date is 2001-Jan-01
     *
     */
    public static String getDateTimeString(int frameCount) {
        SimpleDateFormat gmtDate = new SimpleDateFormat("yyyy-MMM-dd HH:mm:ss");
        gmtDate.setTimeZone(TimeZone.getTimeZone("GMT"));

        return gmtDate.format(getDate(frameCount));
    }

    public static String getDateTimeString(String frameCountString) {
        return getDateTimeString(Integer.parseInt(frameCountString));
    }

    /**
     * get a String converting a frame count to a date and time
     * - format follows conventions in Sun docs for SimpleDateFormat
     *
     */
    public static String getDateTimeString(int frameCount, String format) {
        SimpleDateFormat gmtDate = new SimpleDateFormat(format);
        gmtDate.setTimeZone(TimeZone.getTimeZone("GMT"));

        return gmtDate.format(getDate(frameCount));
    }

    public static String getDateTimeString(String frameCountString, String format) {
        return getDateTimeString(Integer.parseInt(frameCountString), format);
    }

    /**
     * get frame count from a date
     * - note: use this one only if you don't want to specify your own
     *   format (which should also mean you're using the
     *   getDateTimeString without a specified format
     */
    public static int getFrameCount(String date) {
        SimpleDateFormat formattedGmtDate = new SimpleDateFormat("yyyy-MMM-dd HH:mm:ss");
        formattedGmtDate.setTimeZone(TimeZone.getTimeZone("GMT"));

        Date gmtDate = formattedGmtDate.parse(date, new java.text.ParsePosition(0));

        // get number of milliseconds since 1970-Jan-01 00:00:00 GMT and convert it to frameCount
        return getFrameCount(gmtDate.getTime());
    }

    public static int getFrameCount(String date, String format) {
        SimpleDateFormat formattedGmtDate = new SimpleDateFormat(format);
        formattedGmtDate.setTimeZone(TimeZone.getTimeZone("GMT"));

        Date gmtDate = formattedGmtDate.parse(date, new java.text.ParsePosition(0));

        // get number of milliseconds since 1970-Jan-01 00:00:00 GMT and convert it to frameCount
        return getFrameCount(gmtDate.getTime());
    }

    /**
     * return frame count given number of milliseconds since 1970-Jan-01 00:00:00 GMT
     */
    public static int getFrameCount(long millisecondsSince1970) {
        return (int)(FRAMES_PER_DAY/SECONDS_PER_DAY*
                 (millisecondsSince1970/1000 -  (MJD2000 - MJD1970)*SECONDS_PER_DAY));
    }

    public static int computeFrame(double mjd) {
        // Add one unit of least precision to the significand (mantissa). This
        // makes sure that the mjd value is above the timestamp just in case
        // the data was rounded incorrectly. This MUST be done before you perform
        // any calculations with the value, or you will get the wrong result.
        mjd = Math.nextAfter(mjd, Double.MAX_VALUE);

        Double d = new Double(FRAMES_PER_DAY * (mjd - MJD2000));
        return d.intValue();
    }

    public static int computeClosestFrame(double mjd) {
        // See computeFrame, except this rounds to the nearest frame,
        // avoiding the pitfalls of real => integer conversion
        mjd = Math.nextAfter(mjd, Double.MAX_VALUE);

        return (int)Math.round(FRAMES_PER_DAY * (mjd - MJD2000));
    }
};
