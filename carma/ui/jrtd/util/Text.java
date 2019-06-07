
// $Id: Text.java,v 1.1 2004/11/01 22:13:55 paul Exp $

package carma.ui.jrtd.util;

import java.text.*;
import java.util.*;

/**
 * Formatted conversion of floating point numbers to a string,
 * with some similarities to the functionality of the C library printf().
 *
 * Unlike printf, the return string is guaranteed to never exceed
 * the requested width. If the width of the formatted string
 * would exceed the requested width, then a string of asterisks (*)
 * is returned.
 *
 * @author Steve Scott
 * $RCSfile: Text.java,v $
 * @version $Revision: 1.1 $ $Date: 2004/11/01 22:13:55 $
 *
 *
 */


public class Text {

    static private final double expConv = Math.log(10.0);

    /**
     * Represent a floating point number as a fixed length string.
     * The number is right justified in the string.
     * @param <code>x</code> The floating point number to represent.
     * @param <code>places</code> The number of places to the right
     *   of the decimal point (a value of zero suppresses the decimal
     *   point as well).
     * @param <code>width</code> Total length of the string.
     */
    public static String format(double x, int places, int length) {
        if ((places+2) > length)return stars(length);
        StringBuffer sb = new StringBuffer(format(x,places));
        if (sb.length() > length) return stars(length);
        for (int i=sb.length(); i<length; i++)sb.insert(0," ");
        return sb.toString();
    }

    /**
     * Represent a floating point number as a fixed length string.
     * The number is right justified in the string.
     * @param <code>x</code> The floating point number to represent.
     * @param <code>places</code> The number of places to the right
     *   of the decimal point (a value of zero suppresses the decimal
     *   point as well).
     * @param <code>length</code> Total width of the string.
     */
    public static String format(float x, int places, int length) {
        return format((double)x, places, length);
    }

    /**
     * Represent a floating point number as a fixed length string,
     * with the number of digits to the right of the decimal point
     * dependent on the magnitude of the number. Numbers greater than
     * or equal to 9.95 have no decimal point or decimal digits, 
     * while those less
     * than 9.95 have one digit to the right of the decimal point.
     * The number is right justified in the string.
     * @param <code>x</code> The floating point number to represent.
     * @param <code>length</code> Total width of the string.
     */
    public static String format10(double x, int length) {
        if (x >= 9.95)return format(x, 0, length);
        return format(x, 1, length);
    }

    /**
     * Represent a floating point number as a fixed length string,
     * with the number of digits to the right of the decimal point
     * dependent on the magnitude of the number. Numbers greater than
     * or equal to 9.95 have no decimal point or decimal digits, 
     * while those less
     * than 9.95 have one digit to the right of the decimal point.
     * The number is right justified in the string.
     * @param <code>x</code> The floating point number to represent.
     * @param <code>length</code> Total width of the string.
     */
    public static String format10(float x, int length) {
        return format10((double)x, length);
    }
    /**
     * Represent a floating point number as a three character long string,
     * with the number of digits to the right of the decimal point
     * dependent on the magnitude of the number. Numbers greater than
     * or equal to 9.95 have no decimal point or decimal digits, 
     * while those less
     * than 9.95 have one digit to the right of the decimal point.
     * The number is right justified in the string.
     * @param <code>x</code> The floating point number to represent.
     * @param <code>length</code> Total width of the string.
     */
    public static String format10(double x) {
        return format10(x, 3);
    }

    /**
     * Represent a floating point number as a three character long string,
     * with the number of digits to the right of the decimal point
     * dependent on the magnitude of the number. Numbers greater than
     * or equal to 9.95 have no decimal point or decimal digits, 
     * while those less
     * than 9.95 have one digit to the right of the decimal point.
     * The number is right justified in the string.
     * @param <code>x</code> The floating point number to represent.
     * @param <code>length</code> Total width of the string.
     */
    public static String format10(float x) {
        return format10((double)x, 3);
    }

    /**
     * Represent a floating point number as a string.
     * @param <code>x</code> The floating point number to represent.
     * @param <code>places</code> The number of places to the right
     *   of the decimal point (a value of zero suppresses the decimal
     *   point as well).
     */
    public static String format(double x, int places) {
        boolean negative = (x < 0.0);
        
        // Not a Number returns an empty string
        if (Double.isNaN(x))return "";
        
        if (negative)x = -x;
        if (places < 0)places = 0;
        else if (places > 20)places = 20;
        double scale = Math.exp(expConv*places);
        long   i     = Math.round(x*scale);
        StringBuffer sb = new StringBuffer();
        sb.append(i);
        if (places > 0) {
            int minlen = places+1;
            int len = sb.length();
            if (len < (minlen)){
                for (i=len; i<minlen; i++)
                    sb.insert(0, "0");
            }
            sb.insert(sb.length()-places, ".");
        }
        if (negative)sb.insert(0, "-");
        return sb.toString();
    }

    /**
     * Represent a floating point number as a string.
     * @param <code>x</code> The floating point number to represent.
     * @param <code>places</code> The number of places to the right
     *   of the decimal point (a value of zero suppresses the decimal
     *   point as well).
     */
    public static String format(float x, int places) {
        return format((double)x, places);
    }

    /**
     * Generate a string of asterisks.
     * @param <code>length</code> The length of the string of asterisks.
     */
    private static String stars(int length) {
        StringBuffer sb = new StringBuffer();
        for(int i=0; i<length; i++)sb.append("*");
        return sb.toString();
    }

	/**
	 *  Generate a 24hr Time string given an MJD as defined at OVRO
	 */
	public static String format24hr(double mjd) {
		// offset to epoch Jan. 1, 1970. in days
		double offset = 40587.0;

		// convert to milliseconds since above epoch.
		Date d = new Date((long)((mjd - offset) * 86400000.0)); 
		DateFormat df = DateFormat.getTimeInstance();
		((SimpleDateFormat) df).applyPattern("HH:mm:ss");
		return df.format(d);
	}

    //-------------------------------------------------------------------------
    public static void main(String[] argv) {
        System.out.println("//"+format10(-20.0,5)+"//");
        System.out.println("//"+format10(9.94d,5)+"//");
        System.out.println("//"+format10(9.96f,5)+"//");
        System.out.println("//"+format10(0.1,5)+"//");
        System.out.println("//"+format10(192)+"//");
        System.out.println("//"+format10(6192)+"//");
        System.out.println("//"+format10(Float.NaN)+"//");
        System.out.println("//"+format10(Double.NaN)+"//");
        System.out.println("//"+format10(11.0d,5)+"//");
    }



}

