
// $Id: MJD.java,v 1.1 2004/11/01 22:13:55 paul Exp $

//:MJD.java 
/**
 * Utility class for manipulating Modified Julian Date
 *
 * @author Steve Scott
 * @version $Revision: 1.1 $ $Date: 2004/11/01 22:13:55 $
 *
 * History:
 * 03Mar98 1.00  S. Scott  Original version 
 * 10Jul98 1.1   M. Pound modifications:
 *         - new public methods 
 *           getLST(double longitude) : returns LST at given longitude and
 *           getJulian() : returns Julian day 
 *           getUT() : returns UT in decimal hours
 *         - implement Externalizable and read/writeExternal methods for 
 *           custom serialization/externalization.
 *	   - added documentation
 */

package carma.ui.jrtd.util;

import java.util.*;
import java.io.*;
import java.text.*;

public class MJD extends Date implements Externalizable
{
	static private int timezoneOffset = Calendar.getInstance().ZONE_OFFSET;
	static private double timezoneOffsetDays = (timezoneOffset<12)?
		timezoneOffset/24.0:
		timezoneOffset/24.0 - 1;
	public double mjd;
	int    imjd;
	int    imicrodays;
	double dms;  // Millisecs since 1970

	/**
	 * This constructor gets the current data and stashes parts in structs
	 * @param none
	 */
	public MJD(){
		super();
		Long L = new Long(getTime());
		dms    = L.doubleValue();
		mjd    = 40587.0;      // MJD on Jan0 1970
		mjd   += dms/(1000.0*86400.0);
		imjd = (int)mjd;
		imicrodays = (int)(1e6*(mjd-imjd));
		//Util.spew("timezoneOffset:"+timezoneOffset+ "  days:"+timezoneOffsetDays);
	}

	/**
	 * And this one takes an input MJD and stashes it so we can work on it later
	 * @param mjd the modified julian day number
	 */
	public MJD(double mjd){
		super();
		setMJD(mjd);
	}

	/**
	 * print method for debugging 
	 * @param none
	 */
	public void print(){
		System.out.print("MJD:" 
				+Integer.toString(imjd)+
				"."+Integer.toString(imicrodays));
		System.out.println(" UT:" + getHMS());
	}

	/**
	 * get the modified julian day number
	 * @param none
	 * @return the mjd
	 */
	public double getMJD(){
		return mjd;
	}

	/**
	 * set the modified julian day number
	 * @param mjd The modified julian day number
	 */
	public void setMJD(double mjd) {
		this.mjd = mjd;
		imjd = (int)mjd;
		imicrodays = (int)(1e6*(mjd-imjd));
	}

	/**
	 * Get the Universal Time as an HH:MM:SS string
	 * @param none
	 * @return String UT in HMS format
	 */
	public String getHMS(){
		return getHMS(this.mjd);
	}
	/**
	 * Get the Universal Time as an HH:MM:SS string
	 * @param none
	 * @return String UT in HMS format
	 */
	public static String getHMS(double mjd){
		int imjd = (int)mjd;
		int imicrodays = (int)(1e6*(mjd-imjd));
		double fracday = 1e-6*imicrodays;
		int H = (int)(fracday*24);
		int M = (int)(fracday*24*60);   
		int S = (int)(fracday*24*3600);
		M %= 60;   S %= 60;
		String s = (H/10) +""+ (H%10) + ":" + 
			(M/10) +""+ (M%10) + ":" +
			(S/10) +""+ (S%10);
		return s;
	}

	/**
	 * Get the Universal Time as an HH:MM:SS.S string (includes the 
	 * tenth seconds)
	 * @param none
	 * @return String UT in HMS format
	 */
	// And with tenths of seconds
	public String getHMST(){
		return getHMST(this.mjd);
	}

	/**
	 * Get the Universal Time as an HH:MM:SS.S string (includes the 
	 * tenth seconds)
	 * @param mjd Use this modified julian day number
	 * @return String UT in HMS format
	 */
	// And with tenths of seconds
	public static String getHMST(double mjd){
		int imjd = (int)mjd;
		int imicrodays = (int)(1e6*(mjd-imjd));
		double fracday = 1e-6*imicrodays;
		int H = (int)(fracday*24);
		int M = (int)(fracday*24*60);   
		int S = (int)(fracday*24*3600);
		int T = (int)(fracday*24*36000);
		M %= 60;   S %= 60;
		String s = (H/10) +""+ (H%10) + ":" + 
			(M/10) +""+ (M%10) + ":" +
			(S/10) +""+ (S%10) + "." + (T%10);
		return s;
	}

	/** 
	 * compute the Julian day from the Modified Julian Day
	 * @param none
	 * @return the current Julian day
	 */
	public double getJulian() {
		return getJulian(this.mjd);
	}
	/** 
	 * compute the Julian day from the Modified Julian Day
	 * @param mjd Modified Julian day number
	 * @return the current Julian day
	 */
	public static double getJulian(double mjd) {
		return (mjd+2400000.5);
	}
	/** 
	 * compute the current Universal Time
	 * @param none
	 * @return the current UT Time in hours
	 */
	public double getUT() {
		return getUT(this.mjd);
	}

	/** 
	 * compute the current Universal Time
	 * @param mjd Modified Julian Day number
	 * @return the current UT Time in hours
	 */
	public static double getUT(double mjd) {
		double uthrs, jd;

		jd = getJulian(mjd);
		uthrs = 24.0 * ((jd - 0.5) - (int)(jd - 0.5));

		return(uthrs);
	}

	/**
	 * compute the Local Mean Sidereal Time for the given longitude
	 * @param longitude  the longitude in decimal degrees
	 * @return the current LMST Time in hours.
	 */
	public double getLST(double longitude) {
		return getLST(longitude, this.mjd);
	}

	/**
	 * compute the Local Mean Sidereal Time for the given longitude
	 * @param longitude  the longitude in decimal degrees
	 * @param mjd  the modified julian day
	 * @return the current LMST Time in hours.
	 */
	public static double getLST(double longitude, double mjd) {
		int m;
		double t0, gmst;
		double lst, jd, uthrs;

		jd = getJulian(mjd);
		uthrs = getUT(mjd);

		// Compute the Greenwich mean sidereal time from the 0UT Julian date.
		m = (int)(jd - 0.5);
		t0 = ((m + 0.5) - 2451545.0) / 36525.0;
		gmst = 24110.54841 + (8640184.812866 * t0) +
			(0.093104 * t0 * t0) - (6.2E-6 * t0 * t0 * t0);
		gmst /= 3600.0;               // Convert from seconds to hours.
		m = (int)(gmst / 24.0);       // Get rid of whole number of days.
		if (gmst < 0) m--;
		gmst -= (m  * 24);            // GMST (hours) at 0UT for this day.

		// Add the equivalent mean sidereal time interval from 0UT to now.
		lst = 1.002737909350795 + (5.9006E-11 * t0) - (5.9E-15 * t0 * t0);
		lst *= uthrs;
		lst += gmst;

		lst += (longitude / 15.0);    // Add in the east longitude offset.
		while (lst < 0.0) {
			lst += 24.0;
		}
		while (lst > 24.0) {
			lst -= 24.0;
		}

		return(lst);
	}
	/** 
	 * get the Local Mean Sidereal Time as a String
	 * @param longitude  the longitude in degrees
	 * @return String representation of LST as HH:MM:SS.S
	 */
	public String getLSTString(double longitude) {
		return getLSTString(longitude, this.mjd);
	}
	/** 
	 * get the Local Mean Sidereal Time as a String
	 * @param longitude  the longitude in degrees
	 * @param mjd Modified Julian Day number
	 * @return String representation of LST as HH:MM:SS.S
	 */
	public static String getLSTString(double longitude, double mjd) {

		NumberFormat number=NumberFormat.getNumberInstance();
		number.setMinimumFractionDigits(1);
		number.setMaximumFractionDigits(1);
		int hr, min; 
		double sec;
		double lst = getLST(longitude, mjd);
		String s;

		hr = (int)lst;
		min = (int)(60.0 * (lst - hr));
		sec = 3600.0*(lst - hr) - (60.0 * min) ;
		s = ((hr<10)?"0"+String.valueOf(hr):String.valueOf(hr)) + ":";
		s += ((min<10)?"0"+String.valueOf(min):String.valueOf(min)) + ":";
		s += (sec<10)?"0"+number.format(sec):number.format(sec);
		return(s);
	}

	// custom externalization - reading
	/**
	 * Read in the relevant fields of this class
	 * @param s any class which implements ObjectInput (e.g. ObjectInputStream)
	 */
	public void readExternal(ObjectInput s)
		throws ClassNotFoundException, IOException {
			timezoneOffset=s.readInt();
			timezoneOffsetDays=s.readDouble();
			imjd=s.readInt();
			imicrodays=s.readInt();
			dms=s.readDouble();
			mjd=s.readDouble();
		}


	// custom externalization - writing
	/**
	 * Write out the relevant fields of this class
	 * @param s any class which implements ObjectOutput (e.g. ObjectOutputStream)
	 */

	public void writeExternal(ObjectOutput s) throws IOException {
		s.writeInt(timezoneOffset);
		s.writeDouble(timezoneOffsetDays);
		s.writeInt(imjd);
		s.writeInt(imicrodays);
		s.writeDouble(dms);
		s.writeDouble(mjd);
	}
}///:~
