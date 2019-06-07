package carma.observertools.pdbi.utils;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import carma.observertools.pdbi.messages.*;

/**
 * class which does simple conversions between units
 * @author friedel
 *
 */
public class Conversions {
	/**
	 * method to convert RA in hours and minutes to RA in radians
	 * @param hours the hours
	 * @param minutes the minutes
	 * @return the RA in radians
	 */
	public static String TimeToRadians(int hours, int minutes){
		double time = (double)hours + ((double)minutes)/60.0;
		String timeString = "" + (time*HRS2RAD);
		return timeString;
	}
	
	/**
	 * method to convert RA in hours and minutes to RA in radians
	 * @param hours the hours
	 * @param minutes the minutes
	 * @return the RA in radians
	 */
	public static String TimeToRadians(float hours, float minutes){
		double time = (double)hours + ((double)minutes)/60.0;
		String timeString = "" + (time*HRS2RAD);
		return timeString;
	}	
	
	/**
	 * method to convert RA in hours and minutes to RA in radians
	 * @param hours the hours
	 * @param minutes the minutes
	 * @return the RA in radians
	 */	
	public static String TimeToRadians(String hours, String minutes){
		return Conversions.TimeToRadians(Integer.parseInt(hours), Integer.parseInt(minutes)	);
	}
	
	/**
	 * method to convert RA in hours and minutes to RA in radians
	 * @param hours the hours
	 * @param minutes the minutes
	 * @return the RA in radians
	 */
	public static double DTimeToRadians(String hours, String minutes){
		return (Double.parseDouble(hours) + (Double.parseDouble(minutes))/60.0)*HRS2RAD;
	}

	/**
	 * method to an hour angle to radians based on the RA
	 * @param adjust the HA adjustment
	 * @param hours the RA hours
	 * @param minutes the RA minutes
	 * @param sub which way to go (subtract/add)
	 * @return the adjusted HA in radians
	 */
	public static double DTimeToRadians(String adjust, String hours, String minutes,boolean sub){
		double time = 0.0;
		if(sub){
			time = Double.parseDouble(hours) - Double.parseDouble(adjust);
			if(time < 0.0) time += 24.0;
		}
		else{
			time = Double.parseDouble(hours) + Double.parseDouble(adjust);
			if(time > 23.99999999)time -= 24.0;
		}
		return Conversions.DTimeToRadians("" + time, minutes);
	}
	
	/**
	 * method to an hour angle to radians based on the RA
	 * @param adjust the HA adjustment
	 * @param hours the RA hours
	 * @param minutes the RA minutes
	 * @param sub which way to go (subtract/add)
	 * @return the adjusted HA in radians
	 */	
	public static String TimeToRadians(String adjust, String hours, String minutes,boolean sub){
		if(sub){
			return Conversions.TimeToRadians(Float.parseFloat(hours)-Float.parseFloat(adjust), Float.parseFloat(minutes));
		}
		return Conversions.TimeToRadians(Float.parseFloat(hours)+Float.parseFloat(adjust), Float.parseFloat(minutes));
	}
	
	/**
	 * method to convert RA in radians to hh:mm format
	 * @param rad the RA in radians
	 * @return the RA in hh:mm
	 */
	public static String RadiansToTime(double rad){
		return HoursToTime(RadiansToHours(rad));
	}
	
	public static String RadiansToDec(double rad){
		return HoursToFullTime(RadiansToDegrees(rad));
	}
	
	public static String RadiansToFullTime(double rad) {
		return HoursToFullTime(RadiansToHours(rad));
	}
	
	/**
	 * method to convert RA in radians to RA in raw hours
	 * @param rad the RA in radians
	 * @return the RA in raw hours
	 */
	public static double RadiansToHours(double rad){
		return rad/HRS2RAD;
	}
	
	public static double RadiansToDegrees(double rad){
		return rad/DEG2RAD;
	}
	
	/**
	 * method to convert from raw hours to hh:mm format
	 * @param hours time in raw hours
	 * @return the time in hh:mm format
	 */
	public static String HoursToTime(double hours){
		boolean neg = false;
		double thours = hours;
		if(hours < 0.0){
			neg = true;
			thours *= -1.0;
		}
		double dhour = Math.floor(thours);
		int minutes = (int)((thours - dhour)*60);
		int hour = (int)dhour;
		NumberFormat numberFormat = new DecimalFormat("00");
		String hr = numberFormat.format(hour);
		String min = numberFormat.format(minutes);
		if(neg){
			return "-" + hr + ":" + min;
		}
		return hr + ":" + min;
	}
	
	public static String HoursToFullTime(double hours){
		boolean neg = false;
		double thours = hours;
		if(hours < 0.0){
			neg = true;
			thours *= -1.0;
		}
		double dhour = Math.floor(thours);
		double dminutes = ((thours - dhour)*60.0);
		int minutes = (int)dminutes;
		int seconds = (int)((dminutes - (double)minutes) * 60);
		int hour = (int)dhour;
		NumberFormat numberFormat = new DecimalFormat("00");
		String hr = numberFormat.format(hour);
		String min = numberFormat.format(minutes);
		String sec = numberFormat.format(seconds);
		if(neg){
			return "-" + hr + ":" + min + ":" + sec;
		}
		return hr + ":" + min + ":" + sec;
	}
	
	/**
	 * method to convert strings of year, month, and day to xquery:date format
	 * @param year
	 * @param month
	 * @param day
	 * @return
	 */
	public static String dateConvert(String year, String month, String day){
		int iyear = Integer.parseInt(year);
		int imonth = Integer.parseInt(month);
		int iday = Integer.parseInt(day);
		NumberFormat numberFormat = new DecimalFormat("00");
		return "20" + numberFormat.format(iyear) + "-" + numberFormat.format(imonth) + "-" + numberFormat.format(iday);
	}
	
	/**
	 * method to split a fully qualified obsblock into its components (pid, obsblock, subObsblock)
	 * @param fullName
	 * @return a string[] containing the components
	 */
	public static String[] splitPid(String fullName){
		String[] splitName = new String[3];
		String[] tempString = fullName.split("\\.",3);
		if(tempString.length < 2){
			new ExceptionHandler("Improper project name it must be pid.obsblock.subObsblock or pid.obsblock",false);
		}
		splitName[0] = tempString[0];
		splitName[1] = tempString[1];
		if(tempString.length == 2){
			splitName[2] = "";
		}
		else{
			splitName[2] = tempString[2];
		}
		return splitName;
	}
	
	public static final Double HRS2RAD = Math.PI/12.0;
	public static final Double DEG2RAD = Math.PI/180.0;
	public static final double twoPI = 2.0*Math.PI;
}
