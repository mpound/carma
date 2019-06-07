package carma.ui.jrtd.util;

import java.applet.*;
import java.awt.*;
import java.net.*;
import javax.swing.JOptionPane;

// A place to share common context information about running in an applet
// and a shared output method

public class Util extends Object {
    static private AppletContext app   = null;
    static private boolean inApplet    = false;
    static private boolean canControl  = false;

    static private final double expConv = Math.log(10.0);
    static private double oldUsedMemory = 0.0;
      
       
    public static void setContext(AppletContext _app, boolean _inApplet) {
        app      = _app;
        inApplet = _inApplet;
    } 
     
    public static AppletContext getAppletContext() {
        return app;
    } 
    
    public static boolean canControl() {
        return canControl;
    }
    
    public static void setCanControl(boolean tf) {
        canControl = tf;
    }
    
    // If in an applet, output must go to the status line at the bottom 
    // (System.out.println() will throw an exception).
    // But if an application, can only print to the console.
    public static void spew(String s) {
         if (inApplet) app.showStatus(s);
         System.out.println(s);
    } 
    
    public static boolean canPrint() {
        return !inApplet;
    }
     
    public static boolean isApplet(){
        return inApplet;
    }    
    
    private static double meg(long b) {
        return 0.000001*b;
    }
    
    public static void dumpMemory(String prefix) {
        Runtime rt = Runtime.getRuntime();
        double totalMemory = meg(rt.totalMemory());
        double freeMemory  = meg(rt.freeMemory());
        double usedMemory;
        usedMemory  = totalMemory - freeMemory;
        
        //spew("Total memory: "+rt.totalMemory());
        String memoryNoPrefix = "Memory";
        String memoryWithPrefix = " memory";
        String fullPrefix = prefix;
        if (prefix.length() == 0) fullPrefix += memoryNoPrefix;
        else                      fullPrefix += memoryWithPrefix;
        spew(fullPrefix
            + " (used/free/total)M: "
            + Text.format(usedMemory,  2, 6)
            + "/"+Text.format(freeMemory, 2, 6)
            + "/"+Text.format(totalMemory, 2, 6)
            + "     incremental use from previous=" +
              Text.format(usedMemory-oldUsedMemory, 2));
        oldUsedMemory = usedMemory;
    }
    
    public static void dumpMemory() {
        dumpMemory("");
    }
    
     /**
     * Puts up a warning dialog box if running low on memory
     *
     */
    public static void checkVM(Component parent) {
        // If the amount of free memory is < 2 MB, recommend restart
        double freeMem = meg(Runtime.getRuntime().freeMemory());
        if (freeMem < 2.0) {
            JOptionPane.showMessageDialog(parent,
                    "Your RTD program is getting low on memory.\n"
                    + "Advise exit and restart of program.",
                    "JVM memory warning",
                    JOptionPane.WARNING_MESSAGE);        
        }
    } 
    
   // Formatted conversion of floating point numbers to a string (a la printf).
   public static String printf(double x, int places) {
        boolean negative = (x < 0.0);
        if (negative)x = -x;
        if (places < 0)places = 0;
        else if (places > 20)places = 20;
        double scale = Math.exp(expConv*places);
        long i = (long) (x*scale + 0.5);
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
    
    private static String stars(int width) {
        StringBuffer sb = new StringBuffer();
        for(int i=0; i<width; i++)sb.append("*");
        return sb.toString();
    } 
      
    public static String printf(double x, int places, int width) {
        if ((places+2) > width)return stars(width);
        StringBuffer sb = new StringBuffer(printf(x,places));
        if (sb.length() > width) return stars(width);
        for (int i=sb.length(); i<width; i++)sb.insert(0," ");
        return sb.toString();
    } 
    
    public static String printf(float x, int places) {
        return printf((double)x, places);
    }  
       
    public static String printf(float x, int places, int width) {
        return printf((double)x, places, width);
    } 
       
    /**
    * This is a simple routine to avoid the object creation overhead
    * associated with Double.valueOf(String).
    * Evaluate the string stored in a byte array as a double.
    * Return -2100000000 if the string doesn't parse as a legitimate number.
    * Embedded spaces anywhere in the string are ignored.
    */
    public static double doubleValue(byte[] b, int beg, int len) {
        boolean minusSignFound = false;
        boolean decimalPointFound  = false;
        double rodScale = 1.0; // Scale to the right of the decimal point
        double tot = 0;
        if (b == null) return -2100000000;
	for (int i=0; i<len; i++) {
	    byte v = b[beg+i];
	    if (v >= '0' && v <= '9') {  // Is it a digit?
		if (decimalPointFound) {
		    rodScale *= 0.1;
		    tot += rodScale*(v - 48);
		}
		else {
		    tot *= 10;
		    tot += (v - 48);  // Convert from ascii
		}
	    }
	    else if (v == '.') {
		decimalPointFound = true;
	    }
	    else if (v == '-') {
		minusSignFound = true;
	    }
	    else if (v == ' ') {
		// Skip blanks (no matter where they are!)
	    }
	    else {
		// Illegitimate character found
		return -2100000000;
	    }
	}
	return minusSignFound?-tot:tot;
    } 
    
    public static double doubleValue(byte[] b) {
        if (b == null) return -2100000000;
        return doubleValue(b, 0, b.length);
    }

    public static void main(String arg[]){
        int argCount = arg.length;
        if (argCount < 1) {
            System.out.println("Must input a double");
            System.exit(1);
        }   
        //System.out.println(argCount);
        double x;
        Double dx = Double.valueOf(arg[0]);
        x = dx.doubleValue();
        int    p = 1;
        if (argCount > 1)p = Integer.parseInt(arg[1]);
        int    w = 6; 
        if (argCount > 2)w = Integer.parseInt(arg[2]);
        if (argCount>2)
            System.out.println(x + " f"+w+"."+p+"  /"+Util.printf(x,p,w)+"/");
        else 
            System.out.println(x + " f"+w+"."+p+"  /"+printf(x,p)+"/");
    }
 }
