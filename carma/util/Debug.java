// $Id: Debug.java,v 1.2 2007/02/08 19:22:04 tcosta Exp $

package carma.util;

import java.io.*;

/**
 *  Class used to print debug messages. If variable debug is set
 *  to false, then compiler will remove debug code.
 *
 *  @author     Rick Hobbs
 *  @version    $Revision: 1.2 $, $Date: 2007/02/08 19:22:04 $
 *  @since      JDK1.3
 */
public final class Debug {
    public static final boolean debug = true;

    // types
    public static final int NONE    = -1;
    public static final int INFO    = 0;
    public static final int STATUS  = 1;
    public static final int WARNING = 2;
    public static final int ERROR   = 3;

    public static int minType = INFO;

    private static final String[] TYPE = {"INFO", "STATUS", "WARNING", "ERROR"};

    private static String _prevClass;
    private static String _curClass;

    private Debug() { }

    /**
     *  Print out Debug message with a give type.
     *  Valid types are:
     *  <ul>
     *  <li>Debug.NONE     - Nothing is printed for type
     *  <li>Debug.INFO     - &lt;INFO&gt; is printed
     *  <li>Debug.STATUS   - &lt;STATUS&gt; is printed
     *  <li>Debug.WARNGING - &lt;WARNING&gt; is printed
     *  <li>Debug.ERROR   - &lt;ERROR&gt; is printed
     *  </ul>
     */
    public static void print(Object o, int type, String message) {
        if ( (type == NONE) || (type >= minType) ) {
            _curClass = o.getClass().getName();
            checkObject(o);
            // check for valid type
            if (type == INFO || type == STATUS || type == WARNING ||
                type == ERROR)
                System.err.println("\t <" + TYPE[type] + "> " + message);
            else
                System.err.println("\t" + message);
        }
    }

    /**
     *  Print out a simple message
     */
    public static void print(Object o, String message) {
        print( o, NONE, message );
    }

    private static void checkObject(Object o) {
        if (_prevClass == null) {
            _prevClass = _curClass;
            System.err.println("\n" + _curClass + ":");
        } else
            if (!_prevClass.equals(_curClass)) {
                _prevClass = _curClass;
                System.err.println("\n" + _curClass + ":");
            }
    }
}
