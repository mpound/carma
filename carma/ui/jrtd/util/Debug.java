package carma.ui.jrtd.util;

import java.io.*;

/**
 * Class used to print debugging and other informational messages on a
 * per-class basis. You should instantiate one of these objects per
 * object type as a static member. This will allow you to debug a single type
 * of object at a time without enabling all debugging across RTD.
 *
 * This object is somewhat modeled after the python logging class.
 *
 * @author      Ira W. Snyder
 * @version     $Revision: 1.2 $
 */

public final class Debug {
    private final String object;
    private final boolean enable;

    /**
     * Constructor
     *
     * @param object the object type
     * @param enable enable debugging for this object type
     */
    public Debug(final String object, final boolean enable) {
        this.object = object;
        this.enable = enable;
    }

    // disallow empty construction, must provide required parameters
    private Debug() {
        this.object = "BUG";
        this.enable = false;
    }

    /**
     * Print a debug message to System.out
     *
     * @param message the message to print
     */
    public void println(final String message) {
        if (this.enable)
            System.out.println(object + ": " + message);
    }

    /**
     * Is debugging enabled
     *
     * This is helpful for writing your own debugging routines that are outside
     * the scope of this class.
     */
    public boolean isEnabled() {
        return this.enable;
    }
}
