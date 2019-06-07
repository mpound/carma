// package carma.ui.jrtd;
package carma.ui.jrtd.event;

import java.awt.event.*;

/** 
 * A listener for an Alert box. The method in this interface must be
 * implemented by a class that wishes to be informed of the pressing of
 * the Close box. Note that each alert class can only have one listener
 * If more that one listener is assigned to the Alert class then the
 * most recent will be the one that is used.
 *
 * @author Simon Weaver
 * @varsion 1.0 06-Aug-1998   - original
 */
 
 public interface AlertCloseListener {
 
    /* Called when the close button is pressed */
    public void alertClosed(ActionEvent e);
 
}
