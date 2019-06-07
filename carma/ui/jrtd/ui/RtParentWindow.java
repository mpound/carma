package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.Parameters;
import carma.ui.jrtd.rtd.RTDManager;
import java.awt.Point;

/** 
 * This interface is defined for the RtMenuListener class
 * It allows us to include a menu of choices in both an ObsLog
 * and Display window (they both implement this interface).
 */
public interface RtParentWindow {
    public Parameters getParams();
    public void bailOut();
    public Point getLocationOnScreen();
    public RTDManager getRTDManager();
}
