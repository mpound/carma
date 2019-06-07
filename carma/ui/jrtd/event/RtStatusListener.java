package carma.ui.jrtd.event;



/** 
 * Implemented by any object that can be informed about server problems/death
 * of parent windows. For example when we bring up a plot window and close the
 * parent the plot window may be left behind. The sourceDied() method is called
 * in this case. However if the data is only lost temporarily then we call the
 * sourceAbsent() method. We can call the sourceReturned() method when the server
 * recovers. In the case of a sourceDied() event the source will never recover.
 *
 * @author  Simon Weaver
 * @version 1.01 10-Jul-1998
 *
 * History:
 * 10Jul98 1.00  S. Weaver  Original version 
 */
 
public interface RtStatusListener {
    public void sourceDied();
    public void sourceAbsent();
    public void sourceAlive();
    public void bailOut();
}

