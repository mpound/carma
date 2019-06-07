package carma.ui.jrtd.ui;

import java.awt.*;
    
/** 
 * A static class which provides simple useful operations on windows
 * 
 * @author Simon Weaver
 * @version 1.0 23-Jun-1998
 *
 * History:
 * 23Jun98 1.0  S. Weaver   Original Version
 *
 */

public abstract class WindowUtils {

    /** Centers a window on the screen
    *   @param window The window which it is to be centered
    */
    public static void centerWindow(Window window){
	Dimension screen = window.getToolkit().getScreenSize();
	Dimension windowSize = window.getSize();
	window.setLocation(screen.width/2 - windowSize.width/2, screen.height/2 - windowSize.height/2);
    }
    
    /** Centers a window on the left half of the screen 
    *   @param window The window which it is to be centered
    */
    public static void centerWindowOnLeft(Window window){
	Dimension screen = window.getToolkit().getScreenSize();
	Dimension windowSize = window.getSize();
	window.setLocation(screen.width/4 - windowSize.width/2, screen.height/2 - windowSize.height/2);
	
	// if the window is too big to do this with then put it in the middle
	if (isOffScreen(window))
	    centerWindow(window);
    }
    
    /** Centers a window - such as a dialog box within a parent window 
    *   @param window The smaller window
    *   @param parent The window in which it is to be centered
    */
    public static void centerWithin(Window window, Window parent){

	Point bigLoc = parent.getLocation();
	Dimension bigSize = parent.getSize();
	Dimension windowSize = window.getSize();
	window.setLocation(bigLoc.x + bigSize.width/2 - windowSize.width/2, bigLoc.y+ bigSize.height/2 - windowSize.height/2);
    }
    
    /** Tries to place a window so that it does not cover its parent. Various positions
    *   will be tried until a suitable position is found.
    *   @param window The new window that is about to be opened
    *   @param parent The parent window that we are trying to avoid
    */
    public static void avoidParent(Window window, Window parent) {
	Rectangle parentBounds = parent.getBounds();
	Rectangle windowBounds = window.getBounds();
	
	//if we're ok where we are already then don't do anything!
	//if (!parentBounds.intersects(windowBounds)) 
	//    return;
	
	// try putting it on the right
	if (putOnRight(window, parent) )
	    return;
	    
	// if not then try putting it on the left
	if (putOnLeft(window, parent) )
	    return;
	
	// if this is still no good then center it on the screen
	centerWindow(window);
	
    }
    
    /** Puts window <b>window</b> on the right of window <b>parent</b>
    *   @param window The window that is being placed
    *	@param parent The window that does not move
    *	@returns True if the window has been successfully placed in the desired position
    */
    
    public static boolean putOnRight(Window window, Window parent) {
	Point bigWindowPos = parent.getLocation();
	Point originalPos = window.getLocation();
	Rectangle parentBounds = parent.getBounds();
			
	// put the window to the right with the tops aligned
	int xRHS = parentBounds.x + parentBounds.width;
	window.setLocation(xRHS, bigWindowPos.y);
	
	Rectangle windowBounds = window.getBounds();
	// The window takes a while to report its new location sometimes, so try one more time if reqd.
	if (xRHS != windowBounds.x)	window.setLocation(xRHS, bigWindowPos.y);

	//System.out.println("putOnRHS:"+xRHS + "  " + windowBounds);

	// makes sure the window is not too far down
	if (isOffBottomOfScreen(window)){
	    window.setLocation( parentBounds.x+parentBounds.width,
				bigWindowPos.y+parentBounds.height - windowBounds.height);
    	}
	
	// if is it still off the bottom of the screen move it up
	if (isOffBottomOfScreen(window)){
	    Dimension screen = window.getToolkit().getScreenSize();
	    window.setLocation( parentBounds.x+parentBounds.width,
				screen.height - windowBounds.height);
	}
	
	// make sure the window is not too far up
	if (isOffTopOfScreen(window)){
	    window.setLocation( parentBounds.x+parentBounds.width,0);
	}
		
	// makes sure the window is on the right and has not gone off screen
	return (!isOffRightOfScreen(window));
    } 
    
    /** Puts window <b>window</b> on the left of window <b>parent</b>
    *   @param window The window that is being placed
    *	@param parent The window that does not move
    *	@returns True if the window has been successfully placed in the desired position
    */
    
    public static boolean putOnLeft(Window window, Window parent) {
	Point bigWindowPos = parent.getLocation();
	Point originalPos = window.getLocation();
	Rectangle parentBounds = parent.getBounds();
	Rectangle windowBounds = window.getBounds();
	
	window.setLocation(parentBounds.x-windowBounds.width, bigWindowPos.y);
	
	windowBounds = window.getBounds();	
	
	// makes sure the window is not too far down
	if (isOffBottomOfScreen(window)){
	    window.setLocation( parentBounds.x-windowBounds.width,
				bigWindowPos.y+parentBounds.height - windowBounds.height);
    	}
	
	// if it is still off the bottom of the screen move it up
	if (isOffBottomOfScreen(window)){
	    Dimension screen = window.getToolkit().getScreenSize();
	    window.setLocation( parentBounds.x-windowBounds.width,
				screen.height - windowBounds.height);
	}
	
	// make sure the window is not too far up
	if (isOffTopOfScreen(window)){
	    window.setLocation( parentBounds.x-windowBounds.width,0);
	}
	
	// makes sure the window is on the right and has not gone off screen
	return (!isOffLeftOfScreen(window));
    }  

    /** Returns true if the window is on the screen */
    public static boolean isOffScreen(Window window) {
	return (   isOffTopOfScreen(window) || isOffBottomOfScreen(window) ||
		 isOffRightOfScreen(window) || isOffLeftOfScreen(window));
    }
    
    /** Returns true if the window is off the screen to the right*/
    public static boolean isOffRightOfScreen (Window window){
	Rectangle windowBounds = window.getBounds();

	// check to see if we are off the right of the screen
	Dimension screen = window.getToolkit().getScreenSize();
	
	if (windowBounds.x + windowBounds.width > screen.width ) {
	    return true;
	}
	else {
	    //System.out.println("OffRight:"+windowBounds +"  "+screen);	
	    return false;
	}
    }
    
    /** Returns true if the window is off the screen to the left */
    public static boolean isOffLeftOfScreen (Window window){
	Rectangle windowBounds = window.getBounds();

	// check to see if we are off the left of the screen	
	if (windowBounds.x < 0) return true;
	
	return false;
    }
    
    /** Returns true if the window is off the screen to the bottom */
    public static boolean isOffBottomOfScreen (Window window){
	Rectangle windowBounds = window.getBounds();

	// check to see if we are off the bottom of the screen
	Dimension screen = window.getToolkit().getScreenSize();
	if (windowBounds.y + windowBounds.height > screen.height) return true;
	
	return false;
    }
    
    /** Returns distance the window is off the screen to the bottom. 0 if the window is on screen */
    public static int distanceOffBottomOfScreen (Window window){
	Rectangle windowBounds = window.getBounds();

	// check to see how far we are off the bottom of the screen
	Dimension screen = window.getToolkit().getScreenSize();
	int dist = (windowBounds.y + windowBounds.height - screen.height);
	
	if (dist>0)
	    return dist;
	else
	    return 0;
    }
    
    /** Returns true if the window is off the screen to the top */
    public static boolean isOffTopOfScreen (Window window){
	Rectangle windowBounds = window.getBounds();

	// check to see if we are off the top of the screen	
	if (windowBounds.y < 0) return true;
	
	return false;
    }
}
