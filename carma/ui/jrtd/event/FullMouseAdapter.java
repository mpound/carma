//:FullMouseAdapter.java
/**
 * The adapter which receives mouse events.
 * A combination of MouseListener and MouseMotionListener
 * The methods in this class are empty;  this class is provided as a
 * convenience for easily creating listeners by extending this class
 * and overriding only the methods of interest.
 *
 * @author Marc Pound
 * @author mpound@astro.umd.edu
 * @version 1.0
 */
package carma.ui.jrtd.event;

import java.awt.event.*;

public abstract class FullMouseAdapter
	implements MouseListener, MouseMotionListener {

    public static final int NOBUTTON=-1;
    public static final int LEFTBUTTON=0;
    public static final int MIDDLEBUTTON=1;
    public static final int RIGHTBUTTON=2;

    public void mouseClicked(MouseEvent e) {}
    public void mousePressed(MouseEvent e) {}
    public void mouseReleased(MouseEvent e) {}
    public void mouseEntered(MouseEvent e) {}
    public void mouseExited(MouseEvent e) {}
    public void mouseDragged(MouseEvent e) {}
    public void mouseMoved(MouseEvent e) {}

/**
 * detect left button click
 * @param MouseEvent e
 */
  	public final boolean isLeftMouseButton(MouseEvent m)
        {
//====================================
// This is to workaround a bug in JDK1.1.* under Solaris/NT!!!
// See http://developer.javasoft.com/developer/bugParade/bugs/4084266.html
// Reported against: 1.1.4,1.1.3,1.2beta2,1.1.5,1.1.6,1.1.7
//
        if(m.getModifiers() == 0) return true;
//====================================
        return ((m.getModifiers() & InputEvent.BUTTON1_MASK)
                == InputEvent.BUTTON1_MASK );
        }

/**
 * detect middle button click
 * @param MouseEvent m
 */
  	public final boolean isMiddleMouseButton(MouseEvent m)
        {
        return ((m.getModifiers() & InputEvent.BUTTON2_MASK)
                == InputEvent.BUTTON2_MASK );
        }
/**
 * detect right button click
 * @param MouseEvent m
 */
 	public final boolean isRightMouseButton(MouseEvent m)
        {
        return ((m.getModifiers() & InputEvent.BUTTON3_MASK)
                == InputEvent.BUTTON3_MASK );
        }

/**
 * 
 * @param m the MouseEvent generated by the mouse button click
 * @return FullMouseAdapter.LEFTBUTTON if the left button was pressed
 * <br>FullMouseAdapter.MIDDLEBUTTON if the middle button was pressed
 * <br>FullMouseAdapter.RIGHTBUTTON if the right button was pressed
 * <br>FullMouseAdapter.NOBUTTON otherwise.
 * @exception none
 */
	public final int whichMouseButton(MouseEvent m) {
		if(isLeftMouseButton(m)) return LEFTBUTTON;
		if(isRightMouseButton(m)) return RIGHTBUTTON;
		if(isMiddleMouseButton(m)) return MIDDLEBUTTON;
		return NOBUTTON;
	}

}///:~
