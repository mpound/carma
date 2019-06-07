package carma.ui.jrtd.ui;

import carma.ui.jrtd.rtd.*;
import carma.ui.jrtd.ui.*;
import carma.ui.jrtd.util.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.JOptionPane;

/**
 * New super-class of ovro.cma.rtd.RtMenuListener and
 * EDU.umd.astro.jstatus.XXMenuListener
 *
 * Used by the monitoring windows and ObsLog to select a choice from the list
 * of windows available. When the action is performed the listener will
 * open up a new window and optionally close the original one.
 *
 * @author  Simon Weaver
 * @version $Revision: 1.6 $, $Date: 2013/11/19 03:48:46 $, $Author: iws $
 */
public class RtMenuListener implements ActionListener {
    protected final String menuType;
    protected final RtParentWindow parent;
    protected final boolean morphRequested;

    /**
     * Listener for opening a new monitoring window.
     * There will be one of these for *each* window listed in the New and Morph
     * menus. The result is many of these, each with a distinct menuType and
     * morph flag.
     *
     * @param <code>parent</code> The parent window (to which the menu is attached)
     * @param <code>menuType</code> The string representation of the new window to open
     * @param <code>morphRequested</code> True if we are morphing windows
     */
    public RtMenuListener(RtParentWindow parent, String menuType, boolean morphRequested) {
        this.menuType = menuType;
        this.parent = parent;
        this.morphRequested = morphRequested;
    }

    public String getMenuType() {
        return menuType;
    }

    /* ---------------------------------------------------------------------- */
    /* ActionListener Interface                                               */
    /* ---------------------------------------------------------------------- */

    @Override
    public void actionPerformed (ActionEvent e) {
        if (parent instanceof RtDisplay) {
            final RtDisplay display = (RtDisplay)parent;
            if (morphRequested) {
                display.morphWindow(menuType);
            } else {
                display.createNewWindow(menuType);
            }
        }
    }
}
