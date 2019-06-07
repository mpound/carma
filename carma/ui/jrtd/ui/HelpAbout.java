package carma.ui.jrtd.ui;

import carma.ui.jrtd.event.*;
import carma.ui.jrtd.rtd.*;
import carma.ui.jrtd.ui.*;
import carma.ui.jrtd.util.*;

import java.awt.*;
import java.awt.event.*;

/**
 * A CARMA specific window that gives version info and download options.
 *
 * @author  Steve Scott
 * @version $Id: HelpAbout.java,v 1.8 2013/12/09 21:39:55 iws Exp $
 */
public class HelpAbout {

    private TextPanel titleGraphic;

    public HelpAbout(Frame parentFrame) {
        // Create a canvas for the help action
        titleGraphic = new TextPanel();
        titleGraphic.setBackground(Color.white);
        titleGraphic.addString("CARMA RTD", 40);
        titleGraphic.addString("Combined Array for Reseach", Font.BOLD, Color.red, 15);
        titleGraphic.addString("in Millimeter-Wave Astronomy", Font.BOLD, Color.red, 15);
        titleGraphic.addString("Realtime observing/control windows", 12);
        String username = System.getProperty("user.name");
        String userString = "Username: " + username;
        titleGraphic.addString(userString, Color.black, 12);
        titleGraphic.addSpace(10);
        titleGraphic.addString("This Java client version: " + Version.getClientVersion(), Color.black, 15);
        titleGraphic.addSpace(10);

        titleGraphic.addString("JVM version: " + System.getProperty("java.vendor") + " v" +
                System.getProperty("java.version"), Color.gray, 12 );
        titleGraphic.addString("OS: " + System.getProperty("os.name") + " " +
                System.getProperty("os.version"), Color.gray, 12);

        new Alert(parentFrame, "CARMA RTD", titleGraphic).popItUp();
    }
}
