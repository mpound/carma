package carma.ui.jrtd.util;

import carma.ui.jrtd.ui.*;
import java.util.List;
import java.util.ArrayList;

import java.awt.*;

/**
 * A simple container for all information needed to start a single RTD
 * display. Nothing more. No command line parsing, nothing.
 */
public class Parameters implements Cloneable {
    // update rate in milliseconds
    protected int updateRate = 2000;

    // RTD display and plotter host/port
    // The display and plotter servers are required to live on the same
    // machine to make it easier for users to change their RTD connection
    protected String rtdHost = "rtd.mmarray.org";
    protected int rtdPort = 3100;
    protected int plotPort = 5668;

    // RTD Display GUI properties
    protected boolean timebar = true;
    protected int deltaFontSize = 0;
    protected String windowName = "default";
    protected Geometry windowGeometry = null;

    public Parameters() {
        // nothing, the ParameterParser class will reach in directly and
        // set every member variable as needed
    }

    public Object clone() {
        try {
            return super.clone();
        } catch(Exception e) {
            Util.spew("Parameters can't clone!");
            return null;
        }
    }

    public int getUpdateRate() {
        return updateRate;
    }

    public String getRtdHost() {
        return rtdHost;
    }

    public int getRtdPort() {
        return rtdPort;
    }

    public int getPlotPort() {
        return plotPort;
    }

    public boolean hasTimeBar() {
        return timebar;
    }

    public String getWindowName() {
        return windowName;
    }

    public Point getPosition() {
        if (windowGeometry == null)
            return null;

        return windowGeometry.getPosition();
    }

    public void setGeometry (Point p, Dimension s) {
        windowGeometry = new Geometry(p, s);
    }

    public Dimension getSize() {
        if (windowGeometry == null)
            return null;

        return windowGeometry.getSize();
    }

    public int getDeltaFontSize() {
        return deltaFontSize;
    }

    public void setWindowName(String s){
        windowName = s;
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
