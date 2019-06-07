package carma.ui.jrtd.util;

import java.util.List;
import java.util.ArrayList;


/**
 * Parse RTD paramters and produce a List of Parameters instances for each
 * window which should be opened. This way the Parameters class can have all
 * of the parameters necessary for exactly one window, and nothing more.
 */
public final class ParameterParser
{
    private static final Debug debug = new Debug("ParameterParser", false);

    // update rate in milliseconds
    private int updateRate = 2000;

    // RTD display and plotter host/port
    // The display and plotter servers are required to live on the same
    // machine to make it easier for users to change their RTD connection
    private String rtdHost = "rtd.mmarray.org";
    private int rtdPort = 3100;
    private int plotPort = 5668;

    // RTD Display GUI properties
    private boolean timebar = true;
    private int deltaFontSize = 0;

    // Parameters which can be specified multiple times
    private List<String> windowList = new ArrayList<String>();
    private List<Geometry> geometryList = new ArrayList<Geometry>();

    // Success parsing command line arguments
    private boolean OK = false;

    /**
     * Constructor.
     *
     * Parse all command-line parameters and store them into the member
     * variables. These members will be used to provide a list of windows
     * which should be created at startup.
     */
    public ParameterParser(final String[] args) {
        for (final String s : args) {
            if (!storeParameter(s)) {
                OK = false;
                return;
            }
        }

        OK = true;
    }

    /**
     * Were all parameters parsed successfully?
     *
     * @return true if all parameters were parsed successfully, false otherwise
     */
    public boolean isSuccess() {
        return this.OK;
    }

    /**
     * Get a list of all parameters specified on the command line
     */
    public List<Parameters> getParametersList() {
        final List<Parameters> plist = new ArrayList<Parameters>();

        // always make sure there is at least one window, the default window
        if (windowList.size() == 0) {
            windowList.add("default");
        }

        for (int i = 0; i < windowList.size(); i++) {

            // Thanks to the protected keyword on all members of the Parameters
            // class, we can reach directly into it and set all necessary values
            // without lots of member functions
            final Parameters params = new Parameters();
            params.updateRate = this.updateRate;
            params.rtdHost = this.rtdHost;
            params.rtdPort = this.rtdPort;
            params.plotPort = this.plotPort;
            params.timebar = this.timebar;
            params.deltaFontSize = this.deltaFontSize;
            params.windowName = windowList.get(i);

            if (i < geometryList.size()) {
                params.windowGeometry = geometryList.get(i);
            }

            plist.add(params);
        }

        return plist;
    }

    public String getRtdHost() {
        return this.rtdHost;
    }

    public int getRtdPort() {
        return this.rtdPort;
    }

    public int getPlotPort() {
        return this.plotPort;
    }

    /* ---------------------------------------------------------------------- */
    /* Private Methods                                                        */
    /* ---------------------------------------------------------------------- */

    private boolean storeParameter(String s) {
        // Clean it up...
        s = s.toLowerCase().trim();
        final int eq = s.indexOf('=');
        final String lhs;
        final String rhs;
        if (eq >= 1) {
            lhs = s.substring(0, eq);
            rhs = s.substring(eq + 1);
        } else {
            lhs = s;
            rhs = "";
        }

        debug.println("storeParameter: s=" + s);

        // "updaterate"
        if (lhs.startsWith("upd")) {
            try {
                updateRate = Integer.parseInt(rhs);
            } catch (NumberFormatException e) {
                System.out.println("You must supply a numerical value for the update rate in milliseconds");
            }
        }
        // "servername"
        else if (lhs.startsWith("serv")) {
            rtdHost = rhs;
        }
        // "portnumber"
        else if (lhs.startsWith("port")) {
            rtdPort = Integer.parseInt(rhs);
        }
        // "plotPort"
        else if (lhs.startsWith("plotPort")) {
            plotPort = Integer.parseInt(rhs);
        }
        // "timebar"
        else if (lhs.startsWith("time")) {
            if (rhs.startsWith("f") || rhs.startsWith("n")) {
                timebar = false;
            }
        }
        // "deltaFontsize"
        else if (lhs.startsWith("del")) {
            deltaFontSize= Integer.parseInt(rhs);
        }
        // "windowname"
        else if (lhs.startsWith("win")) {
            windowList.add(rhs);
        }
        // "geometry"
        else if (lhs.startsWith("geo")) {
            final Geometry g = new Geometry(rhs);
            if (g.isValid())
                geometryList.add(g);
            else
                return false;
        }
        // "Help"
        else if (lhs.startsWith("help")) {
            help();
            return false;
        }
        else {
            Util.spew("Invalid parameter name:" + lhs);
            help();
            return false;
        }

        return true;
    }

    private void help() {
        Util.spew("Usage: param1=value1 param2=value2 ...");
        Util.spew("Parameter names:");
        Util.spew("  upd*ateRate    in milliseconds");
        Util.spew("  serv*erName    internet name of data server");
        Util.spew("  port*Number    ip port number on data server");
        Util.spew("  plotPort       port number of the plot server");
        Util.spew("  time*bar       display timebar;   yes or no");
        Util.spew("  del*taFontsize  change in font size (pts)");
        Util.spew("  win*dowName    name of realtime window");
        Util.spew("  geo*metry      size and/or location as WidthxHeight+Xoff+Yoff");
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
