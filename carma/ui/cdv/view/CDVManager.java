// $Id: CDVManager.java,v 1.2 2013/11/21 17:34:40 iws Exp $
// vim: set ts=4 sts=4 sw=4 et:

package carma.ui.cdv.view;

import org.omg.CosNaming.NameComponent;

import java.io.File;
import java.io.IOException;

import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.Properties;
import java.util.Collections;

import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import carma.ui.cdv.config.CarmaConfig;
import carma.ui.cdv.config.CDVxmlConfig;

import carma.ui.cdv.model.PlotInfo;
import carma.ui.cdv.model.CorrelatorDataRouter;

import carma.util.Debug;

/**
 * Non-GUI Manager of one or more CDV display windows.
 */
public final class CDVManager {
    private final CorrelatorDataRouter router_ = new CorrelatorDataRouter("CDV-router");
    private List<PlotFrame> plotFrames_ = Collections.synchronizedList(
            new ArrayList<PlotFrame>());
    private final List<String> nameServers_;

    private CDVManager(final CarmaConfig config) {
        this.nameServers_ = config.getNameServers();

        // set extra system properties
        final Properties sysProps = System.getProperties();
        final Map<String, String> extraProps = config.getProperties();
        for (final Map.Entry<String, String> entry : extraProps.entrySet()) {
            final String key = entry.getKey();
            final String value = entry.getValue();
            sysProps.setProperty(key, value);
        }

        // automatically start the first window
        handleAutoStart(config);
    }

    public void showPlotFrame(final String imr, final PlotInfo info) {
        // hook up the CORBA data source
        final int bandNo = info.getBandNumber();
        final boolean integrated = info.getIntegrated();
        final NameComponent[] nc = this.getAstrobandNameComponent(bandNo, integrated);
        final String key = router_.startCorbaSource(imr, nc);

        // create and show the plot frame
        final PlotFrame frame = new PlotFrame(this, imr, key, info);

        // be notified about window close
        plotFrames_.add(frame);
        frame.addWindowListener(new WindowAdapter() {
            public void windowClosed(WindowEvent e) {
                plotFrames_.remove(frame);
                exitIfNecessary();
            }
        });

        frame.show();
    }

    public CorrelatorDataRouter getRouter() {
        return this.router_;
    }

    public List<String> getNameServers() {
        return this.nameServers_;
    }

    private void exitIfNecessary() {
        if (plotFrames_.size() == 0)
            System.exit(0);
    }

    private void handleAutoStart(final CarmaConfig config) {
        final Map<String, String> m = config.getInitialPlotInfo();
        if (m == null)
            return;

        final String imr = m.get("IMR");
        final String dataType = m.get("DataType");
        final String view = m.get("DataView");
        final int bandNo = Integer.parseInt(m.get("BandNumber"));
        final String pol = m.get("Polarization");
        final boolean integrated = Boolean.parseBoolean(m.get("Integrated"));

        final PlotInfo info = new PlotInfo(dataType, view, bandNo, pol, integrated);
        showPlotFrame(imr, info);
    }

    /**
     * Parse the command line arguments and setup debugging based on them
     */
    private static void parseAndSetDebugLevel(final String[] args) {
        boolean debugLevelSet = false;
        for (int i = 0; i < args.length; i++) {
            if (args[i].equalsIgnoreCase("-DebugLevel")) {

                if ((i + 1) >= args.length) {
                    System.err.println("ERROR: parameter -DebugLevel has too few arguments");
                    System.exit(1);
                }

                String param = args[i + 1];
                if (param.equalsIgnoreCase("INFO")) {
                    Debug.minType = Debug.INFO;
                    debugLevelSet = true;
                } else if (param.equalsIgnoreCase("STATUS")) {
                    Debug.minType = Debug.STATUS;
                    debugLevelSet = true;
                } else if (param.equalsIgnoreCase("WARNING")) {
                    Debug.minType = Debug.WARNING;
                    debugLevelSet = true;
                } else if (param.equalsIgnoreCase("WARN")) {
                    Debug.minType = Debug.WARNING;
                    debugLevelSet = true;
                } else if (param.equalsIgnoreCase("ERROR")) {
                    Debug.minType = Debug.ERROR;
                    debugLevelSet = true;
                }
            }
        }

        // no default debug level set: print warnings
        if (debugLevelSet != true)
            Debug.minType = Debug.WARNING;
    }

    /**
     * Parse the command line arguments and return the configuration file
     * name if it was provided
     */
    private static String parseArgsForConfig(final String[] args) {
        // only one argument, assume it is the config file name
        if (args.length == 1 && args[0].endsWith("xml"))
            return args[0];

        // for each argument
        for (int i = 0; i < args.length; i++) {
            if (args[i].equalsIgnoreCase("-XMLFile")) {
                if ((i + 1) >= args.length) {
                    System.err.println("ERROR: parameter -XMLFile has too few arguments");
                    System.exit(1);
                }

                String param = args[i + 1];
                if (param.endsWith("xml"))
                    return param;
            }
        }

        return null;
    }

    /**
     * Get the data source path for a specific astroband number
     */
    public static NameComponent[] getAstrobandNameComponent(int bandNo, boolean integrated) {
        NameComponent[] nc = null;

        if (integrated) {
            nc = new NameComponent[3];
            for (int i = 0; i < nc.length; i++)
                nc[i] = new NameComponent("", "");

            nc[0].id = "carma";
            nc[1].id = "correlator";
            if (bandNo <= 8)
                nc[2].id = String.format("slPubInt%d", bandNo);
            else
                nc[2].id = String.format("wbPubInt%d", bandNo);
        } else {
            nc = new NameComponent[4];
            for (int i = 0; i < nc.length; i++)
                nc[i] = new NameComponent("", "");

            nc[0].id = "carma";
            nc[1].id = "correlator";
            nc[2].id = String.format("astroband%d", bandNo);
            nc[3].id = "data";
        }

        return nc;
    }

    /**
     *  Returns a key based on the namecomponent array.
     */
    private static String getNCKey(String imr, NameComponent[] nc) {
        String key = "IMR=" + imr + " SOURCE=";
        for (NameComponent component : nc)
            key += "/" + component.id.toString() + component.kind.toString();

        return key;
    }

    /**
     * Get the CDV configuration.
     *
     * If the command-line "-XMLFile filename" is present, it will be used.
     * If not, then the built-in carma-cdv-config.xml will be used.
     */
    private static CarmaConfig getConfig(final String[] args) {
        java.net.URL url = CDVManager.class.getResource("/resources/carma-cdv-config.xml");
        final String filename = parseArgsForConfig(args);
        if (filename != null) {
            final File file = new File(filename);
            if (!file.canRead()) {
                final String[] mess = {
                    "Unable to open XML config file: " + filename,
                    "Try using -XMLFile <filename> on the command line.",
                };

                JOptionPane.showMessageDialog(null, mess,
                    "Carma Data Viewer", JOptionPane.ERROR_MESSAGE);
                System.exit(1);
            }

            try {
                url = file.toURI().toURL();
            } catch (java.net.MalformedURLException ex) {
                System.err.println("Malformed URL from file: " + file);
                ex.printStackTrace();
                System.exit(0);
            }
        }

        try {
            return new CDVxmlConfig(url.openStream());
        } catch (IOException ex) {
            JOptionPane.showMessageDialog(null, "Unable to parse configuration file",
                "CARMA Data Viewer", JOptionPane.ERROR_MESSAGE);
            ex.printStackTrace();
            System.exit(1);
        }

        return null;
    }

    public static void main(String[] args) {
        // parse the arguments and setup debugging based on them
        parseAndSetDebugLevel(args);

        // parse arguments for the configuration file
        final CarmaConfig config = getConfig(args);

        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                final CDVManager manager = new CDVManager(config);
            }
        });
    }
}
