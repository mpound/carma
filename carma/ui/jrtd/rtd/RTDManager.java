package carma.ui.jrtd.rtd;

import carma.ui.jrtd.util.Util;
import carma.ui.jrtd.util.Debug;
import carma.ui.jrtd.util.Parameters;
import carma.ui.jrtd.util.ParameterParser;

import carma.ui.jrtd.ui.RtDisplay;
import carma.ui.jrtd.ui.RtDisplayFrame;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

import java.awt.Window;
import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;

import javax.swing.SwingWorker;
import javax.swing.SwingUtilities;

import rtdproto.RTD.UIMessageReply;

import carma.ui.jplotter.network.RTDNetworkClient;
import carma.ui.jplotter.plotter.GenericPlotFrame;
import carma.ui.jplotter.plotter.PlotManagerBase;
import carma.ui.jplotter.plotter.PlotManager;

/**
 * A non-GUI object which keeps track of all running RTD windows attached to
 * this instance of the JVM. It also handles sharing the plotter client
 * between all of the RTD GUI windows.
 */
public final class RTDManager
{
    private static final Debug debug = new Debug("RTDManager", false);

    private final List<RtDisplayFrame> frameList = Collections.synchronizedList(
            new ArrayList<RtDisplayFrame>());
    private final List<RtDisplay> displayList = Collections.synchronizedList(
            new ArrayList<RtDisplay>());
    private final RTDNetworkClient client;
    private final PlotManager plotManager;

    public RTDManager(final ParameterParser parser) {
        // open the RTD plotter client connection (one per JVM) and run it in
        // a background thread
        {
            final String host = parser.getRtdHost();
            final int port = parser.getPlotPort();
            this.client = new RTDNetworkClient(host, port);

            final Thread thr = new Thread(this.client);
            thr.start();
        }

        this.plotManager = new PlotManagerBase(this.client) {
            protected void openPlotsChanged() {
                // update each display's menu of open plots
                for (final RtDisplay display : displayList) {
                    display.updatePlotMenu();
                }

                exitIfNecessary();
            }
        };
    }

    /**
     * Create a new RtDisplay and RtDisplayFrame, show them. Non-blocking.
     */
    public void showDisplayUI(final Parameters params) {
        debug.println("showDisplayUI: " + params.getWindowName());
        if (!SwingUtilities.isEventDispatchThread()) {
            throw new RuntimeException("showDisplayUI: called from non-event thread");
        }

        final RtDisplayFrame frame = new RtDisplayFrame();
        frame.addWindowListener(new WindowAdapter() {
            public void windowClosed(WindowEvent e) {
                frameList.remove(frame);
                exitIfNecessary();
            }
        });
        frameList.add(frame);

        frame.showLoadingGraphic();
        frame.pack();
        frame.show();

        // create an uninitialized display
        final RtDisplay display = new RtDisplay(this, params);

        final SwingWorker<UIMessageReply, Object> worker = new SwingWorker<UIMessageReply, Object>() {
            @Override
            public UIMessageReply doInBackground() {
                // connect to network, initialize, but don't show it yet
                return display.connect();
            }

            @Override
            public void done() {
                try {
                    display.processReply(get());
                    frame.setRtDisplay(display);
                } catch (Exception ex) {
                    debug.println("Exception in showDisplayUI: " + ex);
                    frame.showErrorGraphic(ex);
                }
            }
        };

        worker.execute();
    }

    public PlotManager getPlotManager() {
        return this.plotManager;
    }

    public void addDisplay(final RtDisplay display) {
        debug.println("addDisplay: " + display.getParams().getWindowName());
        displayList.add(display);
    }

    public void removeDisplay(final RtDisplay display) {
        debug.println("removeDisplay: " + display.getParams().getWindowName());
        displayList.remove(display);
        exitIfNecessary();
    }

    /**
     * Check if it is necessary to kill the JVM. This happens when all GUI
     * frames have been destroyed.
     */
    private void exitIfNecessary() {
        final int nPlots = this.plotManager.getPlotFrames().size();
        final int nFrames = frameList.size();
        debug.println("exitIfNecessary: nPlots=" + nPlots + " nFrames=" + nFrames);
        if (nFrames == 0 && nPlots == 0) {
            debug.println("no RtDisplayFrame or GenericPlotFrame remain, exit");
            System.exit(0);
        }
    }

    public static void main(String[] args) {
        // Get command line input (if any)
        final ParameterParser parser = new ParameterParser(args);
        if (!parser.isSuccess()) {
            Util.spew("Failure parsing command line input");
            System.exit(0);
        }

        // if we have no applet context then we must have started as an application
        // therefore we need to set a null context
        if (Util.getAppletContext()==null) {
            Util.setContext(null, false);
        }

        // One per JVM
        final RTDManager manager = new RTDManager(parser);

        // Show all requested windows
        final List<Parameters> paramList = parser.getParametersList();
        for (final Parameters params : paramList) {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    manager.showDisplayUI(params);
                }
            });
        }
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
