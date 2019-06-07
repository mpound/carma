package carma.ui.jrtd.ui;

import carma.ui.jrtd.event.FullMouseAdapter;
import carma.ui.jrtd.event.RtStatusListener;

import carma.ui.jrtd.util.MJD;
import carma.ui.jrtd.util.Util;
import carma.ui.jrtd.util.Datum;
import carma.ui.jrtd.util.Debug;
import carma.ui.jrtd.util.FileUtils;
import carma.ui.jrtd.util.Parameters;
import carma.ui.jrtd.util.ProtoBufUtil;
import carma.ui.jrtd.util.DatumContainer;

import carma.ui.jrtd.rtd.RTDManager;
import carma.ui.jrtd.rtd.ServerStream;
import carma.ui.jrtd.rtd.UpdateThread;

import carma.ui.jplotter.plotter.PlotManager;
import carma.ui.jplotter.plotter.PlotProperties;
import carma.ui.jplotter.plotter.GenericPlotFrame;

import java.awt.Font;
import java.awt.Color;
import java.awt.Point;
import java.awt.Window;
import java.awt.Toolkit;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.AWTKeyStroke;
import java.awt.KeyboardFocusManager;

import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.KeyListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.concurrent.FutureTask;

import javax.swing.JMenu;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JComponent;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingWorker;
import javax.swing.ToolTipManager;
import javax.swing.SwingUtilities;

import rtdproto.RTD.UIMessageRequestCode;
import rtdproto.RTD.UIMessageReplyCode;
import rtdproto.RTD.UIMessageRequest;
import rtdproto.RTD.UIMessageReply;

/**
 * An RTD Window, without the JFrame. This class contains all functionality and
 * GUI components of the RTD Window, except the JFrame. This means that we can
 * add and remove this object from a single JFrame without destroying the JFrame
 * and confusing the window manager.
 *
 * @author Steve Scott
 * @author Marc Pound
 * @author Ira W. Snyder
 * @version $Revision: 1.31 $, $Date: 2013/11/19 04:01:14 $, $Author: iws $
 *
 * $CarmaCopyright$
 */
public class RtDisplay extends VBox
    implements RtParentWindow, RtProg, ActionListener, DatumContainer {

    protected static final Debug debug = new Debug("RtDisplay", false);

    protected final RTDManager manager;
    protected RtDisplayFrame parentFrame;

    // the base title
    protected String title;

    // Font specifics
    public static final Font defaultFont = new Font("monospaced", Font.BOLD, 12);
    protected final Font cellFont;
    protected final Font labelFont;

    // Cell specifics
    protected Cell selectedCell;
    // Folders
    protected final List<VBox> folderList = new ArrayList<VBox>();
    protected int currentFolder = 0;
    // some defaults
    protected int sleepTime = 500;
    protected boolean selectionEnabled = false;
    // Start-up info stored  Parameters object
    protected Parameters parameters;
    // Help stuff, generic refers to general program help,
    // specific refers to this display.
    private String genericHelpTitle;
    private String specificHelpTitle;
    private String genericHelp;
    private String specificHelp;
    //
    // Shared GUI
    //
    protected RtMenuBar menuBar;
    protected VBox bottomBox;
    protected HBox timeBox;
    protected RtTimePanel timePanel;

    protected boolean anyAudio = false;
    private final JTabbedPane jTabbedPane_ = new JTabbedPane();
    private final JMenu plotMenu = new JMenu("Plot in existing window");
    protected final MouseListener mouseListener = new MouseA();
    protected final KeyListener keyListener = new DisplayKeyListener();

    // Members originally from carma.ui.jrtd.rtd.Display
    protected ServerStream ss;
    private final UpdateThread updateThread;
    protected double lastUpdate;
    private AudioThread audioThread;
    private final TimeThread timeThread = new TimeThread();
    private final String windowName;
    // the windows that are watching the status (i.e.  plot/list)
    private final List<RtStatusListener> watchers = new ArrayList<RtStatusListener>();

    /**
     * Create a new RtDisplay but steal some of the state from an existing
     * RtDisplay. All of the GUI elements are created from the reinitialization
     * message.
     */
    public static RtDisplay stealParameters(final RtDisplay display, final UIMessageReply msg) {
        debug.println("stealParameters: begin");
        if (!SwingUtilities.isEventDispatchThread()) {
            throw new RuntimeException("stealParameters: called on non-event thread");
        }

        final RTDManager manager = display.getRTDManager();
        final Parameters parameters = display.getParams();

        // stop update thread
        debug.println("stealParameters: stop old update thread");
        display.getUpdateThread().endLoop();

        final RtDisplay nd = new RtDisplay(manager, parameters);
        nd.setSleepTime(display.getSleepTime());
        nd.ss = display.ss;
        display.ss = null;

        // initialize using the new GUI description
        if (!nd.processIni(msg)) {
            throw new RuntimeException("Unable to reinitialize display: " + display.windowName);
        }

        // update data is always present with initialization data, so update
        // now to avoid graphical glitches when this is shown
        if (!nd.processUpd(msg)) {
            throw new RuntimeException("Unable to reinitialize/update display: " + display.windowName);
        }

        debug.println("stealParameters: end");
        return nd;
    }

    /**
     * Construct the RtDisplay. This constructs the entire GUI portion of the
     * display without having any contact with the server process. This means
     * that you have an empty display which is ready to contact the server, but
     * which has made any connections yet.
     */
    public RtDisplay(final RTDManager manager, final Parameters parameters) {
        super("Page", VBox.Border.NO_BORDER);

        this.manager = manager;
        this.parameters = parameters;
        this.sleepTime = parameters.getUpdateRate();
        this.windowName = parameters.getWindowName();

        // Disable tooltips permanently
        ToolTipManager.sharedInstance().setEnabled(false);

        removeDefaultFocusTraversal();

        // Required to create the peer so that fonts will exist
        addNotify();

        {
            this.cellFont = new Font("monospaced", Font.BOLD, 12);
            this.labelFont = new Font("Serif", Font.BOLD, 14);

            RtLabel.setDefaultFont(labelFont);

            // Set the font for menus, etc.
            // The default font is the prettiest; use it if we can -
            // otherwise it's Sansserif
            String fontName = "Sansserif";
            Font font = getFont();
            if (font != null) {
                String s = font.getName();
                if (s != null)
                    fontName = s;
            }

            setFont(new Font(fontName, Font.PLAIN, 12));
        }

        this.updateThread = new UpdateThread(this);
        timeThread.start();

        // inform the manager about ourselves
        manager.addDisplay(this);

        debug.println("At end of display constructor");
    }

    // The Swing default focus traversal will consume all the Tab/Shift-Tab
    // events that we want to use for our own focus traversal implementaion
    static void removeDefaultFocusTraversal() {
        final java.util.Set<AWTKeyStroke> emptyset = Collections.emptySet();
        KeyboardFocusManager kfm =
            KeyboardFocusManager.getCurrentKeyboardFocusManager();
        kfm.setDefaultFocusTraversalKeys(
                KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS,
                emptyset);
        kfm.setDefaultFocusTraversalKeys(
                KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS,
                emptyset);
    }

    /**
     * Accessor for the font used for labels
     */
    public Font getLabelFont() {
        return labelFont;
    }
    /**
     * Accessor for the font used for cells
     */
    public Font getCellFont() {
        return cellFont;
    }

    /**
     * Accessor for the timePanel
     */
    public RtTimePanel getTimePanel() {
        return timePanel ;
    }
    /**
     * Mutatoror for the timePanel
     */
    public void setTimePanel(RtTimePanel t) {
        timePanel = t;
    }

    public void setRtMenuBar(RtMenuBar mb) {
        this.menuBar = mb;
    }

    public RtMenuBar getRtMenuBar() {
        return this.menuBar;
    }

    /**
     * Used for convenient access to the mouse events for Cell
     */
    @Override
    protected void processMouseEvent(MouseEvent e) {
        super.processMouseEvent(e);
    }

    /**
     * Set the parameters of the generic help for this program
     * @param <code>t</code> a title for the generic help frame.
     * @param <code>s</code> the (typically long) help string to be displayed
     *			 in the generic help frame
     */
    public void setGenericHelp(String t, String s) {
        genericHelpTitle = t;
        genericHelp      = s;
    }

    /**
     * Set the parameters of the specific help for this RtDisplay
     * @param <code>t</code> a title for the specifc help frame.
     * @param <code>s</code> the (typically long) help string to be displayed
     *			 in the specific help frame
     */
    public void setSpecificHelp(String t, String s) {
        specificHelpTitle = t;
        specificHelp      = s;
    }

    /**
     * Returns the title of the generic help frame
     * @return the title of the generic help frame
     */
    public String getGenericHelpTitle() {
        return genericHelpTitle;
    }

    /**
     * What does generic help say?
     * @return the contents of the generic help frame
     */
    public String getGenericHelp() {
        return genericHelp;
    }

    /**
     * Returns the title of the specific help frame
     * @return the title of the specific help frame
     */
    public String getSpecificHelpTitle() {
        return specificHelpTitle;
    }

    /**
     * What does specific help say?
     * Whatever you want it to say about this specific frame.
     * @return the contents of the specific help frame
     */
    public String getSpecificHelp() {
        return specificHelp;
    }

    /**
     * Set the base title of this RtDisplay, also
     * set the Frame title to <code>s</code>
     * @param <code>s</code> the new base title String
     */
    public void setBaseTitle(String s) {
        title = s;
        setTitle(s);
    }

    public void setTitle(String s) {
        final JFrame f = this.parentFrame;
        if (f != null)
            f.setTitle(s);
    }

    public String getTitle() {
        final JFrame f = this.parentFrame;
        return (f == null ? "" : f.getTitle());
    }

    /**
     * Get the base title of this RtDisplay, which is not
     * necessarily the current Frame title
     * @return the current base title String
     */
    public String getBaseTitle() {
        return title;
    }


    //=================================================================
    // Cells, Folders
    //=================================================================

    /**
     * We only display the audio button if there is an audio cell somewhere
     * If any cells have audio capabilities, then set <code>anyAudio<code>
     * to true;
     */
    protected void setAnyAudio() {
        anyAudio = false;
        for (final Datum d : this.datumList) {
            if (d instanceof Cell) {
                final Cell c = (Cell)d;
                if (c.hasAudio()) {
                    anyAudio = true;
                    return;
                }
            }
        }
    }

    public void addFolder(final VBox box) {
        this.folderList.add(box);

        // Pick up font size for the tabs themselves
        jTabbedPane_.setFont(box.getFont());
        jTabbedPane_.addTab(box.getName(), new JScrollPane(box));
    }

    private void spew(String s) {
        System.out.println(s);
    }

    /**
     * Returns a folder.
     * @return the folder
     */
    public VBox getFolder(int index) {
        return this.folderList.get(index);
    }

    /**
     * Returns the Cell which currently has focus.
     * @return the Cell that is currently selected (has focus)
     */
    public Cell getSelectedCell() {
        return selectedCell;
    }

    /**
     * Set focus on a given Cell.
     * @param <code>c</code> the Cell to select.
     * @param <code>click</code> true if this selection came from mouse click,
     */
    public void setSelectedCell(Cell c, boolean click) {
        selectionEnabled = true;
        setSelectedCell(c);
    }

    /**
     * Select a given cell. But don't set the focus on it.
     * Focus is on the Frame,
     * while our selected cell is a construct below that.
     * @param <code>c</code> the Cell on which to focus.
     */
    public void setSelectedCell(Cell c) {
        //First event can be bogus (from system placing focus on first item)
        if (!selectionEnabled)c=null;
        menuBar.updateCellMenu(c);
        // deselect previous cell
        if (selectedCell != null) {
            selectedCell.setSelected(false);
        }
        // select new cell
        selectedCell = c;
        if (selectedCell != null) {
            selectedCell.setSelected(true);
        }
        // Return focus to the frame, just in case it gets set somewhere else
        // (like the buttons)
        requestFocus();
    }

    // check for triggered audio
    boolean anyAudioTriggered() {
        for (final Datum d : this.datumList) {
            if (d instanceof Cell) {
                final Cell c = (Cell)d;
                if (c.isAudioTriggered())
                    return true;
            }
        }

        return false;
    }

    UpdateThread getUpdateThread() {
        return this.updateThread;
    }

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Message Processing                                     */
    /* ---------------------------------------------------------------------- */

    public synchronized UIMessageReply connect() {
        this.ss = new ServerStream(parameters);

        // now connect the display to the network and initialize
        if (!this.ss.connect(this.windowName)) {
            return null;
        }

        // initialize
        return init();
    }

    /**
     * Process any type of reply message from the server.
     *
     * @param msg the message to process
     * @return true if the reply was processed successfully, false otherwise
     */
    public boolean processReply(final UIMessageReply msg) {
        if (!SwingUtilities.isEventDispatchThread()) {
            throw new RuntimeException("processReply called on non-event thread!");
        }

        if (msg.getCode() == UIMessageReplyCode.REP_INITIALIZE) {
            debug.println("processReply: begin initialization");
            final boolean ret = processIni(msg);
            debug.println("processReply: end initialization");
            return ret;
        } else if (msg.getCode() == UIMessageReplyCode.REP_REINITIALIZE) {
            debug.println("processReply: begin reInitialization");
            final boolean ret = processRei(msg);
            debug.println("processReply: end reInitialization");
            return ret;
        } else if (msg.getCode() == UIMessageReplyCode.REP_UPDATE) {
            debug.println("processReply: begin processUpd");
            final boolean ret = processUpd(msg);
            debug.println("processReply: end processUpd");
            return ret;
        } else {
            Util.spew("Unknown message code: " + msg.getCode());
            return false;
        }
    }

    /**
     * Send an initialization request to the server, return the response.
     */
    public synchronized UIMessageReply init() {
        final UIMessageRequest req = UIMessageRequest.newBuilder()
            .setCode(UIMessageRequestCode.REQ_INITIALIZE)
            .setSleeptime(this.sleepTime)
            .build();

        return ss.sendRecv(req);
    }

    /**
     * Process an initialization message.
     */
    private synchronized boolean processIni(final UIMessageReply msg) {
        debug.println("At beginning of processIni");

        if (!msg.hasDisplay()) {
            debug.println("processIni: RtDisplay is required");
            return false;
        }

        final rtdproto.RTD.RtDisplay pb = msg.getDisplay();
        if (!pb.hasStaticdata()) {
            debug.println("processIni: RtDisplay::StaticData is required");
            return false;
        }

        final rtdproto.RTD.RtDisplay.StaticData sd = pb.getStaticdata();

        setBaseTitle(sd.getTitle());
        // TODO FIXME: border code is ignored
        final int fontsize = sd.getFontsize() + getParams().getDeltaFontSize();
        debug.println("Fontsize: " + fontsize);

        // increase font size if requested
        {
            final Font font = getFont();
            final Font newFont = new Font(font.getFamily(), font.getStyle(), fontsize);
            setFont(newFont);
        }

        // add generic help
        {
            final rtdproto.RTD.RtDisplay.RtHelp help = sd.getGenerichelp();
            setGenericHelp(help.getTitle(), help.getText());
        }

        // add specific help
        {
            final rtdproto.RTD.RtDisplay.RtHelp help = sd.getSpecifichelp();
            setSpecificHelp(help.getTitle(), help.getText());
        }

        debug.println("processIni: process object list");
        ProtoBufUtil.processObjectList(this, this, this, pb.getObjectsList());
        debug.println("processIni: set up folders");

        // setup folders
        {
            final BoxConstraints bc = new BoxConstraints();
            bc.stretchType = BoxConstraints.Stretch.SPRING;
            bc.alignment = BoxConstraints.Align.FILL;

            if (this.folderList.size() > 1) {
                jTabbedPane_.setBackground(Color.white);
                jTabbedPane_.setInputMap(JComponent.WHEN_FOCUSED, null);

                add(jTabbedPane_, bc);
                currentFolder = jTabbedPane_.getSelectedIndex();
            } else {
                // added scrollpane here for non-tabbed windows
                add(new JScrollPane(getFolder(0)), bc);
            }
        }

        setAnyAudio();

        if (this.timePanel != null) {
            boolean v = !timePanel.isInvisible();
            if (v)
                v = parameters.hasTimeBar();

            timePanel.setVisible(v);
        }

        {
            final BoxConstraints bc = new BoxConstraints();
            bc.stretchType = BoxConstraints.Stretch.FIXED;
            bc.alignment = BoxConstraints.Align.FILL;

            makeBottomBox();
            if (bottomBox != null)
                add(bottomBox, bc);
        }

        {
            final Dimension size = parameters.getSize();
            if (size != null)
                setSize(size);
        }

        {
            final Point pos = parameters.getPosition();
            if (pos != null)
                setLocation(pos);
        }

        if (anyAudio) {
            audioThread = new AudioThread();
            audioThread.start();
        }

        updatePlotMenu();
        updateThread.start();

        debug.println("At end of processIni");
        return true;
    }

    /**
     * Uncompress (if required) the update stream and then update the components.
     * If we have data in raw form (uncompressed), then it is allowed to have a variable length.
     * To accomodate this, the master array is allowed to shrink and grow. This type of data
     * is usually non-cell data that changes on each update and has a veriable length data
     * stream that it sends.
     */
    private synchronized boolean processUpd(final UIMessageReply msg) {
        debug.println("processUpd: begin");

        if (!msg.hasDisplay()) {
            debug.println("processUpd: RtDisplay is required");
            return false;
        }

        final rtdproto.RTD.RtDisplay pb = msg.getDisplay();
        ProtoBufUtil.updateObjects(pb.getObjectsList(), datumList);
        return true;
    }

    private synchronized boolean processRei(final UIMessageReply msg) {
        debug.println("processRei: begin");
        final RtDisplay display = stealParameters(this, msg);
        final RtDisplayFrame pf = this.parentFrame;
        if (pf != null)
            pf.setRtDisplay(display);

        return true;
    }

    //=================================================================
    // GUI objects
    //=================================================================

    /**
     * Create a brand new window. Used by the RtMenuBar "NewWin" menu.
     * This will return immediately, before the new window becomes visible.
     */
    public void createNewWindow(String winType) {
        if (!SwingUtilities.isEventDispatchThread()) {
            throw new RuntimeException("createNewWindow: called on non-event thread");
        }

        final Parameters params = (Parameters)parameters.clone();
        if (!windowName.equals("")) params.setWindowName(windowName);
        if (!winType.equals("")) params.setWindowName(winType);

        manager.showDisplayUI(params);
    }

    /**
     * Morph this window into a new one. Used by the RtMenuBar "Morph" menu.
     * This will return immediately, before the new display is ready.
     */
    public void morphWindow(String winType) {
        if (!SwingUtilities.isEventDispatchThread()) {
            throw new RuntimeException("morphWindow: called on non-event thread");
        }

        final Parameters params = (Parameters)parameters.clone();
        if (!windowName.equals("")) params.setWindowName(windowName);
        if (!winType.equals("")) params.setWindowName(winType);

        final RtDisplay newDisplay = new RtDisplay(manager, params);

        // show the loading graphic now
        parentFrame.showLoadingGraphic();

        final SwingWorker<UIMessageReply, Object> worker = new SwingWorker<UIMessageReply, Object>() {
            @Override
            public UIMessageReply doInBackground() {
                return newDisplay.connect();
            }

            @Override
            public void done() {
                try {
                    final UIMessageReply msg = get();
                    newDisplay.processReply(msg);
                    parentFrame.setRtDisplay(newDisplay);
                } catch (Exception ex) {
                    debug.println("Exception in morphWindow: " + ex);
                    parentFrame.showErrorGraphic(ex);
                }
            }
        };

        worker.execute();
    }

    /**
     * Get the "Plot in existing window" menu
     */
    public JMenu getPlotMenu() {
        return this.plotMenu;
    }

    /**
     * Update the list of window titles in the "Plot in existing window" menu
     */
    public void updatePlotMenu() {
        final JMenu menu = this.plotMenu;
        final PlotManager pm = this.manager.getPlotManager();

        // remove all existing items
        menu.removeAll();

        // add all new items
        final List<GenericPlotFrame> frameList = pm.getPlotFrames();
        for (final GenericPlotFrame frame : frameList) {
            final PlotProperties props = frame.getPlotProperties();

            // skip plots which are not time series
            if (!props.isTimeSeriesPlot())
                continue;

            final JMenuItem item = new JMenuItem(props.getString("plot_title"));
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    addToPlotAction(frame);
                }
            });

            menu.add(item);
        }

        // enable the menu only if there are open plots
        menu.setEnabled(frameList.size() > 0);
    }

    //=================================================================

    /**
     * How to dispose of any windows watching this window
     */
    public void closeWatchers() {
        //inform all the children windows (plots/lists) that the parent has died
        for (RtStatusListener i : watchers) {
            i.bailOut();
        }

        watchers.clear();
    }

    /**
     * what to do when a Plot panel is requested
     */
    public void plotAction() {
        final PlotProperties props = new PlotProperties();
        addPlotRegister(props);

        final PlotManager pm = this.manager.getPlotManager();
        pm.showPlotUI(props);
    }

    /**
     * what to do when a AddToPlot is requested
     */
    public void addToPlotAction(final GenericPlotFrame frame) {
        final PlotProperties props = frame.getPlotProperties();
        addPlotRegister(props);
        frame.getPlotPanel().updateRegisters();
    }

    private void addPlotRegister(final PlotProperties props) {
        if (!props.isTimeSeriesPlot())
            return;

        final Cell cell = getSelectedCell();
        if (cell == null)
            return;

        if (!cell.isPlot())
            return;

        final String mpName = cell.getMpName();
        if (mpName == null || mpName.equals("") || mpName.equals("none"))
            return;

        final String[] origRegisters = props.getStringArray("yaxis_registers");
        final int len = (origRegisters == null ? 0 : origRegisters.length);

        // skip adding registers which are already plotted
        for (int i = 0; i < len; i++) {
            if (mpName.toLowerCase().equals(origRegisters[i].toLowerCase()))
                return;
        }

        final String[] newRegisters = new String[len + 1];

        if (len > 0) {
            System.arraycopy(origRegisters, 0, newRegisters, 0, len);
        }

        newRegisters[len] = mpName;

        props.setStringArray("yaxis_registers", newRegisters);
    }

    /**
     * what to do when a TextWin is requested
     */
    public void listAction() {
        TextWin newWindow = new TextWin(getSelectedCell());
        WindowUtils.avoidParent(newWindow, parentFrame);
        watchers.add(newWindow);
    }

    /**
     * what to do when a StatsWin is requested
     */
    public void statsAction() {
        StatsWin newWindow = getSelectedCell().mkStatsWin();
        WindowUtils.avoidParent(newWindow, parentFrame);
        watchers.add(newWindow);
    }

    /**
     * what to do when an audio event is requested
     * (toggle Audio on or off)
     */
    public void audioAction() {
        getSelectedCell().toggleAudio();
    }

    /**
     * what to do when the a print job is requested
     */
    public void printAction() {
        this.folderList.get(currentFolder).printIt();
    }

    /**
     * respond to help about request, i.e. show the HelpAbout frame
     */
    public void helpAboutAction() {
        new HelpAbout(this.parentFrame);
    }

    /**
     * respond to generic help about request
     */
    public void helpGenericAction() {
        new Help(getGenericHelpTitle(),  getGenericHelp());
    }

    /**
     * respond to a specific help
     */
    public void helpSpecificAction() {
        new Help(getSpecificHelpTitle(),  getSpecificHelp());
    }

    //---------------------------------------------------------------------------------------------------

    /**
     * Make the bottom box containing the optional time panel and button panel.
     */
    public void makeBottomBox() {
        if (timePanel == null) {
            Util.spew ("makeBottomBox: null timePanel");
            return;
        }
        bottomBox          = new VBox("bottomBox", VBox.Border.TWO_PIXELS_LEFT_RIGHT_BOTTOM_BORDER);
        BoxConstraints bc  = new BoxConstraints(
                BoxConstraints.Stretch.SPRING, BoxConstraints.Align.FILL);

        // Put the time panel in an HBox and add to the bottom box
        timeBox = new HBox("timeBox", HBox.Border.ONE_PIXEL_BELOW_BORDER);
        timeBox.setBackground(Color.white);
        bc.stretchType = BoxConstraints.Stretch.SPRING;
        timeBox.add(timePanel, bc);
        bc.stretchType = BoxConstraints.Stretch.INCOMPRESSIBLE;
        bottomBox.add(timeBox, bc);

        timeBox.setVisible(timePanel.isVisible());
    }

    /**
     * Causes bottom box to be redrawn.
     */
    protected void redisplayBottomBox() {
        timeBox.setVisible(timePanel.isVisible());
        // Force a new layout of this and everything above it
        timeBox.invalidate();
        validate(); // Do the layout of the whole frame

        // The reqFoc doesn't work. The system sets it to the buttons async,
        // after the box appears.
        // Return focus to the frame, not in this panel (buttons!)
        requestFocus();
    }

    /* ---------------------------------------------------------------------- */
    /* RtParentWindow Interface                                               */
    /* ---------------------------------------------------------------------- */

    /**
     * Descriptive info about this RtDisplay
     * @return the Parameters describing this RtDisplay
     */
    @Override
    public Parameters getParams() {
        return parameters;
    }

    /**
     * what to do on window closing or quit.
     */
    @Override
    public void bailOut() {
        debug.println("bailOut: called");

        UpdateThread upt = getUpdateThread();
        if (upt != null) upt.endLoop();

        // When we are stealing a previous RtDisplay's connection, we don't
        // want to shut it down. The makeRtDisplay() "steal" version handles
        // setting this to null.
        if (ss != null)
            ss.close();

        timeThread.kill();
        if (audioThread != null)
            audioThread.kill();

        setVisible(false);

        // inform all the children window that the parent has died (plots/lists)
        for (RtStatusListener i : watchers) {
            i.sourceDied();
        }

        // kill the parent frame
        if (parentFrame != null)
            parentFrame.dispose();

        // notify the RTDManager that we're dead
        manager.removeDisplay(this);
    }

    @Override
    public RTDManager getRTDManager() {
        return this.manager;
    }

    /* ---------------------------------------------------------------------- */
    /* RtProg Interface                                                       */
    /* ---------------------------------------------------------------------- */

    /**
     * Get update data from the server.
     */
    @Override
    public synchronized boolean update() {
        final UIMessageRequest req = UIMessageRequest.newBuilder()
            .setCode(UIMessageRequestCode.REQ_UPDATE)
            .setSleeptime(this.sleepTime)
            .build();

        final SwingWorker<UIMessageReply, Object> worker = new SwingWorker<UIMessageReply, Object>() {
            @Override
            public UIMessageReply doInBackground() {
                debug.println("update: send REQ_UPDATE message");
                final UIMessageReply msg = ss.sendRecv(req);
                lastUpdate = new MJD().mjd;
                return msg;
            }

            @Override
            public void done() {
                try {
                    final UIMessageReply msg = get();
                    if (msg != null)
                        processReply(msg);
                } catch (Exception ex) {
                    System.err.println("Exception during RtDisplay.update()");
                    ex.printStackTrace();
                }
            }
        };

        worker.execute();
        return true;
    }

    /**
     * get the default sleep time between updates.
     */
     @Override
     public int getSleepTime() {
         return this.sleepTime;
     }

    /**
     * set the default sleep time between updates.
     * @param <code>st</code> the sleeptime in milliseconds
     */
    @Override
    public void setSleepTime(int st) {
        this.sleepTime = st;

        // notify the update thread so that it can recalculate
        if (updateThread.isRunning())
            updateThread.interrupt();
    }

    /* ---------------------------------------------------------------------- */
    /* ActionListener Interface                                               */
    /* ---------------------------------------------------------------------- */

    /**
     * The required action listener method
     * If subclasses have other ActionEvents to listen for
     * they should add additional action listener(s)
     * to handle those other ActionEvents.
     * Alternatively, subclasses could override this method,
     * but that is not recommended.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        final String cmd = e.getActionCommand();

        if (cmd == null) {
            return;
        } else if (cmd.equals("Load Saved Plot")) {
            final PlotManager pm = this.manager.getPlotManager();
            final List<PlotProperties> propList = pm.loadPlotConfiguration(this.parentFrame);
            for (final PlotProperties props : propList) {
                pm.showPlotUI(props);
            }
        } else if (cmd.equals("Save All Open Plots")) {
            final PlotManager pm = this.manager.getPlotManager();
            pm.savePlotConfigurationAll(this.parentFrame);
        } else if (cmd.equals("New Time Series Plot")) {
            final PlotManager pm = this.manager.getPlotManager();
            pm.configureNewPlot(this.parentFrame, "Time Series");
        } else if (cmd.equals("New X-Y Plot")) {
            final PlotManager pm = this.manager.getPlotManager();
            pm.configureNewPlot(this.parentFrame, "Arbitrary X-Y Plot");
        } else if (cmd.equals("Take Screenshot")) {
            FileUtils.saveScreenShot(this, getTitle() + ".png");
        } else if (cmd.equals("Close")) {
            bailOut();
        } else if (cmd.equals("Exit")) {
            System.exit(0);
        } else if (cmd.equals("CloseWatchers")) {
            closeWatchers();
        } else if (cmd.equals("Plot")) {
            plotAction();
        } else if (cmd.equals("List")) {
            listAction();
        } else if (cmd.equals("Stats")) {
            statsAction();
        } else if (cmd.equals("Audio")) {
            audioAction();
        } else if (cmd.equals("Print")) {
            printAction();
        } else if (cmd.equals("helpGeneric")) {
            helpGenericAction();
        } else if (cmd.equals("helpThis")) {
            helpSpecificAction();
        } else if (cmd.equals("helpVersion")) {
            helpAboutAction();
        }
    }

    /* ---------------------------------------------------------------------- */
    /* DatumContainer Interface                                               */
    /* ---------------------------------------------------------------------- */

    protected final List<Datum> datumList = new ArrayList<Datum>();

    public Datum addDatum(final Datum datum) {
        this.datumList.add(datum);
        return datum;
    }

    public int getDatumCount() {
        return this.datumList.size();
    }

    public Datum getDatum(final int index) {
        return this.datumList.get(index);
    }

    /* ---------------------------------------------------------------------- */
    /* Inner Classes                                                          */
    /* ---------------------------------------------------------------------- */

    class AudioThread extends Thread {
        private boolean keepGoing = true;

        void kill() {
            keepGoing = false;
            interrupt();
        }

        @Override
        public void run() {
            while (keepGoing) {
                if (keepGoing && anyAudioTriggered()) {
                    // Issue 4 beeps (one every quarter second)
                    for (int i = 0; (i < 4) && keepGoing; i++) {
                        Toolkit.getDefaultToolkit().beep();
                        try {
                            sleep(250);
                        } catch (InterruptedException e) {
                            // stifle
                        }
                    }
                }
                // Sleep for one minute
                if (keepGoing) {
                    try {
                        sleep(60000);
                    } catch (InterruptedException e) {
                        // stifle
                    }
                }
            }

            debug.println("Audiothread exiting...");
        }
    }

    /**
     * A thread that periodically determines if the server connection
     * is still alive by  checking the time of last packet. If the
     * connection is dead, it changes the Frame title and also notifies
     * the timePanel if one is registered.
     */
    class TimeThread extends Thread {
        final int timeOut                 = 15;    // Timeout time in seconds
        private boolean keepGoing         = true;  // Used to stop the thread
        private boolean connectionDead    = false;
        private boolean oldConnectionDead = false;

        // Kill the thread
        void kill() {
            keepGoing = false;
            interrupt();
        }

        @Override
        public void run() {
            while (keepGoing) {
                // Once a second if we have a timePanel
                int updateTime = (timePanel == null)? 5000: 1000;
                //Util.spew("Time thread loops again...");
                try {
                    sleep(updateTime);
                } catch (InterruptedException e) {}
                if(!keepGoing) {
                    debug.println("Exiting time thread");
                    return;
                }

                double now = new MJD().mjd;
                double sinceLastUpdate = 86400*(now - lastUpdate);
                // A connection is dead if we haven't had a new packet in sleeptime+timeOut
                connectionDead = (sinceLastUpdate > (timeOut + sleepTime/1000));

                // if the status of the connection has changed - died/recovered then...
                if (connectionDead != oldConnectionDead) {

                    // if the connection is dead, tell all the watching windows the connection is asleep
                    // and put a stalled message at the top of the window
                    if (connectionDead) {
                        for ( RtStatusListener i : watchers ) {
                            i.sourceAbsent();
                        }
                        setTitle(title+"<!! Stalled !!>");
                    }
                    else {
                        setTitle(title);
                    }
                    oldConnectionDead = connectionDead;
                }
                if (timePanel != null)timePanel.update(connectionDead );
            }
            debug.println("Exiting time thread");
        }
    }

    /**
     * Inner class for standard mouse events.
     */
    public class MouseA extends FullMouseAdapter {
        /**
         * Common code for mouse events.
         *
         * Several mouse events are handled with exactly the same code, so it
         * has been centralized in this function to avoid duplication.
         */
        private void mouseCommonCode(MouseEvent e) {
            final Object o = e.getSource();
            if (!(o instanceof Cell)) {
                setSelectedCell(null, true);
                return;
            }

            final Cell c = (Cell)o;
            if (c.getIgnoreFocus())
                return;

            setSelectedCell(c, true);
            folderList.get(currentFolder).repaint();

            if (e.isPopupTrigger()) {
                final JPopupMenu menu = c.getPopupMenu();
                menu.show(e.getComponent(), e.getX(), e.getY());
            }
        }

        public void mouseReleased(MouseEvent e) {
            mouseCommonCode(e);
        }

        public void mousePressed(MouseEvent e) {
            mouseCommonCode(e);
        }
    }

    /**
     * Inner class for catching KeyEvents
     */
    public class DisplayKeyListener implements KeyListener {
        int key;

        public void keyTyped(KeyEvent e) {
            key = e.getKeyCode();
        }

        public void keyPressed(KeyEvent e) {
            key = e.getKeyCode();
            Object ko = e.getSource();
            switch(key) {
            case KeyEvent.VK_P:
                if (folderList.size() > 1) {
                    int i = jTabbedPane_.getSelectedIndex() - 1;
                    if (i < 0)
                        i = jTabbedPane_.getTabCount() - 1;

                    currentFolder = i;
                    jTabbedPane_.setSelectedIndex(i);
                }
                break;
            case KeyEvent.VK_N:
                if (folderList.size() > 1) {
                    int i = jTabbedPane_.getSelectedIndex() + 1;
                    if (i >= jTabbedPane_.getTabCount())
                        i = 0;

                    currentFolder = i;
                    jTabbedPane_.setSelectedIndex(i);
                }
                break;
            }

            //Don't consume the event! Others will need it.
        }

        public void keyReleased(KeyEvent e) {
        }
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
