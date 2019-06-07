package carma.ui.jrtd.ui;

import carma.ui.jrtd.event.*;
import carma.ui.jrtd.util.*;

import java.awt.event.KeyEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ItemListener;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;
import javax.swing.ToolTipManager;
import javax.swing.JCheckBoxMenuItem;

import java.util.ArrayList;

/**
 * The RtMenuBar is a menu for the RtDisplay class.
 *
 * @author    $Author: iws $
 * @version   $Revision: 1.11 $
 */
public class RtMenuBar extends JMenuBar implements Datum {
    private static final int ksctrl = KeyEvent.CTRL_DOWN_MASK;
    private static final int ksctrlshift = KeyEvent.CTRL_DOWN_MASK | KeyEvent.SHIFT_DOWN_MASK;

    private final RtDisplay parent;
    private final JMenu monitorMenu;

    /**
     * Creates a menu. 
     *
     * @param parent the parent RtDisplay of this JMenuBar.
     * @param windows new windows that can be instantiated from this menu.
     */
    public RtMenuBar(RtDisplay parent, final java.util.List<MenuPair> windows, int style) {
        super();

        this.parent = parent;

        // main menus
        this.add(createFileMenu());

        {
            final JMenu menu = new JMenu("NewWin");
            createWindowMenu(windows, 0, menu, false, false);
            this.add(menu);
        }

        {
            final JMenu menu = new JMenu("Morph");
            createWindowMenu(windows, 0, menu, false, true);
            this.add(menu);
        }

        if (style != Style.NOCELL) {
            this.monitorMenu = createMonitorMenu(parent);
            this.add(this.monitorMenu);
        } else {
            this.monitorMenu = null;
        }

        this.add(createPrefsMenu());

        // move all remaining items to far right side of menu bar
        this.add(Box.createHorizontalGlue());

        {
            final JMenu help = createHelpMenu();
            this.add(help);
        }

        // Disable tooltips permanently
        ToolTipManager.sharedInstance().setEnabled(false);
    }

    /**
     * Create a menu from a list of menu names and window names (pairs of strings).
     * Operates recursively to create menus inside of menus.
     * An optional single, double or triple character prefix is used as a code to identify
     * nested menus, control only items, and disabled items. The control prefix *must* be
     * first, followed by disable.
     *   Code    Interpretation
     *     ~     Control only - don't display if no control capabilities
     *     *     Disable
     *     +     Start a new nested sub-menu with the name that follows
     *     -     End of nested sub-menu (can only be single character - no prefix)
     * @param <code>choices</code> Pairs of strings representing menu designation
     *                             and window startup codes
     * @param <code>index</code>   Current index in the choices pairs to begin work
     * @param <code>menu</code>    Menu to attach created menu items to
     * @param <code>suppress</code> Don't display menu item (e.g. requires control
     *                             capability but program doesn't have it)
     * @param <code>morphRequested</code> Control flag for morphing or new window
     * @return integer index of string pair (in choices) to continue menu creation
     *                 (for recursion)
     *
     */
    public int createWindowMenu(final java.util.List<MenuPair> choices,
            int index, JMenu menu, boolean suppress, boolean morphRequested) {
        int i;
        for (i = index; i < choices.size(); i++) {
            String menuName = choices.get(i).menuName;
            boolean disabled = false;
            boolean suppressItem = suppress;

            if (menuName.startsWith("~")) {
                menuName = menuName.substring(1);
                suppressItem = suppress || !Util.canControl();
            }

            if (menuName.startsWith("*")) {
                menuName = menuName.substring(1);
                disabled = true;
            }

            if (menuName.startsWith("-"))
                return i;

            if (menuName.startsWith("+")) {
                i++;
                final JMenu nestedMenu = new JMenu(menuName.substring(1));
                if (disabled)
                    nestedMenu.setEnabled(false);

                if (!suppressItem)
                    menu.add(nestedMenu);

                i = createWindowMenu(choices, i, nestedMenu, suppressItem, morphRequested);
            } else if (!suppressItem) {
                final JMenuItem newItem = new JMenuItem(menuName);
                final String codeName = choices.get(i).codeName.trim();
                final RtMenuListener rml = new RtMenuListener(parent, codeName, morphRequested);
                newItem.addActionListener(rml);

                if (disabled)
                    newItem.setEnabled(false);

                menu.add(newItem);
            }

            if (!suppressItem && menuName.endsWith(" ")) {
                menu.addSeparator();
            }
        }

        return i + 1;
    }

    /**
     * Updates the menubar to reflect currently selected cell
     * @param the currently selected cell
     */
    public void updateCellMenu(final Cell c) {
        // no monitor menu: exit immediately
        if (this.monitorMenu == null)
            return;

        // enable and disable cell-specific menus based on cell abilities
        final boolean haveCell = (c != null);
        for (int i = 0; i < this.monitorMenu.getItemCount(); i++) {
            final JMenuItem item = this.monitorMenu.getItem(i);
            if (item == null)
                continue;

            if (item.getText().equals("Plot")) {
                item.setEnabled(haveCell && c.isPlot());
            } else if (item.getText().equals("Statistics")) {
                item.setEnabled(haveCell && c.isPlot());
            } else if (item.getText().equals("List")) {
                item.setEnabled(haveCell && true);
            } else if (item.getText().equals("Control")) {
                item.setEnabled(haveCell && c.isControlEnabled());
            } else if (item.getText().equals("Audio")) {
                item.setEnabled(haveCell && c.hasAudio());
            }
        }
    }

    /**
     * Create the File menu
     */
    private JMenu createFileMenu() {
        final JMenu menu = new JMenu("File");
        final String os = System.getProperty("os.name").toLowerCase();
        final java.util.List<BasicAction> actions = new ArrayList<BasicAction>();

        actions.add(new BasicAction("New Time Series Plot", "New Time Series Plot", true, null));
        actions.add(new BasicAction("New X-Y Plot", "New X-Y Plot", true, null));
        actions.add(new BasicAction("Load Saved Plot", "Load Saved Plot", true, null));
        actions.add(new BasicAction("Save All Open Plots", "Save All Open Plots", true, null));
        actions.add(new BasicAction("Take Screenshot", "Take Screenshot", true, null));
        actions.add(new BasicAction("Print", "Print", true, KeyStroke.getKeyStroke(KeyEvent.VK_P, ksctrlshift)));

        if (os.indexOf("mac os x") != -1) {
            actions.add(new BasicAction("Close", "Close", true, KeyStroke.getKeyStroke(KeyEvent.VK_W, ksctrl)));
        } else {
            actions.add(new BasicAction("Close", "Close", true, KeyStroke.getKeyStroke(KeyEvent.VK_Q, ksctrl)));
            actions.add(new BasicAction("Exit", "Exit", true, KeyStroke.getKeyStroke(KeyEvent.VK_C, ksctrl)));
        }

        for (final BasicAction action : actions) {
            final JMenuItem item = action.asMenuItem();
            menu.add(item);
            item.addActionListener(this.parent);
        }

        return menu;
    }

    /**
     * Create the Monitor menu
     */
    private JMenu createMonitorMenu(final RtDisplay parent) {
        final JMenu menu = new JMenu("Monitor");

        // add sleep time menu
        menu.add(new SleepTimeMenu(parent));

        final java.util.List<BasicAction> actions = new ArrayList<BasicAction>();
        actions.add(new BasicAction("Plot", "Plot", false, KeyStroke.getKeyStroke(KeyEvent.VK_G, ksctrl)));
        actions.add(new BasicAction("List", "List", false, KeyStroke.getKeyStroke(KeyEvent.VK_L, ksctrl)));
        actions.add(new BasicAction("Statistics", "Stats", false, KeyStroke.getKeyStroke(KeyEvent.VK_I, ksctrl)));
        actions.add(new BasicAction("Close List/Stat", "CloseWatchers", true, KeyStroke.getKeyStroke(KeyEvent.VK_Q, ksctrlshift)));
        actions.add(new BasicAction("Audio", "Audio", false, KeyStroke.getKeyStroke(KeyEvent.VK_A, ksctrl)));
        actions.add(new BasicAction("Control", "Control", false, KeyStroke.getKeyStroke(KeyEvent.VK_C, ksctrlshift)));

        for (final BasicAction action : actions) {
            final JMenuItem item = action.asMenuItem();
            menu.add(item);
            item.addActionListener(this.parent);
        }

        return menu;
    }

    /**
     * Create the Prefs menu
     */
    private JMenu createPrefsMenu() {
        final JMenu menu = new JMenu("Prefs");

        final RtTimePanel tp = parent.getTimePanel();
        final JCheckBoxMenuItem item = new JCheckBoxMenuItem("Time Bar");
        item.setState(tp.isVisible());
        item.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                tp.setVisible(!tp.isVisible());
                parent.redisplayBottomBox();
            }
        });

        menu.add(item);
        return menu;
    }

    /**
     * Create the Help menu
     */
    private JMenu createHelpMenu() {
        final JMenu menu = new JMenu("Help");
        final java.util.List<BasicAction> actions = new ArrayList<BasicAction>();

        actions.add(new BasicAction("Help on this window", "helpThis", true, KeyStroke.getKeyStroke(KeyEvent.VK_H, ksctrl)));
        actions.add(new BasicAction("General Help", "helpGeneric", true, KeyStroke.getKeyStroke(KeyEvent.VK_H, ksctrlshift)));
        actions.add(new BasicAction("Version Info", "helpVersion", true, null));

        for (final BasicAction action : actions) {
            final JMenuItem item = action.asMenuItem();
            menu.add(item);
            item.addActionListener(this.parent);
        }

        return menu;
    }

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Creation                                               */
    /* ---------------------------------------------------------------------- */

    public static RtMenuBar pbInit(final RtDisplay rtd, final rtdproto.RTD.RtMenu pb) {
        if (!pb.hasStaticdata()) {
            throw new IllegalArgumentException("pbInit: RtMenu::StaticData is required");
        }

        final rtdproto.RTD.RtMenu.StaticData sd = pb.getStaticdata();

        // TODO FIXME: title is unused
        // TODO FIXME: fontsize is unused

        final java.util.List<MenuPair> choices = new ArrayList<MenuPair>();
        for (rtdproto.RTD.RtMenu.ItemInfo info : sd.getIteminfosList()) {
            final String menuName = info.getMenuname();
            final String codeName = info.getCodename();

            choices.add(new MenuPair(menuName, codeName));
        }

        final RtMenuBar mb = new RtMenuBar(rtd, choices, RtMenuBar.Style.NORMAL);
        mb.updateCellMenu(null);
        return mb;
    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        // no dynamic data to update for this type of object
    }

    /* ---------------------------------------------------------------------- */
    /* Member Classes                                                         */
    /* ---------------------------------------------------------------------- */

    static class MenuPair {
        public final String menuName;
        public final String codeName;

        public MenuPair(final String menuName, final String codeName) {
            this.menuName = menuName;
            this.codeName = codeName;
        }
    }

    /**
     * Define styles for the menu bar
     */
    static public class Style {
        static public int NORMAL = 1;
        static public int NOCELL = 2;
    }
}
