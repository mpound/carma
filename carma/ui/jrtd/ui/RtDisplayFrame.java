package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.Debug;

import java.io.PrintWriter;
import java.io.StringWriter;

import java.awt.Font;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JMenuBar;
import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import java.util.concurrent.ExecutionException;

import net.miginfocom.swing.MigLayout;

/**
 * A JFrame to hold an instance of an RtDisplay. This makes the GUI able to
 * show a re-initialized display within the same window without completely
 * destroying the window and restarting, which is what the previous
 * incarnation of this code did. This caused havok with all window managers
 * that I tested.
 *
 * By keeping the same window around when a display is auto-morphed, the window
 * manager is not confused, and is able to keep an auto-morphed window in the
 * background.
 */
public class RtDisplayFrame extends JFrame
{
    private static final Debug debug = new Debug("RtDisplayFrame", false);
    private RtDisplay display = null;

    public RtDisplayFrame() {
        setBackground(Color.white);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        setLocationByPlatform(true);

        // Listen for events from window manager
        // Currently just catches window closing events
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                bailOut();
            }
        });
    }

    public synchronized void showLoadingGraphic() {
        debug.println("showLoadingGraphic: start");
        if (!SwingUtilities.isEventDispatchThread()) {
            throw new RuntimeException("showLoadingGraphic: called from non-event thread");
        }

        final String name = "/resources/loading-circle.gif";
        final ImageIcon icon = new ImageIcon(getClass().getResource(name));
        final JLabel label = new JLabel("Loading", icon, JLabel.CENTER);
        label.setVerticalTextPosition(JLabel.BOTTOM);
        label.setHorizontalTextPosition(JLabel.CENTER);
        label.setPreferredSize(new Dimension(400, 200));

        final RtDisplayFrame parentFrame = this;
        final Container pane = parentFrame.getContentPane();
        pane.removeAll();
        pane.setLayout(new MigLayout("insets 0", "[fill]", "[fill]"));
        pane.add(label, "push, align center");
        pane.setBackground(Color.white);
        setJMenuBar(createMenuBar());

        invalidate();
        validate();
        repaint();

        debug.println("showLoadingGraphic: end");
    }

    public synchronized void showErrorGraphic(final Exception ex) {
        debug.println("showErrorGraphic: start");
        if (!SwingUtilities.isEventDispatchThread()) {
            throw new RuntimeException("showErrorGraphic: called from non-event thread");
        }

        final RtDisplayFrame parentFrame = this;
        final Container pane = parentFrame.getContentPane();
        pane.removeAll();

        if (ex instanceof ExecutionException && ex.getCause() instanceof MyNetworkException) {
            final MyNetworkException mne = (MyNetworkException)ex.getCause();

            final String msg = mne.getMessage();
            final String extra = mne.getExtra();
            final String backtrace = ErrorPanel.exceptionAsString(mne.getException());
            pane.add(new ErrorPanel(parentFrame, msg, extra, backtrace));
        } else if (ex instanceof MyNetworkException) {
            final MyNetworkException mne = (MyNetworkException)ex;

            final String msg = mne.getMessage();
            final String extra = mne.getExtra();
            final String backtrace = ErrorPanel.exceptionAsString(mne.getException());
            pane.add(new ErrorPanel(parentFrame, msg, extra, backtrace));
        } else {
            final String msg = ex.getMessage();
            final String backtrace = ErrorPanel.exceptionAsString(ex);
            pane.add(new ErrorPanel(parentFrame, msg, null, backtrace));
        }

        setJMenuBar(createMenuBar());

        // repack to ensure all contents are on screen
        invalidate();
        validate();
        pack();
        repaint();

        debug.println("showErrorGraphic: end");
    }

    public synchronized void setRtDisplay(final RtDisplay newDisplay) {
        debug.println("setRtDisplay: start");
        if (!SwingUtilities.isEventDispatchThread()) {
            throw new RuntimeException("setRtDisplay: called from non-event thread");
        }

        // save the old display for later, save the new display
        final RtDisplay oldDisplay = this.display;
        this.display = newDisplay;

        // remove everything from the content pane and display the new pane
        final RtDisplayFrame parentFrame = this;
        final Container pane = parentFrame.getContentPane();
        pane.removeAll();
        pane.setLayout(new BorderLayout());
        pane.add(newDisplay, "Center");

        // remove and re-add the new menu bar
        setJMenuBar(newDisplay.getRtMenuBar());
        setTitle(newDisplay.getBaseTitle());

        newDisplay.parentFrame = parentFrame;
        newDisplay.addMouseListener(newDisplay.mouseListener);
        newDisplay.addKeyListener(newDisplay.keyListener);

        // repack the window
        pack();
        repaint();

        // make sure the window doesn't get too big
        enforceDisplaySize();

        // teardown the old display, stopping all threads, etc.
        if (oldDisplay != null) {
            oldDisplay.parentFrame = null;
            oldDisplay.bailOut();
        }

        debug.println("setRtDisplay: end");
    }

    public void bailOut() {
        if (this.display != null)
            this.display.bailOut();

        this.dispose();
    }

    private void enforceDisplaySize() {
        // Ensure that the height/width will not be too big.
        // We provide 60px of slop, to help avoid standard toolbars.
        final Dimension screenSize = getToolkit().getScreenSize();
        final int maxWindowWidth = Math.max(1024, screenSize.width - 60);
        final int maxWindowHeight = Math.max(900, screenSize.height - 60);

        final Dimension ps = getPreferredSize();
        int prefWidth = Math.min(ps.width, maxWindowWidth);
        int prefHeight = Math.min(ps.height, maxWindowHeight);

        // scrollbar size (approximate)
        final int SB_SIZE = 20;

        // Accomodate scrollbars, which appear on the opposite axis of the one
        // which is being clamped. This avoids awkward scrollbars where none
        // is needed.
        boolean needSetSize = false;
        if (ps.width != prefWidth) {
            prefHeight = Math.min(prefHeight + SB_SIZE, maxWindowHeight);
            needSetSize = true;
        }

        if (ps.height != prefHeight) {
            prefWidth = Math.min(prefWidth + SB_SIZE, maxWindowWidth);
            needSetSize = true;
        }

        if (needSetSize)
            setSize(prefWidth, prefHeight);
    }

    private JMenu createFileMenu() {
        final JMenu menu = new JMenu("File");

        {
            final JMenuItem item = new JMenuItem("Close");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    bailOut();
                }
            });
            menu.add(item);
        }

        {
            final JMenuItem item = new JMenuItem("Exit");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    System.exit(0);
                }
            });
            menu.add(item);
        }

        return menu;
    }

    private JMenuBar createMenuBar() {
        final JMenuBar mb = new JMenuBar();
        mb.add(createFileMenu());
        return mb;
    }
}

final class ErrorPanel extends JPanel {
    public ErrorPanel(final JFrame parent, final String message, final String resolution, final String backtrace) {
        super(new MigLayout("insets dialog", "[center]", "[][][]"));

        // error label
        {
            final String name = "/resources/error.gif";
            final ImageIcon icon = new ImageIcon(getClass().getResource(name));
            final JLabel label = new JLabel(message, icon, JLabel.CENTER);
            label.setVerticalTextPosition(JLabel.BOTTOM);
            label.setHorizontalTextPosition(JLabel.CENTER);

            // increase font size by 10pt
            {
                final Font font = label.getFont();
                final Font newFont = new Font(font.getName(), font.getStyle(), font.getSize() + 10);
                label.setFont(newFont);
            }

            this.add(label, "grow, wrap");
        }

        // resolution text area
        if (resolution != null) {
            final JTextArea area = new JTextArea(resolution);
            area.setEditable(false);
            this.add(new JScrollPane(area), "grow, wrap");
        }

        // backtrace text area
        if (backtrace != null) {
            final JTextArea area = new JTextArea(backtrace);
            area.setEditable(false);
            this.add(new JScrollPane(area), "grow, wrap");
        }

        // close button
        {
            final JButton button = new JButton("Close");
            button.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    parent.dispose();
                }
            });
            this.add(button, "wrap");
        }
    }

    /**
     * Get the exception backtrace as a string
     */
    public static String exceptionAsString(final Exception ex) {
        if (ex == null)
            return null;

        final StringWriter sw = new StringWriter();
        final PrintWriter pw = new PrintWriter(sw);
        ex.printStackTrace(pw);
        return sw.toString();
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
