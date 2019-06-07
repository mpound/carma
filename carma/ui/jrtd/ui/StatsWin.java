/**
 * This class supports display of the data values of a realtime data cell as
 * text values (a list of values).
 *
 * The size of the text area is limited so that it won't grow indefinitely.
 *
 * $CarmaCopyright$
 */

package carma.ui.jrtd.ui;

import carma.ui.jrtd.ui.Cell;
import carma.ui.jrtd.util.Util;
import carma.ui.jrtd.util.FileUtils;
import carma.ui.jrtd.event.CellDataListener;
import carma.ui.jrtd.event.RtStatusListener;

import java.text.NumberFormat;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JMenu;
import javax.swing.JLabel;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

final class StatsWinPanel extends JPanel {
    private final Cell cell;
    private final JTextField tf_samples = new JTextField(10);
    private final JTextField tf_min = new JTextField(10);
    private final JTextField tf_max = new JTextField(10);
    private final JTextField tf_mean = new JTextField(10);
    private final JTextField tf_rms = new JTextField(10);

    public StatsWinPanel(final StatsWin parent, final Cell cell) {
        super(new MigLayout("insets dialog", "[]8[grow, fill]", ""));
        this.cell = cell;

        add(new JLabel(cell.getMpName()), "span 2, wrap");

        {
            add(new JLabel("Samples:"));
            tf_samples.setEditable(false);
            add(tf_samples, "growx, wrap");
        }

        {
            add(new JLabel("Min:"));
            tf_min.setEditable(false);
            add(tf_min, "growx, wrap");
        }

        {
            add(new JLabel("Max:"));
            tf_max.setEditable(false);
            add(tf_max, "growx, wrap");
        }

        {
            add(new JLabel("Mean:"));
            tf_mean.setEditable(false);
            add(tf_mean, "growx, wrap");
        }

        {
            add(new JLabel("RMS:"));
            tf_rms.setEditable(false);
            add(tf_rms, "growx, wrap");
        }

        {
            final JButton button = new JButton("Close");
            button.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    parent.dispose();
                }
            });
            add(button, "gaptop 10, span 2, split 2, sg btn, center");
        }

        {
            final JButton button = new JButton("Reset");
            button.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    cell.resetStats();
                    updateTextFields();
                }
            });
            add(button, "sg btn, wrap");
        }

        updateTextFields();
    }

    public void saveToFile() {
        final String filename = this.cell.getMpName().trim() + ".txt";
        final String data = getStatsData();

        FileUtils.saveToFile(this, filename, data);
    }

    public synchronized void updateTextFields() {
        tf_samples.setText(Integer.toString(cell.getN()));
        tf_min.setText(sigFigs(cell.getMin(), 6));
        tf_max.setText(sigFigs(cell.getMax(), 6));
        tf_mean.setText(sigFigs(cell.getMean(), 2));
        tf_rms.setText(sigFigs(cell.getRMS(), 4));
    }

    // formats a number with a specified number of significant figures
    private String sigFigs(double num, int sigfigs){
        final NumberFormat nf = NumberFormat.getNumberInstance();
        nf.setMaximumFractionDigits(sigfigs);
        nf.setMinimumFractionDigits(sigfigs);
        nf.setGroupingUsed(false);

        final String nfs = nf.format(num);
        if (nfs.equals("\uFFFD"))
            return "Invalid";

        return nfs;
    }

    private synchronized String getStatsData() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Samples: " + tf_samples.getText() + "\n");
        sb.append("Min: " + tf_min.getText() + "\n");
        sb.append("Max: " + tf_max.getText() + "\n");
        sb.append("Mean: " + tf_mean.getText() + "\n");
        sb.append("RMS: " + tf_rms.getText() + "\n");
        return sb.toString();
    }
}

public class StatsWin extends JFrame implements CellDataListener, RtStatusListener {
    private final StatsWinPanel panel;

    public StatsWin(final Cell cell) {
        super(cell.getMpName());

        this.panel = new StatsWinPanel(this, cell);
        this.add(this.panel);

        final JMenuBar menuBar = new JMenuBar();
        menuBar.add(createFileMenu());
        this.setJMenuBar(menuBar);

        // automatically remove ourselves from the cell's internal list of
        // listeners when we are closed
        {
            final CellDataListener listener = this;

            this.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent e) {
                    cell.removeCellDataListener(listener);
                }
            });

            cell.addCellDataListener(listener);
        }

        setLocationByPlatform(true);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        pack();
        setVisible(true);
    }

    private JMenu createFileMenu() {
        final JMenu menu = new JMenu("File");

        {
            final JMenuItem item = new JMenuItem("Save Data to Text File");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    panel.saveToFile();
                }
            });
            menu.add(item);
        }

        {
            final JMenuItem item = new JMenuItem("Close");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    dispose();
                }
            });
            menu.add(item);
        }

        return menu;
    }

    /* ---------------------------------------------------------------------- */
    /* CellDataListener Interface                                             */
    /* ---------------------------------------------------------------------- */

    @Override
    public void cellDataChanged(final String data) {
        this.panel.updateTextFields();
    }

    /* ---------------------------------------------------------------------- */
    /* RtStatusListener Interface                                             */
    /* ---------------------------------------------------------------------- */

    public void sourceDied() {
        this.panel.setBackground(Color.red);
    }

    public void sourceAbsent() {
    }

    public void sourceAlive() {
    }

    public void bailOut() {
        this.dispose();
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
