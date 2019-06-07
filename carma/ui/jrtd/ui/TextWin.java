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
import carma.ui.jrtd.util.MJD;
import carma.ui.jrtd.util.Util;
import carma.ui.jrtd.util.FileUtils;
import carma.ui.jrtd.event.CellDataListener;
import carma.ui.jrtd.event.RtStatusListener;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JMenu;
import javax.swing.JLabel;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JScrollPane;
import javax.swing.ButtonGroup;
import javax.swing.WindowConstants;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import net.miginfocom.swing.MigLayout;

final class TextWinPanel extends JPanel {
    private final Cell cell;
    private final DefaultTableModel model;
    private final JTable table;
    private final int MAX_ROWS = 20000;
    private boolean autoscroll = true;

    private final TableColumn tc_utc;
    private final TableColumn tc_mjd;
    private final TableColumn tc_data;

    public TextWinPanel(final Cell cell) {
        super(new MigLayout("fill, insets panel", "[]", "[][fill, grow]"));
        this.cell = cell;

        this.add(new JLabel(cell.getMpName()), "growx, wrap");

        final String[] cols = {
            "UTC",
            "MJD",
            "Cell Data",
        };

        this.model = new DefaultTableModel(cols, 0) {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };

        this.table = new JTable(this.model);
        this.table.addComponentListener(new ComponentAdapter() {
            public void componentResized(ComponentEvent e) {
                // the mouse is inside the table, do not autoscroll
                if (!autoscroll)
                    return;

                // auto scroll to last row
                scrollTableToEnd();
            }
        });

        {
            final TableColumnModel cm = this.table.getColumnModel();
            this.tc_utc = cm.getColumn(0);
            this.tc_mjd = cm.getColumn(1);
            this.tc_data = cm.getColumn(2);

            final int timeWidth = 100;
            final int cellWidth = cell.getMinimumSize().width;

            this.tc_utc.setMinWidth(timeWidth);
            this.tc_mjd.setMinWidth(timeWidth);
            this.tc_data.setMinWidth(cellWidth);

            final Dimension d = this.table.getPreferredScrollableViewportSize();
            d.width = timeWidth + cellWidth;
            this.table.setPreferredScrollableViewportSize(d);
        }

        this.add(new JScrollPane(this.table), "growx, wrap");
    }

    /**
     * Add a row to the table.
     *
     * @param data the cell data
     */
    public synchronized void addRow(final String data) {
        final MJD mjd = new MJD();

        final String[] row = {
            mjd.getHMST(),
            Util.printf(mjd.getMJD(), 6),
            data,
        };

        this.model.addRow(row);

        // remove rows if there are too many
        while (model.getRowCount() > MAX_ROWS) {
            this.model.removeRow(0);
        }
    }

    public void setStatus(final String status) {
        if (status.equals("stalled")) {
            this.table.setBackground(Color.yellow);
            addRow("<< !!Stalled!! >>");
        } else if (status.equals("dead")) {
            this.table.setForeground(Color.white);
            this.table.setBackground(Color.red);
            addRow("<< !!Dead!! >>");
        } else if (status.equals("alive")) {
            this.table.setBackground(Color.white);
            addRow("<< !!Restarted!! >>");
        }
    }

    public synchronized void setAutoscroll(final boolean enable) {
        this.autoscroll = enable;
        if (enable)
            scrollTableToEnd();
    }

    public void setClockMode(final ClockMode mode) {
        final TableColumnModel cm = this.table.getColumnModel();

        cm.removeColumn(this.tc_mjd);
        cm.removeColumn(this.tc_utc);
        cm.removeColumn(this.tc_data);

        switch (mode) {
        case CM_UTC:
            cm.addColumn(this.tc_utc);
            break;
        case CM_MJD:
            cm.addColumn(this.tc_mjd);
            break;
        }

        cm.addColumn(this.tc_data);
    }

    public void saveToFile() {
        final String filename = this.cell.getMpName().trim() + ".txt";
        final String data = getTableData();

        FileUtils.saveToFile(this, filename, data);
    }

    private void scrollTableToEnd() {
        final Rectangle rect = table.getCellRect(table.getRowCount() - 1, 0, true);
        table.scrollRectToVisible(rect);
    }

    /**
     * Synchronized with addRow() so that we don't change the row count
     * as we are saving the file.
     */
    private synchronized String getTableData() {
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < this.model.getRowCount(); i++) {
            final String utc = (String)this.model.getValueAt(i, 0);
            final String mjd = (String)this.model.getValueAt(i, 1);
            final String data = (String)this.model.getValueAt(i, 2);

            sb.append(utc + "\t" + mjd + "\t" + data + "\n");
        }

        return sb.toString();
    }
}

public class TextWin extends JFrame implements CellDataListener, RtStatusListener {
    private final TextWinPanel panel;

    public TextWin(final Cell cell) {
        super(cell.getMpName());

        this.panel = new TextWinPanel(cell);
        this.add(this.panel);

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

        final JMenuBar menuBar = new JMenuBar();
        menuBar.add(createFileMenu());
        menuBar.add(createPrefsMenu());
        this.setJMenuBar(menuBar);

        setLocationByPlatform(true);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        pack();
        setVisible(true);
    }

    public void addRow(final String data) {
        this.panel.addRow(data);
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

    private JMenu createPrefsMenu() {
        final JMenu menu = new JMenu("Prefs");
        final ButtonGroup group = new ButtonGroup();

        {
            final JRadioButtonMenuItem item = new JRadioButtonMenuItem("UTC");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    panel.setClockMode(ClockMode.CM_UTC);
                }
            });
            group.add(item);
            menu.add(item);

            item.setSelected(true);
            panel.setClockMode(ClockMode.CM_UTC);
        }

        {
            final JRadioButtonMenuItem item = new JRadioButtonMenuItem("MJD");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    panel.setClockMode(ClockMode.CM_MJD);
                }
            });
            group.add(item);
            menu.add(item);
        }

        menu.addSeparator();

        {
            final JCheckBoxMenuItem item = new JCheckBoxMenuItem("Auto-scroll");
            item.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    panel.setAutoscroll(item.getState());
                }
            });

            item.setState(true);
            menu.add(item);
        }

        return menu;
    }

    /* ---------------------------------------------------------------------- */
    /* RtStatusListener Interface                                             */
    /* ---------------------------------------------------------------------- */

    @Override
    public void sourceDied() {
        this.panel.setStatus("dead");
    }

    @Override
    public void sourceAbsent() {
        this.panel.setStatus("stalled");
    }

    @Override
    public void sourceAlive() {
        this.panel.setStatus("restarted");
    }

    @Override
    public void bailOut() {
        dispose();
    }

    /* ---------------------------------------------------------------------- */
    /* CellDataListener Interface                                             */
    /* ---------------------------------------------------------------------- */

    @Override
    public void cellDataChanged(final String data) {
        this.panel.addRow(data);
    }
}

enum ClockMode {
    CM_UTC,
    CM_MJD,
}

/* vim: set ts=4 sts=4 sw=4 et: */
