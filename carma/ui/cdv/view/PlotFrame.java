// $Id: PlotFrame.java,v 1.8 2013/11/21 17:36:01 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.view;

import java.util.Enumeration;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JMenu;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JButton;
import javax.swing.JSlider;
import javax.swing.JMenuBar;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JMenuItem;
import javax.swing.JComponent;
import javax.swing.JSplitPane;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.RowFilter;
import javax.swing.UIManager;
import javax.swing.SwingUtilities;
import javax.swing.table.TableColumn;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableRowSorter;
import javax.swing.table.TableCellRenderer;
import javax.swing.border.Border;
import javax.swing.border.MatteBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import java.awt.Color;
import java.awt.Insets;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import carma.ui.cdv.model.PlotInfo;
import carma.ui.cdv.model.DataSource;
import carma.ui.cdv.model.DataModel;

import carma.util.Debug;

/**
 * A special class to render the very first column of each row exactly
 * the same as the column headers. This makes for a nice looking table.
 */
class RowHeaderRenderer extends JLabel
						implements TableCellRenderer {

	public RowHeaderRenderer(JTable table) {
		JTableHeader header = table.getTableHeader();
		setOpaque(true);
		setBorder(UIManager.getBorder("TableHeader.cellBorder"));
		setHorizontalAlignment(CENTER);
		setForeground(header.getForeground());
		setBackground(header.getBackground());
		setFont(header.getFont());
	}

	public Component getTableCellRendererComponent(JTable table, Object value,
												   boolean isSelected, boolean hasFocus,
												   int row, int column) {
		setText((value == null) ? "" : value.toString());
		return this;
	}
}

/**
 * A special class to hold the model index of the table column to remove
 * when the box is unchecked.
 */
class ModelCheckBox extends JCheckBox {
	private final int modelIndex;

	public ModelCheckBox(String text, int index, boolean checked) {
		super(text, checked);
		modelIndex = index;
	}

	public int getModelIndex() {
		return modelIndex;
	}
}

class ModelRowFilter<M, I> extends RowFilter<M, I> {
	private List<I> removedRows_;

	public ModelRowFilter(List<I> removedRows) {
		this.removedRows_ = removedRows;
	}

	public boolean include(RowFilter.Entry<? extends M, ? extends I> entry) {
		if (removedRows_.contains(entry.getIdentifier()))
				return false;

		return true;
	}
}

/**
 * A special JFrame that holds all of the controls for changing
 * the width and height of a table of plots.
 */
public class PlotFrame extends JFrame
					   implements ActionListener, ItemListener {

	// this should be close to the golden ratio for nice looking plots
	private static final int INITIAL_ROW_HEIGHT = 100;
	private static final int INITIAL_COL_WIDTH = 160;

	private final PlotInfo info_;
	private final DataModel dataModel_;

	private final JTable table_;
	private final List<ModelCheckBox> checkBoxes_ = new ArrayList<ModelCheckBox>();
	private final List<TableColumn> removedColumns_ = new ArrayList<TableColumn>();
	private final List<Integer> removedRows_ = new ArrayList<Integer>();

	private int rowHeight_ = INITIAL_ROW_HEIGHT;
	private int colWidth_ = INITIAL_COL_WIDTH;

	private final CDVManager manager_;
	private String imr_;

	public PlotFrame(final CDVManager manager, final String imr, String key, PlotInfo info) {
		super(info.getTitle());
		setLayout(new BorderLayout());

		this.manager_ = manager;
		this.imr_ = imr;
		this.info_ = info;

		this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

		setJMenuBar(createMenuBar());

		// configuration panel: left side of the split pane
		JPanel configPanel = new JPanel(new GridLayout(0, 1));
		configPanel.setMinimumSize(new Dimension(0, 0));
		configPanel.setOpaque(true);
		configPanel.setBorder(new MatteBorder(new Insets(4, 4, 4, 4), Color.gray));
		createScaler(configPanel);
		createAdjuster(configPanel);

		if (info.getDataView().equals("ant"))
			createAntSelector(configPanel);
		else
			createInputSelector(configPanel);

		JScrollPane configScroll = new JScrollPane(configPanel);

		// CDV plot data: right side of the split pane
		dataModel_ = new DataModel(manager.getRouter(), key, info);

		table_ = new JTable(dataModel_);
		table_.setOpaque(true);
		table_.setPreferredScrollableViewportSize(new Dimension(1200, 500));
		table_.setFillsViewportHeight(false);

		table_.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		table_.setRowSelectionAllowed(false);
		table_.setColumnSelectionAllowed(false);

		// set the initial width so the data looks decent
		table_.setRowHeight(INITIAL_ROW_HEIGHT);
		setColumnWidth(INITIAL_COL_WIDTH);

		// add the row filter to allow deselecting rows
		TableRowSorter<DataModel> sorter = new TableRowSorter<DataModel>(dataModel_);
		sorter.setRowFilter(new ModelRowFilter<DataModel, Integer>(removedRows_));
		table_.setRowSorter(sorter);

		// render strings in the row cells just like the column header
		table_.setDefaultRenderer(String.class, new RowHeaderRenderer(table_));

		JScrollPane dataScroll = new JScrollPane(table_);

		// the split pane
		JSplitPane pane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, configScroll, dataScroll);

		// add both panes
		this.add(pane, BorderLayout.CENTER);
		this.add(new StatusBar(this), BorderLayout.SOUTH);

		this.setLocationByPlatform(true);
		this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		this.pack();

		enforceDisplaySize();
		this.setVisible(true);
	}

	public void dispose() {
		Debug.print(this, Debug.INFO, "dispose: called");
		if (dataModel_ != null) {
			// Stop the DataModel, which will stop the CORBA data
			Debug.print(this, Debug.INFO, "dispose: stopping data model");
			dataModel_.stop();
		}

		super.dispose();
	}

	public CDVManager getManager() {
		return this.manager_;
	}

	public void setImr(final String imr) {
		this.imr_ = imr;
	}

	public String getImr() {
		return this.imr_;
	}

	/*------------------------------------------------------------------------*/
	/* ActionListener Interface                                               */
	/*------------------------------------------------------------------------*/

	public void actionPerformed(ActionEvent event) {
		String cmd = event.getActionCommand();
		int startIndex = 0;
		int endIndex = 0;

		Debug.print(this, Debug.INFO, "actionPerformed: handle command " + cmd);

		if (cmd.equals("ant1-15")) {
			startIndex = 1;
			endIndex = 15;
		} else if (cmd.equals("ant16-23")) {
			startIndex = 16;
			endIndex = 23;
		} else if (cmd.equals("input1-15")) {
			startIndex = 1;
			endIndex = 15;
		} else if (cmd.equals("input1-8")) {
			startIndex = 1;
			endIndex = 8;
		} else if (cmd.equals("selectAll")) {
			startIndex = 1;
			endIndex = 32;
		} else if (cmd.equals("unselectAll")) {
			startIndex = 0;
			endIndex = 0;
		} else {
			throw new RuntimeException("unknown command: " + cmd);
		}

		for (ModelCheckBox box : checkBoxes_) {
			boolean s = (box.getModelIndex() >= startIndex && box.getModelIndex() <= endIndex);
			box.setSelected(s);
		}
	}

	/*------------------------------------------------------------------------*/
	/* ItemListener Interface                                                 */
	/*------------------------------------------------------------------------*/

	public void itemStateChanged(ItemEvent event) {

		Object source = event.getItemSelectable();
		int modelIndex = -1;

		for (ModelCheckBox box : checkBoxes_) {
			if (source == box) {
				modelIndex = box.getModelIndex();
				break;
			}
		}

		if (event.getStateChange() == ItemEvent.DESELECTED) {
			removeTableRow(modelIndex);
			removeTableColumn(modelIndex);
			if (info_.getDataView().equals("ant"))
				dataModel_.removeAntenna(modelIndex);
			else
				dataModel_.removeInput(modelIndex);
		} else if (event.getStateChange() == ItemEvent.SELECTED) {
			addTableRow(modelIndex);
			addTableColumn(modelIndex);
			if (info_.getDataView().equals("ant"))
				dataModel_.addAntenna(modelIndex);
			else
				dataModel_.addInput(modelIndex);
		}

		// force the filters to re-filter the rows
		dataModel_.fireTableDataChanged();

		// force any new data cells to be the correct size
		table_.setRowHeight(rowHeight_);
		setColumnWidth(colWidth_);

		// redraw
		table_.repaint();
	}

	/*------------------------------------------------------------------------*/
	/* Private Methods
	/*------------------------------------------------------------------------*/

	private void removeTableRow(int modelIndex) {
		Debug.print(this, Debug.INFO, "rtr: index=" + modelIndex);
		removedRows_.add(modelIndex - 1);
	}

	private void addTableRow(int modelIndex) {
		Debug.print(this, Debug.INFO, "atr: index=" + modelIndex);
		removedRows_.remove(Integer.valueOf(modelIndex - 1));
	}

	private void removeTableColumn(int modelIndex) {
		Debug.print(this, Debug.INFO, "rtc: index=" + modelIndex);

		Enumeration en = table_.getColumnModel().getColumns();
		while (en.hasMoreElements()) {
			TableColumn c = (TableColumn)en.nextElement();
			if (modelIndex == c.getModelIndex()) {
				// save the removed column so it can be re-added later
				removedColumns_.add(c);

				// remove the column from the view
				table_.removeColumn(c);
				return;
			}
		}

		Debug.print(this, Debug.ERROR, "rtc: unable to find col index=" + modelIndex);
	}

	private void addTableColumn(int modelIndex) {
		Debug.print(this, Debug.INFO, "atc: index=" + modelIndex);
		TableColumn foundColumn = null;

		// find the column in the list of removed columns
		for (TableColumn c : removedColumns_) {
			if (modelIndex == c.getModelIndex()) {
				foundColumn = c;
			}
		}

		// check to make sure we actually found the column
		if (foundColumn == null) {
			Debug.print(this, Debug.ERROR, "atc: did not find column!");
			return;
		}

		// now that we are done iterating, remove from the list and add
		// the column back to the table
		removedColumns_.remove(foundColumn);
		table_.addColumn(foundColumn);

		// move the column from the end of the table back to the correct
		// position in the view
		int viewIndex = table_.convertColumnIndexToView(modelIndex);
		Enumeration en = table_.getColumnModel().getColumns();
		while (en.hasMoreElements()) {
			TableColumn c = (TableColumn)en.nextElement();
			if (c.getModelIndex() > modelIndex) {
				int targetModelIndex = c.getModelIndex();
				int targetViewIndex = table_.convertColumnIndexToView(targetModelIndex);
				table_.moveColumn(viewIndex, targetViewIndex);
				return;
			}
		}
	}

	/**
	 * Add a checkbox for scaling the USB + LSB together
	 */
	private void createScaler(JComponent component) {
		JCheckBox check;

		check = new JCheckBox("Stop plotter", false);
		check.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent event) {
				if (event.getStateChange() == ItemEvent.DESELECTED) {
					dataModel_.setStopPlotter(false);
				} else if (event.getStateChange() == ItemEvent.SELECTED) {
					dataModel_.setStopPlotter(true);
				}
			}
		});

		component.add(check);

		check = new JCheckBox("Scale USB + LSB together", true);
		check.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent event) {
				if (event.getStateChange() == ItemEvent.DESELECTED) {
					dataModel_.setScaleUSBLSBTogether(false);
				} else if (event.getStateChange() == ItemEvent.SELECTED) {
					dataModel_.setScaleUSBLSBTogether(true);
				}
			}
		});

		component.add(check);

		JComboBox<String> combo = new JComboBox<String>();
		combo.addItem("None");
		combo.addItem("Plot Scale");
		combo.addItem("Linear Fit");
		combo.addItem("Peak Amplitude");
		combo.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent event) {
				if (event.getStateChange() != ItemEvent.SELECTED)
					return;

				final String item = (String)event.getItem();
				dataModel_.setShowPlotLabel(item);
			}
		});

		component.add(combo);
	}

	/**
	 * Add buttons and checkboxes to select certain antennas
	 */
	private void createAntSelector(JComponent component) {
		JButton selectAllButton = new JButton("Select all antennas");
		selectAllButton.setActionCommand("selectAll");
		selectAllButton.addActionListener(this);
		component.add(selectAllButton);

		JButton unselectAllButton = new JButton("Unselect all antennas");
		unselectAllButton.setActionCommand("unselectAll");
		unselectAllButton.addActionListener(this);
		component.add(unselectAllButton);

		// automatic ant 1-15 selector
		JButton ant15Button = new JButton("Ant 1-15 only");
		ant15Button.setActionCommand("ant1-15");
		ant15Button.addActionListener(this);
		component.add(ant15Button);

		// automatic ant 16-23 selector
		JButton ant8Button = new JButton("Ant 16-23 only");
		ant8Button.setActionCommand("ant16-23");
		ant8Button.addActionListener(this);
		component.add(ant8Button);

		// individual checkboxes for each antenna
		for (int i = 1; i <= 23; i++) {
			ModelCheckBox check = new ModelCheckBox(String.format("Ant %d", i), i, true);
			check.addItemListener(this);
			checkBoxes_.add(check);
			component.add(check);
		}
	}

	/**
	 * Add buttons and checkboxes to select certain inputs
	 */
	private void createInputSelector(JComponent component) {
		JButton selectAllButton = new JButton("Select all inputs");
		selectAllButton.setActionCommand("selectAll");
		selectAllButton.addActionListener(this);
		component.add(selectAllButton);

		JButton unselectAllButton = new JButton("Unselect all inputs");
		unselectAllButton.setActionCommand("unselectAll");
		unselectAllButton.addActionListener(this);
		component.add(unselectAllButton);

		// automatic input 1-15 selector
		JButton input15button = new JButton("Input 1-15 only");
		input15button.setActionCommand("input1-15");
		input15button.addActionListener(this);
		component.add(input15button);

		// automatic input 16-23 selector
		JButton input8button = new JButton("Input 1-8 only");
		input8button.setActionCommand("input1-8");
		input8button.addActionListener(this);
		component.add(input8button);

		// individual checkboxes for each antenna
		for (int i = 1; i <= 32; i++) {
			ModelCheckBox check = new ModelCheckBox(String.format("Input %d", i), i, true);
			check.addItemListener(this);
			checkBoxes_.add(check);
			component.add(check);
		}
	}

	private void setColumnWidth(int width) {
		Enumeration en = table_.getColumnModel().getColumns();

		// skip the row header column
		if (en.hasMoreElements())
			en.nextElement();

		// resize the data columns
		while (en.hasMoreElements()) {
			TableColumn c = (TableColumn)en.nextElement();
			c.setMinWidth(width);
			c.setMaxWidth(width);
			c.setPreferredWidth(width);
		}
	}

	/**
	 * Add labels and sliders to adjust the data plot height and width
	 */
	private void createAdjuster(JComponent component) {

		// row height slider
		JLabel rowLabel = new JLabel("Row Height");
		rowLabel.setOpaque(true);
		JSlider rowSlider = new JSlider(JSlider.HORIZONTAL, 50, 500, INITIAL_ROW_HEIGHT);
		rowSlider.getAccessibleContext().setAccessibleName("Row Height");
		rowSlider.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				rowHeight_ = ((JSlider)e.getSource()).getValue();
				table_.setRowHeight(rowHeight_);
				table_.repaint();
			}
		});

		component.add(rowLabel);
		component.add(rowSlider);

		// col width slider
		JLabel colLabel = new JLabel("Column Width");
		colLabel.setOpaque(true);
		JSlider colSlider = new JSlider(JSlider.HORIZONTAL, 50, 500, INITIAL_COL_WIDTH);
		colSlider.getAccessibleContext().setAccessibleName("Column Width");
		colSlider.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				colWidth_ = ((JSlider)e.getSource()).getValue();
				setColumnWidth(colWidth_);
				table_.repaint();
			}
		});

		component.add(colLabel);
		component.add(colSlider);
	}

	private JMenuBar createMenuBar() {
		final JMenuBar jmb = new JMenuBar();
		final List<String> nameservers = manager_.getNameServers();

		jmb.add(createFileMenu());
		jmb.add(new SourceMenu(this, nameservers));

		jmb.add(new PolarizationMenu(this, "INPUT", 1, 24));
		jmb.add(new PolarizationMenu(this, "LL", 1, 24));
		jmb.add(new PolarizationMenu(this, "LR", 1, 24));
		jmb.add(new PolarizationMenu(this, "RL", 1, 24));
		jmb.add(new PolarizationMenu(this, "RR", 1, 24));
		jmb.add(new PolarizationMenu(this, "INPUT-3G", 25, 40));
		jmb.add(new PolarizationMenu(this, "LL-3G", 25, 40));
		jmb.add(new PolarizationMenu(this, "LR-3G", 25, 40));
		jmb.add(new PolarizationMenu(this, "RL-3G", 25, 40));
		jmb.add(new PolarizationMenu(this, "RR-3G", 25, 40));
		jmb.add(Box.createHorizontalGlue());
		jmb.add(createHelpMenu());

		return jmb;
	}

	private JMenu createFileMenu() {
		final JMenu menu = new JMenu("File");

		{
			final JMenuItem item = new JMenuItem("Close");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					dispose();
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

	private JMenu createHelpMenu() {
		final JMenu menu = new JMenu("Help");
		final JFrame frame = this;

		{
			final JMenuItem item = new JMenuItem("About...");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					final String[] mess = {
						"Carma Data Viewer",
						"Version: Beta",
						"Documentation can be found at: www.mmarray.org/software",
						"Submit bugs at: http://www.mmarray.org/bugzilla",
					};
					JOptionPane.showMessageDialog(frame, mess, "Help Menu",
												  JOptionPane.INFORMATION_MESSAGE);
				}
			});
			menu.add(item);
		}

		return menu;
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
}
