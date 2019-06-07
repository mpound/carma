package carma.ui.jplotter.dialog;

import java.io.*;
import java.util.*;

import java.util.regex.Pattern;
import java.util.regex.Matcher;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.*;
import java.awt.event.*;
import java.awt.Window;

// MigLayout
import net.miginfocom.swing.MigLayout;
import net.miginfocom.layout.PlatformDefaults;

import com.google.common.base.Joiner;

import carma.ui.jplotter.plotter.HelpMenu;
import carma.ui.jplotter.plotter.PlotManager;
import carma.ui.jplotter.plotter.PlotProperties;
import carma.ui.jplotter.util.MigUtils;

/**
 * Monitor Point Search Tab
 */
final class MonitorPanel extends JPanel
{
	private final List<String> mps;
	private final PlotProperties props;
	private final MonitorDialog dialog;

	/* ---------------------------------------------------------------------- */
	/* Public Methods                                                         */
	/* ---------------------------------------------------------------------- */

	public MonitorPanel(final MonitorDialog dialog, final PlotProperties props, final List<String> mps) {
		super(new MigLayout("insets dialog, fill", "[][][]"));

		this.mps = mps;
		this.props = props;
		this.dialog = dialog;

		final JList<String> listSearch = new JList<String>(new DefaultListModel<String>());
		final JList<String> listXAxis = createList(props, "xaxis_registers");
		final JList<String> listYAxis = createList(props, "yaxis_registers");

		MigUtils.addSeparator(this, "Monitor Point Search");

		{
			listSearch.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			listSearch.setLayoutOrientation(JList.VERTICAL);
			listSearch.setVisibleRowCount(12);

			{
				final JTextField field = new JTextField("", 50);
				final JCheckBox check = new JCheckBox("Regular Expression");
				final JButton button = new JButton("Search");
				MigUtils.addIcon(button, "/resources/gtk-find.png");

				// Search button triggers a search
				button.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final String text = field.getText();
						final boolean regex = check.isSelected();
						handleSearch(text, regex, listSearch);
					}
				});

				// Pressing Enter in the TextField triggers a search
				field.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final String text = field.getText();
						final boolean regex = check.isSelected();
						handleSearch(text, regex, listSearch);
					}
				});

				// perform a default search (shows all monitor points)
				handleSearch("", false, listSearch);

				this.add(field, "growx, span 1");
				this.add(check, "span 1");
				this.add(button, "span 1, sg nav, wrap");
			}

			// the search results list
			this.add(new JScrollPane(listSearch), "span 2, growx, growy");

			if (!props.isTimeSeriesPlot()) {
				final JButton button = new JButton("X Axis");
				button.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final List<String> elements = getSelectedValues(listSearch);
						addElements(elements, listXAxis);
					}
				});
				MigUtils.addIcon(button, "/resources/gtk-add.png");
				enableButtonWithSelection(button, listSearch);

				this.add(button, "split 3, flowy, sg nav");
			}

			{
				final JButton button = new JButton("Y Axis");
				button.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final List<String> elements = getSelectedValues(listSearch);
						addElements(elements, listYAxis);
					}
				});
				MigUtils.addIcon(button, "/resources/gtk-add.png");
				enableButtonWithSelection(button, listSearch);

				if (props.isTimeSeriesPlot())
					this.add(button, "split 2, flowy, sg nav");
				else
					this.add(button, "sg nav");
			}

			// push the buttons to the top
			{
				final JSeparator sep = new JSeparator();
				sep.setVisible(false);
				this.add(sep, "growy, wrap");
			}
		}

		if (!props.isTimeSeriesPlot())
			addAxisList("X Axis", listXAxis);

		addAxisList("Y Axis", listYAxis);

		MigUtils.addBar(this);

		// ok
		{
			final JButton button = new JButton("Ok");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					dialog.finalizeProperties(props);
					dialog.setValue(ConfigurationDialogAction.OK);
					dialog.dispose();
				}
			});

			// enable the button based on monitor point selection requirements
			props.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					button.setEnabled(props.validateMonitorPoints());
				}
			});

			MigUtils.addIcon(button, "/resources/gtk-ok.png");
			this.add(button, "flowx, span, split 2, sg nav, tag ok");
		}

		// cancel
		{
			final JButton button = new JButton("Cancel");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					dialog.setValue(ConfigurationDialogAction.CANCEL);
					dialog.dispose();
				}
			});

			MigUtils.addIcon(button, "/resources/gtk-cancel.png");
			this.add(button, "sg nav, wrap, tag cancel");
		}
	}

	/* ---------------------------------------------------------------------- */
	/* Private Methods                                                        */
	/* ---------------------------------------------------------------------- */

	private void addAxisList(final String label, final JList<String> list) {
		MigUtils.addSeparator(this, label);

		list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		list.setLayoutOrientation(JList.VERTICAL);
		list.setVisibleRowCount(8);

		setupDeleteKey(list);

		this.add(new JScrollPane(list), "flowx, span 2, growx, growy, pad 0 0 1 0");

		{
			final JButton button = new JButton("Move Up");
			MigUtils.addIcon(button, "/resources/gtk-up.png");
			button.setHorizontalAlignment(SwingConstants.LEFT);
			enableButtonWithSelection(button, list);
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					JListHelper.moveUp(list);
				}
			});

			this.add(button, "split 4, flowy, sg nav");
		}

		{
			final JButton button = new JButton("Move Down");
			MigUtils.addIcon(button, "/resources/gtk-down.png");
			button.setHorizontalAlignment(SwingConstants.LEFT);
			enableButtonWithSelection(button, list);
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					JListHelper.moveDown(list);
				}
			});

			this.add(button, "sg nav");
		}

		// push the buttons apart
		{
			final JSeparator sep = new JSeparator();
			sep.setVisible(false);
			this.add(sep, "growy");
		}

		{
			final JButton button = new JButton("Remove");
			MigUtils.addIcon(button, "/resources/gtk-remove.png");
			button.setHorizontalAlignment(SwingConstants.LEFT);
			enableButtonWithSelection(button, list);
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					JListHelper.removeItems(list);
				}
			});

			this.add(button, "push, sg nav, wrap");
		}
	}

	/**
	 * Perform a search and display the results in the search results JList.
	 *
	 * @param input the text to search for
	 * @param regex regular expression or normal search
	 * @param list the list view to update
	 */
	private void handleSearch(final String input, boolean regex, final JList<String> list) {

		// trim existing whitespace, then split the string apart by any
		// whitespace in the middle
		final String[] splitInput = input.trim().split("\\s+");
		final String search;

		if (splitInput.length == 1) {
			// user didn't do anything weird, save their search text as-is
			search = splitInput[0];
		} else {
			// user assumes space is ok, logically OR it together for them
			search = Joiner.on(".*").skipNulls().join(splitInput);
			regex = true;
		}

		final int flags = Pattern.CASE_INSENSITIVE;
		final Pattern p;
		if (regex) {
			p = Pattern.compile(search, flags);
		} else {
			p = Pattern.compile(Pattern.quote(search), flags);
		}

		final DefaultListModel<String> model = new DefaultListModel<String>();

		// special case the empty search to make startup faster
		if (search.equals("")) {
			for (String s : this.mps)
				model.addElement(s);
		} else {
			// iterate through list adding matching items
			for (String s : this.mps) {
				final Matcher m = p.matcher(s);
				if (m.find())
					model.addElement(s);
			}
		}

		// update the list view
		list.setModel(model);
	}

	/**
	 * Get current list selection as a list of strings.
	 */
	private List<String> getSelectedValues(final JList<String> list) {
		final List<String> result = new ArrayList<String>();

		final Object[] values = list.getSelectedValues();
		if (values != null && values.length >= 1) {
			for (final Object obj : values) {
				result.add((String)obj);
			}
		}

		return result;
	}

	/**
	 * Append one or more elements to the end of the axis list display.
	 *
	 * @param elements the elements to append
	 */
	private void addElements(final List<String> elements, final JList<String> list) {
		final DefaultListModel<String> model = (DefaultListModel<String>)list.getModel();

		for (final String elem : elements) {
			model.addElement(elem);
		}
	}

	/**
	 * Add a ListSelectionListener to automatically enable/disable a button
	 * when the list selection changes.
	 *
	 * @param button the button to enable/disable
	 * @param list the list whose selection should be tracked
	 */
	private void enableButtonWithSelection(final JButton button, final JList<String> list) {
		list.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				final boolean enable = getSelectedValues(list).size() >= 1;
				button.setEnabled(enable);
			}
		});

		// set button state to match current list selection state
		final boolean enable = getSelectedValues(list).size() >= 1;
		button.setEnabled(enable);
	}

	private static JList<String> createList(final PlotProperties props, final String key) {
		final DefaultListModel<String> model = new DefaultListModel<String>();
		final String[] values = props.getStringArray(key);
		if (values != null) {
			for (String s : values) {
				if (s.equals(""))
					continue;

				model.addElement(s);
			}
		}

		final PropJList<String> list = new PropJList<String>(model, props, key);
		list.addListValidator(new ListValidator() {
			public boolean isValid(final JList list) {
				return list.getModel().getSize() >= 1;
			}
		});

		// Non-timeseries plots must validate that both axis lists have
		// equal numbers of monitor points. Additionally, we must ensure that
		// validation is re-checked every time the PlotProperties is changed.
		//
		// This is tricky and ugly, but I couldn't find a better way.
		if (!props.isTimeSeriesPlot()) {
			list.addListValidator(new ListValidator() {
				public boolean isValid(final JList list) {
					return props.validateMonitorPoints();
				}
			});

			props.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					list.reValidate();
				}
			});
		}

		return list;
	}

	/**
	 * Setup DELETE and BACK_SPACE to remove all selected items from the list.
	 */
	private static void setupDeleteKey(final JList<String> list) {
		final String delAction = "deleteItems";
		final KeyStroke delKey = KeyStroke.getKeyStroke("DELETE");
		list.getInputMap().put(delKey, delAction);

		final KeyStroke backKey = KeyStroke.getKeyStroke("BACK_SPACE");
		list.getInputMap().put(backKey, delAction);

		list.getActionMap().put(delAction, new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				JListHelper.removeItems(list);
			}
		});
	}
}

/**
 * A JDialog which holds the Monitor Point Search dialog (step 2 in the wizard).
 */
public final class MonitorDialog extends AbstractConfigurationDialog
{
	private final PlotManager manager;
	private ConfigurationDialogAction action = ConfigurationDialogAction.CANCEL;
	private final PlotProperties props;

	/* ---------------------------------------------------------------------- */
	/* Public Methods                                                         */
	/* ---------------------------------------------------------------------- */

	public MonitorDialog(final Window window, final PlotProperties props, final PlotManager manager) {
		super(window, "Monitor Point Selection", JDialog.DEFAULT_MODALITY_TYPE);

		this.manager = manager;
		this.props = props;

		final JMenuBar menuBar = new JMenuBar();
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

		menuBar.add(menu);

		// Help Menu on far right side of window
		menuBar.add(Box.createHorizontalGlue());
		menuBar.add(new HelpMenu("window-specific.txt"));

		setJMenuBar(menuBar);

		// Add the main panel
		{
			final List<String> mps = manager.getRegisterList();
			final JPanel panel = new MonitorPanel(this, new PlotProperties(props), mps);
			panel.setBorder(new EmptyBorder(16, 16, 4, 16));

			this.add(panel);
		}

		setLocationByPlatform(true);
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		pack();
		setLocationRelativeTo(window);
		setVisible(true);
	}

	/* ---------------------------------------------------------------------- */
	/* AbstractConfigurationDialog Methods                                    */
	/* ---------------------------------------------------------------------- */

	@Override
	public void setValue(ConfigurationDialogAction action) {
		this.action = action;
	}

	@Override
	public ConfigurationDialogAction getValue() {
		return this.action;
	}

	@Override
	public void finalizeProperties(final PlotProperties props) {
		this.props.copyFrom(props, "xaxis_registers");
		this.props.copyFrom(props, "yaxis_registers");
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
