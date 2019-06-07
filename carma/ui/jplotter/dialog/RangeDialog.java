package carma.ui.jplotter.dialog;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.Border;

import java.awt.*;
import java.awt.event.*;

import net.miginfocom.swing.MigLayout;

import carma.ui.jplotter.plotter.PlotProperties;
import carma.ui.jplotter.util.MigUtils;

final class RangeDialogPanel extends JPanel
{
	private final RangeDialog dialog;
	private final JTextField tfMin;
	private final JTextField tfMax;
	private final JTextField tfWindow;

	/**
	 * Constructor
	 */
	public RangeDialogPanel(final RangeDialog dialog, final PlotProperties props, final String prefix, final boolean timeseries) {
		super(new MigLayout("insets dialog, fill", "[][]"));

		this.dialog = dialog;
		this.tfMin = createDoubleField(8, props, prefix + "range_min");
		this.tfMax = createDoubleField(8, props, prefix + "range_max");
		this.tfWindow = createIntegerField(8, props, prefix + "range_window");

		MigUtils.addSeparator(this, prefix.toUpperCase() + " Axis Range");

		final ButtonGroup group = new ButtonGroup();

		if (prefix.equals("x") && timeseries) {
			final String key = prefix + "range_type";
			final String val = props.getString(key);

			final JRadioButton button = new JRadioButton("Automatic (full width)");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					props.setString(key, "full width");
					handleRadioButtonChange(props, prefix);
				}
			});
			button.setSelected(val.equals("full width"));

			group.add(button);
			this.add(button, "span 2, wrap");
		}

		{
			final String key = prefix + "range_type";
			final String val = props.getString(key);

			final JRadioButton button = new JRadioButton("Automatic (scale to data)");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					props.setString(key, "scale to data");
					handleRadioButtonChange(props, prefix);
				}
			});
			button.setSelected(val.equals("scale to data"));

			group.add(button);
			this.add(button, "span 2, wrap");
		}

		if (prefix.equals("x") && timeseries) {
			final String key = prefix + "range_type";
			final String val = props.getString(key);

			final JRadioButton button = new JRadioButton("Automatic (sliding window)");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					props.setString(key, "sliding window");
					handleRadioButtonChange(props, prefix);
				}
			});
			button.setSelected(val.equals("sliding window"));

			group.add(button);
			this.add(button, "span 2, wrap");

			this.add(new JLabel("Size (sec)"), "span, split 2");
			this.add(tfWindow, "span 1, growx, wrap");
		}

		{
			final String key = prefix + "range_type";
			final String val = props.getString(key);

			final JRadioButton button = new JRadioButton("Custom");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					props.setString(key, "custom");
					handleRadioButtonChange(props, prefix);
				}
			});
			button.setSelected(val.equals("custom"));

			group.add(button);
			this.add(button, "span 2, wrap");
		}

		this.add(tfMin, "span, split 2, growx, sg minmax");
		this.add(tfMax, "growx, sg minmax, wrap");

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

			props.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					final String range_type = props.getString(prefix + "range_type");
					if (range_type.equals("custom")) {
						if (!isDouble(props.getString(prefix + "range_min"))) {
							button.setEnabled(false);
							return;
						}

						if (!isDouble(props.getString(prefix + "range_max"))) {
							button.setEnabled(false);
							return;
						}
					}

					if (range_type.equals("sliding window")) {
						if (!isInteger(props.getString(prefix + "range_window"))) {
							button.setEnabled(false);
							return;
						}
					}

					button.setEnabled(true);
				}
			});

			MigUtils.addIcon(button, "/resources/gtk-ok.png");
			this.add(button, "span, split 2, sg okcancel, tag ok");
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
			this.add(button, "sg okcancel, wrap, tag cancel");
		}

		// setup initial state
		handleRadioButtonChange(props, prefix);
	}

	private void handleRadioButtonChange(final PlotProperties props, final String prefix) {
		final boolean custom = props.getString(prefix + "range_type").equals("custom");
		final boolean window = props.getString(prefix + "range_type").equals("sliding window");

		tfMin.setEnabled(custom);
		tfMax.setEnabled(custom);
		tfWindow.setEnabled(window);

		if (custom)
			tfMin.requestFocus();

		if (window)
			tfWindow.requestFocus();

	}

	private static boolean isDouble(final String text) {
		try {
			Double.parseDouble(text);
			return true;
		} catch (NumberFormatException ex) {
			return false;
		}
	}

	private static boolean isInteger(final String text) {
		try {
			Integer.parseInt(text);
			return true;
		} catch (NumberFormatException ex) {
			return false;
		}
	}

	/**
	 * Create a PropJTextField for double values.
	 */
	private static JTextField createDoubleField(final int cols, final PlotProperties props, final String key) {
		final String val = props.getString(key);
		final PropJTextField field = new PropJTextField(val, cols, props, key);

		field.addTextFieldValidator(new TextFieldValidator() {
			public boolean isValid(final JTextField field) {
				try {
					Double.parseDouble(field.getText());
				} catch (NumberFormatException ex) {
					return false;
				}

				return true;
			}
		});

		return field;
	}

	/**
	 * Create a PropJTextField for integers only.
	 */
	private static JTextField createIntegerField(final int cols, final PlotProperties props, final String key) {
		final String val = props.getString(key);
		final PropJTextField field = new PropJTextField(val, cols, props, key);

		field.addTextFieldValidator(new TextFieldValidator() {
			public boolean isValid(final JTextField field) {
				return isValidWindowSize(field);
			}
		});

		return field;
	}

	private static boolean isValidWindowSize(final JTextField field) {
		try {
			final int seconds = Integer.parseInt(field.getText());
			if (seconds <= 0)
				return false;
		} catch (NumberFormatException ex) {
			return false;
		}

		return true;
	}
}

/**
 * A JDialog which allows the user to select a TimeZone from a JComboBox.
 */
public final class RangeDialog extends AbstractConfigurationDialog
{
	private ConfigurationDialogAction action = ConfigurationDialogAction.CANCEL;
	private final PlotProperties props;
	private final String prefix;
	private final boolean timeseries;

	/* ---------------------------------------------------------------------- */
	/* Public Methods                                                         */
	/* ---------------------------------------------------------------------- */

	/**
	 * Constructor.
	 *
	 * @param window the parent window to center within
	 * @param props the PlotProperties object to obtain default values from
	 * @param prefix the key prefix for this plot
	 * @param timeseries is this a timeseries plot
	 */
	public RangeDialog(final Window window, final PlotProperties props, final String prefix, final boolean timeseries) {
		super(window, prefix.toUpperCase() + " Axis Range", JDialog.DEFAULT_MODALITY_TYPE);

		this.props = props;
		this.prefix = prefix;
		this.timeseries = timeseries;

		this.add(new RangeDialogPanel(this, new PlotProperties(props), prefix, timeseries));

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
		this.props.copyFrom(props, prefix + "range_type");
		this.props.copyFrom(props, prefix + "range_min");
		this.props.copyFrom(props, prefix + "range_max");
		this.props.copyFrom(props, prefix + "range_window");
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
