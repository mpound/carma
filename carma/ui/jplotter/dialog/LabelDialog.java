package carma.ui.jplotter.dialog;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.Border;

import java.awt.*;
import java.awt.event.*;

import net.miginfocom.swing.MigLayout;

import carma.ui.jplotter.plotter.PlotProperties;
import carma.ui.jplotter.util.MigUtils;

final class LabelDialogPanel extends JPanel
{
	private final LabelDialog dialog;

	/**
	 * Constructor
	 */
	public LabelDialogPanel(final LabelDialog dialog, final PlotProperties props, final String prefix) {
		super(new MigLayout("insets dialog, fill", "[][]"));

		this.dialog = dialog;

		final JTextField field;
		{
			final String key = prefix + "label_custom";
			final String val = props.getString(key);
			field = new PropJTextField(val, props, key);
		}

		MigUtils.addSeparator(this, prefix.toUpperCase() + " Axis Label");

		final ButtonGroup group = new ButtonGroup();

		{
			final String key = prefix + "label_type";
			final String val = props.getString(key);

			final JRadioButton button = new JRadioButton("Disable");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					props.setString(key, "disable");
					handleRadioButtonChange(props, prefix, field);
				}
			});
			button.setSelected(val.equals("disable"));

			group.add(button);
			this.add(button, "span 2, wrap");
		}

		{
			final String key = prefix + "label_type";
			final String val = props.getString(key);

			final JRadioButton button = new JRadioButton("Automatic");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					props.setString(key, "automatic");
					handleRadioButtonChange(props, prefix, field);
				}
			});
			button.setSelected(val.equals("automatic"));

			group.add(button);
			this.add(button, "span 2, wrap");
		}

		{
			final String key = prefix + "label_type";
			final String val = props.getString(key);

			final JRadioButton button = new JRadioButton("Custom");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					props.setString(key, "custom");
					handleRadioButtonChange(props, prefix, field);
				}
			});
			button.setSelected(val.equals("custom"));

			group.add(button);
			this.add(button, "span 2, wrap");
		}

		this.add(field, "span 2, growx, wrap");

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
		handleRadioButtonChange(props, prefix, field);
	}

	private void handleRadioButtonChange(final PlotProperties props, final String prefix, final JTextField field) {
		final boolean custom = props.getString(prefix + "label_type").equals("custom");
		field.setEnabled(custom);
		if (custom)
			field.requestFocus();
	}
}

/**
 * A JDialog which allows the user to select a TimeZone from a JComboBox.
 */
public final class LabelDialog extends AbstractConfigurationDialog
{
	private ConfigurationDialogAction action = ConfigurationDialogAction.CANCEL;
	private final PlotProperties props;
	private final String prefix;

	/* ---------------------------------------------------------------------- */
	/* Public Methods                                                         */
	/* ---------------------------------------------------------------------- */

	/**
	 * Constructor.
	 *
	 * @param window the parent window to center within
	 * @param props the PlotProperties object to obtain default values from
	 * @param prefix the key prefix for this plot
	 */
	public LabelDialog(final Window window, final PlotProperties props, final String prefix) {
		super(window, prefix.toUpperCase() + " Axis Label", JDialog.DEFAULT_MODALITY_TYPE);

		this.props = props;
		this.prefix = prefix;

		this.add(new LabelDialogPanel(this, new PlotProperties(props), prefix));

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
		this.props.copyFrom(props, prefix + "label_type");
		this.props.copyFrom(props, prefix + "label_custom");
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
