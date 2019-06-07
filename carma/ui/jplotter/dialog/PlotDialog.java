package carma.ui.jplotter.dialog;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.Border;

import java.awt.*;
import java.awt.event.*;

import net.miginfocom.swing.MigLayout;

import carma.ui.jplotter.plotter.PlotProperties;
import carma.ui.jplotter.util.MigUtils;

final class PlotDialogPanel extends JPanel
{
	private final PlotDialog dialog;

	/**
	 * Constructor
	 */
	public PlotDialogPanel(final PlotDialog dialog, final PlotProperties props) {
		super(new MigLayout("insets dialog, fill", "[][][][]"));

		this.dialog = dialog;

		MigUtils.addSeparator(this, "Title");

		{
			this.add(new JLabel("Title"), "span 1");

			final String key = "plot_title";
			final String val = props.getString(key);
			final JTextField field = new PropJTextField(val, 15, props, key);
			this.add(field, "growx, span 3, wrap");
		}

		MigUtils.addSeparator(this, "Marker");

		{
			this.add(new JLabel("Type"), "span 1");

			final String key = "marker_type";
			final String val = props.getString("marker_type");
			final JComboBox combo = new PropJComboBox<String>(new String[] {
				"Dot",
				"Line",
				"Shape",
			}, props, key);
			combo.setSelectedItem(val);
			this.add(combo, "span 3, growx, wrap");
		}

		{
			this.add(new JLabel("Size"), "span 1");

			final String key = "marker_size";
			final String val = props.getString(key);
			final JComboBox combo = new PropJComboBox<String>(new String[] {
				"Small",
				"Medium",
				"Large",
				"Ultra",
			}, props, key);
			combo.setSelectedItem(val);
			this.add(combo, "span 3, growx, wrap");
		}

		MigUtils.addSeparator(this, "Legend");

		{
			this.add(new JLabel("Location"), "span 1");

			final String key = "legend_location";
			final String val = props.getString(key);
			final JComboBox combo = new PropJComboBox<String>(new String[] {
				"Top",
				"Bottom",
				"Left",
				"Right",
				"Disable",
			}, props, key);
			combo.setSelectedItem(val);
			this.add(combo, "span 3, growx, wrap");
		}

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
	}
}

/**
 * A JDialog which allows the user to select a TimeZone from a JComboBox.
 */
public final class PlotDialog extends AbstractConfigurationDialog
{
	private ConfigurationDialogAction action = ConfigurationDialogAction.CANCEL;
	private final PlotProperties props;

	/* ---------------------------------------------------------------------- */
	/* Public Methods                                                         */
	/* ---------------------------------------------------------------------- */

	/**
	 * Constructor.
	 *
	 * @param window the parent window to center within
	 * @param props the PlotProperties object to obtain default values from
	 */
	public PlotDialog(final Window window, final PlotProperties props) {
		super(window, "Plot Attributes", JDialog.DEFAULT_MODALITY_TYPE);

		this.props = props;

		this.add(new PlotDialogPanel(this, new PlotProperties(props)));

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
		this.props.copyFrom(props, "plot_title");
		this.props.copyFrom(props, "marker_type");
		this.props.copyFrom(props, "marker_size");
		this.props.copyFrom(props, "legend_location");
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
