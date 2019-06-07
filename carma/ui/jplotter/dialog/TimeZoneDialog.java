package carma.ui.jplotter.dialog;

import javax.swing.*;
import javax.swing.event.*;

import java.awt.*;
import java.awt.event.*;

import net.miginfocom.swing.MigLayout;

import carma.ui.jplotter.plotter.PlotProperties;
import carma.ui.jplotter.util.MigUtils;

final class TimeZoneDialogPanel extends JPanel
{
	public TimeZoneDialogPanel(final TimeZoneDialog dialog, final PlotProperties props, final String prefix) {
		super(new MigLayout("insets dialog", "[]push[]", "[fill, grow][]"));

		// combo box
		{
			final String key = prefix + "axis_timezone";
			final String val = props.getString(key);
			final JComboBox combo = new PropJComboBox<String>(new String[] {
				"UTC",
				"US Pacific (CARMA Local Time)",
				"Local Time (this computer)",
			}, props, key);
			combo.setSelectedItem(val);

			this.add(combo, "span 2, growx, wrap");
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
public final class TimeZoneDialog extends AbstractConfigurationDialog
{
	private ConfigurationDialogAction action = ConfigurationDialogAction.CANCEL;
	private final PlotProperties props;
	private final String prefix;

	/* ---------------------------------------------------------------------- */
	/* Public Methods                                                         */
	/* ---------------------------------------------------------------------- */

	public TimeZoneDialog(final Window window, final PlotProperties props, final String prefix) {
		super(window, "Time Zone Selection", JDialog.DEFAULT_MODALITY_TYPE);

		this.props = props;
		this.prefix = prefix;

		this.add(new TimeZoneDialogPanel(this, new PlotProperties(props), prefix));

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
		this.props.copyFrom(props, prefix + "axis_timezone");
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
