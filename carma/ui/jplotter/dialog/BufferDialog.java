package carma.ui.jplotter.dialog;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.Border;

import java.awt.*;
import java.awt.event.*;

import net.miginfocom.swing.MigLayout;

import carma.ui.jplotter.plotter.PlotProperties;
import carma.ui.jplotter.util.MigUtils;

final class BufferDialogPanel extends JPanel
{
	private final BufferDialog dialog;

	/**
	 * Constructor
	 */
	public BufferDialogPanel(final BufferDialog dialog, final PlotProperties props) {
		super(new MigLayout("insets dialog, fill", "[][]"));

		this.dialog = dialog;


		MigUtils.addSeparator(this, "Buffer Size");

		final JTextField field = createIntegerField(8, props, "buffer_size");
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

			props.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					final boolean valid = isValidBufferSize(field);
					button.setEnabled(valid);
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

	private static boolean isValidBufferSize(final JTextField field) {
		try {
			final int seconds = Integer.parseInt(field.getText());
			if (seconds <= 0)
				return false;
		} catch (NumberFormatException ex) {
			return false;
		}

		return true;
	}

	/**
	 * Create a PropJTextField for integers only.
	 */
	private static JTextField createIntegerField(final int cols, final PlotProperties props, final String key) {
		final String val = props.getString(key);
		final PropJTextField field = new PropJTextField(val, cols, props, key);

		field.addTextFieldValidator(new TextFieldValidator() {
			public boolean isValid(final JTextField field) {
				return isValidBufferSize(field);
			}
		});

		return field;
	}
}

/**
 * A JDialog which allows the user to select a TimeZone from a JComboBox.
 */
public final class BufferDialog extends AbstractConfigurationDialog
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
	 * @param buffer_size the default buffer size
	 */
	public BufferDialog(final Window window, final PlotProperties props) {
		super(window, "Buffer Size", JDialog.DEFAULT_MODALITY_TYPE);

		this.props = props;

		this.add(new BufferDialogPanel(this, new PlotProperties(props)));

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
		this.props.copyFrom(props, "buffer_size");
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
