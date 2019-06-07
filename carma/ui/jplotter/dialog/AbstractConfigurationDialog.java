package carma.ui.jplotter.dialog;

import java.lang.String;
import javax.swing.JDialog;

import carma.ui.jplotter.plotter.PlotProperties;

/**
 * Abstract class to serve as a base class for configuration dialogs.
 */
public abstract class AbstractConfigurationDialog
	extends JDialog
{
	/**
	 * Constructor
	 */
	protected AbstractConfigurationDialog(java.awt.Frame frame, String title, boolean modal) {
		super(frame, title, modal);
	}

	/**
	 * Constructor
	 */
	protected AbstractConfigurationDialog(java.awt.Window window, String title, java.awt.Dialog.ModalityType modality) {
		super(window, title, modality);
	}

	/**
	 * Set return value, required method.
	 */
	protected abstract void setValue(ConfigurationDialogAction action);

	/**
	 * Get return value, required method.
	 */
	public abstract ConfigurationDialogAction getValue();

	/**
	 * Copy changed values from duplicate PlotProperties
	 */
	protected abstract void finalizeProperties(final PlotProperties props);
}

// vim: set ts=4 sts=4 sw=4 noet:
