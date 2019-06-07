package carma.ui.jplotter.dialog;

import javax.swing.JTextField;

public interface TextFieldValidator
{
	/**
	 * Validate the contents of a JTextField in any way desired
	 */
	public abstract boolean isValid(final JTextField field);
}

// vim: set ts=4 sts=4 sw=4 noet:
