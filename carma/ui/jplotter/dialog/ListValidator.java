package carma.ui.jplotter.dialog;

import javax.swing.JList;

public interface ListValidator
{
	/**
	 * Validate the contents of a JList in any way desired.
	 */
	public abstract boolean isValid(final JList list);
}

// vim: set ts=4 sts=4 sw=4 noet:
