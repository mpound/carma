package carma.ui.jplotter.dialog;

import java.util.Properties;
import javax.swing.JCheckBox;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;

/**
 * JCheckBox which automatically updates a Properties key/value pair
 * when it is changed.
 */
public final class PropJCheckBox extends JCheckBox
{
	public PropJCheckBox(String text, final Properties props, final String key) {
		super(text);

		// add change listener
		this.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				handleItemStateChanged(props, key);
			}
		});

		// update to current state
		handleItemStateChanged(props, key);
	}

	private void handleItemStateChanged(final Properties props, final String key) {
		final String val = isSelected() ? "true" : "false";
		props.setProperty(key, val);
		//System.out.println("PropJCheckBox: key=" + key + " val=" + val);
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
