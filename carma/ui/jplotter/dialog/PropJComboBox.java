package carma.ui.jplotter.dialog;

import java.util.Properties;
import javax.swing.JComboBox;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;

/**
 * JComboBox which automatically updates a Properties key/value pair
 * when it is changed.
 */
public final class PropJComboBox<E> extends JComboBox<E>
{
	public PropJComboBox(E[] items, final Properties props, final String key) {
		super(items);

		this.addItemListener(new ItemListener() {
			public void itemStateChanged(ItemEvent e) {
				handleItemStateChanged(props, key);
			}
		});

		handleItemStateChanged(props, key);
	}

	private void handleItemStateChanged(final Properties props, final String key) {
		final String val = getSelectedItem().toString();
		props.setProperty(key, val);
		//System.out.println("PropJComboBox: key=" + key + " val=" + val);
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
