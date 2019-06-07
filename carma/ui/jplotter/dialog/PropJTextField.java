package carma.ui.jplotter.dialog;

import java.util.ArrayList;
import java.util.Properties;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.BorderFactory;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.Color;

/**
 * JTextField which automatically updates a Properties key/value pair
 * when it is changed.
 */
public final class PropJTextField extends JTextField
{
	private final ArrayList<TextFieldValidator> validators;
	private final Properties props;
	private final String key;
	private final Border defaultBorder;

	public PropJTextField(String text, final Properties props, final String key) {
		this(text, 0, props, key);
	}

	public PropJTextField(String text, int cols, final Properties props, final String key) {
		super(text, cols);

		this.validators = new ArrayList<TextFieldValidator>();
		this.props = props;
		this.key = key;
		this.defaultBorder = getBorder();

		DocumentListener dl = new DocumentListener() {
			public void changedUpdate(DocumentEvent e) {
				handleItemStateChanged();
			}

			public void insertUpdate(DocumentEvent e) {
				handleItemStateChanged();
			}

			public void removeUpdate(DocumentEvent e) {
				handleItemStateChanged();
			}
		};

		getDocument().addDocumentListener(dl);
		handleItemStateChanged();
	}

	public void addTextFieldValidator(final TextFieldValidator validator) {
		this.validators.add(validator);

		// re-validate
		handleItemStateChanged();
	}

	private void handleItemStateChanged() {
		final String val = getText();
		this.props.setProperty(key, val);
		//System.out.println("PropJTextField: key=" + key + " val=" + val);

		setBorder(this.defaultBorder);

		// not enabled means that it is always valid
		if (!isEnabled())
			return;

		for (TextFieldValidator validator : validators) {
			if (!validator.isValid(this))
				setBorder(BorderFactory.createLineBorder(Color.red, 2));
		}
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
