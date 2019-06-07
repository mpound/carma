package carma.ui.jplotter.dialog;

import java.lang.StringBuilder;
import java.util.Properties;
import java.util.ArrayList;
import javax.swing.JList;
import javax.swing.ListModel;
import javax.swing.border.Border;
import javax.swing.BorderFactory;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import java.awt.Color;

/**
 * JList which automatically updates a Properties key/value pair
 * when it is changed.
 */
public final class PropJList<E> extends JList<E>
{
	private final ArrayList<ListValidator> validators;
	private final Properties props;
	private final String key;
	private final Border defaultBorder;

	public PropJList(ListModel<E> model, final Properties props, final String key) {
		super(model);

		this.validators = new ArrayList<ListValidator>();
		this.props = props;
		this.key = key;
		this.defaultBorder = getBorder();

		this.myAddListDataListener(model);
		this.handleItemStateChanged(model);
	}

	@Override
	public void setModel(ListModel<E> model) {
		super.setModel(model);
		this.myAddListDataListener(model);
		this.handleItemStateChanged(model);
	}

	/**
	 * Add a validator to be called when the list contents change in any way.
	 * If any validator indicates that the contents are invalid, the list
	 * background is changed to be red.
	 */
	public void addListValidator(final ListValidator validator) {
		this.validators.add(validator);

		// re-validate
		handleItemStateChanged(this.getModel());
	}

	/**
	 * Re-run the validation code code for this list. This is intended to be
	 * used to allow externally triggered criteria to be used during list
	 * validation.
	 */
	public void reValidate() {
		handleValidation();
	}

	private void myAddListDataListener(final ListModel<E> model) {
		ListDataListener ldl = new ListDataListener() {
			public void intervalAdded(ListDataEvent e) {
				//System.out.println("intervalAdded: " + e);
				handleItemStateChanged(model);
			}

			public void intervalRemoved(ListDataEvent e) {
				//System.out.println("intervalRemoved: " + e);
				handleItemStateChanged(model);
			}

			public void contentsChanged(ListDataEvent e) {
				//System.out.println("contentsChanged: " + e);
				handleItemStateChanged(model);
			}
		};

		model.addListDataListener(ldl);
	}

	private synchronized void handleItemStateChanged(final ListModel<E> model) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < model.getSize(); i++) {
			if (sb.length() > 0)
				sb.append(" ");

			sb.append(model.getElementAt(i));
		}

		this.props.setProperty(this.key, sb.toString());
		//System.out.println("PropJList: key=" + key + " val=" + sb.toString());

		handleValidation();
	}

	private synchronized void handleValidation() {
		setBorder(this.defaultBorder);
		for (ListValidator validator : validators) {
			if (!validator.isValid(this))
				setBorder(BorderFactory.createLineBorder(Color.red, 2));
		}
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
