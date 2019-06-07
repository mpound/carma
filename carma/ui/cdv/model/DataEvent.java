// $Id: DataEvent.java,v 1.3 2011/04/06 18:16:50 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import java.util.EventObject;

import carma.ui.cdv.model.CorrelatorData;

/**
 *  Class used to hold the DataSource when data has changed.
 *
 *  @author Rick Hobbs
 *  @version $Revision: 1.3 $, $Data:$
 *  @since JDK1.3
 */
public class DataEvent extends EventObject {
	private final String name_;
	private final Object data_;

	public DataEvent(DataSource src, Object data, String name) {
		super(src);
		name_ = name;
		data_ = data;
	}

	/**
	 * Returns the name of DataSource. 
	 */
	public String getName() {
		return name_;
	}

	/**
	 *  Returns the DataSource object.
	 */
	public DataSource getDataSource() {
		return (DataSource)getSource();
	}

	/**
	 * Return the data object associated with this event
	 *
	 * This will usually be a CorrelatorData or a SpectralRecord
	 * object. You will need to cast it correctly depending on the
	 * data source.
	 */
	public Object getData() {
		return data_;
	}
}
