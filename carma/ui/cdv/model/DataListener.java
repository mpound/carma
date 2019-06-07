// $Id: DataListener.java,v 1.2 2011/04/06 18:16:50 iws Exp $
// vim: set ts=4 sts=4 sw=4 et:

package carma.ui.cdv.model;

import java.util.EventListener;

/**
 *  Interface for all DataListeners. The DataEvent object
 *  contains the DataSource whos data has changed.
 *
 *  @author Rick Hobbs
 *  @version $Revision: 1.2 $, $Data:$
 *  @since JDK1.3
 */
public interface DataListener extends EventListener {
    void dataChanged(DataEvent e);
}
