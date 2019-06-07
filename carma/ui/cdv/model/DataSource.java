// $Id: DataSource.java,v 1.2 2011/04/06 18:16:50 iws Exp $
// vim: set ts=4 sts=4 sw=4 et:

package carma.ui.cdv.model;

/**
 *  Interface for all DataSource objects
 *
 *  @author Rick Hobbs
 *  @version $Revision: 1.2 $, $Data:$
 *  @since JDK1.3
 */
public interface DataSource {
    public abstract void addDataListener(DataListener l, String key);
    public abstract void removeDataListener(DataListener l, String key);
    public abstract String getName();
}
