// $Id: CarmaConfig.java,v 1.3 2011/04/06 18:16:48 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.config;

import java.util.*;
import javax.swing.JMenu;
import javax.swing.AbstractAction;

/**
 *  Interface used to set and get Configuration parameters for
 *  Carma applications.
 *
 *  @author Rick Hobbs
 *  @version $Revision: 1.3 $, $Date: 2011/04/06 18:16:48 $
 *  @since JDK1.3
 */
public interface CarmaConfig {

	/**
	 * Return a list of CORBA nameservers to use
	 */
	public abstract List<String> getNameServers();

	/**
	 *  Returns the initial Look and Feel.
	 */
	public abstract String getLookAndFeel();

	/**
	 *  Get the size of the cache used to store ObsRecord quantities.
	 */
	public abstract int getCacheSize();

	/**
	 *  Returns a Map of additional properties
	 */
	public abstract Map<String, String> getProperties();

	/**
	 * Returns a Map of key value pairs for the automatically
	 * started plot
	 */
	public abstract Map<String, String> getInitialPlotInfo();
}
