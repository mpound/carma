package carma.observertools.pdbi.config;

import java.util.*;

/**
*  Interface used for intracting with the CARMA CORBA system 
*  Based on file of the same name from CDV.
*
*  @author Douglas Friedel
*  @since JDK1.5
*/

public interface CarmaConfig {
	/**
	 *  Returns the NameComponent ID's to use on startup
	 */
	public abstract Object[] getNameComponentIDs();

	/**
	 *  Returns the NameComponent kinds to use on startup
	 */
	public abstract Object[] getNameComponentKinds();
		
	/**
	 *  Returns a hashtable of additional properties
	 */
	@SuppressWarnings("unchecked")
	public abstract Hashtable getProperties();

	/**
	 *  Sets a hashtable of additional properties
	 */
	public abstract void setProperties(Hashtable<String, String> h);

	/**
	 *  Returns an Object array of all possible nameservers
	 *  where CORBA objects can be found.
	 */
	public abstract Object[] getNameServers();

	/**
	 *  Sets an Object array of all possible nameservers
	 *  where CORBA objects can be found.
	 */
	public abstract void setNameServers(String[] nameservers);

	/**
	 *  Adds a nameserver to the list
	 */
	public abstract void addNameServer(String ns);

	/**
	 *  Removes a nameserver from the list.
	 *  @return The object removed. Null if object not in list.
	 */
	 public abstract Object removeNameServer(String ns);

	/**
	 *  Returns an Object array of all possible ports that nameservers
	 *  for CORBA objects can be found.
	 */
	public abstract Object[] getNameServerPorts();

	/**
	 *  Sets an Object array of all possible ports that nameservers
	 *  for CORBA objects can be found.
	 */
	public abstract void setNameServerPorts(String[] ports);

	/**
	 *  Adds a nameserver port to the list
	 */
	public abstract void addNameServerPort(String nsp);

	/**
	 *  Removes a nameserver port from the list.
	 *  @return The object removed. Null if object not in list.
	 */
	public abstract Object removeNameServerPort(String nsp);

	/**
	 *  Get the size of the cache used to store ObsRecord quantities.
	 */
	public abstract int getCacheSize();

	/**
	 *  Set the size of the cache used to store ObsRecord quantities.
	 */
	public abstract void setCacheSize(int size);


	/**
	 *  Saves the state.
	 *  @param name Can be used to indicate where to save Config. Can be null.
	 */
	public abstract void saveConfig(String name);
}

