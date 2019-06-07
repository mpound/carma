package carma.observertools.pdbi.config;

import java.io.*;
import java.util.*;
import org.jdom.*;

/**
*  Class used to configure the PDBI and to connect with the
*  CARMA CORBA system
*  Based on file from CDV.
*
*  @author Douglas Friedel
*  @since JDK1.5
*/


public class PdbiConfig extends AbstractXMLConfig implements CarmaConfig{
	private static PdbiConfig _pdbiConfig = null;
	private ArrayList<String> _nameservers;
	private ArrayList<String> _nameserverPorts;
	private ArrayList<String> _nameComponentIDs;
	private ArrayList<String> _nameComponentKinds;
	private String _obsRecordEventChannelName;
	private Hashtable<String, String> _props;
	private int _cacheSize;
	private static String scriptLocationSci1 = "";
	private static String scriptLocationSci2 = "";
        private static String scriptLocationFT = "";
	private static String gradeWeb = "";

	private PdbiConfig(InputStream in) throws IOException {
		super(in);
	}

	private PdbiConfig(String filename) throws IOException {
		super(filename);
	}

	/**
	 *  Get an Instance of a PdbiConfig
	 *  @param ins InputStream
	 */
	public static PdbiConfig getInstance(InputStream ins) throws IOException {
		if (_pdbiConfig == null) {
			_pdbiConfig = new PdbiConfig(ins);
		}
		return _pdbiConfig;
	}

	public static String getScriptLocSci1(){
		return scriptLocationSci1;
	}
	
	public static String getScriptLocSci2(){
		return scriptLocationSci2;
	}

        public static String getScriptLocFT(){
	        return scriptLocationFT;
	}
	
	public static String getWeb(){
		return gradeWeb;
	}
	
	public static void setScriptLocSci1(String sl){
		scriptLocationSci1 = sl;
	}
	
	public static void setScriptLocSci2(String sl){
		scriptLocationSci2 = sl;
	}

        public static void setScriptLocFT(String sl){
	        scriptLocationFT = sl;
	}
	
	public static void setWeb(String wl){
		gradeWeb = wl;
	}
	/**
	 *  Get an instance of a PdbiConfig
	 *  If instance is null, then it will search for <b>pdbi-config.xml</b>
	 *  in <b>/usr/local/PDBI</b> and the current directory
	 */
	public static PdbiConfig getInstance() throws IOException {
		if (_pdbiConfig == null)
			return getInstance("");
		else
			return _pdbiConfig;
	}

	/**
	 *  Get an instance of a PdbiConfig.
	 *  If it can't find <b>filename</b>, then it will search 
	 *  for <b>pdbi-config.xml</b> in <b>/usr/local/PDBI</b> and the 
	 *  current directory
	 *  @param filename filename for XML config file
	 */
	public static PdbiConfig getInstance(String filename) throws
	                                                        IOException {
		if (_pdbiConfig == null) {
			try {
				_pdbiConfig = new PdbiConfig(filename);
			}
			catch (IOException ioe) {	// try some default locations
				System.err.println("Couldn't find config file at " +
				  			       filename);
				System.err.println("Trying some default locations:");
				String[] locations = new String[3];
				locations[0] = "/usr/local/PDBI/pdbi-config.xml";
				locations[1] = "pdbi-config.xml";
                locations[2] = System.getProperty("user.dir") + 
                               "/pdbi-config.xml";
				for (int i = 0; i < locations.length; i++) {
                    System.err.println("Trying " + locations[i] + ":");
					try {
						_pdbiConfig = new PdbiConfig(locations[i]);
						return _pdbiConfig;
					}
					catch (IOException e){
						System.err.println("\tCouldn't find config file at " +
						                    locations[i]);
					}
				}
				throw ioe;
			}
		}
		return _pdbiConfig;
	}

	/**
	 *  Get an instance of a PdbiConfig.
	 *  If it can't find <b>filename</b>, then it will search 
	 *  for <b>pdbi-config.xml</b> in <b>/usr/local/PDBI</b> and the 
	 *  current directory
	 *  @param args String array which should consist of -XMLFile <filename>
	 */
	public static PdbiConfig getInstance(String[] args) {
		if (args != null) {
			for (int i = 0; i < args.length - 1; i++) {
				if (args[i].equalsIgnoreCase("-XMLFile"))
					try {
						return getInstance(args[i + 1]);
					} catch (IOException e) {}
			}
		} else {
			try {
				return getInstance("");
			} catch (IOException e) {}
		}
		System.err.println("Must define an XML file with -XMLFile <filename>");
		System.exit(1);
		return null; // pointless, but the compiler needs it
	}

	/////////////////////////////////////////////////////////
	//                                                     //
	//  These methods implements the CarmaConfig interface //
	//                                                     //
	/////////////////////////////////////////////////////////

	/**
	 *  Returns the size of the cache to be used for ObsRecord quantities
	 */
	public int getCacheSize() {
		return _cacheSize;
	}

	/**
	 *  Sets the size of the cache to be used for ObsRecord quantities
	 */
	public void setCacheSize(int size) {
		_cacheSize = size;
	}

	/**
	 *  Return a Hashtable of additional Java properties
	 *  mainly for the ORB
	 */
	@SuppressWarnings("unchecked")
	public Hashtable getProperties() {
		return _props;
	}

	/**
	 *  Sets additional Java properties
	 *  mainly for the ORB
	 */
	public void setProperties(Hashtable<String, String> h) {
		_props = h;
	}

	/**
	 *  Returns the names for the initial remote object to connect to
	 *  on startup.
	 *  @return Object[] containing names
	 */
	public Object[] getNameComponentIDs() {
		if (_nameComponentIDs != null)
			return _nameComponentIDs.toArray();
		else
			return null;
	}

	/**
	 *  Returns the kinds for the initial remote object to connect to
	 *  on startup.
	 *  @return Object[] containing kinds
	 */
	public Object[] getNameComponentKinds() {
		if (_nameComponentKinds != null)
			return _nameComponentKinds.toArray();
		else
			return null;
	}

	/**
	 *  Sets the id's and kinds for the initial remote object to connect to
	 *  on startup.
	 *  @param names String[] containing id's
	 *  @param names String[] containing kinds
	 */
	public void setNameComponent(String[] ids, String[] kinds) {
		if (ids != null && kinds != null && ids.length == kinds.length) {
			if (_nameComponentIDs == null)
				_nameComponentIDs = new ArrayList<String>();
			if (_nameComponentKinds == null)
				_nameComponentKinds = new ArrayList<String>();
			_nameComponentIDs.clear();
			_nameComponentKinds.clear();
			for (int i = 0; i < ids.length; i++) {
				_nameComponentIDs.add(ids[i]);
				_nameComponentKinds.add(kinds[i]);
			}
		}
	}

	/**
	 *  Returns an Object array of all possible nameservers
	 *  where CORBA objects can be found.
	 */
	public Object[] getNameServers() {
		return _nameservers.toArray();
	}

	/**
	 *  Sets an Object array of all possible nameservers
	 *  where CORBA objects can be found.
	 */
	public void setNameServers(String[] nameservers) {
		if (nameservers != null) {
			_nameservers.clear();
			for (int i = 0; i < nameservers.length; i++)
				_nameservers.add(nameservers[i]);
		}
	}

	/**
	 *  Adds a nameserver to the list
	 */
	public void addNameServer(String ns) {
		if (ns != null)
		// check to see if item is already in list
		if (_nameservers.indexOf(ns) < 0)
			_nameservers.add(ns);
	}

	/**
	 *  Removes a nameserver from the list
	 *  @return The object removed. Null if object is not in list.
	 */
	public Object removeNameServer(String ns) {
		if (ns != null) {
			int idx = _nameservers.indexOf(ns);
			if (idx > 0) {
				_nameservers.remove(idx);
				return ns;
			}
		}
		return null;
	}
				
	/**
	 *  Returns an Object array of all possible ports that nameservers
	 *  for CORBA objects can be found.
	 */
	public Object[] getNameServerPorts() {
		return _nameserverPorts.toArray();
	}

	/**
	 *  Sets an Object array of all possible nameservers ports
	 *  where CORBA objects can be found.
	 */
	public void setNameServerPorts(String[] nsp) {
		if (nsp != null) {
			_nameserverPorts.clear();
			for (int i = 0; i < nsp.length; i++)
				_nameserverPorts.add(nsp[i]);
		}
	}

	/**
	 *  Adds a nameserver port to the list
	 */
	public void addNameServerPort(String nsp) {
		if (nsp != null)
		// check to see if item is already in list
		if (_nameserverPorts.indexOf(nsp) < 0)
			_nameserverPorts.add(nsp);
	}

	/**
	 *  Removes a nameserver port from the list
	 *  @return The object removed. Null if object is not in list.
	 */
	public Object removeNameServerPort(String nsp) {
		if (nsp != null) {
			int idx = _nameserverPorts.indexOf(nsp);
			if (idx > 0) {
				_nameserverPorts.remove(idx);
				return nsp;
			}
		}
		return null;
	}

	/**
	 *  Implementation of the saveConfig in the CarmaConfig interface
	 */
	public void saveConfig(String name) {
		try {
			saveXMLConfig(name);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/////////////////////////////////////////////////////////
	//                                                     //
	// These methods implement the AbstractXMLConfig class //
	//                                                     //
	/////////////////////////////////////////////////////////

	/**
	 *  Used to parse the XML document. Called from AbstractXMLConfig.
	 */
	@SuppressWarnings("unchecked")
	protected void parseConfig() {
		_nameservers = new ArrayList<String>();
		_nameserverPorts = new ArrayList<String>();

		_props = new Hashtable<String,String>();
		// Get the Carma namespace
		Namespace ns = Namespace.getNamespace("Carma",
				"http://www.mmarray.org");


		// Cache Size
		_cacheSize = Integer.parseInt(_root.getChild("pdbi", ns).getChild(
				"CacheSize", ns).getTextTrim());

		// get the location of the scripts and web pages
		scriptLocationSci1 = _root.getChild("pdbi",ns).getChild("scriptDirectorySci1",ns).getTextTrim();
		scriptLocationSci2 = _root.getChild("pdbi",ns).getChild("scriptDirectorySci2",ns).getTextTrim();
		scriptLocationFT = _root.getChild("pdbi",ns).getChild("scriptDirectoryFT",ns).getTextTrim();
		gradeWeb = _root.getChild("pdbi",ns).getChild("webDirectory",ns).getTextTrim();
		// Load Properties
		List propElements = _root.getChild("pdbi", ns)
		                         .getChild("properties", ns)
								 .getChildren("prop", ns);
		Iterator i = propElements.iterator();
		while (i.hasNext()) {
			Element current = (Element)i.next();
			_props.put(current.getAttributeValue("key"),
			           current.getAttributeValue("value"));
		}

		// Load Starting NameComponent
		String start = _root.getChild("pdbi", ns)
		                    .getChild("NameComponent", ns)
							.getAttributeValue("start");
		if (start.equals("1") || start.equals("true")) {
			_nameComponentIDs = new ArrayList<String>();
			_nameComponentKinds = new ArrayList<String>();
			List ncElements = _root.getChild("pdbi", ns)
		                           .getChild("NameComponent", ns)
								   .getChildren("item", ns);
			i = ncElements.iterator();
			while (i.hasNext()) {
				Element current = (Element)i.next();
				String id = current.getAttributeValue("id");
				String kind = current.getAttributeValue("kind");
				_nameComponentIDs.add(id);
				_nameComponentKinds.add(kind);
			}
		} else {
			_nameComponentIDs = null;
			_nameComponentKinds = null;
		}
			

		// Load NameServers
		List nameServerElements = _root.getChild("pdbi", ns)
			.getChild("nameServers", ns)
			.getChildren("nameServer", ns);
		i = nameServerElements.iterator();
		while (i.hasNext()) {
			Element current = (Element)i.next();
			_nameservers.add(current.getChild("identifier", ns)
					.getTextTrim());
		}

		// Load NameServer Ports
		List nameServerPortElements = _root.getChild("pdbi", ns)
			.getChild("nameServerPorts", ns)
			.getChildren("nameServerPort", ns);
		i = nameServerPortElements.iterator();
		while (i.hasNext()) {
			Element current = (Element)i.next();
			_nameserverPorts.add(current.getChild("pidentifier", ns)
					.getTextTrim());
		}
	}


	/**
	 *  Saves the member variables prior to being written out.
	 *  Called by AbstractXMLConfig.
	 */
	public void saveXMLConfig() {

		// Get the Carma namespace
		Namespace ns = Namespace.getNamespace("Carma",
				"http://www.mmarray.org");

		// Update NameServers. First remove them all, then add since
		// the number of them could have changed
		Element nameserversElement = _root.getChild("pdbi", ns)
		                                  .getChild("nameServers", ns);
		nameserversElement.removeChildren("nameServer", ns);

		// Set the new children one at a time
		for (int i = 0; i < _nameservers.size(); i++) {
			 nameserversElement.addContent( new Element("nameServer", ns).
			     addContent(new Element("identifier", ns).
				     setText((String)_nameservers.get(i))));
		}

		// Update NameServerPorts. First remove them all, then add since
		// the number of them could have changed
		Element nameserverPortsElement = _root.getChild("pdbi", ns)
		                                      .getChild("nameServerPorts", ns);
		nameserverPortsElement.removeChildren("nameServerPort", ns);

		// Set the new children one at a time
		for (int i = 0; i < _nameserverPorts.size(); i++) {
			 nameserverPortsElement.addContent( new Element("nameServerPort",
			                                                ns).
			     addContent(new Element("pidentifier", ns).
				     setText((String)_nameserverPorts.get(i))));
		}

		// Update obsRecord Event Channel Name
		_root.getChild("server", ns)
		     .getChild("obsRecordEventChannelName", ns)
		     .setText(_obsRecordEventChannelName);
	}
}
