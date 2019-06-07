// $Id: CDVxmlConfig.java,v 1.3 2011/04/06 18:16:48 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.config;

import java.io.*;
import java.util.*;
import javax.swing.*;

import org.jdom.*;
import org.jdom.input.*;
import org.jdom.adapters.*;

import carma.ui.cdv.model.*;

import carma.util.Debug;

/**
 *  Class used to read and parse an XML file containing
 *  configuration parameter for the CarmaDataViewer.
 *
 *  @author Rick Hobbs
 *  @version $Revision: 1.3 $, $Date: 2011/04/06 18:16:48 $
 *  @since JDK1.3
 */
public class CDVxmlConfig extends AbstractXMLConfig
						  implements CarmaConfig {

	private List<String> _nameservers;
	private Map<String, String> _props;
	private String _initialLookAndFeel;
	private Map<String, String> _initialPlotMap;
	private int _cacheSize;

	public CDVxmlConfig(InputStream in) throws IOException {
		super(in);
	}

	public CDVxmlConfig(String filename) throws IOException {
		super(filename);
	}

	/////////////////////////////////////////////////////////
	//                                                     //
	//  These methods implements the CarmaConfig interface //
	//                                                     //
	/////////////////////////////////////////////////////////

	/**
	 *  Returns the size of the cache to be used
	 */
	public int getCacheSize() {
		return _cacheSize;
	}

	/**
	 *  Return a Map of additional Java properties
	 *  mainly for the ORB
	 */
	public Map<String, String> getProperties() {
		return _props;
	}

	/**
	 *  Returns an Object array of all possible nameservers
	 *  where CORBA objects can be found.
	 */
	public List<String> getNameServers() {
		return _nameservers;
	}

	/**
	 *  Returns the initial Look and Feel.
	 */
	public String getLookAndFeel() {
		return _initialLookAndFeel;
	}

	public Map<String, String> getInitialPlotInfo() {
		return _initialPlotMap;
	}

	/////////////////////////////////////////////////////////
	//                                                     //
	// These methods implement the AbstractXMLConfig class //
	//                                                     //
	/////////////////////////////////////////////////////////

	/**
	 *  Used to parse the XML document. Called from AbstractXMLConfig.
	 */
	protected void parseConfig() {
		_nameservers = new ArrayList<String>();
		_props = new HashMap<String, String>();

		// Get the Carma namespace
		Namespace ns = Namespace.getNamespace("Carma", "http://www.mmarray.org");

		// Load NameServers
		List nameServerElements = _root.getChild("cdv", ns)
									   .getChild("NameServers", ns)
									   .getChildren("NameServer", ns);

		Iterator nsIterator = nameServerElements.iterator();
		while (nsIterator.hasNext()) {
			Element current = (Element)nsIterator.next();
			String nameserver = current.getTextTrim();
			Debug.print(this, Debug.INFO, "nameserver=" + nameserver);
			_nameservers.add(nameserver);
		}

		// Load Look and Feel
		_initialLookAndFeel = _root.getChild("cdv", ns)
								   .getChild("LookAndFeel", ns)
								   .getTextTrim();

		// Cache Size
		_cacheSize = Integer.parseInt(_root.getChild("cdv", ns)
										   .getChild("CacheSize", ns)
										   .getTextTrim());

		// Load Properties
		List propElements = _root.getChild("cdv", ns)
								 .getChild("properties", ns)
								 .getChildren("prop", ns);

		Iterator propIterator = propElements.iterator();
		while (propIterator.hasNext()) {
			Element current = (Element)propIterator.next();
			String key = current.getAttributeValue("key");
			String value = current.getAttributeValue("value");
			Debug.print(this, Debug.INFO, "property: key=" + key + " value=" + value);
			_props.put(key, value);
		}

		// Load InitialPlot
		String start = _root.getChild("cdv", ns)
							.getChild("InitialPlot", ns)
							.getAttributeValue("start");

		if (start.equals("1") || start.equals("true")) {
			_initialPlotMap = new HashMap<String, String>();
			List initialPlotElements = _root.getChild("cdv", ns)
											.getChild("InitialPlot", ns)
											.getChildren();

			Iterator ipIterator = initialPlotElements.iterator();
			while (ipIterator.hasNext()) {
				Element current = (Element)ipIterator.next();
				String key = current.getName();
				String val = current.getTextTrim();
				Debug.print(this, Debug.INFO, "InitialPlot: key=" + key + " val=" + val);
				_initialPlotMap.put(key, val);
			}
		}
	}

	/**
	 *  Saves the member variables prior to being written out.
	 *  Called by AbstractXMLConfig.
	 */
	public void saveXMLConfig() {
		throw new RuntimeException("saveXMLConfig: not implemented");
	}
}
