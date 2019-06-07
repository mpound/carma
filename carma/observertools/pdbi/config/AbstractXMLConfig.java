package carma.observertools.pdbi.config;


import java.io.*;

import org.jdom.*;
import org.jdom.input.*;
import org.jdom.output.*;

/**
*  Abstract Class used to prepare concrete classes for parsing
*  XML documents. Based on file of the same name from CDV.
*
*  @author Douglas Friedel
*  @since JDK1.5
*/
public abstract class AbstractXMLConfig {
	protected InputStream _ins;
	protected Element _root;
	private Document _doc;
	private String _filename;

	protected AbstractXMLConfig(InputStream in) throws IOException {
		_ins = in;
		_doc = null;

		// initialize by reading in the XML document and
		// getting the root Element
		initConfig();

		// parse the XML configuration file
		// concrete classes must implement this
		parseConfig();

		// after finished parsing. Close the Stream
		_ins.close();
	}

	protected AbstractXMLConfig(String filename) throws IOException {
		this(new FileInputStream(filename));

		// Save the filename. This will be used if saveConfig is
		// called with a null argument.
		_filename = filename;
	}

	/**
	 *  starts the parsing process and gets the root node
	 *  for concrete classes to use.
	 */
	private void initConfig() {
		try {
			// request DOM Implementation and Xerces Parser
			//DOMBuilder builder = new DOMBuilder(
			//                         "org.jdom.adapters.XercesDOMAdapter");
			SAXBuilder builder = new SAXBuilder(false);
			
			// Get the Configuration Document from the InputStream
			if (_ins != null)
				_doc = builder.build(_ins);
			else
				System.err.println("InputStream null");

			// Get the root element
			_root = _doc.getRootElement();
			if (_root == null)
				System.err.println("_root is null");
		} catch (JDOMException e) {
			e.printStackTrace();
		} catch (IOException ioe) {
         ioe.printStackTrace();
     }
	}

	/**
	 *  Saves the XML configuration
	 */
	public synchronized void saveXMLConfig(String filename) throws IOException {
		OutputStream os = null;
		if (_filename == null)
			System.err.println("_filename is null");
		if (filename == null)	// use same file from which config was read.
			os = new FileOutputStream(_filename);
		else
			os = new FileOutputStream(filename);

		// call concrete classes implementation of this
		saveXMLConfig();

		// Output the document
		XMLOutputter fmt = new XMLOutputter();
		fmt.output(_doc, os);

		// Close the stream
			os.close();
	}

	/**
	 *  Concrete classes must implement this for specific XML Documents
	 */
	protected abstract void parseConfig();

	/**
	 *  Concrete classes must implement this for specific XML Documents
	 */
	protected abstract void saveXMLConfig();
}
