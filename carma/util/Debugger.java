
// $Id: Debugger.java,v 1.2 2005/04/13 21:54:03 rick Exp $

package carma.util;

import java.io.*;

/**
 *  Class used to print debug messages at various levels of verbosity
 *  and types.
 *
 *	@author		Rick Hobbs
 *	@version	$Revision: 1.2 $, $Date: 2005/04/13 21:54:03 $
 *	@since		JDK1.2
 */
public class Debugger {
	// verbosity
	/**
	 *  Verbosity Level
	 */
	public static final int NONE	= 10;
	/**
	 *  Verbosity Level
	 */
	public static final int LOW		= 11;
	/**
	 *  Verbosity Level
	 */
	public static final int MED		= 12;
	/**
	 *  Verbosity Level
	 */
	public static final int HIGH	= 13;
	/**
	 *  Verbosity Level
	 */
	public static final int TRACE   = 14;
	/**
	 *  Verbosity Level
	 */
	public static final int TRACE_ONLY = 20;
	/**
	 *  Verbosity Level
	 */
	public static final int HIGH_ONLY  = 21;
	/**
	 *  Verbosity Level
	 */
	public static final int MED_ONLY   = 22;

	// types
	/**
	 *  Message Type
	 */
	public static final int INFO	= 0;
	/**
	 *  Message Type
	 */
	public static final int STATUS	= 1;
	/**
	 *  Message Type
	 */
	public static final int WARNING	= 2;
	/**
	 *  Message Type
	 */
	public static final int ERROR	= 3;

	private static final String[] TYPE = {"INFO", "STATUS", "WARNING", "ERROR"};

	private static String _prevClass;
	private static String _curClass;
	private static int _verb = NONE;
	private static boolean _printIt;

	private Debugger() { }

	/**
	 *  Sets the verbosity level. All messages at or less than
	 *  this value will be printed.
	 *	@param o  Object setting the verbosity
	 *  @param verb Verbosity level
	 */
	public static void setVerbosity(Object o, int verb) {
        if (o != null)
            _curClass = o.getClass().getName();
        else
            _curClass = "null";
		checkVerb(verb, true);
	}

	/**
	 *  Outputs a message of a given type and verbosity level.
	 *  @param o Object printing message. Usually <b> this </b>
	 *  @param verb Verbosity level of this message.
	 *  @param type Type of message
	 *  @param message The message
	 */
	public static void printDebug(Object o, int verb, 
	                              int type, String message) {
		_curClass = o.getClass().getName();
		checkVerb(verb, false);
		checkObject(o);
		printMessage(type, message);
	}

	private static void checkVerb(int verb, boolean setIt) {
		// first check if verb is valid
		if (verb == NONE || verb == LOW || verb == MED || verb == HIGH ||
		    verb == TRACE || verb == TRACE_ONLY || verb == HIGH_ONLY ||
			verb == MED_ONLY)
			if (setIt) {
				_verb = verb;
				System.err.println("** Debugger: <INFO> " +
				                  "Verbosity being set in class: " + _curClass);
			} else
				switch (_verb) {
					case (TRACE_ONLY):
						if (verb == TRACE)
							_printIt = true;
						else
							_printIt = false;
						break;
					case (HIGH_ONLY):
						if (verb == HIGH)
							_printIt = true;
						else
							_printIt = false;
						break;
					case (MED_ONLY):
						if (verb == MED)
							_printIt = true;
						else
							_printIt = false;
						break;
					default:
						if (verb <= _verb)
							_printIt = true;
						else
							_printIt = false;
						break;
				}
		else
			System.err.println("** Debugger: <ERROR> " +
			                   "Unknown verbosity set in class: " + _curClass);
	}

	private static void checkObject(Object o) {
		if (_printIt) {
			if (_prevClass == null) {
				_prevClass = _curClass;
				System.err.println("\n" + _curClass + ":");
			} else
				if (!_prevClass.equals(_curClass)) {
					_prevClass = _curClass;
					System.err.println("\n" + _curClass + ":");
				}
		}
	}

	private static void printMessage(int type, String mess) {
		if (_printIt)
			// check for valid type
			if (type == INFO || type == STATUS || type == WARNING ||
			    type == ERROR)
					System.err.println("\t <" + TYPE[type] + "> " + mess);
			else
				System.err.println("Debugger: <ERROR> unknown message type");
	}
}
