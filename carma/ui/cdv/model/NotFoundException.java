// $Id: NotFoundException.java,v 1.1 2005/04/05 23:24:54 rick Exp $

package carma.ui.cdv.model;


/**
 * @file NotFoundException.java
 * 
 * Started: Fri Apr  1 12:29:48 PST 2005
 * 
 * @version $Revision: 1.1 $, $Date: 2005/04/05 23:24:54 $
 * 
 * @author Rick Hobbs
 */
public class NotFoundException extends Exception {

    /**
     * Constructor
     */
    public NotFoundException() {
        super();
    }

    /**
     *  Constructor
     */
    public NotFoundException(String msg) {
        super(msg);
    }

    /**
     *  Static method for unit Testing.
     */
    public static void main(String[] args) {
        NotFoundException nfe = new NotFoundException("band 1 not found");
        System.err.println("Error: " + nfe);
    }

} // End class NotFoundException

