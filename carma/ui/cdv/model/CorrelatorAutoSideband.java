// $Id: CorrelatorAutoSideband.java,v 1.2 2005/03/30 00:11:23 rick Exp $

package carma.ui.cdv.model;

import java.nio.ByteBuffer;

/**
 * @file CorrelatorAutoSideband.java
 * 
 * Started: Tue Mar 29 15:08:24 PST 2005
 * 
 * @version $Revision: 1.2 $, $Date: 2005/03/30 00:11:23 $
 * 
 * @author Rick Hobbs
 */
public class CorrelatorAutoSideband extends CorrelatorSideband {

    /**
     * Constructor.
     */
    public CorrelatorAutoSideband() {
        setAuto(true);
        setUSB(false);
        setLSB(false);
    }

    public boolean isAuto() {
        return true;
    }

    public boolean isLSB() {
        return false;
    }

    public boolean isUSB() {
        return false;
    }

    /**
     *  Static method for unit Testing.
     */
    public static void main(String[] args) {
        CorrelatorAutoSideband casb = new CorrelatorAutoSideband();
        casb.createTestData();
        ByteBuffer bb = casb.serial();

        System.err.println("ByteBuffer(CorrelatorAutoSideband) length= " +
                           bb.array().length);

        CorrelatorAutoSideband casb2 = new CorrelatorAutoSideband();
        casb2.deserial(bb);
        if (casb.isEqual(casb2))
            System.err.println("CorrelatorAutoSideband serial/deserial: PASSED");
        else
            System.err.println("CorrelatorAutoSideband serial/deserial: FAILED");
    }

} // End class CorrelatorAutoSideband

