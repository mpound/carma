// $Id: CorrelatorUpperSideband.java,v 1.2 2005/04/05 23:24:12 rick Exp $

package carma.ui.cdv.model;

import java.nio.ByteBuffer;

/**
 * @file CorrelatorUpperSideband.java
 * 
 * Started: Tue Mar 29 15:16:50 PST 2005
 * 
 * @version $Revision: 1.2 $, $Date: 2005/04/05 23:24:12 $
 * 
 * @author Rick Hobbs
 */
public class CorrelatorUpperSideband extends CorrelatorSideband {

    /**
     * Constructor.
     */
    public CorrelatorUpperSideband() {
        setAuto(false);
        setUSB(true);
    }

    public boolean isAuto() {
        return false;
    }

    public boolean isLSB() {
        return false;
    }

    public boolean isUSB() {
        return true;
    }

    /**
     *  Static method for unit Testing.
     */
    public static void main(String[] args) {
        CorrelatorUpperSideband cusb = new CorrelatorUpperSideband();
        cusb.createTestData();
        ByteBuffer bb = cusb.serial();

        System.err.println("ByteBuffer(CorrelatorUpperSideband) length= " +
                           bb.array().length);

        CorrelatorUpperSideband cusb2 = new CorrelatorUpperSideband();
        cusb2.deserial(bb);
        if (cusb.isEqual(cusb2))
            System.err.println("CorrelatorUpperSideband serial/deserial: PASSED");
        else
            System.err.println("CorrelatorUpperSideband serial/deserial: FAILED");
    }

} // End class CorrelatorUpperSideband

