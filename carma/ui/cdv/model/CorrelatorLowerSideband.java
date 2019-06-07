// $Id: CorrelatorLowerSideband.java,v 1.3 2005/04/05 23:24:11 rick Exp $

package carma.ui.cdv.model;

import java.nio.ByteBuffer;

/**
 * @file CorrelatorLowerSideband.java
 * 
 * Started: Tue Mar 29 15:15:23 PST 2005
 * 
 * @version $Revision: 1.3 $, $Date: 2005/04/05 23:24:11 $
 * 
 * @author Rick Hobbs
 */
public class CorrelatorLowerSideband extends CorrelatorSideband {

    /**
     * Constructor.
     */
    public CorrelatorLowerSideband() {
        setAuto(false);
        setLSB(true);
    }

    /**
     *  Static method for unit Testing.
     */
    public static void main(String[] args) {
        CorrelatorLowerSideband clsb = new CorrelatorLowerSideband();
        clsb.createTestData();
        ByteBuffer bb = clsb.serial();

        System.err.println("ByteBuffer(CorrelatorLowerSideband) length= " +
                           bb.array().length);

        CorrelatorLowerSideband clsb2 = new CorrelatorLowerSideband();
        clsb2.deserial(bb);
        if (clsb.isEqual(clsb2))
            System.err.println("CorrelatorLowerSideband serial/deserial: PASSED");
        else
            System.err.println("CorrelatorLowerSideband serial/deserial: FAILED");

    }

} // End class CorrelatorLowerSideband

