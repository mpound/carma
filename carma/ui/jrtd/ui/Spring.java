package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;
import carma.ui.jrtd.ui.BoxConstraints.*;

import java.awt.*;

/**
 * Implements a spring component for use in ui.BoxLayout.
 *
 */
public class Spring extends UIComponentBase implements Datum {
    private static final Debug debug = new Debug("Spring", false);

    private final Dimension min;
    private final Dimension pref;
    static private int serialNumber = 0;

    public Spring(int minWidth, int prefWidth, double springiness) {
        BoxConstraints bc = super.getConstraints();
        min  = new Dimension(minWidth,  0);
        pref = new Dimension(prefWidth, 0);
        bc.stretchFactor = springiness;
        bc.stretchType   = BoxConstraints.Stretch.SPRING;
        setName("spring" + serialNumber);
        serialNumber++;
    }

    public Spring(int minWidth, int prefWidth) {
        this(minWidth, prefWidth, 1.0);
    }

    public Spring(int minWidth, double springiness) {
        this(minWidth, minWidth, springiness);
    }

    public Spring(int minWidth) {
        this(minWidth, minWidth, 1);
    }

    public Spring(double springiness) {
        this(0, 0, springiness);
    }

    public Spring() {
        this(0, 0, 1);
    }

    public Dimension getMinimumSize() {
        return new Dimension(min);
    }

    public Dimension getPreferredSize() {
        return new Dimension(pref);
    }

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Creation                                               */
    /* ---------------------------------------------------------------------- */

    public static Spring pbInit(final RtDisplay rtd, final rtdproto.RTD.RtSpring pb) {
        debug.println("pbInit: begin");

        if (!pb.hasStaticdata()) {
            throw new IllegalArgumentException("pbInit: RtSpring::StaticData is required");
        }

        final rtdproto.RTD.RtSpring.StaticData sd = pb.getStaticdata();

        final int minWidth = sd.getMinwidth();
        final int prefWidth = sd.getPrefwidth();
        final double springiness = sd.getSpringiness();

        return new Spring(minWidth, prefWidth, springiness);
    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        // no dynamic data to update for this type of object
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
