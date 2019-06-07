package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;
import carma.ui.jrtd.ui.BoxConstraints.*;

import java.awt.*;

/**
 * Implements a spacer component for use in ui.BoxLayout.
 * Spacers grow proportionally when extra space is available.
 */
public class Spacer extends UIComponentBase implements Datum {
    private static final Debug debug = new Debug("Spacer", false);

    Dimension min;
    Dimension pref;
    static private int serialNumber = 0;

    public Spacer(int minWidth, int prefWidth, double weight) {
        BoxConstraints bc = super.getConstraints();
        min  = new Dimension(minWidth,  0);
        pref = new Dimension(prefWidth, 0);
        bc.stretchFactor = weight;
        bc.stretchType   = BoxConstraints.Stretch.PROPORTIONAL;
        setName("spacer" + serialNumber);
        serialNumber++;
    }
    public Spacer(int minWidth, int prefWidth) {
        this(minWidth, prefWidth, 1.0);
    }
    public Spacer(int minWidth, double weight) {
        this(minWidth, minWidth, weight);
    }
    public Spacer(int minWidth) {
        this(minWidth, minWidth, 1);
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

    public static Spacer pbInit(final RtDisplay rtd, final rtdproto.RTD.RtSpacer pb) {
        debug.println("pbInit: begin");

        if (!pb.hasStaticdata()) {
            throw new IllegalArgumentException("pbInit: RtSpacer::StaticData is required");
        }

        final rtdproto.RTD.RtSpacer.StaticData sd = pb.getStaticdata();

        final int minWidth = sd.getMinwidth();
        final int prefWidth = sd.getPrefwidth();
        final double weight = sd.getWeight();

        return new Spacer(minWidth, prefWidth, weight);
    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        // no dynamic data to update for this type of object
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
