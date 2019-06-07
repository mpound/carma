package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;

/**
 * This class implements a string containing times that is sent from the server
 * over the network.
 *
 * @author  S Scott
 * $Id: TimeString.java,v 1.8 2013/11/19 02:59:11 iws Exp $
 */
public class TimeString implements Datum {
    private static final Debug debug = new Debug("TimeString", false);

    private String title;
    private int    stringLength;
    private int    substringLength;
    private String ut;
    private String lst;
    private String local;

    public TimeString(String title, int stringLength) {
        this(title,stringLength,8);
    }

    public TimeString(String title, int stringLength, int substringLength) {
        this.title        = title;
        this.stringLength = stringLength;
        this.substringLength = substringLength;
    }

    public String getUT() {
        return ut;
    }

    public String getLST() {
        return lst;
    }

    public String getLocalObservatoryTime() {
        return local;
    }

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Creation                                               */
    /* ---------------------------------------------------------------------- */

    public static TimeString pbInit(final RtDisplay rtd, final rtdproto.RTD.RtTimeString pb) {
        debug.println("pbInit: begin");

        if (!pb.hasStaticdata()) {
            throw new IllegalArgumentException("pbInit: RtTimeString::StaticData is required");
        }

        final rtdproto.RTD.RtTimeString.StaticData sd = pb.getStaticdata();
        return new TimeString(sd.getTitle(), sd.getWidth());
    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        if (!rtobj.hasTimestring()) {
            throw new IllegalArgumentException("RtObject does not contain RtTimeString");
        }

        final rtdproto.RTD.RtTimeString pb = rtobj.getTimestring();
        if (!pb.hasDynamicdata()) {
            throw new IllegalArgumentException("RtTimeString does not contain DynamicData");
        }

        final rtdproto.RTD.RtTimeString.DynamicData dd = pb.getDynamicdata();
        this.ut = dd.getUt();
        this.lst = dd.getLst();
        this.local = dd.getLocal();
    }
}
