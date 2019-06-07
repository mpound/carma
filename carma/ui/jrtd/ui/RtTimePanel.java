package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.*;

import java.awt.*;
import java.text.*;
import java.util.*;
import javax.swing.*;

/**
 * RtTimePanel to reconcile differences in ovro.cma.rtd.TimePanel
 * and EDU.umd.astro.jstatus.TimePanel. This is a lightweight component.
 *
 * @author Steve Scott
 * @author scott@ovro.caltech.edu
 * @author Marc Pound
 * @author mpound@astro.umd.edu
 * @version 1.0 02-Mar-1999
 * <br>History:
 * <br> v1.0
 * <br> - mwp created from Steve's original TimePanel.java
 * <br> - got rid of extraneous imports.
 * <br> - use StringBuffer for creating server/location strings, rather than "+"
 */
public class RtTimePanel extends JComponent implements Datum, DatumContainer {
    private static final Debug debug = new Debug("RtTimePanel", false);

    private int width;
    private int height;
    private boolean invisible_;
    protected Font    f;
    protected RtDisplay rtd ;
    protected SimpleDateFormat sf = new SimpleDateFormat();
    protected int charWidth;
    protected int textHeight;
    protected int ascent;
    protected int minChars           = 36;
    protected int preferredChars     = minChars + 9;
    protected int maxChars           = preferredChars + 12;
    protected boolean connectionDead = false;
    protected TimeString timeString;
    protected Color oldbg;
    protected String local           = ""; // local time
    protected String location        = ""; // server location, e.g. "OVRO", "BIMA";
    protected String server          = ""; // server time
    protected String oldServer       = ""; // server string at previous update

    public RtTimePanel(RtDisplay rtd, boolean visible) {
        this.rtd     = rtd;
        invisible_    = !visible;
        Font rtdfont = rtd.getFont();
        f = new Font("Helvetica",    Font.PLAIN, rtdfont.getSize()+2);
        setVisible(visible);
        setFont(f);
        setBackground(Color.white);
        FontMetrics metrics = getFontMetrics(f);
        ascent = metrics.getMaxAscent();
        textHeight = metrics.getHeight() - metrics.getLeading();
        charWidth  = metrics.charWidth('3');
        sf.setTimeZone(TimeZone.getDefault());
        setName("TimePanel");
        //onScreen = true; // Make sure this is displayed!!
        setBackground(Color.white);
        this.location = "CARMA";
        this.sf.applyPattern("HH:mm:ssz");
    }

    public boolean isInvisible() {
        return invisible_;
    }

    public RtDisplay getRtd() {
        return rtd;
    }

    public TimeString getTimeString() {
        return timeString;
    }

    public void setTimeString(TimeString timeString) {
        this.timeString = timeString;
    }

    public void update(boolean connectionDead) {
        this.connectionDead = connectionDead;
        if (connectionDead) setBackground(Color.red);
        else                setBackground(Color.white);
        //if (!isVisible() || !onScreen) return;
        if (!isVisible()) return;
        Dimension d = getSize();
        width  = d.width;
        height = d.height;
        String time = sf.format(new Date());
        local = new StringBuffer("Local ").append(time).toString();
        StringBuffer sb = new StringBuffer(location);
        if (connectionDead)
            server = sb.append(" Connection stalled").toString();
        else if ( width >= maxChars*charWidth)
            server = sb.append(" ").append(timeString.getUT()).append("UT/")
                .append(timeString.getLocalObservatoryTime())
                .append("/")
                .append(timeString.getLST())
                .append("LST").toString();
        else if ( width >= preferredChars*charWidth)
            server = sb.append(" ").append(timeString.getUT()).append("UT/")
                .append(timeString.getLST())
                .append("LST").toString();
        else
            server = sb.append(" ").append(timeString.getUT())
                .append("UT").toString();
        repaint();
    }

    /* ---------------------------------------------------------------------- */
    /* javax.swing.JComponent Overrides                                       */
    /* ---------------------------------------------------------------------- */

    /**
     * Clears the background and draws the time strings.
     */
    @Override
    public void paint(Graphics g) {
        //if (!isVisible() || !onScreen || !isValid() || (width <= 0)) return;
        if (!isVisible() || !isValid() || (width <= 0)) return;
        if (g == null) return;
        g.setColor(getBackground());
        g.fillRect(0, 0, width, height);
        g.setFont(f);
        g.setColor(Color.black);
        int x = width - (local.length()*charWidth + 3);
        g.drawString(server, 2, 2+ascent);
        g.drawString(local,  x, 2+ascent);
    }

    /**
     * Gets the mininimum size of this component, which is determined
     * from the minimum number of chars and the text height.
     * @return A dimension object indicating this component's minimum size.
     * @see #getPreferredSize
     * @see java.awtLayoutManager
     */
    @Override
    public Dimension getMinimumSize() {
        return(new Dimension(minChars*charWidth, textHeight+2));
    }

    /**
     * Gets the preferred size of this component, which is determined
     * from the preferred number of chars and the text height.
     * @return A dimension object indicating this component's preferred size.
     * @see #getMinimumSize
     * @see java.awt.LayoutManager
     */
    @Override
    public Dimension getPreferredSize() {
        return(new Dimension(preferredChars*charWidth, textHeight+2));
    }

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Creation                                               */
    /* ---------------------------------------------------------------------- */

    public static RtTimePanel pbInit(final RtDisplay rtd, final rtdproto.RTD.RtTimePanel pb) {
        debug.println("pbInit: begin");

        if (!pb.hasStaticdata()) {
            throw new IllegalArgumentException("pbInit: RtTimePanel::StaticData is required");
        }

        final rtdproto.RTD.RtTimePanel.StaticData sd = pb.getStaticdata();

        // TODO FIXME: title is unused

        final RtTimePanel tp = new RtTimePanel(rtd, sd.getVisible());

        // process all sub-objects
        ProtoBufUtil.processObjectList(rtd, tp, tp, pb.getObjectsList());
        return tp;
    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        // check that we have the correct type of object
        if (!rtobj.hasTimepanel()) {
            throw new IllegalArgumentException("RtObject does not contain RtTimePanel");
        }

        final rtdproto.RTD.RtTimePanel pb = rtobj.getTimepanel();

        // no dynamic data to update for this type of object

        // update all sub-objects in this protocol buffer
        ProtoBufUtil.updateObjects(pb.getObjectsList(), datumList);

        // force screen update now: the connection is not dead, we just got
        // an update which caused the call into this code!
        update(false);
    }

    /* ---------------------------------------------------------------------- */
    /* DatumContainer Interface                                               */
    /* ---------------------------------------------------------------------- */

    protected final java.util.List<Datum> datumList = new ArrayList<Datum>();

    public Datum addDatum(final Datum datum) {
        this.datumList.add(datum);
        return datum;
    }

    public int getDatumCount() {
        return this.datumList.size();
    }

    public Datum getDatum(final int index) {
        return this.datumList.get(index);
    }
}
