package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.Debug;
import carma.ui.jrtd.util.Datum;
import carma.ui.jrtd.util.DatumContainer;
import carma.ui.jrtd.util.ProtoBufUtil;

import java.util.List;
import java.util.ArrayList;

import java.awt.Font;

/**
 * A vertical box
 */
public class VBox extends UIBox implements Datum, DatumContainer {
    private static final Debug debug = new Debug("VBox", false);
    static private int serialNumber = 0;
    public int datumStart = 0;
    public int datumEnd   = 0;

    public VBox() {
        super(UIBox.VERTICAL);
        setName("vbox" + serialNumber);
        serialNumber++;
    }
    public VBox(Border border) {
        super(UIBox.VERTICAL, border);
    }
    public VBox(String s) {
        super(UIBox.VERTICAL, s);
    }
    public VBox(String s, Border border ) {
        super(UIBox.VERTICAL, s, border);
    }
    public void setDatumStart(int i) {datumStart=i;}
    public void setDatumEnd(int i) {datumEnd=i;}
    public int getDatumStart() {return datumStart;}
    public int getDatumEnd() {return datumEnd;}

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Creation                                               */
    /* ---------------------------------------------------------------------- */

    private static void checkBoxType(final rtdproto.RTD.RtBox pb) {
        final rtdproto.RTD.RtBox.BoxType[] types = {
            rtdproto.RTD.RtBox.BoxType.BOX_VERTICAL,
            rtdproto.RTD.RtBox.BoxType.BOX_FOLDER,
        };

        for (int i = 0; i < types.length; i++) {
            if (pb.getBoxtype() == types[i])
                return;
        }

        throw new IllegalArgumentException("pbInit: called with non-vbox");
    }

    public static VBox pbInit(final RtDisplay rtd, final rtdproto.RTD.RtBox pb) {
        debug.println("pbInit: begin");
        checkBoxType(pb);

        if (!pb.hasStaticdata()) {
            debug.println("pbInit: RtBox::StaticData is required");
            return null;
        }

        final rtdproto.RTD.RtBox.StaticData sd = pb.getStaticdata();

        final String title = sd.getTitle();
        final Border border = convertBorder(sd.getBorder());
        final VBox box = new VBox(title, border);

        // update font size
        {
            final int fontSize = sd.getFontsize() + rtd.getParams().getDeltaFontSize();
            final Font font = (box.getFont() == null ? rtd.getFont() : box.getFont());
            final Font newFont = new Font(font.getFamily(), font.getStyle(), fontSize);
            box.setFont(newFont);
        }

        {
            BoxConstraints bc = new BoxConstraints();
            final double stretchfactor = sd.getStretchfactor();

            if (sd.getStretchtype() == rtdproto.RTD.RtBox.StretchType.STRETCH_SPRING) {
                bc.stretchType = BoxConstraints.Stretch.SPRING;
            }

            bc.alignment = BoxConstraints.Align.FILL;
            box.setConstraints(bc);
        }

        // process all sub-objects
        ProtoBufUtil.processObjectList(rtd, box, box, pb.getObjectsList());
        return box;
    }

    /* ---------------------------------------------------------------------- */
    /* Datum Interface                                                        */
    /* ---------------------------------------------------------------------- */

    public void pbUpdate(final rtdproto.RTD.RtObject rtobj) {
        // check that we have the correct type of object
        if (!rtobj.hasBox()) {
            throw new IllegalArgumentException("RtObject does not contain RtBox");
        }

        final rtdproto.RTD.RtBox pb = rtobj.getBox();
        checkBoxType(pb);

        // no dynamic data to update for this type of object

        // update all sub-objects in this protocol buffer
        ProtoBufUtil.updateObjects(pb.getObjectsList(), datumList);
    }

    /* ---------------------------------------------------------------------- */
    /* DatumContainer Interface                                               */
    /* ---------------------------------------------------------------------- */

    private final java.util.List<Datum> datumList = new ArrayList<Datum>();

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
