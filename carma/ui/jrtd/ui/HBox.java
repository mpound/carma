package carma.ui.jrtd.ui;

import carma.ui.jrtd.util.Debug;
import carma.ui.jrtd.util.Datum;
import carma.ui.jrtd.util.DatumContainer;
import carma.ui.jrtd.util.ProtoBufUtil;

import java.util.List;
import java.util.ArrayList;

import java.awt.Font;

/**
 * A Horizontal box
 */
public class HBox extends UIBox implements Datum, DatumContainer {
    private static final Debug debug = new Debug("HBox", false);
    private static int serialNumber = 0;

    public HBox() {
        super(UIBox.HORIZONTAL);
        setName("hbox" + serialNumber);
        serialNumber++;
    }
    public HBox(Border border) {
        super(UIBox.HORIZONTAL, border);
    }
    public HBox(String s, Border border) {
        super(UIBox.HORIZONTAL, s, border);
    }
    public HBox(String s) {
        super(UIBox.HORIZONTAL, s);
    }

    /* ---------------------------------------------------------------------- */
    /* Protocol Buffer Creation                                               */
    /* ---------------------------------------------------------------------- */

    private static void checkBoxType(final rtdproto.RTD.RtBox pb) {
        final rtdproto.RTD.RtBox.BoxType[] types = {
            rtdproto.RTD.RtBox.BoxType.BOX_HORIZONTAL,
        };

        for (int i = 0; i < types.length; i++) {
            if (pb.getBoxtype() == types[i])
                return;
        }

        throw new IllegalArgumentException("pbInit: called with non-hbox");
    }

    public static HBox pbInit(final RtDisplay rtd, final rtdproto.RTD.RtBox pb) {
        debug.println("pbInit: begin");
        checkBoxType(pb);

        if (!pb.hasStaticdata()) {
            debug.println("pbInit: RtBox::StaticData is required");
            return null;
        }

        final rtdproto.RTD.RtBox.StaticData sd = pb.getStaticdata();

        final String title = sd.getTitle();
        final Border border = convertBorder(sd.getBorder());
        final HBox box = new HBox(title, border);

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
