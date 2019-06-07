package carma.ui.jrtd.util;

import java.util.List;

import carma.ui.jrtd.util.Datum;
import carma.ui.jrtd.util.Debug;

import carma.ui.jrtd.ui.RtDisplay;
import carma.ui.jrtd.ui.Cell;
import carma.ui.jrtd.ui.RtLabel;
import carma.ui.jrtd.ui.Area;
import carma.ui.jrtd.ui.Table;
import carma.ui.jrtd.ui.HBox;
import carma.ui.jrtd.ui.VBox;
import carma.ui.jrtd.ui.Spacer;
import carma.ui.jrtd.ui.Spring;
import carma.ui.jrtd.ui.TimeString;
import carma.ui.jrtd.ui.RtMenuBar;
import carma.ui.jrtd.ui.RtTimePanel;
import carma.ui.jrtd.ui.AzelPlot;
import carma.ui.jrtd.ui.LayoutCode;

import java.awt.Color;
import java.awt.Container;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;

import rtdproto.RTD.RtObject;

public final class ProtoBufUtil {
    private static final Debug debug = new Debug("ProtoBufUtil", false);

    /**
     * Recursively update all sub-objects within a protobuf.
     */
    public static void updateObjects(final List<RtObject> objects, final List<Datum> datums) {
        final int nObjects = objects.size();
        final int nDatums = datums.size();
        if (nObjects != nDatums) {
            throw new IllegalArgumentException("nObjects=" + nObjects
                    + " and nDatums=" + nDatums + " have different lengths");
        }

        for (int i = 0; i < nObjects; i++) {
            final RtObject rtobj = objects.get(i);
            final Datum datum = datums.get(i);

            datum.pbUpdate(rtobj);
        }
    }

    /**
     * Process a "repeated RtObject" field as part of a GUI component.
     *
     * @param rtd the RTD to use
     * @param uiParent the GUI parent container of all objects in this list
     * @param datumParent the DatumContainer which will hold these objects
     * @param objects the sub-objects to process
     */
    public static void processObjectList(
            final RtDisplay rtd,
            final Container uiParent,
            final DatumContainer datumParent,
            final List<RtObject> objects) {

        debug.println("processObjectList: number of objects: " + objects.size());

        for (final RtObject object : objects) {
            processObject(rtd, uiParent, datumParent, object);
        }

        debug.println("processObjectList: success");
    }

    /**
     * Process a single RtObject of any type.
     *
     * This function may call itself recursively to process an object which has
     * sub-objects (example: RtContainer).
     */
    private static void processObject(
            final RtDisplay rtd,
            final Container uiParent,
            final DatumContainer datumParent,
            final RtObject object) {

        if (object.hasCell()) {
            debug.println("processObject: processing RtCell");
            final Cell c = Cell.pbInit(rtd, object.getCell());
            if (uiParent instanceof Area) {
                uiParent.add(c, new Character(convertLayout(c.getLayoutCode())));
            } else {
                uiParent.add(c);
            }
            datumParent.addDatum(c);
        } else if (object.hasRow()) {
            debug.println("processObject: processing RtRow");
        } else if (object.hasColumn()) {
            debug.println("processObject: processing RtColumn");
        } else if (object.hasLabel()) {
            debug.println("processObject: processing RtLabel");
            final RtLabel l = RtLabel.pbInit(rtd, object.getLabel());
            if (uiParent instanceof Area) {
                uiParent.add(l, new Character(convertLayout(l.getLayoutCode())));
            } else {
                uiParent.add(l);
            }
            datumParent.addDatum(l);
        } else if (object.hasArea()) {
            debug.println("processObject: processing RtArea");
            final Area a = Area.pbInit(rtd, object.getArea());
            uiParent.add(a);
            datumParent.addDatum(a);
        } else if (object.hasTable()) {
            debug.println("processObject: processing RtTable");
            final Table t = Table.pbInit(rtd, object.getTable());
            uiParent.add(t);
            datumParent.addDatum(t);
        } else if (object.hasBox()) {
            debug.println("processObject: processing RtBox");
            final rtdproto.RTD.RtBox pb = object.getBox();
            final rtdproto.RTD.RtBox.BoxType boxtype = pb.getBoxtype();

            if (boxtype == rtdproto.RTD.RtBox.BoxType.BOX_FOLDER) {
                final VBox box = VBox.pbInit(rtd, pb);
                box.setBackground(Color.white);
                rtd.addFolder(box);
                datumParent.addDatum(box);
                box.requestFocus();
            } else if (boxtype == rtdproto.RTD.RtBox.BoxType.BOX_HORIZONTAL) {
                final HBox box = HBox.pbInit(rtd, pb);
                uiParent.add(box);
                datumParent.addDatum(box);
            } else if (boxtype == rtdproto.RTD.RtBox.BoxType.BOX_VERTICAL) {
                final VBox box = VBox.pbInit(rtd, pb);
                uiParent.add(box);
                datumParent.addDatum(box);
            } else {
                throw new IllegalArgumentException("unknown RtBox::BoxType: " + boxtype);
            }
        } else if (object.hasSpring()) {
            debug.println("processObject: processing RtSpring");
            final Spring s = Spring.pbInit(rtd, object.getSpring());
            uiParent.add(s);
            datumParent.addDatum(s);
        } else if (object.hasSpacer()) {
            debug.println("processObject: processing RtSpacer");
            final Spacer s = Spacer.pbInit(rtd, object.getSpacer());
            uiParent.add(s);
            datumParent.addDatum(s);
        } else if (object.hasTimestring()) {
            debug.println("processObject: processing RtTimeString");
            final TimeString ts = TimeString.pbInit(rtd, object.getTimestring());
            if (uiParent instanceof RtTimePanel) {
                final RtTimePanel tp = (RtTimePanel)uiParent;
                tp.setTimeString(ts);
                tp.update(false);
            }
            datumParent.addDatum(ts);
        } else if (object.hasTimepanel()) {
            debug.println("processObject: processing RtTimePanel");
            final RtTimePanel tp = RtTimePanel.pbInit(rtd, object.getTimepanel());
            rtd.setTimePanel(tp);
            datumParent.addDatum(tp);
        } else if (object.hasAzelplot()) {
            debug.println("processObject: processing RtAzelPlot");
            final AzelPlot aep = AzelPlot.pbInit(rtd, object.getAzelplot());
            uiParent.add(aep);
            datumParent.addDatum(aep);
        } else if (object.hasMenu()) {
            debug.println("processObject: processing RtMenu");
            final RtMenuBar mb = RtMenuBar.pbInit(rtd, object.getMenu());
            rtd.setRtMenuBar(mb);
            datumParent.addDatum(mb);
        } else {
            System.err.println("processObject: no object in container!");
            throw new IllegalArgumentException("processObject: no object in container!");
        }
    }

    public static LayoutCode convertLayout(final rtdproto.RTD.Layout layout) {
        switch (layout) {
        case NONE_LAYOUT:
            return LayoutCode.NONE_LAYOUT;
        case UNFILLED_LAYOUT:
            return LayoutCode.UNFILLED_LAYOUT;
        case CHAIN_LAYOUT:
            return LayoutCode.CHAIN_LAYOUT;
        case EOL_CENTERED_LAYOUT:
            return LayoutCode.EOL_CENTERED_LAYOUT;
        case EOL_RIGHT_JUSTIFIED_LAYOUT:
            return LayoutCode.EOL_RIGHT_JUSTIFIED_LAYOUT;
        case EOL_LEFT_JUSTIFIED_LAYOUT:
            return LayoutCode.EOL_LEFT_JUSTIFIED_LAYOUT;
        }

        throw new IllegalArgumentException("unknown layout code: " + layout);
    }

    private static char convertLayout(final LayoutCode layout) {
        switch (layout) {
        case NONE_LAYOUT:
            return 'N';
        case UNFILLED_LAYOUT:
            return 'U';
        case CHAIN_LAYOUT:
            return 'C';
        case EOL_CENTERED_LAYOUT:
            return 'E';
        case EOL_RIGHT_JUSTIFIED_LAYOUT:
            return 'R';
        case EOL_LEFT_JUSTIFIED_LAYOUT:
            return 'L';
        }

        throw new IllegalArgumentException("unknown layout code: " + layout);
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
