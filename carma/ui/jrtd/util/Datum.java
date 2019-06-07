package carma.ui.jrtd.util;

/**
 * The Datum interface defines common functionality for data sent over in realtime
 * for items that are visible (like cells) and those that are not directly visible.
 *
 * Common functionality is the ability to update itself from the dynamic data
 * present in a protocol buffer. This update triggers an update of the view or
 * other internal structures.
 *
 * @author  Steve Scott
 * @version 1.0 7-May-1998
 */
public interface Datum {
    /**
     * Update the internal structure and/or the user interface view of an object
     * by using the data stored within a rtdproto.RTD.RtObject protocol buffer.
     *
     * Callers are required to ensure that the RtObject contains the correct
     * sort of object corresponding to the one being updated.
     */
    public void pbUpdate(final rtdproto.RTD.RtObject rtobj);
}

/* vim: set ts=4 sts=4 sw=4 et: */
