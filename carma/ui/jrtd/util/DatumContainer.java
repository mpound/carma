package carma.ui.jrtd.util;

/**
 * An interface which describes the capability for an object which is capable
 * of containing many Datum objects. Examples are RtArea, RtBox, RtDisplay and
 * RtTable.
 *
 * Using this interface, new objects can be added during object creation. When
 * the object is being updated, all sub-objects will arrive in the same order.
 * With this interface, they can each be updated individually.
 */
public interface DatumContainer {
    /**
     * Add a single datum to this container.
     *
     * @return the Datum just added
     */
    public Datum addDatum(final Datum datum);

    /**
     * Get the number of datums in this container.
     */
    public int getDatumCount();

    /**
     * Get a single Datum from this container by zero-based index.
     */
    public Datum getDatum(final int index);
}

/* vim: set ts=4 sts=4 sw=4 et: */
