package carma.ui.jplotter.jfree;

import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.jfree.data.Range;
import org.jfree.data.general.DatasetChangeEvent;
import org.jfree.data.general.DatasetChangeListener;
import org.jfree.data.general.DatasetUtilities;
import org.jfree.data.time.FixedMillisecond;
import org.jfree.data.time.RegularTimePeriod;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesDataItem;
import org.jfree.data.time.TimeSeriesCollection;
import org.jfree.data.xy.AbstractXYDataset;
import org.jfree.data.xy.XYDataset;

/**
 * A {@link XYDataset} implementation that presents a window(subset) of the
 * items in an underlying dataset.  The index of the first "visible"
 * item can be modified, which provides a means of "sliding" through
 * the items in the underlying series dataset.
 *
 * TODO FIXME:
 * This class only works with TimeSeries which are filled with
 * FixedMillisecond types. Everything else will break horribly.
 */
public class SlidingTimeSeriesCollection
	extends AbstractXYDataset
	implements XYDataset, DatasetChangeListener
{
	/** The underlying TimeSeriesCollection. */
	private final TimeSeriesCollection underlying;

	/** The index of the start of the window. */
	private int windowStartIndex;

	/** The maximum number of items to present in the window. */
	private int windowItemCount;

	/** The length of the time interval between items, in milliseconds. */
	private final int itemInterval;

	/** The result dataset */
	private final TimeSeriesCollection resultDataset;

	/**
	 * Creates a new <code>SlidingTimeSeriesCollection</code> class that applies a
	 * dynamically updateable sliding through the underlying dataset.
	 *
	 * @param underlying  the underlying dataset (<code>null</code> not permitted).
	 * @param windowItemCount the number of items in the window
	 * @param itemInterval the interval (in milliseconds) between each item
	 */
	public SlidingTimeSeriesCollection(TimeSeriesCollection underlying, int windowItemCount, int itemInterval) {
		this.underlying = underlying;
		this.underlying.addChangeListener(this);
		this.windowItemCount = windowItemCount;
		this.windowStartIndex = 0;
		this.itemInterval = itemInterval;
		this.resultDataset = new TimeSeriesCollection();
		updateResultDataset();
	}

	/**
	 * Returns the underlying dataset that was supplied to the constructor.
	 *
	 * @return The underlying dataset (never <code>null</code>).
	 */
	public TimeSeriesCollection getUnderlyingDataset() {
		return this.underlying;
	}

	/**
	 * Get the number of items shown in the sliding window.
	 */
	public int getWindowItemCount() {
		return this.windowItemCount;
	}

	/**
	 * Set the number of items shown in the sliding window.
	 */
	public void setWindowItemCount(int count) {
		this.windowItemCount = count;
		fireDatasetChanged();
	}

	/**
	 * Returns the index of the first visible item.
	 *
	 * @return The index.
	 */
	public int getWindowStartIndex() {
		return this.windowStartIndex;
	}

	/**
	 * Sets the index of the first item that should be used from the
	 * underlying dataset, and sends a {@link DatasetChangeEvent} to all
	 * registered listeners.
	 *
	 * @param index the index
	 */
	public void setWindowStartIndex(int index) {
		final Range range = DatasetUtilities.findDomainBounds(underlying);
		final int maximum = convertRangeToSliderMaximum(range);
		if (index < 0 || index > maximum)
			throw new IllegalArgumentException("invalid window start index: " + index);

		this.windowStartIndex = index;
		updateResultDataset();
		fireDatasetChanged();
	}

	/**
	 * Get the minimum slider value.
	 *
	 * This value is always 0.
	 */
	public int getMinimumSliderValue() {
		return 0;
	}

	/**
	 * Get the maximum slider value.
	 *
	 * This value is the number of time periods possible in the dataset,
	 * regardless of whether data is missing in the middle or not.
	 */
	public int getMaximumSliderValue() {
		final Range range = DatasetUtilities.findDomainBounds(underlying);
		return convertRangeToSliderMaximum(range);
	}

	/* ---------------------------------------------------------------------- */
	/* Private Methods                                                        */
	/* ---------------------------------------------------------------------- */

	/**
	 * Update the result dataset due to one of the following reasons:
	 * - new data has arrived in the underlying dataset
	 * - slider value (window start position) has changed
	 *
	 * A new TimeSeriesCollection is created, holding everything that will be
	 * visible in the window. This makes for very easy implementations of all
	 * of the rest of the methods.
	 *
	 * This could be made into a set of utility routines, but it was deemed
	 * easier to follow the mold of the original implementation and make it
	 * a class instead.
	 */
	private void updateResultDataset() {
		resultDataset.removeAllSeries();

		// nothing in the underlying dataset yet, skip everything
		final Range range = DatasetUtilities.findDomainBounds(underlying);
		if (range == null)
			return;

		for (int i = 0; i < underlying.getSeriesCount(); i++) {
			final TimeSeries underlyingSeries = underlying.getSeries(i);
			final TimeSeries newSeries = new TimeSeries(underlyingSeries.getKey());

			// A reference to the list of TimeSeriesDataItem which is part of
			// the underlying TimeSeries. By using this list to get the items,
			// we can avoid copying each item from the underlying time series
			// and reduce the pressure on the memory allocator.
			//
			// The methods of the underlyingSeries class can still be used
			// without causing problems. It is only the items that we need to
			// get from this list.
			final List underlyingList = underlyingSeries.getItems();

			// skip series which have no items yet
			final int seriesItemCount = underlyingSeries.getItemCount();
			if (seriesItemCount <= 0) {
				resultDataset.addSeries(newSeries);
				continue;
			}

			// start searching at first index in the underlying list
			int searchIndex = 0;
			for (int j = -1; j < windowItemCount + 1; j++) {
				final RegularTimePeriod period = convertSliderValue(range, windowStartIndex + j);

				// exit the loop if we have gone past the end of the dataset
				if (period.getStart().getTime() > range.getUpperBound())
					break;

				TimeSeriesDataItem item = null;

				// If we have previously found an item for an earlier period, we
				// can try looking for the next item at the next index. This
				// will save a binary search through the list of items in the
				// most common case (a dataset without many holes).
				if (searchIndex < seriesItemCount) {
					item = (TimeSeriesDataItem)underlyingList.get(searchIndex);
					if (item != null) {
						if (item.getPeriod().equals(period)) {
							// this is the correct item, update the index
							// for the next iteration
							searchIndex++;
						} else {
							// not the correct item for this time period
							item = null;
						}
					}
				}

				// The item was not found by searching for the index, try to
				// find the index and the corresponding matching item. This is
				// where the loop will find the first item on the first
				// iteration, as well as how holes will be skipped over.
				//
				// We avoid trashing the previously found item index to avoid
				// having to perform an extra binary search when a hole ends
				// and the data begins again.
				if (item == null) {
					int tempIndex = underlyingSeries.getIndex(period);
					if (tempIndex >= 0) {
						item = (TimeSeriesDataItem)underlyingList.get(tempIndex);
						searchIndex = tempIndex + 1;
					}
				}

				// The item was not found by using the previous index, nor was
				// it found by searching for it using the time period. This
				// means that it doesn't exist in the dataset.
				//
				// A null value is added for non-existent data items. This makes
				// the JFreeChart automatic scaling work nicely. Empty areas
				// will still scroll past with the correct date.
				if (item == null) {
					item = new TimeSeriesDataItem(period, null);
				}

				newSeries.add(item, false);
			}

			resultDataset.addSeries(newSeries);
		}
	}

	/**
	 * Convert slider value to a RegularTimePeriod
	 *
	 * @param range the Range of the entire underlying series
	 * @param value the slider value
	 *
	 * @return the RegularTimePeriod corresponding to the slider value
	 */
	private RegularTimePeriod convertSliderValue(final Range range, final int value) {
		final long lower = (long)range.getLowerBound();
		final Date date = new Date(lower + (value * itemInterval));
		final RegularTimePeriod rtp = new FixedMillisecond(date);

		return rtp;
	}

	/**
	 * Convert Range to slider maximum value.
	 *
	 * This method was created so that a Range instance can be reused
	 * throughout a single method call.
	 */
	private int convertRangeToSliderMaximum(final Range range) {
		if (range == null)
			return 0;

		// length of the underlying dataset range (in milliseconds)
		final double length = range.getLength();

		// maximum number of possible items in any series in the dataset
		final int max_possible_items = (int)(length / itemInterval);

		final int result = max_possible_items - windowItemCount;
		if (result < 0)
			return 0;

		return result;
	}

	/* ---------------------------------------------------------------------- */
	/* XYDataset Interface                                                    */
	/* ---------------------------------------------------------------------- */

	@Override
	public int getItemCount(int series) {
		if ((series < 0) || (series >= getSeriesCount())) {
			throw new IllegalArgumentException("Series index out of bounds");
		}

		return resultDataset.getItemCount(series);
	}

	@Override
	public double getXValue(int series, int item) {
		return resultDataset.getXValue(series, item);
	}

	@Override
	public Number getX(int series, int item) {
		return resultDataset.getX(series, item);
	}

	@Override
	public double getYValue(int series, int item) {
		return resultDataset.getYValue(series, item);
	}

	@Override
	public Number getY(int series, int item) {
		return resultDataset.getY(series, item);
	}

	@Override
	public int getSeriesCount() {
		return resultDataset.getSeriesCount();
	}

	@Override
	public Comparable getSeriesKey(int series) {
		return resultDataset.getSeriesKey(series);
	}

	/* ---------------------------------------------------------------------- */
	/* DatasetChangeListener Interface                                        */
	/* ---------------------------------------------------------------------- */

	@Override
	public void datasetChanged(DatasetChangeEvent event) {
		updateResultDataset();
		fireDatasetChanged();
	}

	/* ---------------------------------------------------------------------- */
	/* java.lang.Object Overrides                                             */
	/* ---------------------------------------------------------------------- */

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (!(obj instanceof SlidingTimeSeriesCollection)) {
			return false;
		}
		SlidingTimeSeriesCollection that = (SlidingTimeSeriesCollection) obj;
		if (this.windowStartIndex != that.windowStartIndex) {
			return false;
		}
		if (this.windowItemCount != that.windowItemCount) {
			return false;
		}
		if (!this.underlying.equals(that.underlying)) {
			return false;
		}
		return true;
	}

	@Override
	public int hashCode() {
		int result;
		result = this.underlying.hashCode();
		result = 29 * result + this.windowStartIndex;
		result = 17 * result + this.windowItemCount;
		return result;
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
