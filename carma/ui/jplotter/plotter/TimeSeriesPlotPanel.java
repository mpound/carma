package carma.ui.jplotter.plotter;

import java.io.IOException;
import java.util.*;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import java.text.SimpleDateFormat;

import java.awt.Color;
import java.awt.Paint;
import java.awt.Window;
import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

// MigLayout
import net.miginfocom.swing.MigLayout;

// JFreeChart
import org.jfree.chart.JFreeChart;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.chart.axis.DateTickUnit;
import org.jfree.chart.axis.DateTickUnitType;
import org.jfree.data.Range;
import org.jfree.data.time.Day;
import org.jfree.data.time.FixedMillisecond;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.time.TimeSeriesCollection;
import org.jfree.data.general.DatasetUtilities;
import org.jfree.ui.RectangleEdge;

import carma.ui.jplotter.dialog.ConfigurationDialogAction;
import carma.ui.jplotter.dialog.BufferDialog;
import carma.ui.jplotter.dialog.LabelDialog;
import carma.ui.jplotter.dialog.MonitorDialog;
import carma.ui.jplotter.dialog.PlotDialog;
import carma.ui.jplotter.dialog.RangeDialog;
import carma.ui.jplotter.dialog.TimeZoneDialog;
import carma.ui.jplotter.jfree.SlidingTimeSeriesCollection;
import carma.ui.jplotter.network.GraphDataEvent;
import carma.ui.jplotter.network.GraphDataListener;

/**
 * A JPanel to hold a Time Series JFreeChart which expands to fill the window.
 */
public final class TimeSeriesPlotPanel
	extends AbstractPlotPanel
	implements GraphDataListener
{
	/**
	 * Container class for information about each TimeSeries being graphed.
	 */
	private static class TimeSeriesInfo
	{
		public final String register;
		public final TimeSeries series;

		public TimeSeriesInfo(final String register, final TimeSeries series) {
			this.register = register;
			this.series = series;
		}
	};

	private final int INTERVAL = 500; // milliseconds

	private final PlotProperties props;
	private final PlotManager manager;
	private final List<TimeSeriesInfo> infoList;
	private final TimeSeriesCollection collection;
	private final JFreeChart chart;

	// sliding dataset support
	private final JSlider slider = new JSlider();
	private final JLabel leftBound = new JLabel();
	private final JLabel rightBound = new JLabel();
	private final SlidingTimeSeriesCollection slidingCollection;

	/* ---------------------------------------------------------------------- */
	/* Public Methods                                                         */
	/* ---------------------------------------------------------------------- */

	/**
	 * Constructor
	 */
	public TimeSeriesPlotPanel(final PlotProperties props, final PlotManager manager) {
		super();
		setLayout(new MigLayout("insets 0 0 0 0, fill", "[][fill, grow][]", "[grow][]"));

		this.props = props;
		this.manager = manager;
		this.infoList = new ArrayList<TimeSeriesInfo>();
		this.collection = new TimeSeriesCollection();

		this.chart = ChartFactory.createTimeSeriesChart(
				this.props.getString("plot_title"),
				"Time",
				"Value",
				this.collection,
				true,	// legend
				false,	// tooltips
				false	// urls
		);

		final XYPlot plot = chart.getXYPlot();

		// Domain Axes
		// 0	DateAxis	bottom, time only
		// 1	DateAxis	bottom, date only, title display
		// 2	DateAxis	top, time only
		// 3	DateAxis	top, date only, title display

		// add secondary bottom domain axis (for date only)
		try {
			final DateAxis axis = (DateAxis)plot.getDomainAxis().clone();

			// display date with no axis line
			axis.setDateFormatOverride(new SimpleDateFormat("yyyy-MM-dd"));
			axis.setAxisLineVisible(false);

			// one tick per day
			final DateTickUnit dtu = new DateTickUnit(DateTickUnitType.DAY, 1);
			axis.setTickUnit(dtu);

			// add the axis to the plot
			final int count = plot.getDomainAxisCount();
			plot.setDomainAxis(count, axis);
			plot.setDomainAxisLocation(count, AxisLocation.BOTTOM_OR_LEFT);
		} catch (CloneNotSupportedException ex) {
			System.err.println("CloneNotSupportedException: " + ex);
		}

		// add top domain axis (for time only)
		try {
			final ValueAxis axis = (ValueAxis)plot.getDomainAxis().clone();

			// add the axis to the plot
			final int count = plot.getDomainAxisCount();
			plot.setDomainAxis(count, axis);
			plot.setDomainAxisLocation(count, AxisLocation.TOP_OR_RIGHT);
		} catch (CloneNotSupportedException ex) {
			System.err.println("CloneNotSupportedException: " + ex);
		}

		// add secondary top domain axis (for date only)
		try {
			final DateAxis axis = (DateAxis)plot.getDomainAxis().clone();

			// display date with no axis line
			axis.setDateFormatOverride(new SimpleDateFormat("yyyy-MM-dd"));
			axis.setAxisLineVisible(false);

			// one tick per day
			final DateTickUnit dtu = new DateTickUnit(DateTickUnitType.DAY, 1);
			axis.setTickUnit(dtu);

			// add the axis to the plot
			final int count = plot.getDomainAxisCount();
			plot.setDomainAxis(count, axis);
			plot.setDomainAxisLocation(count, AxisLocation.TOP_OR_RIGHT);
		} catch (CloneNotSupportedException ex) {
			System.err.println("CloneNotSupportedException: " + ex);
		}

		// add secondary range axis
		try {
			final ValueAxis axis = (ValueAxis)plot.getRangeAxis().clone();
			final int count = plot.getRangeAxisCount();
			plot.setRangeAxis(count, axis);
			plot.setRangeAxisLocation(count, AxisLocation.TOP_OR_RIGHT);
		} catch (CloneNotSupportedException ex) {
			System.err.println("CloneNotSupportedException: " + ex);
		}

		{
			final ChartPanel panel = new ChartPanel(this.chart);
			panel.setPopupMenu(createPopupMenu(panel));

			// Avoid stretching on large displays when the panel is resized.
			// The resolution set here matches the highest resolution LCD panel
			// available at the time this code was written.
			panel.setMinimumDrawWidth(0);
			panel.setMinimumDrawHeight(0);
			panel.setMaximumDrawWidth(2560);
			panel.setMaximumDrawHeight(1600);

			this.add(panel, "span 3, growx, growy, wrap");
		}

		// set up slider
		{
			final int window_points = props.getInt("xrange_window") * 2;
			slidingCollection = new SlidingTimeSeriesCollection(this.collection, window_points, INTERVAL);

			slider.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					final int value = slider.getValue();
					if (value > 0)
						slidingCollection.setWindowStartIndex(value);
				}
			});

			updateSliderRange(chart, props);
		}

		updateDomainAxis(chart, props, "x1");
		updateDomainAxis(chart, props, "x2");
		updateRangeAxis(chart, props, "y1");
		updateRangeAxis(chart, props, "y2");
		updateDomainAxisRange(chart, props);
		updateRangeAxisRange(chart, props);
		updateBufferSize(chart, props);
		setPlotTitle(chart, props, null);
		setMarker(chart, props);
		setLegendLocation(chart, props);
		updateRegisters();
		updateSlidingDataset(chart, props);
	}

	/* ---------------------------------------------------------------------- */
	/* GraphDataListener Interface                                            */
	/* ---------------------------------------------------------------------- */

	@Override
	synchronized public void graphDataChanged(final GraphDataEvent gde) {
		final GraphDataEvent.Datum recordDatum = gde.getDatum("array.frame.record");
		final List<Double> recordValues = recordDatum.getData();
		if (recordValues == null || recordValues.isEmpty())
			return;

		final Double record = recordValues.get(0);
		final Date date = carma.util.Time.getDate(record.intValue());
		final FixedMillisecond millisecond = new FixedMillisecond(date);

		//System.out.println("NEW RECORD: " + record.intValue());
		//System.out.println("DATE: " + date);

		for (int i = 0; i < infoList.size(); i++) {
			final TimeSeriesInfo tsi = infoList.get(i);
			final boolean notify = (i == (infoList.size() - 1));

			// skip missing data points
			final GraphDataEvent.Datum datum = gde.getDatum(tsi.register);
			final List<Double> values = datum.getData();
			if (values == null || values.isEmpty())
				continue;

			// skip invalid data points
			if (datum.getValidity() == false)
				continue;

			// only signal changes on final series to avoid multiple redraws
			tsi.series.add(millisecond, values.get(0), notify);
		}

		updateSliderRange(chart, props);
	}

	@Override
	synchronized public void updateRegisters() {
		final String[] registers = props.getStringArray("yaxis_registers");

		// remove all series, the collection will be rebuilt in the next step
		collection.removeAllSeries();

		// calculate buffer size in milliseconds
		final long buffer_ms = props.getInt("buffer_size") * 1000;

		// copy the original infoList
		final List<TimeSeriesInfo> list = new ArrayList<TimeSeriesInfo>(infoList);
		infoList.clear();

		if (registers != null) {
			// build the new register info list
			for (final String reg : registers) {
				TimeSeriesInfo tsi = findSeriesInfo(reg, list);
				if (tsi == null) {
					// didn't find it, create a new one
					final TimeSeries series = new TimeSeries(reg);
					series.setMaximumItemAge(buffer_ms);
					tsi = new TimeSeriesInfo(reg, series);
				}

				// add it to the infoList and JFreeChart dataset
				infoList.add(tsi);
				collection.addSeries(tsi.series);
				manager.addRegister(reg);
			}
		}

		// remove all original registers from the network client
		for (final TimeSeriesInfo tsi : list) {
			manager.remRegister(tsi.register);
		}
	}

	/* ---------------------------------------------------------------------- */
	/* Private Methods                                                        */
	/* ---------------------------------------------------------------------- */

	synchronized private TimeSeriesInfo findSeriesInfo(final String reg, final List<TimeSeriesInfo> list) {
		for (final TimeSeriesInfo tsi : list) {
			if (reg.equals(tsi.register))
				return tsi;
		}

		// not found
		return null;
	}

	private TimeZone parseTimeZone(final String timezone) {
		final TimeZone tz;
		if (timezone.equals("UTC")) {
			return TimeZone.getTimeZone("UTC");
		} else if (timezone.equals("US Pacific (CARMA Local Time)")) {
			return TimeZone.getTimeZone("America/Los_Angeles");
		} else if (timezone.equals("Local Time (this computer)")) {
			return TimeZone.getDefault();
		} else {
			throw new RuntimeException("unable to parse timezone");
		}
	}

	private String getDateAxisLabelAuto(final PlotProperties props, final String prefix) {
		final String timezone = props.getString(prefix + "axis_timezone");
		final TimeZone tz = parseTimeZone(timezone);

		final boolean dst = tz.inDaylightTime(new Date());
		return "Time (" + tz.getDisplayName(dst, TimeZone.SHORT) + ")";
	}

	private JPopupMenu createPopupMenu(final ChartPanel panel) {
		final JPopupMenu popupMenu = new JPopupMenu("Chart");

		{
			final JMenuItem item = new JMenuItem("Plot Configuration");
			item.setEnabled(false);
			popupMenu.add(item);
		}

		popupMenu.addSeparator();

		// monitor points
		{
			final JMenuItem item = new JMenuItem("Monitor Points");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					final Window window = SwingUtilities.getWindowAncestor(panel);
					final MonitorDialog dialog = new MonitorDialog(window, props, manager);
					final ConfigurationDialogAction action = dialog.getValue();
					if (action == ConfigurationDialogAction.OK) {
						// properties have been stored already, just make
						// the appropriate changes!
						updateRegisters();
					}
				}
			});

			popupMenu.add(item);
		}

		popupMenu.addSeparator();

		// plot attributes
		{
			final JMenuItem item = new JMenuItem("Plot Attributes");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					final Window window = SwingUtilities.getWindowAncestor(panel);
					final PlotDialog dialog = new PlotDialog(window, props);
					final ConfigurationDialogAction action = dialog.getValue();
					if (action == ConfigurationDialogAction.OK) {
						// properties have been stored already, just make
						// the appropriate changes!
						setPlotTitle(chart, props, window);
						setMarker(chart, props);
						setLegendLocation(chart, props);
					}
				}
			});

			popupMenu.add(item);
		}

		// X Axis
		{
			final JMenu subMenu = new JMenu("X Axis");

			// range
			{
				final JMenuItem item = new JMenuItem("Range");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final Window window = SwingUtilities.getWindowAncestor(panel);
						final RangeDialog dialog = new RangeDialog(window, props, "x", true);
						final ConfigurationDialogAction action = dialog.getValue();
						if (action == ConfigurationDialogAction.OK) {
							updateDomainAxisRange(chart, props);
						}
					}
				});

				subMenu.add(item);
			}

			subMenu.addSeparator();

			{
				final JMenuItem item = new JMenuItem("Primary (bottom)");
				item.setEnabled(false);
				subMenu.add(item);
			}

			subMenu.addSeparator();

			// enable
			{
				final String prefix = "x1";
				final JCheckBoxMenuItem item = new JCheckBoxMenuItem("Enable");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final boolean enable = item.isSelected();
						props.setBoolean(prefix + "axis_enable", enable);
						updateDomainAxis(chart, props, prefix);
					}
				});
				item.setSelected(props.getBoolean(prefix + "axis_enable"));
				subMenu.add(item);
			}

			// label
			{
				final JMenuItem item = new JMenuItem("Label");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final String prefix = "x1";
						final Window window = SwingUtilities.getWindowAncestor(panel);
						final LabelDialog dialog = new LabelDialog(window, props, prefix);
						final ConfigurationDialogAction action = dialog.getValue();
						if (action == ConfigurationDialogAction.OK) {
							updateDomainAxis(chart, props, prefix);
						}
					}
				});

				subMenu.add(item);
			}

			// timezone
			{
				final JMenuItem item = new JMenuItem("Timezone");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final String prefix = "x1";
						final Window window = SwingUtilities.getWindowAncestor(panel);
						final TimeZoneDialog dialog = new TimeZoneDialog(window, props, prefix);
						final ConfigurationDialogAction action = dialog.getValue();
						if (action == ConfigurationDialogAction.OK) {
							updateDomainAxis(chart, props, prefix);
						}
					}
				});

				subMenu.add(item);
			}

			subMenu.addSeparator();

			{
				final JMenuItem item = new JMenuItem("Secondary (top)");
				item.setEnabled(false);
				subMenu.add(item);
			}

			subMenu.addSeparator();

			// enable
			{
				final String prefix = "x2";
				final JCheckBoxMenuItem item = new JCheckBoxMenuItem("Enable");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final boolean enable = item.isSelected();
						props.setBoolean(prefix + "axis_enable", enable);
						updateDomainAxis(chart, props, prefix);
					}
				});
				item.setSelected(props.getBoolean(prefix + "axis_enable"));
				subMenu.add(item);
			}

			// label
			{
				final JMenuItem item = new JMenuItem("Label");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final String prefix = "x2";
						final Window window = SwingUtilities.getWindowAncestor(panel);
						final LabelDialog dialog = new LabelDialog(window, props, prefix);
						final ConfigurationDialogAction action = dialog.getValue();
						if (action == ConfigurationDialogAction.OK) {
							updateDomainAxis(chart, props, prefix);
						}
					}
				});

				subMenu.add(item);
			}

			// timezone
			{
				final JMenuItem item = new JMenuItem("Timezone");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final String prefix = "x2";
						final Window window = SwingUtilities.getWindowAncestor(panel);
						final TimeZoneDialog dialog = new TimeZoneDialog(window, props, prefix);
						final ConfigurationDialogAction action = dialog.getValue();
						if (action == ConfigurationDialogAction.OK) {
							updateDomainAxis(chart, props, prefix);
						}
					}
				});

				subMenu.add(item);
			}

			popupMenu.add(subMenu);
		}

		// Y Axis
		{
			final JMenu subMenu = new JMenu("Y Axis");

			// range
			{
				final JMenuItem item = new JMenuItem("Range");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final Window window = SwingUtilities.getWindowAncestor(panel);
						final RangeDialog dialog = new RangeDialog(window, props, "y", true);
						final ConfigurationDialogAction action = dialog.getValue();
						if (action == ConfigurationDialogAction.OK) {
							updateRangeAxisRange(chart, props);
						}
					}
				});

				subMenu.add(item);
			}

			subMenu.addSeparator();

			{
				final JMenuItem item = new JMenuItem("Primary (left)");
				item.setEnabled(false);
				subMenu.add(item);
			}

			subMenu.addSeparator();

			// enable
			{
				final String prefix = "y1";
				final JCheckBoxMenuItem item = new JCheckBoxMenuItem("Enable");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final boolean enable = item.isSelected();
						props.setBoolean(prefix + "axis_enable", enable);
						updateRangeAxis(chart, props, prefix);
					}
				});
				item.setSelected(props.getBoolean(prefix + "axis_enable"));
				subMenu.add(item);
			}

			// label
			{
				final JMenuItem item = new JMenuItem("Label");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final String prefix = "y1";
						final Window window = SwingUtilities.getWindowAncestor(panel);
						final LabelDialog dialog = new LabelDialog(window, props, prefix);
						final ConfigurationDialogAction action = dialog.getValue();
						if (action == ConfigurationDialogAction.OK) {
							updateRangeAxis(chart, props, prefix);
						}
					}
				});

				subMenu.add(item);
			}

			subMenu.addSeparator();

			{
				final JMenuItem item = new JMenuItem("Secondary (right)");
				item.setEnabled(false);
				subMenu.add(item);
			}

			subMenu.addSeparator();

			// enable
			{
				final String prefix = "y2";
				final JCheckBoxMenuItem item = new JCheckBoxMenuItem("Enable");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final boolean enable = item.isSelected();
						props.setBoolean(prefix + "axis_enable", enable);
						updateRangeAxis(chart, props, prefix);
					}
				});
				item.setSelected(props.getBoolean(prefix + "axis_enable"));
				subMenu.add(item);
			}

			// label
			{
				final JMenuItem item = new JMenuItem("Label");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final String prefix = "y2";
						final Window window = SwingUtilities.getWindowAncestor(panel);
						final LabelDialog dialog = new LabelDialog(window, props, prefix);
						final ConfigurationDialogAction action = dialog.getValue();
						if (action == ConfigurationDialogAction.OK) {
							updateRangeAxis(chart, props, prefix);
						}
					}
				});

				subMenu.add(item);
			}

			popupMenu.add(subMenu);
		}

		// buffer size
		{
			final JMenuItem item = new JMenuItem("Buffer Size");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					final Window window = SwingUtilities.getWindowAncestor(panel);
					final BufferDialog dialog = new BufferDialog(window, props);
					final ConfigurationDialogAction action = dialog.getValue();
					if (action == ConfigurationDialogAction.OK) {
						updateBufferSize(chart, props);
					}
				}
			});

			popupMenu.add(item);
		}

		// drop data
		{
			final JMenuItem item = new JMenuItem("Drop Existing Data");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					dropExistingData(chart, props);
				}
			});

			popupMenu.add(item);
		}

		// Save Image As...
		{
			final JMenuItem item = new JMenuItem("Save Image As...");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					try {
						panel.doSaveAs();
					} catch (IOException exc) {
						System.err.println("IOException: " + exc);
					}
				}
			});

			popupMenu.add(item);
		}

		return popupMenu;
	}

	/* ---------------------------------------------------------------------- */
	/* Dynamic Display Mutators                                               */
	/* ---------------------------------------------------------------------- */

	/**
	 * Set the buffer size for all series which are currently plotted.
	 */
	synchronized private void updateBufferSize(final JFreeChart chart, final PlotProperties props) {
		// calculate buffer size in milliseconds
		final long buffer_ms = props.getInt("buffer_size") * 1000;

		// iterate through existing series and set size appropriately
		for (final TimeSeriesInfo tsi : infoList) {
			tsi.series.setMaximumItemAge(buffer_ms);
		}

		// update scaling to match new buffer size
		updateDomainAxisRange(chart, props);
	}

	/**
	 * Drop all existing plot data
	 */
	synchronized private void dropExistingData(final JFreeChart chart, final PlotProperties props) {
		for (final TimeSeriesInfo tsi : infoList) {
			tsi.series.clear();
		}

		// if we are using a sliding dataset, we need to ensure that the slider
		// is positioned correctly to match the new empty dataset, and then
		// update the dates in the slider control area
		final String xrange_type = props.getString("xrange_type");
		if (xrange_type.equals("sliding window")) {
			slidingCollection.setWindowStartIndex(0);
			updateSliderRange(chart, props);
		}
	}

	private void updateDomainAxis(final JFreeChart chart, final PlotProperties props, final String prefix) {
		final String label_type = props.getString(prefix + "label_type");
		final XYPlot plot = chart.getXYPlot();

		final DateAxis axisTime;
		final DateAxis axisDate;
		if (prefix.equals("x1")) {
			axisTime = (DateAxis)plot.getDomainAxis(0);
			axisDate = (DateAxis)plot.getDomainAxis(1);
		} else if (prefix.equals("x2")) {
			axisTime = (DateAxis)plot.getDomainAxis(2);
			axisDate = (DateAxis)plot.getDomainAxis(3);
		} else {
			throw new RuntimeException("unable to parse prefix: " + prefix);
		}

		// set the axis visibility
		{
			final boolean enable = props.getBoolean(prefix + "axis_enable");
			axisTime.setVisible(enable);
			axisDate.setVisible(enable);
		}

		// time axis label is always disabled
		axisTime.setLabel(null);

		// set the axis label
		if (label_type.equals("disable")) {
			axisDate.setLabel(null);
		} else if (label_type.equals("automatic")) {
			axisDate.setLabel(getDateAxisLabelAuto(props, prefix));
		} else if (label_type.equals("custom")) {
			final String label_custom = props.getString(prefix + "label_custom");
			axisDate.setLabel(label_custom);
		} else {
			throw new RuntimeException("unable to parse label_type: " + label_type);
		}

		// set axis timezone
		{
			final String timezone = props.getString(prefix + "axis_timezone");
			final TimeZone tz = parseTimeZone(timezone);

			axisTime.setTimeZone(tz);
			axisDate.setTimeZone(tz);

			// must update the date formatter timezone also
			final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
			sdf.setTimeZone(tz);
			axisDate.setDateFormatOverride(sdf);
		}
	}

	private void updateRangeAxis(final JFreeChart chart, final PlotProperties props, final String prefix) {
		final String label_type = props.getString(prefix + "label_type");
		final XYPlot plot = chart.getXYPlot();

		final ValueAxis axis;
		if (prefix.equals("y1")) {
			axis = plot.getRangeAxis(0);
		} else if (prefix.equals("y2")) {
			axis = plot.getRangeAxis(1);
		} else {
			throw new RuntimeException("unable to parse prefix: " + prefix);
		}

		// set the axis visibility
		{
			final boolean enable = props.getBoolean(prefix + "axis_enable");
			axis.setVisible(enable);
		}

		// set the axis label
		if (label_type.equals("disable")) {
			axis.setLabel(null);
		} else if (label_type.equals("automatic")) {
			axis.setLabel("Value");
		} else if (label_type.equals("custom")) {
			final String label_custom = props.getString(prefix + "label_custom");
			axis.setLabel(label_custom);
		}
	}

	private void updateDomainAxisRange(final JFreeChart chart, final PlotProperties props) {
		final String xrange_type = props.getString("xrange_type");
		final XYPlot plot = chart.getXYPlot();
		final List<Integer> list = new ArrayList<Integer>();
		for (int i = 0; i < plot.getDomainAxisCount(); i++) {
			final ValueAxis axis = plot.getDomainAxis(i);
			list.add(i);

			if (xrange_type.equals("full width")) {
				final long buffer_ms = props.getInt("buffer_size") * 1000;
				axis.setFixedAutoRange(buffer_ms);
			} else if (xrange_type.equals("sliding window")) {
				final long window_ms = props.getInt("xrange_window") * 1000;
				axis.setFixedAutoRange(window_ms);
			} else if (xrange_type.equals("scale to data")) {
				axis.setAutoRange(true);
				axis.setFixedAutoRange(0.0);
			} else if (xrange_type.equals("custom")) {
				final double min = props.getDouble("xrange_min");
				final double max = props.getDouble("xrange_max");
				axis.setFixedAutoRange(0.0);
				axis.setRange(min, max);
			} else {
				throw new RuntimeException("unable to parse xrange_type");
			}
		}

		updateSlidingDataset(chart, props);

		plot.mapDatasetToDomainAxes(0, list);
	}

	private void updateRangeAxisRange(final JFreeChart chart, final PlotProperties props) {
		final XYPlot plot = chart.getXYPlot();
		final List<Integer> list = new ArrayList<Integer>();
		for (int i = 0; i < plot.getRangeAxisCount(); i++) {
			final ValueAxis axis = plot.getRangeAxis(i);
			list.add(i);

			final String yrange_type = props.getString("yrange_type");
			if (yrange_type.equals("scale to data")) {
				axis.setAutoRange(true);
			} else if (yrange_type.equals("custom")) {
				final double min = props.getDouble("yrange_min");
				final double max = props.getDouble("yrange_max");
				axis.setRange(min, max);
			} else {
				throw new RuntimeException("unable to parse yrange_type");
			}
		}

		plot.mapDatasetToRangeAxes(0, list);
	}

	private void updateSliderRange(final JFreeChart chart, final PlotProperties props) {
		final int maximum = slidingCollection.getMaximumSliderValue();
		final int origValue = slider.getValue();
		final int origMaximum = slider.getMaximum();

		// change date range labels
		{
			final Range range = DatasetUtilities.findDomainBounds(this.collection);
			if (range != null) {
				final double lower = range.getLowerBound();
				final double upper = range.getUpperBound();

				final Date lowerDate = new Date((long)lower + INTERVAL);
				final Date upperDate = new Date((long)upper);

				// peg timezone to bottom x axis timezone
				final String timezone = props.getString("x1axis_timezone");
				final TimeZone tz = parseTimeZone(timezone);
				final SimpleDateFormat fmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss z");
				fmt.setTimeZone(tz);

				leftBound.setText(fmt.format(lowerDate));
				rightBound.setText(fmt.format(upperDate));
			}
		}

		// change slider limits
		slider.setMinimum(0);
		slider.setMaximum(maximum);

		slider.setEnabled(maximum != 0);

		if (origValue == origMaximum) {
			// automatically pull the plot to the right
			slider.setValue(maximum);
		} else if (origMaximum == slider.getMaximum()) {
			// we have filled the buffer, and the slider needs to be moved to
			// the left as we add/expire existing data to keep the window in
			// the same place
			slider.setValue(origValue - 1);
		} else {
			slider.setValue(origValue);
		}
	}

	private void updateSlidingDataset(final JFreeChart chart, final PlotProperties props) {
		final XYPlot plot = chart.getXYPlot();
		final String xrange_type = props.getString("xrange_type");
		if (xrange_type.equals("sliding window")) {
			final int window_points = props.getInt("xrange_window") * 2;
			slidingCollection.setWindowItemCount(window_points);
			updateSliderRange(chart, props);

			// early exit if already enabled
			if (plot.getDataset(0) == slidingCollection)
				return;

			plot.setDataset(0, slidingCollection);

			this.add(leftBound, "span 1, gapleft 10, sg label");
			this.add(slider, "span 1, growx");
			this.add(rightBound, "span 1, gapright 10, wrap, sg label");
		} else {
			// early exit if already disabled
			if (plot.getDataset(0) == collection)
				return;

			plot.setDataset(0, collection);

			this.remove(leftBound);
			this.remove(slider);
			this.remove(rightBound);
		}

		// force the window to repaint to handle add/remove GUI components
		this.revalidate();
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
