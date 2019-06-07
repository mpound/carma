package carma.ui.jplotter.plotter;

import java.io.IOException;
import java.util.*;

import javax.swing.*;

import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

// JFreeChart
import org.jfree.chart.JFreeChart;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.axis.AxisLocation;
import org.jfree.data.time.FixedMillisecond;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.RectangleEdge;

import carma.ui.jplotter.dialog.ConfigurationDialogAction;
import carma.ui.jplotter.dialog.BufferDialog;
import carma.ui.jplotter.dialog.LabelDialog;
import carma.ui.jplotter.dialog.MonitorDialog;
import carma.ui.jplotter.dialog.PlotDialog;
import carma.ui.jplotter.dialog.RangeDialog;
import carma.ui.jplotter.network.GraphDataEvent;
import carma.ui.jplotter.network.GraphDataListener;

/**
 * A JPanel to hold a JFreeChart which expands to fill the window no matter
 * how large of small.
 */
public final class XYPlotPanel
	extends AbstractPlotPanel
	implements GraphDataListener
{
	/**
	 * Container class for information about each XYSeries being graphed.
	 */
	private static class XYSeriesInfo
	{
		public final String xRegister;
		public final String yRegister;
		public final XYSeries series;

		public XYSeriesInfo(final String xReg, final String yReg, final XYSeries series) {
			this.xRegister = xReg;
			this.yRegister = yReg;
			this.series = series;
		}
	}

	private final PlotProperties props;
	private final PlotManager manager;
	private final List<XYSeriesInfo> infoList;
	private final XYSeriesCollection collection;
	private final JFreeChart chart;

	/* ---------------------------------------------------------------------- */
	/* Public Methods                                                         */
	/* ---------------------------------------------------------------------- */

	/**
	 * Constructor
	 */
	public XYPlotPanel(final PlotProperties props, final PlotManager manager) {
		super();

		this.props = props;
		this.manager = manager;
		this.infoList = new ArrayList<XYSeriesInfo>();
		this.collection = new XYSeriesCollection();

		this.chart = ChartFactory.createXYLineChart(
				this.props.getString("plot_title"),
				"Domain",
				"Range",
				this.collection,
				PlotOrientation.VERTICAL,
				true,	// legend
				false,	// tooltips
				false	// urls
		);

		final XYPlot plot = chart.getXYPlot();

		// add secondary domain axis
		try {
			final ValueAxis axis = (ValueAxis)plot.getDomainAxis().clone();

			// add this axis to the plot
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

		updateAxis(chart, props, "x1");
		updateAxis(chart, props, "x2");
		updateAxis(chart, props, "y1");
		updateAxis(chart, props, "y2");
		updateDomainAxisRange(chart, props);
		updateRangeAxisRange(chart, props);
		updateBufferSize(chart, props);
		setPlotTitle(chart, props, null);
		setMarker(chart, props);
		setLegendLocation(chart, props);

		updateRegisters();

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

			this.add(panel);
		}
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
			final XYSeriesInfo xsi = infoList.get(i);
			final boolean notify = (i == (infoList.size() - 1));

			// skip missing data points
			final GraphDataEvent.Datum xDatum = gde.getDatum(xsi.xRegister);
			final List<Double> xValues = xDatum.getData();
			if (xValues == null || xValues.isEmpty())
				continue;

			// skip missing data points
			final GraphDataEvent.Datum yDatum = gde.getDatum(xsi.yRegister);
			final List<Double> yValues = yDatum.getData();
			if (yValues == null || yValues.isEmpty())
				continue;

			// skip points with invalid data
			if (xDatum.getValidity() == false || yDatum.getValidity() == false)
				continue;

			final Double xValue = xValues.get(0);
			final Double yValue = yValues.get(0);

			// only signal changes on final series to avoid multiple redraws
			xsi.series.add(xValue, yValue, notify);
		}
	}

	@Override
	synchronized public void updateRegisters() {
		final String[] xregisters = props.getStringArray("xaxis_registers");
		final String[] yregisters = props.getStringArray("yaxis_registers");

		// remove all series, the collection will be rebuilt in the next step
		collection.removeAllSeries();

		// calculate buffer size in milliseconds
		final long buffer_ms = props.getInt("buffer_size") * 1000;

		// copy the original infoList
		final List<XYSeriesInfo> list = new ArrayList<XYSeriesInfo>(infoList);
		infoList.clear();

		if (xregisters != null && yregisters != null) {
			if (xregisters.length != yregisters.length)
				throw new RuntimeException("xaxis_registers and yaxis_registers must have same length");

			// build the new register info list
			for (int i = 0; i < xregisters.length; i++) {
				final String xreg = xregisters[i];
				final String yreg = yregisters[i];

				XYSeriesInfo xsi = findSeriesInfo(xreg, yreg, list);
				if (xsi == null) {
					// Didn't find it, create a new one. Do not sort based on X-value and
					// do allow duplicate X-values. This creates a classic scatter plot.
					final XYSeries series = new XYSeries(xreg + "/" + yreg, false, true);
					series.setMaximumItemCount((int)(buffer_ms / 500));
					xsi = new XYSeriesInfo(xreg, yreg, series);
				}

				// add it to the infoList and JFreeChart dataset
				infoList.add(xsi);
				collection.addSeries(xsi.series);
				manager.addRegister(xreg);
				manager.addRegister(yreg);
			}
		}

		// remove all original registers from the network client
		for (final XYSeriesInfo xsi : list) {
			manager.remRegister(xsi.xRegister);
			manager.remRegister(xsi.yRegister);
		}
	}

	/* ---------------------------------------------------------------------- */
	/* Private Methods                                                        */
	/* ---------------------------------------------------------------------- */

	/**
	 * Find a XYSeriesInfo given the X and Y Axis Registers.
	 *
	 * @param xreg the X Axis Register to find
	 * @param yreg the Y Axis Register to find
	 * @param list the List to search
	 */
	synchronized private XYSeriesInfo findSeriesInfo(final String xreg, final String yreg, final List<XYSeriesInfo> list) {
		for (final XYSeriesInfo xsi : list) {
			if (xreg.equals(xsi.xRegister) && yreg.equals(xsi.yRegister))
				return xsi;
		}

		// not found
		return null;
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
						final RangeDialog dialog = new RangeDialog(window, props, "x", false);
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
						updateAxis(chart, props, prefix);
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
							updateAxis(chart, props, prefix);
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
						updateAxis(chart, props, prefix);
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
							updateAxis(chart, props, prefix);
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
						final RangeDialog dialog = new RangeDialog(window, props, "y", false);
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
						updateAxis(chart, props, prefix);
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
							updateAxis(chart, props, prefix);
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
						updateAxis(chart, props, prefix);
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
							updateAxis(chart, props, prefix);
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
		for (final XYSeriesInfo xsi : infoList) {
			xsi.series.setMaximumItemCount((int)buffer_ms / 500);
		}

		// update scaling to match new buffer size
		updateDomainAxisRange(chart, props);
	}

	/**
	 * Drop all existing plot data
	 */
	synchronized private void dropExistingData(final JFreeChart chart, final PlotProperties props) {
		for (final XYSeriesInfo xsi : infoList) {
			xsi.series.clear();
		}
	}

	private void updateAxis(final JFreeChart chart, final PlotProperties props, final String prefix) {
		final String label_type = props.getString(prefix + "label_type");
		final XYPlot plot = chart.getXYPlot();

		final ValueAxis axis;
		if (prefix.equals("x1")) {
			axis = plot.getDomainAxis(0);
		} else if (prefix.equals("x2")) {
			axis = plot.getDomainAxis(1);
		} else if (prefix.equals("y1")) {
			axis = plot.getRangeAxis(0);
		} else if (prefix.equals("y2")) {
			axis = plot.getRangeAxis(1);
		} else {
			throw new RuntimeException("unable to parse prefix");
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
			if (prefix.equals("x1") || prefix.equals("x2")) {
				axis.setLabel("Domain");
			} else {
				axis.setLabel("Range");
			}
		} else if (label_type.equals("custom")) {
			final String label_custom = props.getString(prefix + "label_custom");
			axis.setLabel(label_custom);
		}
	}

	private void updateDomainAxisRange(final JFreeChart chart, final PlotProperties props) {
		final XYPlot plot = chart.getXYPlot();
		final List<Integer> list = new ArrayList<Integer>();
		for (int i = 0; i < plot.getDomainAxisCount(); i++) {
			final ValueAxis axis = plot.getDomainAxis(i);
			list.add(i);

			final String xrange_type = props.getString("xrange_type");
			if (xrange_type.equals("scale to data")) {
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
				final NumberAxis numberAxis = (NumberAxis)axis;
				numberAxis.setAutoRangeIncludesZero(false);
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
}

// vim: set ts=4 sts=4 sw=4 noet:
