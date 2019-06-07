/*
 * SliderDemo2.java
 * ---------------
 * A demo that uses a SlidingXYDataset that provides a window of the
 * underlying dataset
 *
 * This example uses TimeSeries
 *
 */

package carma.ui.jplotter.jfree;

import java.util.Date;
import java.awt.BorderLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.time.Minute;
import org.jfree.data.time.RegularTimePeriod;
import org.jfree.data.time.TimeSeries;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.time.TimeSeriesCollection;


/**
 * A demo that uses a "wrapper" dataset that provides a window of the
 * underlying dataset.
 */
public class SliderDemo2 extends JFrame {

	/** The Constant COUNT. */
	static final int COUNT = 200;

	/** The Constant WINDOW. */
	public static final int WINDOW = 25;

	/** The Constant FIRST. */
	public static final int FIRST = 0;

	/**
	 * The Class DemoPanel.
	 */
	private static class DemoPanel
		extends JPanel
		implements ChangeListener {

		/** The chart panel. */
		private ChartPanel chartPanel;

		/** The chart. */
		private JFreeChart chart;

		/** The slider. */
		private JSlider slider;

		/** The dataset. */
		private SlidingTimeSeriesCollection dataset;

		/**
		 * Creates a new demo panel.
		 */
		public DemoPanel() {
			super(new BorderLayout());
			this.chart = createChart();
			this.chartPanel = new ChartPanel(this.chart);
			this.chartPanel.setPreferredSize(new java.awt.Dimension(600, 270));
			add(this.chartPanel);
			JPanel dashboard = new JPanel(new BorderLayout());
			this.slider = new JSlider(0, COUNT - WINDOW - 1, 0);
			slider.setPaintLabels(true);
			slider.setPaintTicks(true);
			slider.setMajorTickSpacing(WINDOW);
			slider.setMaximum(this.dataset.getMaximumSliderValue());
			this.slider.addChangeListener(this);
			dashboard.add(this.slider);
			add(dashboard, BorderLayout.SOUTH);
		}

		/**
		 * Creates the demo chart.
		 *
		 * @return The chart.
		 */
		private JFreeChart createChart() {
			XYDataset dataset1 = createDataset("Random 1-", new Minute(), COUNT);
			JFreeChart chart1 = ChartFactory.createTimeSeriesChart(
				"Sliding Demo 2",
				"Time of Day",
				"Value",
				dataset1,
				true,
				true,
				false
			);

			XYPlot plot = chart1.getXYPlot();
			DateAxis xaxis = (DateAxis) plot.getDomainAxis();
			xaxis.setAutoRange(true);
			xaxis.setFixedAutoRange(25 * 60 * 1000);
			return chart1;
		}


		/**
		 * Creates a sample dataset.
		 *
		 * @param name  the dataset name.
		 * @param start  the starting period.
		 * @param count  the number of values to generate.
		 *
		 * @return The dataset.
		 */
		private XYDataset createDataset(String name, RegularTimePeriod start, int count) {
			final TimeSeriesCollection tsc = new TimeSeriesCollection();
			{
				final TimeSeries series1 = getRandomTimeSeries(name + "1", 100.0, start, count);
				final TimeSeries series2 = getRandomTimeSeries(name + "2", 75.0, start, count / 2);
				tsc.addSeries(series1);
				tsc.addSeries(series2);
			}

			{
				final long millis = System.currentTimeMillis() + (5 * 60 * 1000);
				final Date date = new Date(millis);

				final TimeSeries series3 = getRandomTimeSeries(name + "3", 60.0, new Minute(date), 8);
				tsc.addSeries(series3);

				final TimeSeries series4 = getRandomTimeSeries(name + "4", 60.0, new Minute(date), 0);
				tsc.addSeries(series4);

				final TimeSeries series5 = getRandomTimeSeriesWithHole(name + "5", 60.0, new Minute(date), 500);
				tsc.addSeries(series5);
			}

			final int interval_ms = 60 * 1000; // 60 seconds
			this.dataset = new SlidingTimeSeriesCollection(tsc, WINDOW, interval_ms);
			return dataset;
		}

		/**
		 * Creates a random TimeSeries.
		 *
		 * @param name  the dataset name.
		 * @param base  the starting value.
		 * @param start  the starting period.
		 * @param count  the number of values to generate.
		 *
		 * @return The TimeSeries.
		 */
		private TimeSeries getRandomTimeSeries(String name, double base, RegularTimePeriod start, int count) {
			TimeSeries ts = new TimeSeries(name, start.getClass());
			RegularTimePeriod period = start;
			double value = base;
			for (int i = 0; i < count; i++) {
				ts.add(period, value);
				period = period.next();
				value = value * (1 + (Math.random() - 0.495) / 10.0);
			}

			System.out.println("TimeSeries: name=" + name + " start=" + start + " end=" + period.previous());
			return ts;
		}

		/**
		 * Create a random TimeSeries with a hole in it.
		 *
		 * @param name the dataset name
		 * @param base the starting value
		 * @param start the starting period
		 * @param count the number of values to generate
		 *
		 * @return the TimeSeries
		 */
		private TimeSeries getRandomTimeSeriesWithHole(String name, double base, RegularTimePeriod start, int count) {
			TimeSeries ts = new TimeSeries(name, start.getClass());
			RegularTimePeriod period = start;
			double value = base;
			System.out.println("TimeSeriesHole: name=" + name + " start=" + start);
			for (int i = 0; i < count; i++) {
				ts.add(period, value);
				period = period.next();
				value = value * (1 + (Math.random() - 0.495) / 10.0);

				// add a hole at (count / 4) which is (count / 4) elements long
				if (i == (count / 4)) {
					System.out.println("Hole: start=" + period);
					//final int HOLE_SIZE = count / 4;
					final int HOLE_SIZE = 15;
					for (int j = 0; j < HOLE_SIZE; j++) {
						period = period.next();
					}
					System.out.println("Data: re-start=" + period);
				}
			}

			System.out.println("Date: end=" + period.previous());
			return ts;
		}

		/**
		 * Handles a state change event.
		 *
		 * @param event  the event.
		 */
		public void stateChanged(ChangeEvent event) {
			int value = this.slider.getValue();
			this.dataset.setWindowStartIndex(value);
		}
	}

	/**
	 * The Constructor.
	 *
	 * @param title  the frame title.
	 */
	public SliderDemo2(String title) {
		super(title);
		setContentPane(new DemoPanel());
	}

	/**
	 * Creates the demo panel.
	 *
	 * @return A panel.
	 */
	public static JPanel createDemoPanel() {
		return new DemoPanel();
	}

	/**
	 * Starting point for the demonstration application.
	 *
	 * @param args  ignored.
	 */
	public static void main(String[] args) {
		SliderDemo2 demo = new SliderDemo2("SliderDemo Demo2");
		demo.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		demo.pack();
		demo.setVisible(true);
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
