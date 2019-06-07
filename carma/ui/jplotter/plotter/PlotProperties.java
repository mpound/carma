package carma.ui.jplotter.plotter;

import java.util.*;

import java.io.Reader;
import java.io.InputStream;
import java.io.IOException;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

public final class PlotProperties extends Properties
{
	private final EventListenerList listeners = new EventListenerList();

	/**
	 * Constructor
	 */
	public PlotProperties(final String type) {
		super();

		// set defaults for all properties
		super.setProperty("plot_title", "");
		super.setProperty("marker_type", "Line");
		super.setProperty("marker_size", "Small");
		super.setProperty("legend_location", "Bottom");

		super.setProperty("xrange_type", "full width");
		super.setProperty("xrange_min", "0.0");
		super.setProperty("xrange_max", "5.0");
		super.setProperty("xrange_window", "60");

		super.setProperty("yrange_type", "scale to data");
		super.setProperty("yrange_min", "0.0");
		super.setProperty("yrange_max", "5.0");

		super.setProperty("x1axis_enable", "true");
		super.setProperty("x1axis_timezone", "UTC");
		super.setProperty("x1label_type", "automatic");
		super.setProperty("x1label_custom", "");

		super.setProperty("x2axis_enable", "false");
		super.setProperty("x2axis_timezone", "US Pacific (CARMA Local Time)");
		super.setProperty("x2label_type", "automatic");
		super.setProperty("x2label_custom", "");

		super.setProperty("y1axis_enable", "true");
		super.setProperty("y1label_type", "automatic");
		super.setProperty("y1label_custom", "");

		super.setProperty("y2axis_enable", "false");
		super.setProperty("y2label_type", "automatic");
		super.setProperty("y2label_custom", "");

		super.setProperty("buffer_size", "600");

		super.setProperty("win_xlocation", "100");
		super.setProperty("win_ylocation", "100");
		super.setProperty("win_height", "500");
		super.setProperty("win_width", "800");

		// xaxis_registers and yaxis_registers have no defaults (default to null)

		if (type.equals("Time Series")) {
			super.setProperty("plot_type", "Time Series");
			super.setProperty("plot_title", "Untitled Time Series Plot");
			super.setProperty("xrange_type", "full width");
			super.setProperty("yrange_type", "scale to data");
		} else if (type.equals("Arbitrary X-Y Plot")) {
			super.setProperty("plot_type", "Arbitrary X-Y Plot");
			super.setProperty("plot_title", "Untitled X-Y Plot");
			super.setProperty("xrange_type", "scale to data");
			super.setProperty("yrange_type", "scale to data");

			// Most people expect scatter plots to be a set of dots. The Large
			// marker looks pretty good, so we'll go with that as the default.
			super.setProperty("marker_type", "Dot");
			super.setProperty("marker_size", "Large");
		} else {
			throw new RuntimeException("BUG: PlotProperties has invalid type: " + type);
		}
	}

	/**
	 * Constructor
	 */
	public PlotProperties() {
		this("Time Series");
	}

	/**
	 * Copy Constructor.
	 */
	public PlotProperties(final PlotProperties props) {
		this();

		// This is a really dumb way to deep-copy the original PlotProperties
		// object, but it was the only reliable way I was able to implement.
		try {
			final ByteArrayOutputStream out = new ByteArrayOutputStream();
			props.storeToXML(out, null);
			final ByteArrayInputStream in = new ByteArrayInputStream(out.toByteArray());
			this.loadFromXML(in);
		} catch (IOException ex) {
			System.err.println("IOException: " + ex);
		}
	}

	/**
	 * Copy a given key/value from another PlotProperties instance.
	 */
	public void copyFrom(final PlotProperties props, final String key) {
		setProperty(key, props.getProperty(key));
	}

	/* ---------------------------------------------------------------------- */
	/* ChangeListener Notifications                                           */
	/* ---------------------------------------------------------------------- */

	synchronized public void addChangeListener(final ChangeListener listener) {
		listeners.add(ChangeListener.class, listener);

		// update all listeners now
		this.notifyChangeListeners();
	}

	synchronized public void removeChangeListener(final ChangeListener listener) {
		listeners.remove(ChangeListener.class, listener);
	}

	synchronized public void notifyChangeListeners() {
		final ChangeListener[] array = listeners.getListeners(ChangeListener.class);
		for (int i = 0; i < array.length; i++) {
			final ChangeListener listener = array[i];
			listener.stateChanged(new ChangeEvent(this));
		}
	}

	/* ---------------------------------------------------------------------- */
	/* Override Default Interfaces                                            */
	/* ---------------------------------------------------------------------- */

	@Override
	public void load(InputStream inStream) throws IOException {
		super.load(inStream);
		this.notifyChangeListeners();
	}

	@Override
	public void load(Reader reader) throws IOException {
		super.load(reader);
		this.notifyChangeListeners();
	}

	@Override
	public void loadFromXML(InputStream in) throws IOException {
		super.loadFromXML(in);
		this.notifyChangeListeners();
	}

	@Override
	public Object setProperty(String key, String value) {
		final Object obj = super.setProperty(key, value);
		this.notifyChangeListeners();
		return obj;
	}

	/* ---------------------------------------------------------------------- */
	/* Programmer Convenience: setters                                        */
	/* ---------------------------------------------------------------------- */

	public void setBoolean(final String key, final boolean val) {
		this.setProperty(key, Boolean.toString(val));
	}

	public void setInt(final String key, final int val) {
		this.setProperty(key, Integer.toString(val));
	}

	public void setDouble(final String key, final double val) {
		this.setProperty(key, Double.toString(val));
	}

	public void setString(final String key, final String val) {
		this.setProperty(key, val);
	}

	public void setStringArray(final String key, final String[] values) {
		final StringBuilder sb = new StringBuilder();
		for (String v : values) {
			if (sb.length() > 0)
				sb.append(" ");

			sb.append(v);
		}

		this.setProperty(key, sb.toString());
	}

	/* ---------------------------------------------------------------------- */
	/* Programmer Convenience: getters                                        */
	/* ---------------------------------------------------------------------- */

	public boolean getBoolean(final String key) {
		final String val = this.getProperty(key);
		return Boolean.valueOf(val);
	}

	public int getInt(final String key) {
		try {
			final String val = this.getProperty(key);
			return Integer.parseInt(val);
		} catch (NumberFormatException ex) {
			// stifle
		}

		return 0;
	}

	public double getDouble(final String key) {
		try {
			final String val = this.getProperty(key);
			return Double.parseDouble(val);
		} catch (NumberFormatException ex) {
			// stifle
		}

		return 0.0;
	}

	public String getString(final String key) {
		return this.getProperty(key);
	}

	public String[] getStringArray(final String key) {
		final String values = this.getProperty(key);
		if (values == null)
			return null;

		return values.split("\\s+");
	}

	/* ---------------------------------------------------------------------- */
	/* Plot Type Helper                                                       */
	/* ---------------------------------------------------------------------- */

	/**
	 * Convenience helper for retrieving plot type.
	 */
	public boolean isTimeSeriesPlot() {
		return getString("plot_type").equals("Time Series");
	}

	/* ---------------------------------------------------------------------- */
	/* Plot Parameter Completeness Validation                                 */
	/* ---------------------------------------------------------------------- */

	/**
	 * Validate the attributes used on the configuration dialog (step 1 in the
	 * wizard). This does not validate any other part of the PlotProperties
	 * object.
	 */
	public boolean validateConfiguration() {
		// plot_type
		{
			final ArrayList<String> values = new ArrayList<String>();
			values.add("Time Series");
			values.add("Arbitrary X-Y Plot");

			if (!values.contains(getProperty("plot_type"))) {
				System.err.println("INVALID PROPERTY: plot_type");
				return false;
			}
		}

		// marker_type
		{
			final ArrayList<String> values = new ArrayList<String>();
			values.add("Dot");
			values.add("Line");
			values.add("Shape");

			if (!values.contains(getProperty("marker_type"))) {
				System.err.println("INVALID PROPERTY: marker_type");
				return false;
			}
		}

		// marker_size
		{
			final ArrayList<String> values = new ArrayList<String>();
			values.add("Small");
			values.add("Medium");
			values.add("Large");
			values.add("Ultra");

			if (!values.contains(getProperty("marker_size"))) {
				System.err.println("INVALID PROPERTY: marker_size");
				return false;
			}
		}

		// legend_location
		{
			final ArrayList<String> values = new ArrayList<String>();
			values.add("Top");
			values.add("Bottom");
			values.add("Left");
			values.add("Right");
			values.add("Disable");

			if (!values.contains(getProperty("legend_location"))) {
				System.err.println("INVALID PROPERTY: legend_location");
				return false;
			}
		}

		// xrange_type + yrange_type
		{
			final ArrayList<String> values = new ArrayList<String>();
			values.add("scale to data");
			values.add("custom");

			if (!values.contains(getProperty("yrange_type"))) {
				System.err.println("INVALID PROPERTY: yrange_type");
				return false;
			}

			// extra options for xrange_type on Time Series plots only
			if (isTimeSeriesPlot()) {
				values.add("full width");
				values.add("sliding window");
			}

			if (!values.contains(getProperty("xrange_type"))) {
				System.err.println("INVALID PROPERTY: xrange_type");
				return false;
			}
		}

		// xrange_min, xrange_max, yrange_min, yrange_max
		{
			final ArrayList<String> keys = new ArrayList<String>();
			keys.add("xrange_min");
			keys.add("xrange_max");
			keys.add("yrange_min");
			keys.add("yrange_max");

			for (final String key : keys) {
				final String value = getProperty(key, "");
				try {
					Double.parseDouble(value);
				} catch (NumberFormatException ex) {
					System.err.println("INVALID PROPERTY: " + key);
					return false;
				}
			}
		}

		// xrange_window
		{
			final String xrange_window = getProperty("xrange_window", "");
			try {
				Integer.parseInt(xrange_window);
			} catch (NumberFormatException ex) {
				System.err.println("INVALID PROPERTY: xrange_window");
				return false;
			}
		}

		// axis
		{
			final ArrayList<String> prefixes = new ArrayList<String>();
			prefixes.add("x1");
			prefixes.add("x2");
			prefixes.add("y1");
			prefixes.add("y2");

			for (final String prefix : prefixes) {
				// axis_enable
				{
					final String value = getProperty(prefix + "axis_enable", "");
					try {
						Boolean.parseBoolean(value);
					} catch (NumberFormatException ex) {
						System.err.println("INVALID PROPERTY: " + prefix + "axis_enable");
						return false;
					}
				}

				// axis_timezone
				if (prefix.charAt(0) == 'x' && isTimeSeriesPlot()) {
					final ArrayList<String> values = new ArrayList<String>();
					values.add("UTC");
					values.add("US Pacific (CARMA Local Time)");
					values.add("Local Time (this computer)");

					if (!values.contains(getProperty(prefix + "axis_timezone"))) {
						System.err.println("INVALID PROPERTY: " + prefix + "axis_timezone");
						return false;
					}
				}

				// label_type
				{
					final ArrayList<String> values = new ArrayList<String>();
					values.add("automatic");
					values.add("custom");

					if (!values.contains(getProperty(prefix + "label_type"))) {
						System.err.println("INVALID PROPERTY: " + prefix + "label_type");
						return false;
					}
				}

				// label_custom needs no validation, it is an arbitrary string
			}
		}

		// buffer_size
		{
			final String value = getProperty("buffer_size", "");
			try {
				Integer.parseInt(value);
			} catch (NumberFormatException ex) {
				System.err.println("INVALID PROPERTY: buffer_size");
				return false;
			}
		}

		// window properties
		{
			final ArrayList<String> keys = new ArrayList<String>();
			keys.add("win_xlocation");
			keys.add("win_ylocation");
			keys.add("win_height");
			keys.add("win_width");

			for (final String key : keys) {
				try {
					Integer.parseInt(getProperty(key, ""));
				} catch (NumberFormatException ex) {
					System.err.println("INVALID PROPERTY: " + key);
					return false;
				}
			}
		}

		// all checks passed
		return true;
	}

	/**
	 * Validate the attributes used on the monitor point selection dialog
	 * (step 2 in the wizard). This does not validate any other parts of the
	 * PlotProperties object.
	 */
	public boolean validateMonitorPoints() {
		// xaxis_registers (skipped for time series plots)
		if (!isTimeSeriesPlot()) {
			final String[] values = getStringArray("xaxis_registers");
			if (values == null || values.length <= 0 || values[0].equals(""))
				return false;
		}

		// yaxis_registers
		{
			final String[] values = getStringArray("yaxis_registers");
			if (values == null || values.length <= 0 || values[0].equals(""))
				return false;
		}

		// lengths must be the same for arbitrary plots
		if (!isTimeSeriesPlot()) {
			final String[] xvalues = getStringArray("xaxis_registers");
			final String[] yvalues = getStringArray("yaxis_registers");

			if (xvalues == null || yvalues == null)
				return false;

			if (xvalues.length != yvalues.length)
				return false;
		}

		// all checks passed
		return true;
	}

	/**
	 * Validate all attributes of the PlotProperties object.
	 */
	public boolean validate() {
		return validateConfiguration() && validateMonitorPoints();
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
