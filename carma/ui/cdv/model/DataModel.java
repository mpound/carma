// $Id: DataModel.java,v 1.20 2014/08/26 17:58:45 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import java.util.List;
import java.util.Set;
import java.util.Hashtable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Comparator;
import java.util.Collections;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import java.awt.Color;
import java.awt.Image;
import java.awt.Graphics;
import java.awt.Component;
import java.awt.Dimension;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.SwingUtilities;
import javax.swing.table.AbstractTableModel;

import java.text.NumberFormat;

import carma.util.Debug;

import carma.ui.cdv.plot.XYPlot;
import carma.ui.cdv.model.PlotInfo;
import carma.ui.cdv.util.EasyTimer;
import carma.ui.cdv.util.LinearFit;

/**
 *	Concrete DataModel
 *
 *	@author Rick Hobbs
 *	@version $Revision: 1.20 $ , $Date: 2014/08/26 17:58:45 $
 *	@since JDK1.3
 */
public class DataModel extends AbstractTableModel
					   implements DataListener {

	private enum MetricType {
		AMPLITUDE,
		PHASE_IN_DEG,
	};

	// static grid sizes
	private static final int NUM_ANTENNAS = 23;
	private static final int NUM_INPUTS = 32;

	// data source
	private DataSource dataSource_;
	private final String dataSourceKey_;

	// plot info
	private PlotInfo info_;

	// setup information
	private String dataType_;
	private String dataView_;
	private int bandNumber_;
	private String polarization_;

	private ArrayList<String> sbTypes_;

	// rows and cols
	private List<String> colNames_;
	private List<String> rowNames_;

	// plots
	private Hashtable<String, XYPlot> plots_;

	private int[] antNumbers_;
	private List<AntPair> antPairs_;
	private Set<Integer> removedAntNumbers_;

	private int[] inputNumbers_;
	private List<InputPair> inputPairs_;
	private Set<Integer> removedInputNumbers_;

	// Lock and synchronized boolean
	private final Lock plotLock_ = new ReentrantLock();
	private boolean plotComplete_;

	// configurable settings
	private boolean scaleUSBLSBTogether_ = true;
	private String showPlotLabels_ = "none";
	private boolean stopPlotter_ = false;

	protected Icon blankIcon_ = createBlankIcon();

	/*------------------------------------------------------------------------*/
	/* Public Methods														  */
	/*------------------------------------------------------------------------*/

	public DataModel(DataSource s, String key, PlotInfo info) {
		super();
		plotComplete_ = true;

		dataSource_ = s;
		dataSourceKey_ = key;
		info_ = info;

		dataType_ = info.getDataType();
		dataView_ = info.getDataView();
		bandNumber_ = info.getBandNumber();
		polarization_ = info.getPolarization();

		sbTypes_ = new ArrayList<String>(3);
		sbTypes_.add("USB");
		sbTypes_.add("LSB");
		sbTypes_.add("AUTO");

		removedAntNumbers_ = new HashSet<Integer>();
		removedInputNumbers_ = new HashSet<Integer>();

		init();

		// add the data source last, so we don't get events before we are ready
		dataSource_.addDataListener(this, dataSourceKey_);
	}

	public void setScaleUSBLSBTogether(boolean scaleTogether) {
		scaleUSBLSBTogether_ = scaleTogether;
	}

	public void setStopPlotter(boolean stopPlotter) {
		stopPlotter_ = stopPlotter;
	}

	public void setShowPlotLabel(String label) {
		showPlotLabels_ = label;
	}

	public void addAntenna(int antNumber) {
		removedAntNumbers_.remove(antNumber);
	}

	public void removeAntenna(int antNumber) {
		removedAntNumbers_.add(antNumber);
	}

	public void addInput(int inputNumber) {
		removedInputNumbers_.remove(inputNumber);
	}

	public void removeInput(int inputNumber) {
		removedInputNumbers_.add(inputNumber);
	}

	public void stop() {
		Debug.print(this, Debug.INFO, "stop: unhooking from data stream");
		dataSource_.removeDataListener(this, dataSourceKey_);
	}

	/*------------------------------------------------------------------------*/
	/* DataListener Interface												  */
	/*------------------------------------------------------------------------*/

	/**
	 *	Called by a DataSource when its data changes.
	 */
	public void dataChanged(DataEvent event) {

		Debug.print(this, Debug.INFO, "dataChanged: name=" + event.getName());
		final CorrelatorDataRouter dataRouter = (CorrelatorDataRouter)(event.getDataSource());
		final int band = info_.getBandNumber();
		final String pol = info_.getPolarization();
		final String key = dataRouter.createKey(dataSourceKey_, band, pol);

		// if the key for this event does not match the key we will use for
		// our data, we should ignore the data
		if (!key.equals(event.getName())) {
			Debug.print(this, Debug.INFO, "dataChanged: keys do not match, ignore event");
			return;
		}

		// if the plotter is stopped, don't plot
		if (stopPlotter_) {
			Debug.print(this, Debug.INFO, "dataChanged: plotter is stopped, ignore event");
			return;
		}

		// start the plotter
		boolean plotComplete = testAndSetPlotComplete(false);
		if (!plotComplete) {
			Debug.print(this, Debug.INFO, "dataChanged: plotter active, dropping data");
			return;
		}

		SpectralRecord record = null;
		if (dataType_.equals("avg"))
			record = dataRouter.getAvgSpectra(key);
		else if (dataType_.equals("spec"))
			record = dataRouter.getRawSpectra(key);
		else
			throw new RuntimeException("BUG: unknown data type: " + dataType_);

		// check that we actually have some data now
		if (record == null) {
			Debug.print(this, Debug.INFO, "no data from the router yet");
			testAndSetPlotComplete(true);
			return;
		}

		// call rePlot() in the Swing thread context
		SwingUtilities.invokeLater(new PlotWorker(record));
	}

	/*------------------------------------------------------------------------*/
	/* Private Methods														  */
	/*------------------------------------------------------------------------*/

	private void init() {
		plots_ = new Hashtable<String, XYPlot>();

		// statically sized grid (antennas)
		antNumbers_ = new int[NUM_ANTENNAS];
		for (int i = 0; i < antNumbers_.length; i++)
			antNumbers_[i] = i + 1;

		antPairs_ = new ArrayList<AntPair>();

		// statically sized grid (inputs)
		inputNumbers_ = new int[NUM_INPUTS];
		for (int i = 0; i < inputNumbers_.length; i++)
			inputNumbers_[i] = i + 1;

		inputPairs_ = new ArrayList<InputPair>();

		createColNames();
		createRowNames();
	}

	/*
	 * We do our own synchronization here since Java synchronized methods are
	 * synchronized against *all* other synchronized methods in the same class.
	 *
	 * This means users of the following two functions can block for a LONG time
	 * while rePlot() is running. That sucks for performance.
	 *
	 * This is a classic atomic test and set the plotComplete_ member variable.
	 * It will return the previous value, and set the new value atomically.
	 * This creates an easy, race-free update mechanism.
	 */
	private boolean testAndSetPlotComplete(boolean b) {
		boolean ret;
		plotLock_.lock();
		try {
			ret = plotComplete_;
			plotComplete_ = b;
		} finally {
			plotLock_.unlock();
		}
		return ret;
	}

	/**
	 *	Sets the Comparator to use for Sorting
	 */
	public void setComparator(Comparator c) {
		throw new RuntimeException("BUG: no support for setComparator");
	}

	// called by PlotWorker to draw plots.
	// a typical path is: rePlot -> ampPhasePlot
	private synchronized void rePlot(SpectralRecord record) {
		final CorrelatorSpectraMap usbMap = record.getUSBSpectra();
		antPairs_ = getAntPairs(usbMap);
		inputPairs_ = getInputPairs(usbMap);

		createRowNames();
		createColNames();

		EasyTimer timer = new EasyTimer();
		timer.start();
		ampPhasePlot(record);
		timer.stop();
		Debug.print(this, Debug.INFO, "rePlot: time to update plot: " + timer);

		fireTableDataChanged();
		testAndSetPlotComplete(true);
	}

	public void setPlotInfo(PlotInfo info) {
		throw new RuntimeException("BUG: no support for setPlotInfo");
	}

	// All methods below are helpers which take the data from a source
	// and arranges it for a JTable view. These methods are
	// designed to present the data in 2 different ways.

	private int mod(int i, int j) {
		return i - j * (i / j);
	}

	// implementation of getValueAt() for triangle plots
	private Object getValueAtTriangle(int row, int col) {
		// antenna plot
		if (dataView_.equals("ant")) {
			int ant1 = antNumbers_[row];
			int ant2 = antNumbers_[col - 1];

			// diagonal
			if (row == (col - 1)) {
				String colName = sbTypes_.get(2);
				String key = createKey(ant1, ant2, colName);
				return plots_.get(key);
			}

			// lower triangle
			if (row > (col - 1)) {
				String colName = sbTypes_.get(1);
				String key = createKey(ant1, ant2, colName);
				return plots_.get(key);
			}

			// upper triangle
			String colName = sbTypes_.get(0);
			String key = createKey(ant1, ant2, colName);
			return plots_.get(key);
		}

		// input plot
		if (dataView_.equals("input")) {
			int input1 = inputNumbers_[row];
			int input2 = inputNumbers_[col - 1];

			// diagonal
			if (row == (col - 1)) {
				String colName = sbTypes_.get(2);
				String key = createKey(input1, input2, colName);
				return plots_.get(key);
			}

			// lower triangle
			if (row > (col - 1)) {
				String colName = sbTypes_.get(1);
				String key = createKey(input1, input2, colName);
				return plots_.get(key);
			}

			// upper triangle
			String colName = sbTypes_.get(0);
			String key = createKey(input1, input2, colName);
			return plots_.get(key);
		}

		// fallback
		return blankIcon_;
	}

	/*------------------------------------------------------------------------*/
	/* TableModel Interface                                                   */
	/*------------------------------------------------------------------------*/

	/**
	 *  Return the Row count
	 *  @return The number of rows
	 */
	public int getRowCount() {
		if (rowNames_ != null)
			return rowNames_.size();

		return 1;
	}

	/**
	 *  Return the Column count
	 *  @return The number of columns
	 */
	public int getColumnCount() {
		if (colNames_ != null)
			return colNames_.size();

		return 1;
	}

	public String getColumnName(int col) {
		return colNames_.get(col);
	}

	/**
	 *	called by JTable for rendering. This method is also called
	 *	as the mouse is moved across the cell.
	 */
	public Object getValueAt(int row, int col) {

		try {
			// zero'th column is row names
			if (col == 0) {
				if (rowNames_ != null)
					return rowNames_.get(row);
				else
					return "";
			}

			// triangle plot
			return getValueAtTriangle(row, col);

		} catch (IndexOutOfBoundsException e) {
			if (antNumbers_ == null)
				Debug.print(this, Debug.ERROR, "getValueAt: antNumbers_ is null");
			else
				Debug.print(this, Debug.ERROR, "getValueAt: antNumbers_.length=" + antNumbers_.length);

			// fallback for errors
			Debug.print(this, Debug.INFO, "getValueAt: fallback");
			return blankIcon_;
		}
	}

	public Class getColumnClass(int c) {
		if (c == 0)
			return getValueAt(0, c).getClass();
		else
			return Icon.class;
	}

	private XYPlot getPlot(String key) {

		// the plot already exists, return it immediately
		if (plots_.containsKey(key))
			return plots_.get(key);

		// create the plot (override the getPreferredSize() method)
		XYPlot plot = new XYPlot() {
			public Dimension getPreferredSize() {
				return new Dimension(400, 100);
			}
		};

		plot.setAxisMinMax(XYPlot.Axis.X_AXIS, 0.0, 64.0);
		plot.setAxisMinMax(XYPlot.Axis.Y_AXIS, -180.0, 180.0);
		plot.setDrawAxis(XYPlot.Axis.X_AXIS, true);
		plot.setDrawAxis(XYPlot.Axis.Y_AXIS, true);
		plot.setDrawAxisLabel(XYPlot.Axis.X_AXIS, false);
		plot.setDrawAxisLabel(XYPlot.Axis.Y_AXIS, false);
		plot.setPadding(5);
		plot.setSize(200, 100);

		plots_.put(key, plot);
		return plot;
	}

	private float[] rescalePhase(float[] phase, float min, float max) {
		float[] data = new float[phase.length];
		float m = (max - min) / 360.f;
		float b = (max + min) * 0.5f;
		for (int i = 0; i < phase.length; i++)
			if (!Float.isNaN(phase[i]))
				data[i] = m * phase[i] + b;

		return data;
	}

	/**
	 * InnerClass used to update plots in the Swing event thread.
	 */
	private class PlotWorker implements Runnable {
		private final SpectralRecord record;

		public PlotWorker(SpectralRecord record) {
			this.record = record;
		}

		public void run() {
			rePlot(record);
		}
	}

	/*------------------------------------------------------------------------*/
	/* New Interface Methods                                                  */
	/*------------------------------------------------------------------------*/

	private boolean skipMinMax(CorrelatorSpectra spectra) {

		// antenna view
		if (dataView_.equals("ant")) {
			if (removedAntNumbers_.contains(spectra.getAnt1()))
				return true;

			if (removedAntNumbers_.contains(spectra.getAnt2()))
				return true;

			return false;
		}

		// input view
		if (dataView_.equals("input")) {
			if (removedInputNumbers_.contains(spectra.getInput1()))
				return true;

			if (removedInputNumbers_.contains(spectra.getInput2()))
				return true;

			return false;
		}

		return false;
	}

	private MinMax computeMinMax(CorrelatorSpectraMap map, MetricType mt) {

		float min = Float.MAX_VALUE;
		float max = Float.MIN_VALUE;

		// for each spectra
		for (CorrelatorSpectra spectra : map.values()) {

			// skip hidden antennas / inputs
			if (skipMinMax(spectra))
				continue;

			float[] data = null;
			switch (mt) {
			case AMPLITUDE:
				data = spectra.getAmplitudes();
				break;
			case PHASE_IN_DEG:
				data = spectra.getPhases();
				break;
			default:
				throw new RuntimeException("BUG: unsupported metric type");
			}

			for (float point : data) {
				min = Math.min(point, min);
				max = Math.max(point, max);
			}
		}

		return new MinMax(min, max);
	}

	private MinMax computeMinMax(CorrelatorSpectra spectra, MetricType mt) {

		float min = Float.MAX_VALUE;
		float max = Float.MIN_VALUE;

		float[] data = null;
		switch (mt) {
		case AMPLITUDE:
			data = spectra.getAmplitudes();
			break;
		case PHASE_IN_DEG:
			data = spectra.getPhases();
			break;
		default:
			throw new RuntimeException("BUG: unsupported metric type");
		}

		for (float point : data) {
			min = Math.min(point, min);
			max = Math.max(point, max);
		}

		return new MinMax(min, max);
	}

	private List<Integer> getAntennas(CorrelatorSpectraMap map) {
		Set<Integer> antennas = new HashSet<Integer>();
		for (CorrelatorSpectra spectra : map.values()) {
			antennas.add(spectra.getAnt1());
			antennas.add(spectra.getAnt2());
		}

		List<Integer> ret = new ArrayList<Integer>(antennas);
		Collections.sort(ret);
		return ret;
	}

	private List<Integer> getInputs(CorrelatorSpectraMap map) {
		Set<Integer> inputs = new HashSet<Integer>();
		for (CorrelatorSpectra spectra : map.values()) {
			inputs.add(spectra.getInput1());
			inputs.add(spectra.getInput2());
		}

		List<Integer> ret = new ArrayList<Integer>(inputs);
		Collections.sort(ret);
		return ret;
	}

	private List<AntPair> getAntPairs(CorrelatorSpectraMap map) {
		List<Integer> antennas = getAntennas(map);
		List<AntPair> pairs = new ArrayList<AntPair>();

		for (int ant1 : antennas) {
			for (int ant2 : antennas) {
				// skip the incorrectly ordered ant pairs
				if (ant1 <= ant2)
					pairs.add(new AntPair(ant1, ant2));
			}
		}

		return pairs;
	}

	private List<InputPair> getInputPairs(CorrelatorSpectraMap map) {
		List<Integer> inputs = getInputs(map);
		List<InputPair> pairs = new ArrayList<InputPair>();

		for (int input1 : inputs) {
			for (int input2 : inputs) {
				// skip the incorrectly ordered input pairs
				if (input1 <= input2)
					pairs.add(new InputPair(input1, input2));
			}
		}

		return pairs;
	}

	/*------------------------------------------------------------------------*/
	/* Internal Methods Reimplementation                                      */
	/*------------------------------------------------------------------------*/

	private void createRowNames() {
		ArrayList<String> rowNames = new ArrayList<String>();

		// antenna view
		if (dataView_.equals("ant")) {
			for (int ant : antNumbers_) {
				rowNames.add("A" + ant);
			}
		}

		// input view
		if (dataView_.equals("input")) {
			for (int input : inputNumbers_) {
				rowNames.add("I" + input);
			}
		}

		// fallback
		if (rowNames.size() == 0) {
			rowNames.add("NONE");
		}

		rowNames_ = rowNames;
	}

	private void createColNames() {
		ArrayList<String> colNames = new ArrayList<String>();

		// the first column is always row names
		colNames.add("");

		// antenna view
		if (dataView_.equals("ant")) {
			for (int ant : antNumbers_) {
				colNames.add("A" + ant);
			}
		}

		// input view
		if (dataView_.equals("input")) {
			for (int input : inputNumbers_) {
				colNames.add("I" + input);
			}
		}

		// fallback
		if (colNames.size() == 0) {
			colNames.add("NONE");
		}

		colNames_ = colNames;
	}

	/**
	 * Create a key for a specific plot
	 */
	private String createKey(int a, int b, String sbType) {

		// keep the key with lower numbered antennas / inputs first
		if (a > b) {
			int tmp = b;
			b = a;
			a = tmp;
		}

		if (dataView_.equals("ant"))
			return String.format("a-%d-%d-%s", a, b, sbType);

		if (dataView_.equals("input"))
			return String.format("i-%d-%d-%s", a, b, sbType);

		throw new RuntimeException("BUG: unknown view type: " + dataView_);
	}

	/**
	 * Create a key for a specific plot
	 */
	private String createKey(CorrelatorSpectra spectra, String sbType) {
		if (dataView_.equals("ant"))
			return String.format("a-%d-%d-%s", spectra.getAnt1(), spectra.getAnt2(), sbType);

		if (dataView_.equals("input"))
			return String.format("i-%d-%d-%s", spectra.getInput1(), spectra.getInput2(), sbType);

		throw new RuntimeException("BUG: unknown view type: " + dataView_);
	}

	// create all of the amplitude and phase plots
	private void ampPhasePlot(SpectralRecord record) {
		MinMax usbMM = null;
		MinMax lsbMM = null;
		MinMax autoMM = null;
		MinMax bothMM = null;
		MinMax xpautoMM = null;

		// drop all plots so that we don't display old data
		plots_.clear();

		// if we have an input plot, we may need to do cross-pol autocorrelation
		// scaling depending on the data set. Calculate it.
		//
		// Afterwards, we need to remove these baselines from the usb and lsb maps
		// used for scaling the USB and LSB data.
		if (dataView_.equals("input")) {
			CorrelatorSpectraMap xpautoMap = getXPAutoCorrelationMap(record);
			xpautoMM = computeMinMax(xpautoMap, MetricType.AMPLITUDE);

			// copies of the maps: we do not want to alter the underlying record as
			// it will be used later
			CorrelatorSpectraMap usbMap = record.getUSBSpectra();
			CorrelatorSpectraMap lsbMap = record.getLSBSpectra();

			// remove the cross-polarization auto correlations from the copies
			// of the USB and LSB data sets
			for (String key : xpautoMap.keySet()) {
				usbMap.remove(key);
				lsbMap.remove(key);
			}

			// now compute the min and max over the USB and LSB datasets minus
			// the cross-polarization auto correlations
			usbMM = computeMinMax(usbMap, MetricType.AMPLITUDE);
			lsbMM = computeMinMax(lsbMap, MetricType.AMPLITUDE);
		}

		// Antenna view is trivial: just get the USB and LSB data and scale
		// appropriately
		if (dataView_.equals("ant")) {
			CorrelatorSpectraMap usbMap = record.getUSBSpectra();
			CorrelatorSpectraMap lsbMap = record.getLSBSpectra();

			usbMM = computeMinMax(usbMap, MetricType.AMPLITUDE);
			lsbMM = computeMinMax(lsbMap, MetricType.AMPLITUDE);
		}

		{
			// this is so simple we always do it
			float min = Math.min(usbMM.min, lsbMM.min);
			float max = Math.max(usbMM.max, lsbMM.max);
			bothMM = new MinMax(min, max);

			// calculate the AUTO correlation scale
			CorrelatorSpectraMap autoMap = record.getAUTOSpectra();
			autoMM = computeMinMax(autoMap, MetricType.AMPLITUDE);
		}

		for (CorrelatorSpectraMap map : record.getALLSpectra()) {

			// create the plot for each spectra
			for (CorrelatorSpectra spectra : map.values()) {

				MinMax mm = null;
				boolean diagonal = false;
				String plotKey = createKey(spectra, map.getSidebandType());
				XYPlot plot = getPlot(plotKey);

				// choose the correct scale and shading
				if (isXPAutoCorrelation(spectra)) {
					mm = xpautoMM;
					diagonal = true;
				} else if (map.getSidebandType().equals("AUTO")) {
					mm = autoMM;
					diagonal = true;
				} else if (scaleUSBLSBTogether_) {
					mm = bothMM;
				} else if (map.getSidebandType().equals("USB")) {
					mm = usbMM;
				} else if (map.getSidebandType().equals("LSB")) {
					mm = lsbMM;
				} else {
					throw new RuntimeException("BUG: unknown plot type for scaling");
				}

				createSpectraPlot(plot, diagonal, spectra, mm);
			}

		}
	}

	private boolean isXPAutoCorrelation(CorrelatorSpectra spectra) {
		return spectra.antNumbersEqual()
			&& !spectra.polarizationsEqual()
			&& dataView_.equals("input");
	}

	/**
	 * Create a "fake" CorrelatorSpectraMap containing all of the cross-pol
	 * autocorrelations so that we can compute the MinMax of this data set.
	 * This will be used for special scaling in input mode.
	 */
	private CorrelatorSpectraMap getXPAutoCorrelationMap(SpectralRecord record) {
		int band = info_.getBandNumber();
		String pol = info_.getPolarization();
		String sbType = "XPAUTO";
		int frame = 0;

		CorrelatorSpectraMap ret = new CorrelatorSpectraMap(band, pol, sbType, frame);
		for (CorrelatorSpectraMap map : record.getALLSpectra()) {
			for (CorrelatorSpectra spectra : map.values()) {
				if (isXPAutoCorrelation(spectra))
					ret.add(spectra);
			}
		}

		return ret;
	}

	private void createSpectraPlot(XYPlot plot, boolean diagonal, CorrelatorSpectra spectra, MinMax mm) {
		float[] amplitudes = spectra.getAmplitudes();
		float[] phases = spectra.getPhases();

		// if you hit this, your amps and phases came from two different data sources
		if (amplitudes.length != phases.length)
			throw new RuntimeException("BUG: amps and phases have different lengths!");

		// set the plot size limits
		plot.setAxisMinMax(XYPlot.Axis.X_AXIS, 0.0, amplitudes.length);
		plot.setAxisMinMax(XYPlot.Axis.Y_AXIS, 0.0, mm.max);

		// phases must be rescaled to fit in the plots
		phases = rescalePhase(phases, 0.0f, mm.max);

		// diagonal plots have a special background and amplitude color
		if (diagonal) {
			plot.setBackgroundColor(Color.lightGray);
			plot.addDataSeries(amplitudes, Color.white, XYPlot.Marker.MARKER_LINE);
		} else {
			plot.setBackgroundColor(Color.white);
			plot.addDataSeries(amplitudes, Color.red, XYPlot.Marker.MARKER_LINE);
		}

		// all plots have the same display for phases
		plot.addDataSeries(phases, Color.blue, XYPlot.Marker.MARKER_X);

		// if linear fit plot labels are enabled, add them
		if (showPlotLabels_.equals("Linear Fit")) {
			plot.setAxisLabel(XYPlot.Axis.X_AXIS, getLinearFit(phases, mm));
			plot.setDrawAxisLabel(XYPlot.Axis.X_AXIS, true);
		}

		// if minmax labels are enabled, add them
		if (showPlotLabels_.equals("Plot Scale")) {
			final String label = String.format("Y max=%s", mm.max);
			plot.setAxisLabel(XYPlot.Axis.X_AXIS, label);
			plot.setDrawAxisLabel(XYPlot.Axis.X_AXIS, true);
		}

		// if peak amplitude labels are enabled, add them
		if (showPlotLabels_.equals("Peak Amplitude")) {
			final MinMax ampMM = computeMinMax(spectra, MetricType.AMPLITUDE);
			final String label = String.format("Peak Amp=%s", ampMM.max);
			plot.setAxisLabel(XYPlot.Axis.X_AXIS, label);
			plot.setDrawAxisLabel(XYPlot.Axis.X_AXIS, true);
		}
	}

	private String getLinearFit(float[] data, MinMax mm) {
		LinearFit fit = new LinearFit();
		fit.compute(data);

		NumberFormat fmt = NumberFormat.getNumberInstance();
		fmt.setMinimumFractionDigits(2);
		fmt.setMaximumFractionDigits(2);

		final double amp2deg = 360.0 / (mm.max - mm.min);
		final String m = fmt.format(fit.getSlope() * amp2deg);
		final String b = fmt.format(fit.getIntercept() * amp2deg - 180.0);

		return String.format("phase[deg]=%sx + %s", m, b);
	}

	private Icon createBlankIcon() {
		Icon blankIcon = new ImageIcon() {
			private Image blankIconImage;
			private int _w = 0;;
			private int _h = 0;;
			private Color _color = Color.black;
			public void paintIcon(Component c, Graphics g, int x, int y) {
				int w = c.getSize().width;
				int h = c.getSize().height;
				if (blankIconImage == null || _w != w || _h != h) {
					_w = w;
					_h = h;
					blankIconImage = c.createImage(w, h);
					Graphics img = blankIconImage.getGraphics();
					img.setColor(Color.black);
					img.fillRect(0, 0, w, h);
				}
				g.drawImage(blankIconImage, 0, 0, null);
			}
		};
		return blankIcon;
	}
}
