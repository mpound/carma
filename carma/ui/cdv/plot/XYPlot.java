// $Id: XYPlot.java,v 1.6 2012/01/20 17:13:12 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.plot;

import java.awt.Color;
import java.awt.Point;
import java.awt.Image;
import java.awt.Graphics;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.BorderLayout;

import javax.swing.Icon;
import javax.swing.JFrame;
import javax.swing.JPanel;

import java.util.List;
import java.util.LinkedList;

class MinMax {
	public double min;
	public double max;

	public MinMax(double min, double max) {
		this.min = min;
		this.max = max;
	}
}

class DataSeries {
	public final float[] data;
	public final Color color;
	public final XYPlot.Marker marker;

	public DataSeries(float[] d, Color c, XYPlot.Marker m) {
		this.data = d;
		this.color = c;
		this.marker = m;
	}
}

/**
 *  Class used to generate XY scatter and line plots
 *
 *  @author   Rick Hobbs
 *  @version  $Revision: 1.6 $, $Date: 2012/01/20 17:13:12 $
 *  @since    JDK1.2
 *
 */
public class XYPlot extends JPanel implements Icon {

	// Axis types
	public enum Axis {
		X_AXIS,
		Y_AXIS,
	};

	// Marker to plot
	public enum Marker {
		MARKER_CIRCLE,
		MARKER_SQUARE,
		MARKER_X,
		MARKER_LINE,
		MARKER_NONE,
	};

	// Axis configuration
	private boolean _drawXaxis = true;
	private boolean _drawYaxis = true;
	private boolean _drawXlabel = false;
	private boolean _drawYlabel = false;
	private MinMax _xMM = new MinMax(0.0, 1.0);
	private MinMax _yMM = new MinMax(0.0, 1.0);
	private String _xLabel = "";
	private String _yLabel = "";

	// Size of the backing image itself
	private int _imagepanelW = 0;
	private int _imagepanelH = 0;

	// Image used to draw on
	public Image _theplot;

	// Padding around the outside of the plot
	private int _padding = 0;

	// Scaling from raw data to graph size
	private double _scalex;
	private double _scaley;

	// Plot Origin on the image
	private int _xorig;
	private int _yorig;

	// default colors
	private Color _fgcolor = Color.white;
	private Color _bgcolor = Color.black;

	// All plottable data series
	private List<DataSeries> _series;

	/*---------------------------------------------------------------------------*/
	/* Public Methods                                                            */
	/*---------------------------------------------------------------------------*/

	/**
	 *  Construct a plot
	 */
	public XYPlot() {
		// Internal JPanel setup
		setLayout(new BorderLayout());
		setOpaque(true);

		_series = new LinkedList<DataSeries>();
	}

	/**
	 *  Set the background color of plot
	 */
	public void setBackgroundColor(Color bg) {
		_bgcolor = bg;
	}

	/**
	 *  Set the foreground color of plot
	 */
	public void setForegroundColor(Color fg) {
		_fgcolor = fg;
	}

	public void setPadding(int p) {
		_padding = p;
	}

	public int getPadding() {
		return _padding;
	}

	/**
	 *  Sets the min and max for the X axis
	 */
	public void setAxisMinMax(Axis a, double min, double max) {
		switch (a) {
		case X_AXIS:
			_xMM = new MinMax(min, max);
			break;
		case Y_AXIS:
			_yMM = new MinMax(min, max);
			break;
		}
	}

	public void setAxisLabel(Axis a, String label) {
		switch (a) {
		case X_AXIS:
			_xLabel = label;
			break;
		case Y_AXIS:
			_yLabel = label;
			break;
		}
	}

	public void setDrawAxisLabel(Axis a, boolean enabled) {
		switch (a) {
		case X_AXIS:
			_drawXlabel = enabled;
			break;
		case Y_AXIS:
			_drawYlabel = enabled;
			break;
		}
	}

	public void setDrawAxis(Axis a, boolean enabled) {
		switch (a) {
		case X_AXIS:
			_drawXaxis = enabled;
			break;
		case Y_AXIS:
			_drawYaxis = enabled;
			break;
		}
	}

	public Dimension getImageSize() {
		return new Dimension(_imagepanelW, _imagepanelH);
	}

	/**
	 *  Clears the plot, drops all data series
	 */
	public void clearPlot() {
		_series.clear();

		if (_theplot != null) {
			drawBackgroundTheme(_theplot.getGraphics());
		}
	}

	public void addDataSeries(float[] d, Color c, Marker m) {
		DataSeries s = new DataSeries(d, c, m);
		_series.add(s);
	}

	/*---------------------------------------------------------------------------*/
	/* JPanel Overrides                                                          */
	/*---------------------------------------------------------------------------*/

	public void paintComponent(Graphics g) {
		// call the higher level paintComponent()
		super.paintComponent(g);

		replot(this);
		g.drawImage(_theplot, 0, 0, null);
	}

	/*---------------------------------------------------------------------------*/
	/* Icon Interface                                                            */
	/*---------------------------------------------------------------------------*/

	public int getIconHeight() {
		return _imagepanelH;
	}

	public int getIconWidth() {
		return _imagepanelW;
	}

	public void paintIcon (Component c, Graphics g, int x, int y) {
		replot(c);
		g.drawImage(_theplot, 0, 0, null);
	}

	/*---------------------------------------------------------------------------*/
	/* Private Methods                                                           */
	/*---------------------------------------------------------------------------*/

	/**
	 * replots all internal data
	 */
	private void replot(Component c) {

		final Dimension size = c.getSize();
		_imagepanelW = size.width;
		_imagepanelH = size.height;

		_theplot = c.createImage(size.width, size.height);
		Graphics g = _theplot.getGraphics();

		// draw the background
		drawBackgroundTheme(g);

		// draw the plot, minus the data
		drawPlot(c, g);

		// plot each data series
		for (DataSeries series : _series) {
			final Color color = series.color;
			final float[] data = series.data;

			g.setColor(color);
			drawMarker(g, series, 2);
		}
	}

	private void drawPlot(Component c, Graphics g) {
		final int padding = _padding;

		// calculate the image height (the whole thing)
		final int imageWidth = c.getSize().width;
		final int imageHeight = c.getSize().height;

		// calculate the plot width and height (just the plot inside the axes)
		final int yAxisWidth = this.getYAxisWidth(g);
		final int plotWidth = Math.max(0, imageWidth - ((padding * 2) + yAxisWidth));

		final int xAxisHeight = this.getXAxisHeight(g);
		final int plotHeight = Math.max(0, imageHeight - ((padding * 2) + xAxisHeight));

		// calculate the X and Y origin (BOTTOM LEFT)
		final int xOrigin = padding + yAxisWidth;
		final int yOrigin = imageHeight - (padding + xAxisHeight);

		// save internal state for point2grid()
		_xorig = xOrigin;
		_yorig = yOrigin;
		_scalex = plotWidth / (_xMM.max - _xMM.min);
		_scaley = plotHeight / (_yMM.max - _yMM.min);

		// save internal state for drawBackgroundTheme()
		_imagepanelH = imageHeight;
		_imagepanelW = imageWidth;

		// draw the X axis
		if (_drawXaxis) {
			g.setColor(Color.black);
			final int x1 = xOrigin - 1;
			final int y1 = yOrigin;
			final int x2 = x1 + plotWidth;
			final int y2 = yOrigin;
			g.drawLine(x1, y1, x2, y2);
		}

		// draw the Y axis
		if (_drawYaxis) {
			g.setColor(Color.black);
			final int x1 = xOrigin - 1;
			final int y1 = yOrigin;
			final int x2 = x1;
			final int y2 = yOrigin - plotHeight;
			g.drawLine(x1, y1, x2, y2);
		}

		// draw the X axis label
		if (_drawXlabel) {
			g.setColor(Color.black);

			final FontMetrics m = g.getFontMetrics();
			final int fontHeight = m.getHeight();

			final int x = xOrigin;
			final int y = yOrigin + fontHeight;
			g.drawString(_xLabel, x, y);
		}

		// draw a box around the image
		if (false) {
			g.setColor(Color.green);

			int x1, x2, y1, y2;

			// top
			x1 = padding;
			y1 = padding;
			x2 = x1 + plotWidth;
			y2 = y1;
			g.drawLine(x1, y1, x2, y2);

			// left
			x1 = padding;
			y1 = padding;
			x2 = x1;
			y2 = y1 + plotHeight;
			g.drawLine(x1, y1, x2, y2);

			// bottom
			x1 = padding;
			y1 = padding + plotHeight;
			x2 = x1 + plotWidth;
			y2 = y1;
			g.drawLine(x1, y1, x2, y2);

			// right
			x1 = padding + plotWidth;
			y1 = padding;
			x2 = x1;
			y2 = y1 + plotHeight;
			g.drawLine(x1, y1, x2, y2);
		}
	}

	private int getXAxisHeight(Graphics g) {
		int height = 0;

		// X axis is a 1px line
		if (_drawXaxis) {
			height += 1;
		}

		// X label size depends on font
		if (_drawXlabel) {
			final FontMetrics m = g.getFontMetrics();
			height += m.getHeight();
		}

		return height;
	}

	private int getYAxisWidth(Graphics g) {
		int width = 0;

		// Y axis is a 1px line
		if (_drawYaxis) {
			width += 1;
		}

		// Y label is not supported
		if (_drawYlabel) {
			throw new RuntimeException("BUG: not able to draw Y axis label yet");
		}

		return width;
	}

	private void drawBackgroundTheme(Graphics g) {
		if (isOpaque()) {
			g.setColor(_bgcolor);
			g.fillRect(0, 0, _imagepanelW, _imagepanelH);
		}
	}

	private Point[] point2grid(float[] data) {
		Point[] pts = new Point[data.length];

		/*
		 * TODO FIXME:
		 *
		 * After profiling, if this loop is too slow due to function call
		 * overhead, then you can inline the point2grid(double, double) function
		 * into the loop below. I hope the JVM is smart enough to do this.
		 */
		for (int i = 0; i < pts.length; i++) {
			pts[i] = point2grid(i, data[i]);
		}

		return pts;
	}

	private Point point2grid(double xpt, double ypt) {
		// check input validity
		if (Double.isNaN(xpt) || Double.isNaN(ypt))
			return null;

		// check X axis limits
		if (xpt < _xMM.min || xpt > _xMM.max)
			return null;

		// check Y axis limits
		if (ypt < _yMM.min || ypt > _yMM.max)
			return null;

		final int ix = (int)(_xorig + (_scalex * (xpt - _xMM.min)));
		final int iy = (int)(_yorig - (_scaley * (ypt - _yMM.min)));
		return new Point(ix, iy);
	}

	private void drawMarker(Graphics g, DataSeries series, int dia) {
		final float[] ypt = series.data;
		final Color color = series.color;
		final Marker marker = series.marker;

		int w = dia / 2;
		Point[] gridpts = point2grid(ypt);
		Color tmp = g.getColor();
		g.setColor(color);
		if (gridpts != null) {
			switch (marker) {
			case MARKER_CIRCLE:
				for (int i = 0; i < gridpts.length; i++) {
					if (gridpts[i] != null)
						g.drawOval((int)gridpts[i].getX() - w,
								   (int)gridpts[i].getY() - w, dia, dia);
				}
				break;
			case MARKER_SQUARE:
				for (int i = 0; i < gridpts.length; i++) {
					if (gridpts[i] != null)
						g.drawRect((int)gridpts[i].getX() - w,
								   (int)gridpts[i].getY() - w, dia, dia);
				}
				break;
			case MARKER_X:
				for (int i = 0; i < gridpts.length; i++) {
					if (gridpts[i] != null) {
						int x = (int)gridpts[i].getX();
						int y = (int)gridpts[i].getY();
						g.drawLine(x - w, y - w, x + w, y + w);
						g.drawLine(x + w, y - w, x - w, y + w);
					}
				}
				break;
			case MARKER_LINE:
				for (int i = 0; i < gridpts.length - 1; i++) {
					final Point pt1 = gridpts[i];
					final Point pt2 = gridpts[i + 1];
					if (pt1 == null || pt2 == null)
						continue;

					g.drawLine((int)pt1.getX(), (int)pt1.getY(),
							   (int)pt2.getX(), (int)pt2.getY());
				}
				break;
			case MARKER_NONE:
				break;
			}
		}

		g.setColor(tmp);
	}

	/*---------------------------------------------------------------------------*/
	/* Test Program                                                              */
	/*---------------------------------------------------------------------------*/

	public static void main(String[] args) {

		// amplitude & phase data
		float[] amp = new float[6];
		float[] pha = new float[6];

		amp[0] = 1.0f;
		amp[1] = 2.0f;
		amp[2] = 3.0f;
		amp[3] = 4.0f;
		amp[4] = 5.0f;
		amp[5] = 5.0f;

		pha[0] = 0.0f;
		pha[1] = 1.0f;
		pha[2] = 2.0f;
		pha[3] = 3.0f;
		pha[4] = 4.0f;
		pha[5] = 5.0f;

		// test plot which mimics CDV data
		XYPlot plot = new XYPlot();
		plot.setAxisMinMax(Axis.X_AXIS, 0.0, 64.0);
		plot.setAxisMinMax(Axis.Y_AXIS, -180.0, 180.0);
		plot.setDrawAxis(Axis.X_AXIS, true);
		plot.setDrawAxis(Axis.Y_AXIS, true);
		plot.setDrawAxisLabel(Axis.X_AXIS, false);
		plot.setDrawAxisLabel(Axis.Y_AXIS, false);
		plot.setSize(200, 100);

		plot.setAxisMinMax(Axis.X_AXIS, 0.0, 5.0);
		plot.setAxisMinMax(Axis.Y_AXIS, 0.0, 5.0);
		plot.setBackgroundColor(Color.white);
		plot.setPadding(5);

		// add the data
		plot.addDataSeries(amp, Color.red, XYPlot.Marker.MARKER_LINE);
		plot.addDataSeries(pha, Color.blue, XYPlot.Marker.MARKER_X);

		// frame to hold the plot
		JFrame frame = new JFrame();
		frame.setSize(200,600);
		frame.getContentPane().add(plot, BorderLayout.CENTER);
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setVisible(true);
	}
}
