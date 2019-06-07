package carma.ui.jplotter.plotter;

import javax.swing.*;

import java.awt.Paint;
import java.awt.Shape;
import java.awt.Window;
import java.awt.BasicStroke;
import java.awt.BorderLayout;

// JFreeChart
import org.jfree.chart.ChartColor;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.plot.DefaultDrawingSupplier;
import org.jfree.chart.renderer.xy.XYDotRenderer;
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer;
import org.jfree.chart.title.LegendTitle;
import org.jfree.ui.RectangleEdge;

import carma.ui.jplotter.network.GraphDataEvent;
import carma.ui.jplotter.network.GraphDataListener;
import carma.ui.jplotter.jfree.StandardXYItemRendererFast;

/**
 * Abstract parent class for various types of plot panel implementations.
 */
public abstract class AbstractPlotPanel
	extends JPanel
	implements GraphDataListener
{
	public AbstractPlotPanel() {
		super(new BorderLayout());
	}

	@Override
	public abstract void graphDataChanged(GraphDataEvent gde);

	/**
	 * Update the network client when registers in use change.
	 */
	public abstract void updateRegisters();

	/* ---------------------------------------------------------------------- */
	/* Common Plot Configuration Methods                                      */
	/* ---------------------------------------------------------------------- */

	protected final static Paint[] PLOT_COLORS = ChartColor.createDefaultPaintArray();
	protected final static Shape[] PLOT_SHAPES = DefaultDrawingSupplier.createStandardSeriesShapes();

	/**
	 * Set plot marker properties.
	 */
	protected static void setMarker(JFreeChart chart, PlotProperties props) {
		final String marker_type = props.getString("marker_type");
		final String marker_size = props.getString("marker_size");

		final int size;
		if (marker_size.equals("Small"))
			size = 1;
		else if (marker_size.equals("Medium"))
			size = 2;
		else if (marker_size.equals("Large"))
			size = 4;
		else if (marker_size.equals("Ultra"))
			size = 8;
		else
			throw new RuntimeException("unable to parse marker_size");

		final XYPlot plot = chart.getXYPlot();
		if (marker_type.equals("Dot")) {
			final XYDotRenderer renderer = new XYDotRenderer();
			renderer.setDotHeight(size);
			renderer.setDotWidth(size);
			plot.setRenderer(renderer);
		} else if (marker_type.equals("Line")) {
			final StandardXYItemRendererFast renderer = new StandardXYItemRendererFast(StandardXYItemRendererFast.LINES);
			plot.setRenderer(renderer);
			for (int i = 0; i < plot.getSeriesCount(); i++)
				plot.getRenderer().setSeriesStroke(i, new BasicStroke(size));
		} else if (marker_type.equals("Shape")) {
			final StandardXYItemRendererFast renderer = new StandardXYItemRendererFast(StandardXYItemRendererFast.SHAPES);
			plot.setRenderer(renderer);
			for (int i = 0; i < plot.getSeriesCount(); i++) {
				// Setting shape size doesn't work. I'd have to recreate the
				// PLOT_SHAPES array each time through this routine, using
				// different sizes.
				plot.getRenderer().setSeriesStroke(i, new BasicStroke(size));

				final Shape shape = PLOT_SHAPES[i % PLOT_SHAPES.length];
				plot.getRenderer().setSeriesShape(i, shape);
			}
		} else {
			throw new RuntimeException("unable to parse marker_type");
		}

		// the plot colors always start with the same color for the first
		// graphed monitor point
		for (int i = 0; i < plot.getSeriesCount(); i++) {
			final Paint paint = PLOT_COLORS[i % PLOT_COLORS.length];
			plot.getRenderer().setSeriesPaint(i, paint);
		}
	}

	/**
	 * Set plot title.
	 */
	protected static void setPlotTitle(JFreeChart chart, PlotProperties props, Window window) {
		final String text = props.getString("plot_title");

		if (window instanceof JFrame) {
			JFrame frame = (JFrame)window;

			String frameTitle = text;
			if (frameTitle == null || frameTitle.isEmpty())
				frameTitle = "Untitled Plot Window";

			frame.setTitle(frameTitle);
		}

		chart.setTitle(text);
	}

	/**
	 * Set legend location.
	 */
	protected static void setLegendLocation(JFreeChart chart, PlotProperties props) {
		final LegendTitle legend = chart.getLegend();

		RectangleEdge position = RectangleEdge.BOTTOM;
		boolean visible = true;

		final String location = props.getString("legend_location");
		if (location.equals("Top")) {
			position = RectangleEdge.TOP;
		} else if (location.equals("Bottom")) {
			position = RectangleEdge.BOTTOM;
		} else if (location.equals("Left")) {
			position = RectangleEdge.LEFT;
		} else if (location.equals("Right")) {
			position = RectangleEdge.RIGHT;
		} else {
			visible = false;
		}

		legend.setPosition(position);
		legend.setVisible(visible);
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
