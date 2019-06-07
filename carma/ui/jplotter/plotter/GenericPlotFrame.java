package carma.ui.jplotter.plotter;

import java.util.*;

import javax.swing.*;

import java.awt.Point;
import java.awt.Window;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import carma.ui.jplotter.network.GraphDataEvent;
import carma.ui.jplotter.network.GraphDataListener;

/**
 * JFrame which holds any type of plot.
 */
public final class GenericPlotFrame
	extends JFrame
	implements GraphDataListener
{
	private final PlotProperties props;
	private final AbstractPlotPanel plotPanel;
	private final NetworkLabel networkLabel = new NetworkLabel();

	/* ---------------------------------------------------------------------- */
	/* Public Methods                                                         */
	/* ---------------------------------------------------------------------- */

	public GenericPlotFrame(final PlotProperties props, final PlotManager manager) {
		super(generateTitle(props));
		this.props = props;

		final Window win = this;
		final JMenuBar menuBar = new JMenuBar();
		final JMenu menu = new JMenu("File");

		// TODO FIXME: reconfigure this plot

		{
			final JMenuItem item = new JMenuItem("Save This Plot Only");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					manager.savePlotConfiguration(win, props);
				}
			});
			menu.add(item);
		}

		{
			final JMenuItem item = new JMenuItem("Save All Open Plots");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					manager.savePlotConfigurationAll(win);
				}
			});
			menu.add(item);
		}

		{
			final JMenuItem item = new JMenuItem("Close");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					dispose();
				}
			});
			menu.add(item);
		}

		menuBar.add(menu);

		// Help Menu on far right side of window
		menuBar.add(Box.createHorizontalGlue());

		// Network Connectivity Label
		menuBar.add(networkLabel);
		menuBar.add(Box.createHorizontalStrut(32));

		menuBar.add(new HelpMenu("window-specific.txt"));

		setJMenuBar(menuBar);

		// add the plot panel itself
		if (props.isTimeSeriesPlot()) {
			plotPanel = new TimeSeriesPlotPanel(props, manager);
		} else {
			plotPanel = new XYPlotPanel(props, manager);
		}
		this.add(plotPanel);

		// set saved location on screen
		{
			setLocationByPlatform(true);

			final int x = props.getInt("win_xlocation");
			final int y = props.getInt("win_ylocation");

			setLocation(x, y);
		}

		// set saved size
		{
			final int width = props.getInt("win_width");
			final int height = props.getInt("win_height");

			setPreferredSize(new Dimension(width, height));
		}

		// automatically update location and size properties
		addComponentListener(new ComponentListener() {
			public void componentResized(ComponentEvent e) {
				final Dimension dim = getSize();
				props.setInt("win_width", dim.width);
				props.setInt("win_height", dim.height);
			}

			public void componentMoved(ComponentEvent e) {
				final Point pnt = getLocation();
				props.setInt("win_xlocation", pnt.x);
				props.setInt("win_ylocation", pnt.y);
			}

			public void componentShown(ComponentEvent e) {
				// nothing
			}

			public void componentHidden(ComponentEvent e) {
				// nothing
			}
		});

		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		pack();
		setVisible(true);
	}

	@Override
	public void graphDataChanged(final GraphDataEvent gde) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				networkLabel.graphDataChanged(gde);
				plotPanel.graphDataChanged(gde);
			}
		});
	}

	public AbstractPlotPanel getPlotPanel() {
		return this.plotPanel;
	}

	public PlotProperties getPlotProperties() {
		return this.props;
	}

	/* ---------------------------------------------------------------------- */
	/* Private Methods                                                        */
	/* ---------------------------------------------------------------------- */

	/**
	 * Generate the JFrame (Window) title if none was specified.
	 */
	private static String generateTitle(final PlotProperties props) {
		final String untitled = "Untitled Plot Window";
		final String val = props.getProperty("plot_title");
		if (val == null)
			return untitled;

		if (val.equals(""))
			return untitled;

		return val;
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
