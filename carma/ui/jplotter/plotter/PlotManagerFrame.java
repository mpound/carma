package carma.ui.jplotter.plotter;

import java.io.*;
import java.util.*;

import javax.swing.*;

import java.awt.event.*;
import java.awt.Window;
import java.awt.Dimension;

// MigLayout
import net.miginfocom.swing.MigLayout;
import net.miginfocom.layout.PlatformDefaults;

import carma.ui.jplotter.network.GraphDataEvent;
import carma.ui.jplotter.network.GraphDataListener;
import carma.ui.jplotter.network.RTDNetworkClient;
import carma.ui.jplotter.util.MigUtils;

/**
 * Simple display panel for all open Plot Data Windows.
 */
class PlotManagerPanel extends JPanel
{
	private final DefaultListModel<String> model = new DefaultListModel<String>();

	/* ---------------------------------------------------------------------- */
	/* Public Methods                                                         */
	/* ---------------------------------------------------------------------- */

	public PlotManagerPanel(final PlotManager manager) {
		super(new MigLayout("", "[grow, fill][sg]"));

		{
			final JList<String> list = new JList<String>(this.model);
			list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			list.setLayoutOrientation(JList.VERTICAL);
			list.setVisibleRowCount(16);
			list.setEnabled(false);
			this.add(new JScrollPane(list), "growx, growy");
		}

		{
			final JButton button = new JButton("New Time Series Plot");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					final Window win = SwingUtilities.getWindowAncestor(button);
					manager.configureNewPlot(win, "Time Series");
				}
			});
			MigUtils.addIcon(button, "/resources/gtk-add.png");
			button.setHorizontalAlignment(SwingConstants.LEFT);

			this.add(button, "span, flowy, split 5, sg button");
		}

		{
			final JButton button = new JButton("New X-Y Plot");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					final Window win = SwingUtilities.getWindowAncestor(button);
					manager.configureNewPlot(win, "Arbitrary X-Y Plot");
				}
			});
			MigUtils.addIcon(button, "/resources/gtk-add.png");
			button.setHorizontalAlignment(SwingConstants.LEFT);

			this.add(button, "span 1, sg button");
		}

		{
			final JSeparator sep = new JSeparator();
			sep.setVisible(false);
			this.add(sep, "growy, span 1, sg sep");
		}

		{
			final JButton button = new JButton("Load Saved Plot");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					final Window win = SwingUtilities.getWindowAncestor(button);
					final List<PlotProperties> proplist = manager.loadPlotConfiguration(win);
					for (final PlotProperties props : proplist) {
						manager.showPlotUI(props);
					}
				}
			});
			MigUtils.addIcon(button, "/resources/gtk-open.png");
			button.setHorizontalAlignment(SwingConstants.LEFT);

			this.add(button, "span 1, sg button");
		}

		{
			final JButton button = new JButton("Save All Open Plots");
			button.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					final Window win = SwingUtilities.getWindowAncestor(button);
					manager.savePlotConfigurationAll(win);
				}
			});
			MigUtils.addIcon(button, "/resources/gtk-save.png");
			button.setHorizontalAlignment(SwingConstants.LEFT);

			this.add(button, "span 1, sg button, wrap");
		}
	}

	public void updateWindows(final List<String> list) {
		model.clear();
		for (final String s : list) {
			model.addElement(s);
		}
	}
}

/**
 * Manage all plot configuration windows and plot data windows.
 */
public final class PlotManagerFrame extends JFrame
{
	private final JMenu winmenu = new JMenu("Windows");
	private final RTDNetworkClient client;
	private final NetworkLabel networkLabel = new NetworkLabel();
	private final PlotManager manager;
	private final PlotManagerPanel panel;

	/* ---------------------------------------------------------------------- */
	/* Constructor                                                            */
	/* ---------------------------------------------------------------------- */

	public PlotManagerFrame(String title, String server, int port) {
		super(title);

		// start RTD network client in background thread
		{
			this.client = new RTDNetworkClient(server, port);
			final Thread thr = new Thread(client);
			thr.start();
		}

		// hook up network connectivity label
		client.addGraphDataListener(new GraphDataListener() {
			public void graphDataChanged(final GraphDataEvent gde) {
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						networkLabel.graphDataChanged(gde);
					}
				});
			}
		});

		// create a PlotManager and update window list/menu when plots are
		// added or removed
		this.manager = new PlotManagerBase(this.client) {
			protected void openPlotsChanged() {
				updateWindows();
			}
		};

		this.panel = new PlotManagerPanel(this.manager);

		final JMenuBar menuBar = new JMenuBar();

		// File Menu
		{
			final JMenu menu = new JMenu("File");

			{
				final JMenuItem item = new JMenuItem("Quit");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						System.exit(0);
					}
				});
				menu.add(item);
			}

			menuBar.add(menu);
		}

		// Plot Menu
		{
			final JMenu menu = new JMenu("Plot");
			final Window win = this;

			{
				final JMenuItem item = new JMenuItem("New Time Series Plot");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						manager.configureNewPlot(win, "Time Series");
					}
				});

				menu.add(item);
			}

			{
				final JMenuItem item = new JMenuItem("New X-Y Plot");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						manager.configureNewPlot(win, "Arbitrary X-Y Plot");
					}
				});

				menu.add(item);
			}

			{
				final JMenuItem item = new JMenuItem("Load Saved Plot");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final Window win = SwingUtilities.getWindowAncestor(item);
						final List<PlotProperties> proplist = manager.loadPlotConfiguration(win);
						for (final PlotProperties props : proplist) {
							manager.showPlotUI(props);
						}
					}
				});

				menu.add(item);
			}

			{
				final JMenuItem item = new JMenuItem("Save All Open Plots");
				item.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						final Window win = SwingUtilities.getWindowAncestor(item);
						manager.savePlotConfigurationAll(win);
					}
				});

				menu.add(item);
			}

			menuBar.add(menu);
		}

		menuBar.add(winmenu);
		updateWindows();

		// move all remaining items to far right side of window
		menuBar.add(Box.createHorizontalGlue());

		// network connectivity label
		menuBar.add(networkLabel);
		menuBar.add(Box.createHorizontalStrut(32));

		menuBar.add(new HelpMenu("window-specific.txt"));

		setJMenuBar(menuBar);

		this.add(this.panel);

		setPreferredSize(new Dimension(600, 300));
		setLocationByPlatform(true);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		pack();
		setVisible(true);
	}

	/* ---------------------------------------------------------------------- */
	/* Private Methods                                                        */
	/* ---------------------------------------------------------------------- */

	/**
	 * Update the menu containing the list of windows. This is intended as a
	 * demonstration of how to dynamically generate a menu in the RTD client
	 * so that monitor points can be added to a window in real time.
	 */
	private void updateWindows() {
		this.winmenu.removeAll();

		final List<String> list = new ArrayList<String>();
		for (final GenericPlotFrame frame : this.manager.getPlotFrames()) {
			final PlotProperties props = frame.getPlotProperties();
			list.add(props.getString("plot_title"));
		}

		for (final String s : list) {
			final JMenuItem item = new JMenuItem(s);
			item.setEnabled(false);
			this.winmenu.add(item);
		}

		this.winmenu.setEnabled(list.size() > 0);
		this.panel.updateWindows(list);
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
