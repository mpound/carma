package carma.ui.jplotter.plotter;

import carma.ui.jplotter.dialog.ConfigurationDialogAction;
import carma.ui.jplotter.dialog.MonitorDialog;
import carma.ui.jplotter.network.RTDNetworkClient;

import java.io.File;
import java.io.IOException;
import java.io.FileOutputStream;
import java.io.ByteArrayOutputStream;

import java.lang.String;

import java.awt.Window;
import java.awt.Component;
import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;

import java.util.*;
import java.util.zip.ZipFile;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import java.util.zip.ZipException;

import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.filechooser.FileNameExtensionFilter;

/**
 * A simple implementation of the PlotManager interface. It should be used
 * internally by any object implementing the PlotManager interface with simple
 * passthroughs for most things.
 */
public abstract class PlotManagerBase implements PlotManager, ChangeListener
{
	private final RTDNetworkClient client;
	private final List<GenericPlotFrame> openFrames = new ArrayList<GenericPlotFrame>();

	/**
	 * Constructor.
	 *
	 * @param client the RTD network client
	 */
	public PlotManagerBase(final RTDNetworkClient client) {
		this.client = client;
	}

	/**
	 * A method to be implemented in classes which use this. It will be called
	 * when the list of open plots changes (a plot is added or removed).
	 */
	protected abstract void openPlotsChanged();

	/* ---------------------------------------------------------------------- */
	/* PlotManager Interface                                                  */
	/* ---------------------------------------------------------------------- */

	/**
	 * Load a saved PlotProperties configuration file using a GUI file chooser.
	 */
	@Override
	public List<PlotProperties> loadPlotConfiguration(final Window window) {
		final List<PlotProperties> proplist = new ArrayList<PlotProperties>();

		final JFileChooser fc = new JFileChooser();
		final FileNameExtensionFilter filter = new FileNameExtensionFilter("RTD Multi-Plot Format (.plot)", "plot");
		fc.setFileFilter(filter);

		final int ret = fc.showOpenDialog(window);
		if (ret == JFileChooser.APPROVE_OPTION) {
			final File file = fc.getSelectedFile();
			try {
				final ZipFile zipFile = new ZipFile(file);
				final Enumeration<?> enu = zipFile.entries();
				while (enu.hasMoreElements()) {
					final ZipEntry zipEntry = (ZipEntry)enu.nextElement();
					try {
						final PlotProperties props = new PlotProperties();
						props.loadFromXML(zipFile.getInputStream(zipEntry));
						if (!props.validate()) {
							System.err.println("Invalid properties ignored");
							continue;
						}

						proplist.add(props);
					} catch (IOException ex) {
						System.err.println("IOException: " + ex);
					}
				}
			} catch (ZipException ex) {
				System.err.println("ZipException: " + ex);
			} catch (IOException ex) {
				System.err.println("IOException: " + ex);
			}
		}

		return proplist;
	}

	@Override
	public void savePlotConfiguration(final Window window, final PlotProperties props) {
		final List<PlotProperties> proplist = new ArrayList<PlotProperties>();
		proplist.add(props);
		savePlotConfiguration(window, proplist);
	}

	/**
	 * Save a PlotProperties configuration file using a GUI file chooser.
	 *
	 * @param props the PlotProperties to save
	 */
	@Override
	public void savePlotConfiguration(final Window window, final List<PlotProperties> proplist) {
		final JFileChooser fc = new JFileChooser();
		final FileNameExtensionFilter filter = new FileNameExtensionFilter("RTD Multi-Plot Format (.plot)", "plot");
		fc.setFileFilter(filter);

		// nothing to save, return now
		if (proplist.isEmpty())
			return;

		// set the default filename in the dialog to match the first plot's title
		{
			final String title = proplist.get(0).getString("plot_title");
			if (!title.trim().equals("")) {
				fc.setSelectedFile(new File(title + ".plot"));
			}
		}

		final int ret = fc.showSaveDialog(window);
		if (ret == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();

			// automatically add ".plot" extension
			if (!file.getAbsolutePath().endsWith(".plot")) {
				file = new File(file.getAbsolutePath() + ".plot");
			}

			try {
				final Date date = new Date();
				final ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(file));
				try {
					for (int i = 0; i < proplist.size(); i++) {
						try {
							final String filename = "Plot" + i + ".plot";
							final ZipEntry ze = new ZipEntry(filename);
							try {
								zos.putNextEntry(ze);

								final ByteArrayOutputStream out = new ByteArrayOutputStream();
								final PlotProperties props = proplist.get(i);
								props.storeToXML(out, date.toString());

								final byte[] buf = out.toByteArray();
								zos.write(buf, 0, buf.length);
							} finally {
								zos.closeEntry();
							}
						} catch (ZipException ex) {
							System.err.println("ZipException: " + ex);
						} catch (IOException ex) {
							System.err.println("IOException: " + ex);
						}
					}
				} finally {
					zos.close();
				}
			} catch (IOException ex) {
				System.out.println("IOException: " + ex);
			}
		}
	}

	@Override
	public void savePlotConfigurationAll(final Window window) {
		final List<PlotProperties> list = new ArrayList<PlotProperties>();
		for (final GenericPlotFrame frame : this.openFrames) {
			list.add(frame.getPlotProperties());
		}

		savePlotConfiguration(window, list);
	}

	@Override
	public void configureNewPlot(final Window window, final String type) {
		final PlotManager manager = this;
		final PlotProperties props = new PlotProperties(type);
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				final MonitorDialog dialog = new MonitorDialog(window, props, manager);
				final ConfigurationDialogAction action = dialog.getValue();
				if (action == ConfigurationDialogAction.OK) {
					showPlotUI(props);
				}
			}
		});
	}

	/**
	 * Show the Plot Data Window UI using a saved set of properties.
	 */
	@Override
	public void showPlotUI(final PlotProperties props) {
		final PlotManager manager = this;
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				final GenericPlotFrame gpf = new GenericPlotFrame(props, manager);
				addWindowCloseListener(gpf, props);
				addWindow(gpf, props);
			}
		});
	}

	/**
	 * Get a reference to all open plot frames (and therefore their
	 * corresponding PlotProperties, if needed).
	 */
	@Override
	public List<GenericPlotFrame> getPlotFrames() {
		return this.openFrames;
	}

	/**
	 * Get the available register list
	 */
	@Override
	public List<String> getRegisterList() {
		return client.getRegisterList();
	}

	@Override
	public void addRegister(final String name) {
		client.addRegister(name);
	}

	@Override
	public void remRegister(final String name) {
		client.remRegister(name);
	}

	/* ---------------------------------------------------------------------- */
	/* ChangeListener Interface                                               */
	/* ---------------------------------------------------------------------- */

	@Override
	public void stateChanged(ChangeEvent e) {
		// this catches the case where the plot title changed and we want to
		// update all menus which display the plot title
		this.openPlotsChanged();
	}

	/* ---------------------------------------------------------------------- */
	/* Private Methods                                                        */
	/* ---------------------------------------------------------------------- */

	private void addWindow(final GenericPlotFrame frame, final PlotProperties props) {
		this.openFrames.add(frame);

		// add listener for PlotProperties changes
		props.addChangeListener(this);

		// add registers to the network client
		client.addGraphDataListener(frame);

		// notify users that the list of open plots has changed
		this.openPlotsChanged();
	}

	private void removeWindow(final GenericPlotFrame frame, final PlotProperties props) {
		this.openFrames.remove(frame);

		// remove listener for PlotProperties changes
		props.removeChangeListener(this);

		// remove registers from the network client
		client.removeGraphDataListener(frame);

		// notify users that the list of open plots has changed
		this.openPlotsChanged();
	}

	/**
	 * Add notifications which are triggered when a JFrame is closed. This
	 * allows the manager to correctly and automatically perform cleanup actions
	 * when a plot window is closed.
	 */
	private void addWindowCloseListener(final GenericPlotFrame frame, final PlotProperties props) {
		frame.addWindowListener(new WindowAdapter() {
			public void windowClosed(WindowEvent e) {
				super.windowClosed(e);
				removeWindow(frame, props);
			}
		});
	}
}
