package carma.ui.jplotter.plotter;

import java.awt.Window;

import java.lang.String;
import java.util.List;

/**
 * An interface for all of the various PlotManager methods.
 */
public interface PlotManager
{
	/**
	 * Save single plot configuration to a file.
	 */
	public void savePlotConfiguration(final Window window, final PlotProperties props);

	/**
	 * Save multi-plot configuration to a file.
	 */
	public void savePlotConfiguration(final Window window, final List<PlotProperties> proplist);

	/**
	 * Save multi-plot configuration for all open plots.
	 */
	public void savePlotConfigurationAll(final Window window);

	/**
	 * Load plot configuration from a file.
	 */
	public List<PlotProperties> loadPlotConfiguration(final Window window);

	/**
	 * Configure a new plot
	 */
	public void configureNewPlot(final Window window, final String type);

	/**
	 * Show the Plot User Interface.
	 */
	public void showPlotUI(final PlotProperties props);

	/**
	 * Get a reference to all open plot frames (and therefore their
	 * corresponding PlotProperties, if needed).
	 */
	public List<GenericPlotFrame> getPlotFrames();

	/**
	 * Get the available register list.
	 */
	public List<String> getRegisterList();

	/**
	 * Start using a register.
	 */
	public void addRegister(final String name);

	/**
	 * Stop using a register.
	 */
	public void remRegister(final String name);
}

// vim: set ts=4 sts=4 sw=4 noet:
