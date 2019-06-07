// $Id: CorrelatorSpectra.java,v 1.3 2011/04/06 18:18:14 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import java.util.*;
import java.io.*;

/**
 * An interface to allow us a consistent method for interfacing with a
 * Correlator Spectra. This will be implemented by both the raw spectra
 * and the averaged spectra.
 *
 * This provides us with a generic way of plotting both raw spectra and
 * continuum plots composed of past raw spectra.
 */
public interface CorrelatorSpectra {

	/**
	 * Get the raw amplitudes or phases of this spectra. This is
	 * what we want to plot on the screen.
	 */
	public float[] getAmplitudes();
	public float[] getPhases();

	/**
	 * Get the antenna numbers of this spectra. This guarantees
	 * that Ant1 <= Ant2, regardless of how the data is
	 * conjugated.
	 */
	public int getAnt1();
	public int getAnt2();

	/**
	 * Get the input numbers of this spectra. This guarantees
	 * that Input1 <= Input2, regardless of how the data is
	 * conjugated.
	 */
	public int getInput1();
	public int getInput2();

	/**
	 * Input and antenna number comparison helpers
	 */
	public boolean inputNumbersEqual();
	public boolean antNumbersEqual();
	public boolean polarizationsEqual();

	/**
	 * Input and antenna number strings. Useful for storing these
	 * objects for quick lookup in a map.
	 */
	public String getInputString();
	public String getAntString();

	/**
	 * Get the averaged amplitude and phase for this
	 * spectra.
	 */
	public float getAvgAmplitude();
	public float getAvgPhase();
}
