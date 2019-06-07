// $Id: AvgCorrelatorSpectra.java,v 1.2 2014/08/26 17:56:04 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import java.util.*;
import java.io.*;

import carma.ui.cdv.model.Cache;
import carma.ui.cdv.model.CorrelatorSpectra;

/**
 * A simple class to hold a raw (single frame) correlator spectra.
 *
 * All of the expensive methods on this class take care to cache their results
 * so that the computation only happens once.
 */
public class AvgCorrelatorSpectra implements CorrelatorSpectra {

	private final int size;

	private final int input1;
	private final int input2;
	private final int ant1;
	private final int ant2;
	private final boolean polsequal;

	// averaged amplitudes and phases
	private final Cache<Float> amplitudes;
	private final Cache<Float> phases;

	private String inputString = null;
	private String antennaString = null;

	public AvgCorrelatorSpectra(RawCorrelatorSpectra raw, int size) {
		this.size = size;

		input1 = raw.getInput1();
		input2 = raw.getInput2();
		ant1 = raw.getAnt1();
		ant2 = raw.getAnt2();
		polsequal = raw.polarizationsEqual();

		amplitudes = new Cache<Float>(size);
		phases = new Cache<Float>(size);

		// prime the caches with zeroes
		for (int i = 0; i < size; i++) {
			amplitudes.add(0.0f);
			phases.add(0.0f);
		}
	}

	/**
	 * Add a new RawCorrelatorSpectra object's as a data point on this
	 * one so that we get one more data point (or replace an old one).
	 */
	public synchronized void addRawSpectra(RawCorrelatorSpectra raw) {
		amplitudes.add(raw.getAvgAmplitude());
		phases.add(raw.getAvgPhase());
	}

	/*------------------------------------------------------------------------*/
	/* CorrelatorSpectra Interface                                            */
	/*------------------------------------------------------------------------*/

	public synchronized float[] getAmplitudes() {
		float[] ret = new float[amplitudes.size()];
		for (int i = 0; i < ret.length; i++)
			ret[i] = amplitudes.get(i);

		return ret;
	}

	public synchronized float[] getPhases() {
		float[] ret = new float[phases.size()];
		for (int i = 0; i < ret.length; i++)
			ret[i] = phases.get(i);

		return ret;
	}

	public int getAnt1() {
		return ant1;
	}

	public int getAnt2() {
		return ant2;
	}

	public int getInput1() {
		return input1;
	}

	public int getInput2() {
		return input2;
	}

	public boolean antNumbersEqual() {
		return ant1 == ant2;
	}

	public boolean inputNumbersEqual() {
		return input1 == input2;
	}

	public boolean polarizationsEqual() {
		return polsequal;
	}

	public String getInputString() {
		if (inputString == null)
			inputString = String.format("[I%d-I%d]", input1, input2);

		return inputString;
	}

	public String getAntString() {
		if (antennaString == null)
			antennaString = String.format("[A%d-A%d]", ant1, ant2);

		return antennaString;
	}

	/*
	 * There is no chance that we'll need these methods for the averaged
	 * object. What is the point of having an average of an average?
	 */

	public float getAvgAmplitude() {
		throw new RuntimeException("BUG: not implemented");
	}

	public float getAvgPhase() {
		throw new RuntimeException("BUG: not implemented");
	}
}
