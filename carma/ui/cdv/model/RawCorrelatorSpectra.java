// $Id: RawCorrelatorSpectra.java,v 1.2 2011/04/06 18:18:14 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import java.util.*;
import java.io.*;

import carma.ui.cdv.model.CorrelatorSpectra;

/**
 * A simple class to hold a raw (single frame) correlator spectra.
 *
 * All of the expensive methods on this class take care to cache their results
 * so that the computation only happens once.
 */
public class RawCorrelatorSpectra implements CorrelatorSpectra {

	private final CorrelatorBaseline baseline;
	private final CorrelatorSideband sideband;

	private final int input1;
	private final int input2;
	private final int ant1;
	private final int ant2;

	private float[] amplitudes = null;
	private float[] phases = null;

	private boolean avgcalculated = false;
	private float avgamplitude;
	private float avgphase;

	private String inputString = null;
	private String antennaString = null;

	public RawCorrelatorSpectra(CorrelatorBaseline bl, CorrelatorSideband sb) {
		this.baseline = bl;
		this.sideband = sb;

		final int i1 = bl.getInput1Number();
		final int i2 = bl.getInput2Number();
		if (i1 <= i2) {
			this.input1 = i1;
			this.input2 = i2;
		} else {
			this.input1 = i2;
			this.input2 = i1;
		}

		final int a1 = bl.getAnt1Number();
		final int a2 = bl.getAnt2Number();
		if (a1 <= a2) {
			this.ant1 = a1;
			this.ant2 = a2;
		} else {
			this.ant1 = a2;
			this.ant2 = a1;
		}
	}

	private void calculateAmpsPhases() {
		ComplexFloat[] cf = new ComplexFloat[sideband.getNumberOfChans()];
		sideband.getData().toArray(cf);

		amplitudes = new float[cf.length];
		phases = new float[cf.length];

		for (int i = 0; i < cf.length; i++) {
			if (cf[i] == null) {
				amplitudes[i] = Float.NaN;
				phases[i] = Float.NaN;
				continue;
			}

			amplitudes[i] = cf[i].getMag();
			phases[i] = cf[i].getPhaseInDeg();
		}
	}

	public float[] getAmplitudes() {
		if (amplitudes == null)
			calculateAmpsPhases();

		return amplitudes;
	}

	public float[] getPhases() {
		if (phases == null)
			calculateAmpsPhases();

		return phases;
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
		return this.getAnt1() == this.getAnt2();
	}

	public boolean inputNumbersEqual() {
		return this.getInput1() == this.getInput2();
	}

	public boolean polarizationsEqual() {
		return baseline.getPolarization1() == baseline.getPolarization2();
	}

	public String getInputString() {
		if (inputString == null)
			inputString = String.format("[I%d-I%d]", getInput1(), getInput2());

		return inputString;
	}

	public String getAntString() {
		if (antennaString == null)
			antennaString = String.format("[A%d-A%d]", getAnt1(), getAnt2());

		return antennaString;
	}

	private void calculateAverageAmpPhase() {
		ComplexFloat average = sideband.getStats().getAverage();
		avgamplitude = average.getMag();
		avgphase = average.getPhaseInDeg();
		avgcalculated = true;
	}

	public float getAvgAmplitude() {
		if (!avgcalculated)
			calculateAverageAmpPhase();

		return avgamplitude;
	}

	public float getAvgPhase() {
		if (!avgcalculated)
			calculateAverageAmpPhase();

		return avgphase;
	}
}
