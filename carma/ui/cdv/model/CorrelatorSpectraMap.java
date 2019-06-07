// $Id: CorrelatorSpectraMap.java,v 1.4 2014/08/26 17:57:23 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import java.util.concurrent.ConcurrentHashMap;
import carma.util.Debug;
import carma.ui.cdv.model.CorrelatorSpectra;

/*
 * A simple class to hold some metadata as well as a list
 * of CorrelatorSpectra objects.
 */
public class CorrelatorSpectraMap extends ConcurrentHashMap<String, CorrelatorSpectra> {
	private final int bandNo;
	private final String pol;
	private final String sbType;
	private final int frame;

	public CorrelatorSpectraMap(int bandNo, String pol, String sbType, int frame) {
		super();

		this.bandNo = bandNo;
		this.pol = pol;
		this.sbType = sbType;
		this.frame = frame;
	}

	// "copy constructor"
	public CorrelatorSpectraMap(CorrelatorSpectraMap map) {
		super(map);

		this.bandNo = map.bandNo;
		this.pol = map.pol;
		this.sbType = map.sbType;
		this.frame = map.frame;
	}

	public String createKey(CorrelatorSpectra spectra) {
		int input1 = spectra.getInput1();
		int input2 = spectra.getInput2();

		if (input1 > input2) {
			String msg = "Recieved correlator spectra where inputs are backwards: ";
			msg += spectra.getInputString() + " " + spectra.getAntString();
			throw new RuntimeException(msg);
		}

		// WARNING: key MUST be unique across the entire set of spectra.
		// WARNING: This is NOT true for antenna numbers!
		return String.format("I%d-I%d", input1, input2);
	}

	// implement the List<CorrelatorSpectra> interface
	public void add(CorrelatorSpectra spectra) {
		// automatically create the key
		this.put(this.createKey(spectra), spectra);
	}

	public int getBandNumber() {
		return this.bandNo;
	}

	public String getPolarization() {
		return this.pol;
	}

	public String getSidebandType() {
		return this.sbType;
	}

	public int getFrameNumber() {
		return this.frame;
	}

	public String toString() {
		return String.format("Band %d %s %s @ %d", bandNo, pol, sbType, frame);
	}
}
