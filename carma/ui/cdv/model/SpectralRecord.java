// $Id: SpectralRecord.java,v 1.2 2014/08/26 17:57:23 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

import carma.util.Debug;
import carma.util.Time;

import carma.ui.cdv.model.CorrelatorSpectra;
import carma.ui.cdv.model.CorrelatorSpectraMap;

/**
 * A class that holds an entire spectral record. This may come in several
 * chunks. Therefore, we provide an interface to allow you to append as much
 * data as necessary to the record.
 *
 * This class is abstract to allow others to override the addXSpectra()
 * methods. This is extremely useful for Raw Spectra vs. Continuum plots. The
 * data presentation is the same to allow for easy plotting, but the underlying
 * storage is quite different.
 */
public abstract class SpectralRecord {
	protected final int band;
	protected final String pol;
	protected final int frame;

	protected CorrelatorSpectraMap usbSpectra;
	protected CorrelatorSpectraMap lsbSpectra;
	protected CorrelatorSpectraMap autoSpectra;

	public SpectralRecord(int band, String pol, int frame) {
		this.band = band;
		this.pol = pol;
		this.frame = frame;

		usbSpectra = new CorrelatorSpectraMap(band, pol, "USB", frame);
		lsbSpectra = new CorrelatorSpectraMap(band, pol, "LSB", frame);
		autoSpectra = new CorrelatorSpectraMap(band, pol, "AUTO", frame);
	}

	public int getBandNumber() {
		return band;
	}

	public String getPolarization() {
		return pol;
	}

	public int getFrameNumber() {
		return frame;
	}

	public CorrelatorSpectraMap getUSBSpectra() {
		final CorrelatorSpectraMap m = new CorrelatorSpectraMap(usbSpectra);
		return m;
	}

	public CorrelatorSpectraMap getLSBSpectra() {
		final CorrelatorSpectraMap m = new CorrelatorSpectraMap(lsbSpectra);
		return m;
	}

	public CorrelatorSpectraMap getAUTOSpectra() {
		final CorrelatorSpectraMap m = new CorrelatorSpectraMap(autoSpectra);
		return m;
	}

	public List<CorrelatorSpectraMap> getALLSpectra() {
		List<CorrelatorSpectraMap> ret = new ArrayList<CorrelatorSpectraMap>();
		ret.add(getUSBSpectra());
		ret.add(getLSBSpectra());
		ret.add(getAUTOSpectra());
		return ret;
	}

	public abstract void addUSBSpectra(CorrelatorSpectra spectra);
	public abstract void addLSBSpectra(CorrelatorSpectra spectra);
	public abstract void addAUTOSpectra(CorrelatorSpectra spectra);
}
