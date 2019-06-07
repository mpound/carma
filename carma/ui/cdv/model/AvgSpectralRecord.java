// $Id: AvgSpectralRecord.java,v 1.2 2012/01/20 17:13:10 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import java.util.HashMap;
import java.util.ArrayList;

import carma.util.Debug;
import carma.util.Time;

import carma.ui.cdv.model.SpectralRecord;
import carma.ui.cdv.model.CorrelatorSpectra;
import carma.ui.cdv.model.AvgCorrelatorSpectra;
import carma.ui.cdv.model.RawCorrelatorSpectra;
import carma.ui.cdv.model.CorrelatorSpectraMap;

/**
 * A class to hold an entire band worth of spectrum. It averages the data as it
 * is added, rather than just storing the raw spectra. This is used for
 * continuum plots.
 */
public class AvgSpectralRecord extends SpectralRecord {
	private static final int size = 64;

	public AvgSpectralRecord(int band, String pol, int frame) {
		super(band, pol, frame);
	}

	public void addUSBSpectra(CorrelatorSpectra spectra) {
		RawCorrelatorSpectra rawSpectra = (RawCorrelatorSpectra)spectra;
		String key = usbSpectra.createKey(spectra);
		CorrelatorSpectra cachedSpectra = usbSpectra.get(key);
		if (cachedSpectra == null) {
			cachedSpectra = new AvgCorrelatorSpectra(rawSpectra, size);
			usbSpectra.put(key, cachedSpectra);
		}

		// add this raw spectra into the cached values inside the AvgCorrelatorSpectra
		((AvgCorrelatorSpectra)cachedSpectra).addRawSpectra(rawSpectra);
	}

	public void addLSBSpectra(CorrelatorSpectra spectra) {
		RawCorrelatorSpectra rawSpectra = (RawCorrelatorSpectra)spectra;
		String key = lsbSpectra.createKey(spectra);
		CorrelatorSpectra cachedSpectra = lsbSpectra.get(key);
		if (cachedSpectra == null) {
			cachedSpectra = new AvgCorrelatorSpectra(rawSpectra, size);
			lsbSpectra.put(key, cachedSpectra);
		}

		// add this raw spectra into the cached values inside the AvgCorrelatorSpectra
		((AvgCorrelatorSpectra)cachedSpectra).addRawSpectra(rawSpectra);
	}

	public void addAUTOSpectra(CorrelatorSpectra spectra) {
		RawCorrelatorSpectra rawSpectra = (RawCorrelatorSpectra)spectra;
		String key = autoSpectra.createKey(spectra);
		CorrelatorSpectra cachedSpectra = autoSpectra.get(key);
		if (cachedSpectra == null) {
			cachedSpectra = new AvgCorrelatorSpectra(rawSpectra, size);
			autoSpectra.put(key, cachedSpectra);
		}

		// add this raw spectra into the cached values inside the AvgCorrelatorSpectra
		((AvgCorrelatorSpectra)cachedSpectra).addRawSpectra(rawSpectra);
	}
}
