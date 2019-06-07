// $Id: RawSpectralRecord.java,v 1.1 2011/04/06 18:16:51 iws Exp $
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
 * A class to hold raw spectra for plotting as-is. This just adds each new
 * spectra on to the end of each group of spectra. We don't care about order,
 * we only want to iterate over each spectra in the dataset.
 */
public class RawSpectralRecord extends SpectralRecord {

	public RawSpectralRecord(int band, String pol, int frame) {
		super(band, pol, frame);
	}

	public void addUSBSpectra(CorrelatorSpectra spectra) {
		usbSpectra.add(spectra);
	}

	public void addLSBSpectra(CorrelatorSpectra spectra) {
		lsbSpectra.add(spectra);
	}

	public void addAUTOSpectra(CorrelatorSpectra spectra) {
		autoSpectra.add(spectra);
	}
}
