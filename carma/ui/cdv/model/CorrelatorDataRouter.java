// $Id: CorrelatorDataRouter.java,v 1.3 2012/01/20 17:13:10 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

import org.omg.CosNaming.NameComponent;

import carma.util.Debug;
import carma.util.Time;

import carma.ui.cdv.model.Cache;
import carma.ui.cdv.model.DataSource;
import carma.ui.cdv.model.DataListener;
import carma.ui.cdv.model.CorrelatorSpectra;
import carma.ui.cdv.model.CorrelatorSpectraMap;
import carma.ui.cdv.model.CorrelatorDataCorbaSource;
import carma.ui.cdv.model.SpectralRecord;

public class CorrelatorDataRouter implements DataListener, DataSource {
	private static final String INPUT_POL = "INPUT";
	private final String name_;

	/*
	 * Mapping of sources to listeners
	 *
	 * This will help us automatically stop a data source after all of
	 * the listeners have been closed.
	 *
	 * In addition, when adding a source, we can check each of the existing
	 * sources to see if we already have a connection to the correct IMR
	 * and NameComponent[]. This avoids adding a duplicate source.
	 */
	private HashMap<DataSource, List<DataListener>> dataMap_;

	/*
	 * Storage for spectral records
	 *
	 * Since the correlator can spit the data at us in several separate
	 * records, we always give the previous record to any users. This ensures
	 * that we will never modify the record after another user gets it. Records
	 * are replaced when we get a new frame of correlator data.
	 *
	 * This stores the records by key, which is composed using a
	 * (source, band, pol) tuple.
	 */
	private HashMap<String, SpectralRecord> prevSpectra_;
	private HashMap<String, SpectralRecord> currSpectra_;

	/*
	 * Storage for continuum (averaged) data
	 *
	 * We keep a cache of the averaged data coming into the router. This
	 * provides us with several benefits:
	 *
	 * 1) we keep around a minimal amount of data (yay, memory!)
	 * 2) we can provide continuum plots of past data (to check for fringes)
	 *
	 * The data is always presented to the user as an array of floats, rather
	 * than the raw cache itself. This allows us to make sure that we don't
	 * run into the problem of concurrently adding new data as users iterate
	 * over it.
	 *
	 * This stores the records by key, which is composed using a
	 * (source, band, pol) tuple.
	 */
	private HashMap<String, SpectralRecord> spectraAverages_;

	/*------------------------------------------------------------------------*/
	/* Public Interface                                                       */
	/*------------------------------------------------------------------------*/

	public CorrelatorDataRouter(String name) {
		name_ = name;

		dataMap_ = new HashMap<DataSource, List<DataListener>>();

		prevSpectra_ = new HashMap<String, SpectralRecord>();
		currSpectra_ = new HashMap<String, SpectralRecord>();

		spectraAverages_ = new HashMap<String, SpectralRecord>();
	}

	public static String createKey(String sourceKey, int band, String pol) {
		return String.format("%s-%d-%s", sourceKey, band, pol);
	}

	public SpectralRecord getRawSpectra(String key) {
		return prevSpectra_.get(key);
	}

	public SpectralRecord getAvgSpectra(String key) {
		return spectraAverages_.get(key);
	}

	public synchronized String startCorbaSource(String imr, NameComponent[] nc) {
		final String sourceKey = CorrelatorDataCorbaSource.createKey(imr, nc);
		CorrelatorDataCorbaSource source = null;

		// check to see if we already have this source connected
		source = (CorrelatorDataCorbaSource)findDataSource(sourceKey);
		if (source != null) {
			Debug.print(this, Debug.INFO, "CORBA source already exists");
			return sourceKey;
		}

		// start the source, add it to the map, and add ourselves as a listener
		Debug.print(this, Debug.INFO, "creating new CORBA source");
		source = new CorrelatorDataCorbaSource(imr, nc);
		dataMap_.put(source, new ArrayList<DataListener>());
		source.addDataListener(this, null);
		return sourceKey;
	}

	/*------------------------------------------------------------------------*/
	/* DataSource Interface Methods                                           */
	/*------------------------------------------------------------------------*/

	/**
	 *  Add a DataListener
	 */
	public synchronized void addDataListener(DataListener dl, String key) {
		DataSource source = findDataSource(key);
		if (source == null)
			throw new RuntimeException("CORBA source was never started: " + key);

		// add this listener to the tracking list
		List<DataListener> listeners = dataMap_.get(source);
		listeners.add(dl);
	}

	/**
	 *  Remove a DataListener
	 */
	public synchronized void removeDataListener(DataListener dl, String key) {
		DataSource source = findDataSource(key);
		if (source == null)
			throw new RuntimeException("CORBA source was never started: " + key);

		// remove the listener
		List<DataListener> listeners = dataMap_.get(source);
		listeners.remove(dl);

		// stop the source if there are no more users
		if (listeners.isEmpty()) {
			Debug.print(this, Debug.INFO, "stopping CORBA source: " + key);
			source.removeDataListener(this, null);
			dataMap_.remove(source);

			CorrelatorDataCorbaSource cs = (CorrelatorDataCorbaSource)source;
			cs.stop();
		}
	}

	/**
	 *  Returns the name for this DataSource.
	 */
	public String getName() {
		return name_;
	}

	/*------------------------------------------------------------------------*/
	/* DataListener Interface Methods                                         */
	/*------------------------------------------------------------------------*/

	/**
	 *  Called by a DataSource when its data changes.
	 */
	public void dataChanged(DataEvent event) {

		final CorrelatorDataCorbaSource source = (CorrelatorDataCorbaSource)event.getDataSource();
		final String sourceKey = source.getKey();

		// get data and associated information
		final String sourceName = event.getName();
		Debug.print(this, Debug.INFO, "dataChanged: new record " + sourceName);
		Set<String> keys = new HashSet<String>();

		CorrelatorData data = (CorrelatorData)event.getData();
		int frameNo = Time.computeFrame(data.getHeader().getMJD());

		// loop through the data record and dump it into the caches
		for (CorrelatorBand band : data.getBands()) {

			// skip invalid bands
			if (!band.isValid())
				continue;

			// save the band number
			int bandNo = band.getBandNumber();

			for (CorrelatorBaseline baseline : band.getBaselines()) {

				// save the polarization
				final String pol = baseline.getPolarization();

				// create the key for this spectral record (polarization aware)
				final String polKey = this.createKey(sourceKey, bandNo, pol);
				keys.add(polKey);

				// get the current raw spectra record we are accumulating into
				SpectralRecord rawRecord = this.currSpectra_.get(polKey);
				if (rawRecord == null) {
					rawRecord = new RawSpectralRecord(bandNo, pol, frameNo);
					this.currSpectra_.put(polKey, rawRecord);
				}

				// check to see if we are starting a new record
				if (rawRecord.getFrameNumber() != frameNo) {
					this.prevSpectra_.put(polKey, rawRecord);
					rawRecord = new RawSpectralRecord(bandNo, pol, frameNo);
					this.currSpectra_.put(polKey, rawRecord);
				}

				// get the current avg spectra record we are accumulating into
				SpectralRecord avgRecord = this.spectraAverages_.get(polKey);
				if (avgRecord == null) {
					avgRecord = new AvgSpectralRecord(bandNo, pol, frameNo);
					this.spectraAverages_.put(polKey, avgRecord);
				}

				// create the key for this spectral record (NOT polarization aware)
				String inputKey = this.createKey(sourceKey, bandNo, INPUT_POL);
				keys.add(inputKey);

				// get the current raw spectra record we are accumulating into
				SpectralRecord allRawRecord = this.currSpectra_.get(inputKey);
				if (allRawRecord == null) {
					allRawRecord = new RawSpectralRecord(bandNo, INPUT_POL, frameNo);
					this.currSpectra_.put(inputKey, allRawRecord);
				}

				// check to see if we are starting a new record
				if (allRawRecord.getFrameNumber() != frameNo) {
					this.prevSpectra_.put(inputKey, allRawRecord);
					allRawRecord = new RawSpectralRecord(bandNo, INPUT_POL, frameNo);
					this.currSpectra_.put(inputKey, allRawRecord);
				}

				// get the current avg spectra record we are accumulating into
				SpectralRecord allAvgRecord = this.spectraAverages_.get(inputKey);
				if (allAvgRecord == null) {
					allAvgRecord = new AvgSpectralRecord(bandNo, INPUT_POL, frameNo);
					this.spectraAverages_.put(inputKey, allAvgRecord);
				}

				for (CorrelatorSideband sideband : baseline.getSidebands()) {

					// create the spectra object
					CorrelatorSpectra spectra = new RawCorrelatorSpectra(baseline, sideband);

					/*
					 * This is going to be confusing, so pay attention.
					 *
					 * The correlator produces data which is labeled appropriately for:
					 * - input plots
					 * - LL plots
					 * - RR plots
					 *
					 * However, for LR and RL plots, the data contains both USB and LSB
					 * data sets, but DOES NOT contain an AUTO data set. We arbitrarily
					 * make the choice to plot only the USB data from this data set as
					 * an AUTO correlation.
					 *
					 * Yes, this is highly crazy.
					 */

					if (sideband.isUSB()) {
						if (spectra.antNumbersEqual()) {
							rawRecord.addAUTOSpectra(spectra);
							avgRecord.addAUTOSpectra(spectra);
						} else {
							rawRecord.addUSBSpectra(spectra);
							avgRecord.addUSBSpectra(spectra);
						}

						allRawRecord.addUSBSpectra(spectra);
						allAvgRecord.addUSBSpectra(spectra);
					} else if (sideband.isLSB()) {
						// drop data from polarization-aware data sets
						if (!spectra.antNumbersEqual()) {
							rawRecord.addLSBSpectra(spectra);
							avgRecord.addLSBSpectra(spectra);
						}

						allRawRecord.addLSBSpectra(spectra);
						allAvgRecord.addLSBSpectra(spectra);
					} else if (sideband.isAuto()) {
						rawRecord.addAUTOSpectra(spectra);
						avgRecord.addAUTOSpectra(spectra);
						allRawRecord.addAUTOSpectra(spectra);
						allAvgRecord.addAUTOSpectra(spectra);
					} else {
						throw new RuntimeException("BUG: spectra is not USB, LSB, or AUTO!");
					}
				}
			}
		}

		notifyDataListeners(source, keys);
	}

	/*------------------------------------------------------------------------*/
	/* Private Methods                                                        */
	/*------------------------------------------------------------------------*/

	/**
	 *  Used to notify DataListeners
	 */
	private void notifyDataListeners(DataSource source, Set<String> keys) {
		List<DataListener> listeners = dataMap_.get(source);
		for (DataListener listener : listeners) {
			for (String key : keys) {
				listener.dataChanged(new DataEvent(this, null, key));
			}
		}
	}

	private DataSource findDataSource(String key) {
		for (DataSource _src : dataMap_.keySet()) {
			CorrelatorDataCorbaSource source = (CorrelatorDataCorbaSource)_src;
			if (key.equals(source.getKey()))
				return source;
		}

		// not found
		return null;
	}
}
