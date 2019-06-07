package carma.ui.jplotter.network;

import java.util.*;

/**
 * Provides notification for new graph data.
 */
public final class GraphDataEvent extends EventObject {
	private final Map<String, Datum> map = new HashMap<String, Datum>();
	private final boolean connected;

	public GraphDataEvent(Object source, boolean connected) {
		super(source);
		this.connected = connected;
	}

	public boolean isNetworkConnected() {
		return this.connected;
	}

	public Set<String> getKeys() {
		return this.map.keySet();
	}

	public Datum getDatum(final String key) {
		Datum datum = this.map.get(key);
		if (datum == null) {
			datum = new Datum();
			this.map.put(key, datum);
		}

		return datum;
	}

	/**
	 * Internal class to hold both data and associated validity.
	 */
	public static class Datum {
		private final List<Double> data = new ArrayList<Double>();
		private boolean isValid = false;

		public Datum() {
			// nothing
		}

		public List<Double> getData() {
			return this.data;
		}

		public void addData(final double d) {
			this.data.add(d);
		}

		public void setValidity(final boolean isValid) {
			this.isValid = isValid;
		}

		public boolean getValidity() {
			return this.isValid;
		}
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
