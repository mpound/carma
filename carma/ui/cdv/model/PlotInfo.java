// $Id: PlotInfo.java,v 1.4 2012/09/25 18:43:49 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

/**
 * A simple class to hold all of the information needed for a plot.
 *
 * The information contained is:
 * Plot Title   - any string
 * Data Type    - "spec" or "avg" (raw spectra or averaged (continuum))
 * Data View    - "ant" or "input" (antenna or input view)
 * Band Number  - the band number of the data to plot
 * Polarization - the polarization of the data to plot
 * Integrated   - is the plot from an integrated data source
 */

public class PlotInfo {
	private String dataType;
	private String dataView;

	private int bandNumber;
	private String polarization;
	private boolean integrated;

	public PlotInfo() {
		this.setDataType("spec");
		this.setDataView("ant");
		this.setBandNumber(1);
		this.setPolarization("LL");
		this.integrated = false;
	}

	public PlotInfo(String type, String view, int band, String pol, boolean integrated) {
		this.setDataType(type);
		this.setDataView(view);
		this.setBandNumber(band);
		this.setPolarization(pol);
		this.integrated = integrated;
	}

	public void setDataType(String type) {
		if (type.equals("spec") || type.equals("avg")) {
			dataType = type;
			return;
		}

		throw new IllegalArgumentException("invalid data type: " + type);
	}

	public void setDataView(String view) {
		if (view.equals("ant") || view.equals("input")) {
			dataView = view;
			return;
		}

		throw new IllegalArgumentException("invalid data view: " + view);
	}

	public void setBandNumber(int number) {
		if (number <= 0 || number > 40)
			throw new IllegalArgumentException("invalid band number: " + number);

		bandNumber = number;
	}

	public void setPolarization(String pol) {
		final String[] validPols = { "LL", "LR", "RL", "RR", "INPUT", };
		for (String validPol : validPols) {
			if (pol.equals(validPol)) {
				polarization = pol;
				return;
			}
		}

		throw new IllegalArgumentException("invalid polarization: " + pol);
	}

	public String getTitle() {
		final String fancyType;
		if (dataType.equals("spec"))
			fancyType = "Spectra";
		else
			fancyType = "Time";

		final String fancyFreq;
		if (integrated)
			fancyFreq = "Integrated";
		else
			fancyFreq = "Frame";

		return String.format("Band %d %s %s %s Plot",
							 bandNumber,
							 polarization,
							 fancyFreq,
							 fancyType);
	}

	public String getDataType() {
		return dataType;
	}

	public String getDataView() {
		return dataView;
	}

	public int getBandNumber() {
		return bandNumber;
	}

	public String getPolarization() {
		return polarization;
	}

	public boolean getIntegrated() {
		return integrated;
	}

	public String toString() {
		return String.format("PlotInfo: type=%s view=%s band=%d pol=%s",
							 dataType, dataView, bandNumber, polarization);
	}
}
