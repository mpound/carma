package carma.ui.jplotter.plotter;

import javax.swing.JLabel;

import carma.ui.jplotter.network.GraphDataEvent;
import carma.ui.jplotter.network.GraphDataListener;
import carma.ui.jplotter.util.MigUtils;

/**
 * A JLabel with Icon which can listen and respond to changing network
 * conditions.
 */
public final class NetworkLabel
	extends JLabel
	implements GraphDataListener
{
	private static final String TEXT_DISCONNECTED = "Network: Disconnected";
	private static final String TEXT_CONNECTED = "Network: Connected";

	private static final String ICON_DISCONNECTED = "/resources/gtk-no.png";
	private static final String ICON_CONNECTED = "/resources/gtk-yes.png";

	private boolean connected = false;

	public NetworkLabel() {
		super(TEXT_DISCONNECTED);
		MigUtils.addIcon(this, ICON_DISCONNECTED);
	}

	public void graphDataChanged(GraphDataEvent gde) {
		if (connected && !gde.isNetworkConnected()) {
			connected = false;
			setText(TEXT_DISCONNECTED);
			MigUtils.addIcon(this, ICON_DISCONNECTED);
		}

		if (!connected && gde.isNetworkConnected()) {
			connected = true;
			setText(TEXT_CONNECTED);
			MigUtils.addIcon(this, ICON_CONNECTED);
		}
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
