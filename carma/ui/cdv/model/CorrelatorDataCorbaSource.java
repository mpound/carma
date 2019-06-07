// $Id: CorrelatorDataCorbaSource.java,v 1.22 2013/01/31 21:59:00 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.model;

import org.omg.CORBA.*;
import org.omg.CosEventChannelAdmin.*;
import org.omg.CosNaming.*;
import org.omg.CosEventComm.*;
import org.omg.PortableServer.*;
import org.omg.PortableServer.POAPackage.*;
import java.util.*;
import java.net.*;
import java.nio.ByteBuffer;
import javax.swing.*;
import carma.correlator.obsRecord2.*;
import carma.ui.cdv.util.*;

import carma.util.CorbaUtils;
import carma.util.NotificationConsumer;
import carma.util.Debug;

/**
 *  Class that wraps an Corba ObsRecord interface and subsequent
 *  data structures.
 *
 *  @author Rick Hobbs
 *  @version $Revision: 1.22 $, $Data:$
 *  @since JDK1.3
 */
public class CorrelatorDataCorbaSource implements DataSource {
	private final String imr_;
	private final NameComponent[] nc_;
	private final String key_;

	private ORB orb_;
	private ArrayList<DataListener> listeners_;
	private NotificationConsumer consumer_;

	/*------------------------------------------------------------------------*/
	/* Public Interface                                                       */
	/*------------------------------------------------------------------------*/

	public CorrelatorDataCorbaSource(String imr, NameComponent[] nc) {
		imr_ = imr;
		nc_ = nc;
		key_ = createKey(imr, nc);

		orb_ = CorbaUtils.getNewORB(null, null, imr_);
		listeners_ = new ArrayList<DataListener>();

		connectToChannel(nc);
		Debug.print(this, Debug.INFO, "ctor: finished connecting to data channel");

		// run data receiver in background thread
		Thread thr = new Thread(consumer_);
		thr.start();
	}

	public static String createKey(String imr, NameComponent[] nc) {
		return String.format("%s-%s", imr, createChannelName(nc));
	}

	/**
	 *  Stop the thread and disconnect from DO
	 */
	public void stop() {
		Debug.print(this, Debug.INFO, "stop(): disconnect push consumer and stopping thread");
		consumer_.disconnect_structured_push_consumer();
	}

	public String getKey() {
		return key_;
	}

	/**
	 *  Returns the name assigend to this object
	 */
	public String getName() {
		return key_;
	}

	/*------------------------------------------------------------------------*/
	/* DataSource Interface                                                   */
	/*------------------------------------------------------------------------*/

	public void addDataListener(DataListener l, String key) {
		listeners_.add(l);
	}

	public void removeDataListener(DataListener l, String key) {
		listeners_.remove(l);
	}

	public void notifyListeners(CorrelatorData data) {
		for (DataListener l : listeners_)
			l.dataChanged(new DataEvent(this, data, key_));
	}

	/*------------------------------------------------------------------------*/
	/* Private Methods                                                        */
	/*------------------------------------------------------------------------*/

	private static String createChannelName(NameComponent[] nc) {
		String name = "";
		for (NameComponent component : nc)
			name += component.id.toString() + ".";

		// trim the trailing dot and return
		return name.substring(0, name.length() - 1);
	}

	/**
	 *  for event channel
	 */
	private void connectToChannel(NameComponent[] nc) {
		try {
			final String channelName = createChannelName(nc);
			Debug.print(this, Debug.INFO, "connectToChannel: channelName=" + channelName);
			consumer_ = new NotificationConsumer(orb_, channelName) {
				public void push_structured_event(org.omg.CosNotification.StructuredEvent event) {
					org.omg.CosNotification.Property data = event.filterable_data[0];
					try {
						byte[] serializedData = CorData_sHelper.extract(data.value).correlatorData;
						ByteBuffer bb = ByteBuffer.wrap(serializedData);
						CorrelatorData corData = new CorrelatorData();
						corData.deserial(bb);

						//checkData(corData);

						Debug.print(this, Debug.INFO, "MJD=" + corData.getHeader().getMJD());
						notifyListeners(corData);
					} catch (org.omg.CORBA.BAD_OPERATION err) {
						System.err.println("connectToChannel: " + err);
					}
				}
			};
			Debug.print(this, Debug.INFO, "connectToChannel: about to run Consumer");
		} catch (org.omg.CORBA.TRANSIENT err) {
			System.err.println("Error getting channel name");
			String msg = "";

			msg += "Error getting channel name.\n";
			msg += "DO not running.\n";

			JOptionPane.showMessageDialog(null, msg, "connectToChannel", JOptionPane.ERROR_MESSAGE);
			err.printStackTrace();
		} catch (org.omg.PortableServer.POAPackage.AdapterAlreadyExists err) {
			System.err.println("connectToChannel " + err);
			err.printStackTrace();
		} catch (org.omg.PortableServer.POAPackage.InvalidPolicy err) {
			System.err.println("connectToChannel " + err);
			err.printStackTrace();
		} catch (org.omg.CORBA.ORBPackage.InvalidName err) {
			System.err.println("connectToChannel " + err);
			err.printStackTrace();
		}

		Debug.print(this, Debug.INFO, "connectToChannel: running");
	}

	private void checkData(CorrelatorData corData) {
		ArrayList<CorrelatorBaseline> cbb = corData.getBands().get(0).getBaselines();
		for (int blIdx = 0; blIdx < cbb.size(); ++blIdx) {
			Debug.print(this, Debug.INFO,
						"[" + cbb.get(blIdx).getInput1Number() + "-" +
						cbb.get(blIdx).getInput2Number() + "]");
			ArrayList<CorrelatorSideband> sbs = cbb.get(blIdx).getSidebands();
			boolean allGood = true;
			for (int sbIdx = 0; sbIdx < sbs.size(); ++sbIdx) {
				ArrayList<ComplexFloat> data = sbs.get(sbIdx).getData();
				for (int dIdx = 0; dIdx < data.size(); ++dIdx) {
					if (data.get(dIdx) == null) {
						allGood = false;
					} else {
						Debug.print(this, Debug.INFO, "cf[" + dIdx + "]=" + data.get(dIdx));
					}
				}
			}
			Debug.print(this, Debug.INFO, "checkData(): allGood= " + allGood);
		}
	}

}
