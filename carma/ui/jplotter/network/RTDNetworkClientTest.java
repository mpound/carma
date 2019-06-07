package carma.ui.jplotter.network;

import java.io.*;
import java.lang.*;
import java.util.*;

class GraphDataPrinter implements GraphDataListener {
	public void graphDataChanged(GraphDataEvent gde) {
		System.out.println("--- Data Frame Received ---");
		for (final String key : gde.getKeys()) {
			final GraphDataEvent.Datum datum = gde.getDatum(key);
			for (final Double value : datum.getData()) {
				System.out.println("PRINTER: " + key + ": " + value);
			}
			System.out.println("PRINTER: " + key + ": VALID=" + datum.getValidity());
		}
	}
}

public final class RTDNetworkClientTest {

	/* ---------------------------------------------------------------------- */
	/* Main Method (test code only)                                           */
	/* ---------------------------------------------------------------------- */

	public static void main(String[] args) throws IOException, InterruptedException {

		final String HOST = "acc.mmarray.org";
		final int PORT = 5668;
		final RTDNetworkClient client = new RTDNetworkClient(HOST, PORT);

		client.addGraphDataListener(new GraphDataPrinter());

		// background thread for the client run task
		final Thread thr = new Thread(client);
		thr.start();

		//final String mp = "array.frame.record";
		final String mp = "Sldc.Band1.Input1.psys";
		client.addRegister(mp);

		Thread.sleep(5000);

		client.addRegister(mp);
		client.remRegister(mp);
		Thread.sleep(5000);

		client.remRegister(mp);
		Thread.sleep(5000);

		final List<String> entries = client.getRegisterList();
		for (int i = 0; i < 20 && i < entries.size(); i++) {
			System.out.println("ENTRY: " + entries.get(i));
		}

		client.shutdown();
		Thread.sleep(500);
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
