package carma.ui.jplotter.plotter;

import javax.swing.UIManager;
import javax.swing.SwingUtilities;

/**
 * Main class for the Improved RTD Plotter Demo.
 */
public final class RTDPlotter
{
	public static void main(final String[] args) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {

				// default values
				String server = "acc.mmarray.org";
				int port = 5668;

				// quick and dirty command line parser
				for (final String s : args) {
					final String[] splitString = s.toLowerCase().split("=");
					if (splitString.length != 2) {
						System.err.println("ERROR: non-keyword parameter ('=' character missing): " + s);
						System.exit(1);
					}

					final String keyword = splitString[0];
					final String argument = splitString[1];

					if (keyword.isEmpty()) {
						System.err.println("ERROR: keyword (before '=' character) is the empty string: " + s);
						System.exit(1);
					}

					if (argument.isEmpty()) {
						System.err.println("ERROR: argument (after '=' character) is the empty string: " + s);
						System.exit(1);
					}

					if (keyword.equals("server")) {
						server = argument;
					} else if (keyword.equals("port")) {
						try {
							port = Integer.parseInt(argument);
						} catch (NumberFormatException ex) {
							System.err.println("ERROR: unable to parse argument as integer: " + s);
							System.err.println("NumberFormatException: " + ex);
							System.exit(1);
						}
					} else {
						System.err.println("unknown command line argument found: " + s);
						System.exit(1);
					}
				}

				try {
					UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
				} catch (Exception ex) {
					System.out.println("UIManager: " + ex);
				}

				final PlotManagerFrame frame = new PlotManagerFrame("Plot Manager", server, port);
			}
		});
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
