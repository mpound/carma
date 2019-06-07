package carma.ui.jplotter.plotter;

import java.io.*;
import java.util.*;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JFrame;

import java.awt.event.*;

// Image
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;

// JCommon
import org.jfree.ui.about.ProjectInfo;
import org.jfree.ui.about.AboutFrame;

/**
 * General Help Menu for all RTD Plotter windows
 */
public final class HelpMenu extends JMenu
{
	public HelpMenu(final String fileName) {
		super("Help");

		{
			final JMenuItem item = new JMenuItem("Help On This Window");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					// TODO FIXME: implement using fileName ctor parameter
					System.out.println("TODO FIXME: implement window-specific help");
				}
			});

			this.add(item);
		}

		{
			final JMenuItem item = new JMenuItem("General Help");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					System.out.println("TODO FIXME: implement general help");
				}
			});

			this.add(item);
		}

		{
			final JMenuItem item = new JMenuItem("Version Information");
			item.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					showAboutUI();
				}
			});

			this.add(item);
		}
	}

	/**
	 * Show a good-looking about window
	 */
	private void showAboutUI() {
		final String name = "CARMA RTD Plotter";
		final String version = "Beta";
		final String info = "A full-featured plotter for the CARMA Real Time Data Viewer tool";
		BufferedImage image = null;
		try {
			image = ImageIO.read(getClass().getResource("/resources/carma.png"));
		} catch (IOException ex) {
			System.out.println("IOException: " + ex);
		}
		final String copyright = "Copyright 2013 CARMA";
		final String licenseName = "CARMA";
		final String licenseText = "CARMA License Text";

		final ProjectInfo pi = new ProjectInfo(name, version, info, image, copyright, licenseName, licenseText);
		final String title = "About " + name;

		final AboutFrame frame = new AboutFrame(title, pi);
		frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		frame.pack();
		frame.setVisible(true);
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
