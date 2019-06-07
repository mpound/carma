package carma.ui.jplotter.util;

import java.io.IOException;

import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.ImageIcon;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;

import java.net.URL;

import javax.imageio.ImageIO;

import java.awt.Font;
import java.awt.Color;
import java.awt.Image;

public final class MigUtils
{
	public static void addSeparator(final JPanel p, final String text) {
		addSeparator(p, text, 0);
	}

	public static void addSeparator(final JPanel p, final String text, final int gaptop) {
		final JLabel l = new JLabel(text, SwingConstants.LEADING);
		l.setForeground(new Color(0, 70, 213));
		l.setFont(l.getFont().deriveFont(Font.ITALIC | Font.BOLD, l.getFont().getSize() + 4.0f));

		final String gap = "gaptop " + Integer.toString(gaptop) + ", ";

		p.add(l, "flowx, " + gap + "gapbottom 1, span, split 2, aligny center");
		p.add(new JSeparator(), gap + "gapleft rel, growx");
	}

	public static void addBar(final JPanel p) {
		p.add(new JSeparator(), "gaptop 8, gapbottom 8, span, growx");
	}

	/**
	 * Add an Icon from the JAR resource directory into the JButton.
	 *
	 * @param button the JButton to add the icon to
	 * @param name the resource name of the icon
	 */
	public static void addIcon(final JButton button, final String name) {
		try {
			final URL url = MigUtils.class.getResource(name);
			Image img = ImageIO.read(url);
			img = img.getScaledInstance(18, 18, Image.SCALE_DEFAULT);
			final ImageIcon icon = new ImageIcon(img);
			button.setIcon(icon);
		} catch (IOException ex) {
			System.out.println("IOException: " + ex);
		}
	}

	/**
	 * Add an Icon from the JAR resource directory into the JLabel.
	 *
	 * @param label the JLabel to add the icon to
	 * @param name the resource name of the icon
	 */
	public static void addIcon(final JLabel label, final String name) {
		try {
			final URL url = MigUtils.class.getResource(name);
			Image img = ImageIO.read(url);
			img = img.getScaledInstance(18, 18, Image.SCALE_DEFAULT);
			final ImageIcon icon = new ImageIcon(img);
			label.setIcon(icon);
		} catch (IOException ex) {
			System.out.println("IOException: " + ex);
		}
	}
}

// vim: set ts=4 sts=4 sw=4 noet:
