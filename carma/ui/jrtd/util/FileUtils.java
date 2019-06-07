package carma.ui.jrtd.util;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import java.awt.Component;
import java.awt.image.BufferedImage;

import javax.imageio.ImageIO;

import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;

/** 
 * A static class which provides simple saving functions.
 */
public abstract class FileUtils {
    /**
     * Save some data to a file, asking the user where using the JFileChooser
     * widget.
     *
     * @param parent the parent component which triggered this action
     * @param defaultFilename the default filename to offer to the user
     * @param data the data to save
     */
    public static void saveToFile(final Component parent, final String defaultFilename, final String data) {
        final JFileChooser fc = new JFileChooser();
        fc.setSelectedFile(new File(defaultFilename));

        final int ret = fc.showSaveDialog(parent);
        if (ret == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();

            try {
                final FileWriter writer = new FileWriter(file);
                try {
                    writer.append(data);
                } finally {
                    writer.close();
                }
            } catch (IOException ex) {
                System.err.println("IOException");
                ex.printStackTrace();
            }
        }
    }

    /**
     * Save a screenshot of the given component in PNG format.
     *
     * @param c the component to take a screenshot of
     * @param defaultFilename the default filename to use
     */
    public static void saveScreenShot(final Component c, final String defaultFilename) {
        final JFileChooser fc = new JFileChooser();
        fc.setSelectedFile(new File(defaultFilename));

        final int ret = fc.showSaveDialog(c);
        if (ret == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();

            try {
                ImageIO.write(captureImage(c), "png", file);
            } catch (IOException ex) {
                System.err.println("IOException");
                ex.printStackTrace();
            }
        }
    }

    public static BufferedImage captureImage(final Component c) {
        final BufferedImage image = new BufferedImage(
                c.getWidth(),
                c.getHeight(),
                BufferedImage.TYPE_INT_RGB);

        c.paint(image.getGraphics());
        return image;
    }
}
