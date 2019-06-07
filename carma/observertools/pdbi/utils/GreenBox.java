package carma.observertools.pdbi.utils;

import java.awt.*;

import javax.swing.JPanel;

/**
 * simple class to display a green box
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class GreenBox extends JPanel{
	protected void paintComponent(Graphics g){
		super.paintComponent(g);
		Graphics2D g2 = (Graphics2D)g;
		// draw box
		g2.setPaint(Color.GREEN);
		g2.fillRect(2,2,8,8);
	}
}
