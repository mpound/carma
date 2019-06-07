package carma.observertools.pdbi.panes;

import javax.swing.event.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.*;

import javax.swing.*;

import carma.observertools.pdbi.messages.*;
import carma.observertools.pdbi.utils.*;

// Class to display the schedPlot image
@SuppressWarnings("serial")
public class SchedPlot extends JPanel{
    public SchedPlot(){
	setLayout(new GridBagLayout());
	mainPanel = new JPanel();
	
	go = new JButton("Run");
	go.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent event){
		    String command = "xterm +hold -e /home/szadaq/cron/schedulePlot.sh";
		    try {
			Process schedPlot = Runtime.getRuntime().exec(command);
			schedPlot.waitFor();
			refreshPane();
		    } catch (IOException e) {
			new ExceptionHandler(e.toString(),false);
		    } catch (InterruptedException e) {
			new ExceptionHandler("Interrupt triggered", false);
		    }
		}
	    });
	mainPanel.add(go, new GBC(0,0).setAnchor(GBC.WEST).setWeight(100,100));
	imagePane = new ImagePane();
	mainPanel.add(imagePane, new GBC(0,1).setFill(GBC.BOTH).setWeight(100,100));
	mainPanel.setPreferredSize(new Dimension(Globals.WIDTH-20,Globals.HEIGHT-100));
	mainPanel.setMinimumSize(new Dimension(Globals.WIDTH-20,Globals.HEIGHT-100));
	mainPanel.setMaximumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT));
	mainPanel.revalidate();
	add(mainPanel);
    }
    
    public static void refreshPane(){
	count = count + 1;
	mainPanel.remove(1);
	imagePane = new ImagePane();
	mainPanel.add(imagePane, new GBC(1,0).setFill(GBC.BOTH).setWeight(100,100));
	mainPanel.revalidate();
	mainPanel.repaint();
    }
    
    private static int count = 0;
    private static ImagePane imagePane = null;
    private static JPanel mainPanel = null;
    private JButton go = null;
}

@SuppressWarnings("serial")
class ImagePane extends JPanel{
    ImagePane(){
	JLabel label = new JLabel();
	ImageIcon imageIcon = new ImageIcon("/home/obs/web_pages/schedule_plot/schedule.png");
	// force it to refresh otherwise the original image will stay cached
	imageIcon.getImage().flush();
	label.setIcon(imageIcon); 
	JScrollPane scrollPane = new JScrollPane(label);
	scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
	scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);		
	scrollPane.setPreferredSize(new Dimension(Globals.WIDTH-50,Globals.HEIGHT-200));
	scrollPane.setMinimumSize(new Dimension(Globals.WIDTH-50,Globals.HEIGHT-200));
	scrollPane.setMaximumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT));
	scrollPane.revalidate();
	add(scrollPane);
    }
}
