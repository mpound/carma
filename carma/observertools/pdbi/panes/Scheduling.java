package carma.observertools.pdbi.panes;
import javax.swing.*;
import java.awt.*;


import carma.observertools.*;
import carma.observertools.pdbi.utils.*;

@SuppressWarnings("serial")
public class Scheduling extends JPanel{
	public Scheduling(ProjectDatabaseManager _pdb){
		tabbedPane = new JTabbedPane();
		tabbedPane.addTab("Schedule", new SchedulePane(_pdb));
		tabbedPane.addTab("SchedPlot", new SchedPlot());
		tabbedPane.setPreferredSize(new Dimension(Globals.WIDTH-2,Globals.HEIGHT-50));
		tabbedPane.setMinimumSize(new Dimension(Globals.WIDTH-50,Globals.HEIGHT-50));
		tabbedPane.setMaximumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT));
		tabbedPane.revalidate();
		add(tabbedPane);
	}
	
	JTabbedPane tabbedPane = null;
}
