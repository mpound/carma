package carma.observertools.pdbi.panes;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;
import java.util.*;

import carma.observertools.pdbi.messages.*;
import carma.observertools.pdbi.utils.*;
import carma.observertools.*;
import javax.swing.event.*;

/**
 * class for the projects within range tab
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class ProjectsWithinRange extends JPanel{
	/**
	 * constructor
	 * @param pdb an instance of the project database manager
	 */
	public ProjectsWithinRange(ProjectDatabaseManager pdb){
		pwrPane = new JPanel();
		pwrPane.add(new PWRSearchPane(pdb),new GBC(0,0).setAnchor(GBC.CENTER).setWeight(100,100).setFill(GBC.HORIZONTAL));
		jtp = new TogglePane("PWR");
		pwrPane.add(jtp,new GBC(0,1).setAnchor(GBC.CENTER).setWeight(100, 100).setAnchor(GBC.CENTER).setFill(GBC.HORIZONTAL));
		pwrPane.add(new ResultsPane(projects_,"All",sort_,jtp,sort), new GBC(0,2).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
		pwrPane.setPreferredSize(new Dimension(Globals.WIDTH-2,Globals.HEIGHT-50));
		pwrPane.setMinimumSize(new Dimension(Globals.WIDTH-50,Globals.HEIGHT-50));
		pwrPane.setMaximumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT));
		pwrPane.revalidate();
		add(pwrPane);

	}
	public static void setBand(String band){
		band_ = band;
	}

	/**
	 * method to refresh the panel if anything changes
	 */
	public static void refreshPane(){
		pwrPane.remove(2);
		pwrPane.add(new ResultsPane(projects_,band_,sort_,jtp,sort), new GBC(0,2).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
		pwrPane.revalidate();
		pwrPane.repaint();
	}

	public static JPanel pwrPane = null;
	private static String band_ = "ALL";
	public static Project[] projects_ = null;
	public static String sort_ = "";
	public static TogglePane jtp = null;
	public static ResultsPane.SortType sort = ResultsPane.SortType.PRIORITY;
}

/**
 * class for the search part of the panel
 * @author friedel
 *
 */
@SuppressWarnings("serial")
class PWRSearchPane extends JPanel{
	PWRSearchPane(ProjectDatabaseManager pdb){
		this.pdb = pdb;
		setLayout(new GridBagLayout());
		setPreferredSize(new Dimension(Globals.WIDTH,100));
		setMinimumSize(new Dimension(Globals.WIDTH,100));
		setMaximumSize(new Dimension(Globals.WIDTH,100));
		// inputs for the frequency constraints
		JPanel startLST = new JPanel();
		startLST.add(new JLabel("Start LST:"));
		minHrsF = new JTextField("00",2);
		minHrsF.getDocument().addDocumentListener(new HourFieldListener());
		minHrsF.setToolTipText("Set the starting LST for the search in HH:MM format");
		startLST.add(minHrsF);
		startLST.add(new JLabel(":"));
		minMinF = new JTextField("00",2);
		minMinF.getDocument().addDocumentListener(new MinuteFieldListener());
		minMinF.setToolTipText("Set the starting LST for the search in HH:MM format");
		startLST.add(minMinF);
		add(startLST, new GBC(0,0).setAnchor(GBC.EAST).setWeight(100,100));
		JPanel endLST = new JPanel();
		endLST.add(new JLabel("End LST:"));
		maxHrsF = new JTextField("23",2);
		maxHrsF.getDocument().addDocumentListener(new HourFieldListener());
		maxHrsF.setToolTipText("Set the ending LST for the search in HH:MM format");
		endLST.add(maxHrsF);
		endLST.add(new JLabel(":"));
		maxMinF = new JTextField("59",2);
		maxMinF.getDocument().addDocumentListener(new MinuteFieldListener());
		maxMinF.setToolTipText("Set the ending LST for the search in HH:MM format");
		endLST.add(maxMinF);
		add(endLST, new GBC(0,1).setAnchor(GBC.EAST).setWeight(100,100));
		JPanel leftP = new JPanel();
		leftP.add(new JLabel("Min. hours left:"));
		hrsLeftF = new JTextField("1",3);
		hrsLeftF.setToolTipText("Set the minimum hours left to be observed for the search");
		hrsLeftF.getDocument().addDocumentListener(new HourFieldListener());
		leftP.add(hrsLeftF);
		leftP.add(new JLabel("   Include Flex HA projects:"));
		flex = new JCheckBox("",true);
		leftP.add(flex);
		add(leftP,new GBC(1,0).setAnchor(GBC.WEST).setWeight(100, 100));
		JPanel array = new JPanel();
		array.add(new JLabel("Array Configuration:"));
		arrayConfig = new ArrayConfiguration();
		array.add(arrayConfig);
		rp = new ReorderPane(new ItemListener(){
			public void itemStateChanged(ItemEvent e) {
				eventChanged();
			}
		});
		array.add(rp);
		add(array,new GBC(1,1).setAnchor(GBC.WEST).setWeight(100, 100));
		update = new JButton("Search");
		update.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent event){
				doSearch();
			}
		});
		add(update,new GBC(2,0,1,2).setAnchor(GBC.SOUTHWEST).setWeight(100, 100));
	}
	
	private void eventChanged(){
		String selected = rp.getSelected();
		if(selected.contains("Reverse RA")){
			ProjectsWithinRange.sort = ResultsPane.SortType.RRA;
		}
		else if(selected.contains("RA")){
			ProjectsWithinRange.sort = ResultsPane.SortType.RA;
		}
		else if(selected.contains("Rev. LST")){
			ProjectsWithinRange.sort = ResultsPane.SortType.RLST;
		}
		else if(selected.contains("LST")){
			ProjectsWithinRange.sort = ResultsPane.SortType.LST;
		}
		else if(selected.contains("Rev. Time")){
			ProjectsWithinRange.sort = ResultsPane.SortType.RTIME;
		}
		else if(selected.contains("Time")){
			ProjectsWithinRange.sort = ResultsPane.SortType.TIME;
		}
		else if(selected.contains("Rev. Pri")){
			ProjectsWithinRange.sort = ResultsPane.SortType.RPRIORITY;
		}
		else if(selected.contains("Prior")){
			ProjectsWithinRange.sort = ResultsPane.SortType.PRIORITY;
		}
// only update if the value changed		
		if(lastSort != ProjectsWithinRange.sort){
			lastSort = ProjectsWithinRange.sort;
			ProjectsWithinRange.refreshPane();
		}
	}
	/**
	 * method to construct and do the search
	 */
	private void doSearch(){
		ArrayList<ItemValue> tempIVS = new ArrayList<ItemValue>();
		ArrayList<ItemValue> temp2IVS = new ArrayList<ItemValue>();
		temp2IVS.add(new ItemValue("exceedTAC","true"));
		temp2IVS.add(new ItemValue("priority","0.1,100000.0"));
		tempIVS.add(new ItemValue("remainingTime",hrsLeftF.getText().trim()));
		tempIVS.add(new ItemValue("obsblockStatus","INCOMPLETE"));
		tempIVS.add(new ItemValue("priority","0.1,100000.0"));
		double start = Conversions.DTimeToRadians(minHrsF.getText().trim(),minMinF.getText().trim());
		double end = Conversions.DTimeToRadians(maxHrsF.getText().trim(), maxMinF.getText().trim());
		//tempIVS.add(new ItemValue("requestedRaCoverage",Conversions.TimeToRadians(minHrsF.getText().trim(), minMinF.getText().trim()) + "," + Conversions.TimeToRadians(maxHrsF.getText().trim(), maxMinF.getText().trim())));
		if(arrayConfig.getSelectedItem() != "Any"){
			tempIVS.add(new ItemValue("arrayConfiguration",arrayConfig.getSelectedItem()));
			temp2IVS.add(new ItemValue("arrayConfiguration",arrayConfig.getSelectedItem()));
		}
		if(!flex.isSelected()){
			tempIVS.add(new ItemValue("isFlex","false"));
			temp2IVS.add(new ItemValue("isFlex","false"));
		}
		tempIVS.add(new ItemValue("notProject","commissioning"));
		temp2IVS.add(new ItemValue("notProject","commissioning"));
		ItemValue[] ivSeq = (ItemValue[])tempIVS.toArray(new ItemValue[tempIVS.size()]);
		ItemValue[] ivSeq2 = (ItemValue[])temp2IVS.toArray(new ItemValue[temp2IVS.size()]);
		ProjectsWithinRange.projects_ = null;
		Project[] run1 = null;
		Project[] run2 = null;
		RunQuery rq1 = new RunQuery(pdb,ivSeq);
		RunQuery rq2 = new RunQuery(pdb,ivSeq2);
		// issue the search in a thread to avoid indefinite hangs
		try{
			long time = 0;
			while((run1 = rq1.checkResults()) == null){
				if(time > 120){
					rq1.killThread();
					new ExceptionHandler("Query to PDB timed out.",false);
					break;
				}
				time++;
				Thread.sleep(1000);
			}
			time = 0;
			while((run2 = rq2.checkResults()) == null){
				if(time > 120){
					rq2.killThread();
					new ExceptionHandler("Query to PDB timed out.",false);
					break;
				}
				time++;
				Thread.sleep(1000);
			}
		}
		catch(InterruptedException e){
			new ExceptionHandler(e.toString(),false);
		}
		ProjectsWithinRange.projects_ = Operators.mergeProjects(run1,run2);
		ProjectsWithinRange.projects_ = TimeSelect.matchTime(ProjectsWithinRange.projects_, start, end);
		ProjectsWithinRange.refreshPane();
	}

	private void updateHours(){
		int hours;
		try{
			hours = Integer.parseInt(minHrsF.getText().trim());
			TestHours(hours,true);
			hours = Integer.parseInt(maxHrsF.getText().trim());
			TestHours(hours,true);
			hours = Integer.parseInt(hrsLeftF.getText().trim());
			TestHours(hours,false);
		}
		catch(NumberFormatException e){
			if(!(e.toString().contains("\"\"") || e.toString().contains("empty"))){
				new ExceptionHandler(e.toString(), false);
			}
		}
	}

	private void TestHours(int hour, boolean lst){
		if(lst){
			if(hour < 0 || hour > 23){
				throw new NumberFormatException("Hours must range from 0-23");
			}
		}
		else{
			if(hour < 0){
				throw new NumberFormatException("Minimum hours left must be positive or 0.");
			}
		}
	}

	private void updateMins(){
		int minutes;
		try{
			minutes = Integer.parseInt(minMinF.getText().trim());
			TestMinutes(minutes);
			minutes = Integer.parseInt(maxMinF.getText().trim());
			TestMinutes(minutes);
		}
		catch(NumberFormatException e){
			if(!(e.toString().contains("\"\"") || e.toString().contains("empty"))){
				new 	ExceptionHandler(e.toString(), false);
			}
		}
	}
	
	private void TestMinutes(int minute){
		if(minute < 0 || minute > 59){
			throw new NumberFormatException("Minutes must range from 0-59");
		}
	}

	private class HourFieldListener implements DocumentListener{
		public void insertUpdate(DocumentEvent event){updateHours();}
		public void removeUpdate(DocumentEvent event){updateHours();}
		public void changedUpdate(DocumentEvent event){updateHours();}		
	}

	private class MinuteFieldListener implements DocumentListener{
		public void insertUpdate(DocumentEvent event){updateMins();}
		public void removeUpdate(DocumentEvent event){updateMins();}
		public void changedUpdate(DocumentEvent event){updateMins();}		
	}

	private JTextField minHrsF = null;
	private JTextField minMinF = null;
	private JTextField maxHrsF = null;
	private JTextField maxMinF = null;
	private JTextField hrsLeftF = null;
	private JCheckBox flex = null;
	private Choice arrayConfig = null;
	private JButton update = null;
	private ProjectDatabaseManager pdb = null;
	private ReorderPane rp = null;
	private static ResultsPane.SortType lastSort = ResultsPane.SortType.PRIORITY;
}

