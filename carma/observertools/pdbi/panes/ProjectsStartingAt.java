package carma.observertools.pdbi.panes;

import java.awt.*;

import javax.swing.*;
import javax.swing.event.*;
import java.util.*;
import java.awt.event.*;

import carma.observertools.*;
import carma.observertools.pdbi.messages.*;
import carma.observertools.pdbi.utils.*;

/**
 * class to search for projects based on RA and HA range
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class ProjectsStartingAt extends JPanel{
	public ProjectsStartingAt(ProjectDatabaseManager pdb){
		psaPane = new JPanel();
		psaPane.add(new PSASearchPane(pdb),new GBC(0,0).setAnchor(GBC.CENTER).setWeight(100,100).setFill(GBC.HORIZONTAL));
		jtp = new TogglePane("PSA");
		psaPane.add(jtp,new GBC(0,1).setAnchor(GBC.CENTER).setWeight(100, 100).setAnchor(GBC.CENTER).setFill(GBC.HORIZONTAL));
		psaPane.add(new ResultsPane(projects_,"All",sort_,jtp,sort), new GBC(0,2).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
		psaPane.setPreferredSize(new Dimension(Globals.WIDTH-2,Globals.HEIGHT-50));
		psaPane.setMinimumSize(new Dimension(Globals.WIDTH-50,Globals.HEIGHT-50));
		psaPane.setMaximumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT));
		psaPane.revalidate();
		add(psaPane);

	}
	public static void setBand(String band){
		band_ = band;
	}

	public static void refreshPane(){
		psaPane.remove(2);
		psaPane.add(new ResultsPane(projects_,band_,sort_,jtp,sort), new GBC(0,2).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
		psaPane.revalidate();
		psaPane.repaint();
	}

	private static JPanel psaPane = null;
	private static String band_ = "ALL";
	public static Project[] projects_ = null;
	public static String sort_ = "";
	public static TogglePane jtp = null;
	public static ResultsPane.SortType sort = ResultsPane.SortType.PRIORITY;
}

@SuppressWarnings("serial")
class PSASearchPane extends JPanel{
	PSASearchPane(ProjectDatabaseManager pdb){
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
		JPanel haLimit = new JPanel();
		haLimit.add(new JLabel("HA Limit:"));
		haLimitF = new JTextField("3",2);
		haLimitF.getDocument().addDocumentListener(new HourFieldListener());
		haLimit.setToolTipText("Hour angle limit - search will be for any project within HA limit hours of the given start LST");
		haLimit.add(haLimitF);
		add(haLimit,new GBC(0,1).setAnchor(GBC.EAST).setWeight(100, 100));
		JPanel leftP = new JPanel();
		leftP.add(new JLabel("Min. hours left:"));
		hrsLeftF = new JTextField("3",3);
		hrsLeftF.getDocument().addDocumentListener(new HourFieldListener());
		hrsLeftF.setToolTipText("Minimum number of hours remaining to be observed");
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
			ProjectsStartingAt.sort = ResultsPane.SortType.RRA;
		}
		else if(selected.contains("RA")){
			ProjectsStartingAt.sort = ResultsPane.SortType.RA;
		}
		else if(selected.contains("Rev. LST")){
			ProjectsStartingAt.sort = ResultsPane.SortType.RLST;
		}
		else if(selected.contains("LST")){
			ProjectsStartingAt.sort = ResultsPane.SortType.LST;
		}
		else if(selected.contains("Rev. Time")){
			ProjectsStartingAt.sort = ResultsPane.SortType.RTIME;
		}
		else if(selected.contains("Time")){
			ProjectsStartingAt.sort = ResultsPane.SortType.TIME;
		}
		else if(selected.contains("Rev. Pri")){
			ProjectsStartingAt.sort = ResultsPane.SortType.RPRIORITY;
		}
		else if(selected.contains("Prior")){
			ProjectsStartingAt.sort = ResultsPane.SortType.PRIORITY;
		}
// only update if the value changed		
		if(lastSort != ProjectsStartingAt.sort){
			lastSort = ProjectsStartingAt.sort;
			ProjectsStartingAt.refreshPane();
		}
	}

	private void doSearch(){
		ArrayList<ItemValue> tempIVS = new ArrayList<ItemValue>();
		ArrayList<ItemValue> temp2IVS = new ArrayList<ItemValue>();
		temp2IVS.add(new ItemValue("exceedTAC","true"));
		temp2IVS.add(new ItemValue("priority","0.1,100000.0"));
		tempIVS.add(new ItemValue("remainingTime",hrsLeftF.getText().trim()));
		tempIVS.add(new ItemValue("obsblockStatus","INCOMPLETE"));
		tempIVS.add(new ItemValue("priority","0.1,100000.0"));
		double start = Conversions.DTimeToRadians(haLimitF.getText().trim(),minHrsF.getText().trim(), minMinF.getText().trim(),true);
		double end = Conversions.DTimeToRadians(haLimitF.getText().trim(),minHrsF.getText().trim(), minMinF.getText().trim(),false);

		//tempIVS.add(new ItemValue("requestedRaCoverage",Conversions.TimeToRadians(haLimitF.getText().trim(),minHrsF.getText().trim(), minMinF.getText().trim(),true) + "," + Conversions.TimeToRadians(haLimitF.getText().trim(),minHrsF.getText().trim(), minMinF.getText().trim(),false)));
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
		Project[] run1 = null;
		Project[] run2 = null;
		RunQuery rq1 = new RunQuery(pdb,ivSeq);
		RunQuery rq2 = new RunQuery(pdb,ivSeq2);
		// issue the query in a thread to avoid hangs
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
		ProjectsStartingAt.projects_ = TimeSelect.matchTime(Operators.mergeProjects(run1, run2), start, end);
		ProjectsStartingAt.refreshPane();
	}
	/**
	 *  Method to update the frequency constraints in query from the inputs
	 */
	private void updateHours(){
		int hours;
		try{
			hours = Integer.parseInt(minHrsF.getText().trim());
			TestHours(hours,true);
			hours = Integer.parseInt(hrsLeftF.getText().trim());
			TestHours(hours,false);
			float hrs = Float.parseFloat(haLimitF.getText().trim());
			if(Math.abs(hrs) > 12.0){
				throw new NumberFormatException("HA limit must be < +/-12");
			}
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
	/**
	 *  Method to update the frequency constraints in query from the inputs
	 */
	private void updateMins(){
		int minutes;
		try{
			minutes = Integer.parseInt(minMinF.getText().trim());
			TestMinutes(minutes);
		}
		catch(NumberFormatException e){
			if(!	(e.toString().contains("\"\"") || e.toString().contains("empty"))){
				new 	ExceptionHandler(e.toString(), false);
			}
		}
	}
	
	private void TestMinutes(int minute){
		if(minute < 0 || minute > 59){
			throw new NumberFormatException("Minutes must range from 0-59");
		}
	}

	/**
	 * Class to listen to changes in the frequency components
	 */
	private class HourFieldListener implements DocumentListener{
		public void insertUpdate(DocumentEvent event){updateHours();}
		public void removeUpdate(DocumentEvent event){updateHours();}
		public void changedUpdate(DocumentEvent event){updateHours();}		
	}

	/**
	 * Class to listen to changes in the frequency components
	 */
	private class MinuteFieldListener implements DocumentListener{
		public void insertUpdate(DocumentEvent event){updateMins();}
		public void removeUpdate(DocumentEvent event){updateMins();}
		public void changedUpdate(DocumentEvent event){updateMins();}		
	}
	
	private JTextField minHrsF = null;
	private JTextField minMinF = null;
	private JTextField haLimitF = null;
	private JTextField hrsLeftF = null;
	private JCheckBox flex = null;
	private Choice arrayConfig = null;
	private JButton update = null;
	private ProjectDatabaseManager pdb = null;
	private ReorderPane rp = null;
	private static ResultsPane.SortType lastSort = ResultsPane.SortType.PRIORITY;
}

