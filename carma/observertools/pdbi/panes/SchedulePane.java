package carma.observertools.pdbi.panes;
import javax.swing.*;
import javax.swing.event.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.text.*;
import java.util.*;

import carma.observertools.*;
import carma.observertools.pdbi.messages.ExceptionHandler;
import carma.observertools.pdbi.utils.*;

/**
 * class for creating a schedule
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class SchedulePane extends JPanel{
	public SchedulePane(ProjectDatabaseManager pdb){
		pdb_ = pdb;
		setLayout(new GridBagLayout());
		sPane = new JPanel();
		sPane.add(new ScheduleSearchPane(),new GBC(0,0).setAnchor(GBC.CENTER).setWeight(100,100).setFill(GBC.HORIZONTAL));
		sPane.add(new ScheduleResultsPane(ommFile,tmmFile,cmFile,ocmFile,nghzFile,scionequeue,scitwoqueue), new GBC(0,1).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
		sPane.setPreferredSize(new Dimension(Globals.WIDTH-2,Globals.HEIGHT-100));
		sPane.setMinimumSize(new Dimension(Globals.WIDTH-50,Globals.HEIGHT-100));
		sPane.setMaximumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT));
		sPane.revalidate();
		add(sPane);
	}
	
	public static void refreshPane(){
		sPane.remove(1);
		sPane.add(new ScheduleResultsPane(ommFile,tmmFile,cmFile,ocmFile,nghzFile,scionequeue,scitwoqueue), new GBC(0,1).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
		sPane.revalidate();
		sPane.repaint();
	}
	
	public static ProjectDatabaseManager pdb_ = null;
	private static JPanel sPane = null;
	public static String ommFile = "/home/obs/web_pages/schedule/calendar.1mm_Sci1";
	public static String tmmFile = "/home/obs/web_pages/schedule/calendar.3mm_Sci1";
	public static String cmFile = "/home/obs/web_pages/schedule/calendar.1cm_Sci1";
	public static String ocmFile = "/home/obs/web_pages/schedule/calendar.1cm_Sci2";
	public static String nghzFile = "/home/obs/web_pages/schedule/calendar.3mm_Sci2";
	public static String scionequeue = "/home/obs/web_pages/schedule/queue.Sci1.out";
	public static String scitwoqueue = "/home/obs/web_pages/schedule/queue.Sci2.out";
}

/**
 * class for the inputs to the schedule program
 * @author friedel
 *
 */
@SuppressWarnings("serial")
class ScheduleSearchPane extends JPanel{
	ScheduleSearchPane(){
		setLayout(new GridBagLayout());
		// Array configuration selection
		add(new JLabel("Sci1:  Array Configuration:"), new GBC(0,0).setAnchor(GBC.CENTER).setWeight(100,100));
		arrayConfig1 = new Choice();
		arrayConfig1.add("A");
		arrayConfig1.add("B");
		arrayConfig1.add("C");
		arrayConfig1.add("D");
		arrayConfig1.add("E");
		arrayConfig2 = new Choice();
		arrayConfig2.add("SL");
		arrayConfig2.add("SH");
		add(arrayConfig1, new GBC(1,0).setAnchor(GBC.WEST).setWeight(100,100));
		
		// Frequency band selection
		add(new JLabel("Band: "), new GBC(2,0).setAnchor(GBC.WEST).setWeight(100,100));
		band1 = new Choice();
		band1.add("All");
		band1.add("3mm");
		band1.add("1mm");
		band2 = new Choice();
		band2.add("All");
		band2.add("1cm");
		band2.add("3mm");
		add(band1, new GBC(3,0).setAnchor(GBC.WEST).setWeight(100,100));
		// Number of days to schedule
		add(new JLabel("Days :"), new GBC(4,0).setAnchor(GBC.WEST).setWeight(100,100));
		days = new JTextField("7",5);
		days.setToolTipText("The maximum number of days to schedule, 0 means no limit.");
		days.getDocument().addDocumentListener(new IntListener());
		add(days, new GBC(5,0).setAnchor(GBC.WEST).setWeight(100,100));
		
		add(new JLabel("Sci2:  Array Configuration:"), new GBC(0,1).setAnchor(GBC.CENTER).setWeight(100,100));
		add(arrayConfig2, new GBC(1,1).setAnchor(GBC.WEST).setWeight(100,100));
		add(new JLabel("Band: "), new GBC(2,1).setAnchor(GBC.WEST).setWeight(100,100));
		add(band2, new GBC(3,1).setAnchor(GBC.WEST).setWeight(100,100));
		
		add(new JLabel("Fixed: "), new GBC(0,2).setAnchor(GBC.WEST).setWeight(100,100));
		fixed = new JTextField("/array/rt/scripts/fixedblocks",25);
		fixed.setToolTipText("File name for fixed blocks from say a previous run. These will be added to the schedule BEFORE any new blocks are scheduled.");
		add(fixed, new GBC(1,2,5,1).setAnchor(GBC.WEST).setWeight(100,100));
		add(new JLabel("Min Hours: "), new GBC(6,2).setAnchor(GBC.WEST).setWeight(100,100));
		minHours = new JTextField("1",5);
		minHours.setToolTipText("Only show projects with at least this many hours left to observe");
		minHours.getDocument().addDocumentListener(new FloatListener());
		add(minHours, new GBC(7,2).setAnchor(GBC.WEST).setWeight(100,100));
		add(new JLabel("Mode: "), new GBC(8,2).setAnchor(GBC.WEST).setWeight(100,100));
		mode = new Choice();
		mode.add("priority");
		mode.add("compact");
		add(mode, new GBC(9,2).setAnchor(GBC.WEST).setWeight(100,100));
		add(new JLabel("Pivot :"), new GBC(10,2).setAnchor(GBC.WEST).setWeight(100,100));
		pivot = new JTextField("0",5);
		pivot.setToolTipText("Grade below which all projects are scheduled in COMPACT mode.");
		pivot.getDocument().addDocumentListener(new FloatListener());
		add(pivot, new GBC(11,2).setAnchor(GBC.WEST).setWeight(100,100));
		JPanel startPanel = new JPanel();
		startPanel.setLayout(new GridBagLayout());
		startPanel.add(new JLabel("Start LST:"), new GBC(0,0).setAnchor(GBC.WEST).setWeight(100,100));
		startLSTHours = new JTextField("0",3);
		startLSTHours.setToolTipText("Local sidereal time at which to start the schedule. This is useful if you want to start the schedule at a time you know a previous script is ending.");
		startLSTHours.getDocument().addDocumentListener(new IntListener());
		startPanel.add(startLSTHours, new GBC(1,0).setAnchor(GBC.WEST).setWeight(100,100));
		startPanel.add(new JLabel(":"), new GBC(2,0).setAnchor(GBC.WEST).setWeight(100,100));
		startLSTMinutes = new JTextField("0",3);
		startLSTMinutes.setToolTipText("Local sidereal time at which to start the schedule. This is useful if you want to start the schedule at a time you know a previous script is ending.");
		startLSTMinutes.getDocument().addDocumentListener(new IntListener());
		startPanel.add(startLSTMinutes, new GBC(3,0).setAnchor(GBC.WEST).setWeight(100,100));
		add(startPanel,new GBC(2,3).setAnchor(GBC.WEST).setWeight(100,100));
		mksched = new JButton("Go");
		mksched.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent event){
				makeSchedule();
				SchedulePane.refreshPane();
			}
		});
		add(mksched, new GBC(6,3).setAnchor(GBC.CENTER).setWeight(100,100));
		reset = new JButton("Reset");
		reset.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent event){
				minHours.setText("1");
				days.setText("7");
				fixed.setText("/array/rt/scripts/fixedblocks");
				pivot.setText("0");
				startLSTHours.setText("0");
				startLSTMinutes.setText("0");
			}
		});
		add(reset, new GBC(7,3).setAnchor(GBC.CENTER).setWeight(100,100));
	}
	
	private static void checkInt(){
		int temp;
		try{
			temp = Integer.parseInt(days.getText().trim());
			if(temp < 0){
				throw new NumberFormatException("Number of days must be positive.");
			}
			temp = Integer.parseInt(startLSTHours.getText().trim());
			if(temp < 0 || temp > 23){
				new ExceptionHandler("Start LST Hours must be between 0 and 23",false);
			}
			temp = Integer.parseInt(startLSTHours.getText().trim());
			if(temp < 0 || temp > 59){
				new ExceptionHandler("Start LST Minutes must be between 0 and 59",false);
			}
		}
		catch(NumberFormatException e){
			if(!(e.toString().contains("\"\"") || e.toString().contains("empty"))){
				new ExceptionHandler(e.toString(), false);
			}
		}
	}
	
	/**
	 * method to check the validity of float input
	 */
	private static void checkFloat(){
		float temp;
		try{
			temp = Float.parseFloat(minHours.getText().trim());
			if(temp < 0.0){
				new ExceptionHandler("Minimum hours must be >= 0.0",false);
			}
			temp = Float.parseFloat(pivot.getText().trim());
		}
		catch(NumberFormatException e){
			if(!(e.toString().contains("\"\"") || e.toString().contains("empty") || e.toString().contains("."))){
				new ExceptionHandler(e.toString(), false);
			}
		}

	}
	
	/**
	 * Class to listen to changes in the integer inputs
	 */
	private class IntListener implements DocumentListener{
		public void insertUpdate(DocumentEvent event){checkInt();}
		public void removeUpdate(DocumentEvent event){checkInt();}
		public void changedUpdate(DocumentEvent event){checkInt();}		
	}

	/**
	 * Class to listen to changes in the float inputs
	 */
	private class FloatListener implements DocumentListener{
		public void insertUpdate(DocumentEvent event){checkFloat();}
		public void removeUpdate(DocumentEvent event){checkFloat();}
		public void changedUpdate(DocumentEvent event){checkFloat();}		
	}

	/**
	 * function which generates the input file and calls the scheduler
	 */
	private void makeSchedule(){
		if(band1.getSelectedItem() == "All"){
			String rband = "3mm";
			ArrayList<ItemValue> tempIVS = new ArrayList<ItemValue>();
			tempIVS.add(new ItemValue("obsblockStatus","INCOMPLETE"));
			tempIVS.add(new ItemValue("remainingTime",minHours.getText().trim()));
			tempIVS.add(new ItemValue("receiverBand",rband));
			tempIVS.add(new ItemValue("arrayConfiguration",arrayConfig1.getSelectedItem()));
			tempIVS.add(new ItemValue("notProject","commissioning"));
			tempIVS.add(new ItemValue("priority","0.1,100000.0"));
			ItemValue[] ivSeq = (ItemValue[])tempIVS.toArray(new ItemValue[tempIVS.size()]);
			projects_ = null;
			RunQuery rq1 = new RunQuery(SchedulePane.pdb_,ivSeq);
			// issue the search in a thread to avoid indefinite hangs
			try{
				long time = 0;
				while((projects_ = rq1.checkResults()) == null){
					if(time > 120){
						rq1.killThread();
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
			if(projects_ == null){
				new ExceptionHandler("No projects found for given criteria",false);
			}
			String fs = fileStamp();
			String fileName = "/home/obs/web_pages/schedule/mksched.in_" + rband + ".sci1." + fs;
			makeInputFile(fileName,rband,arrayConfig1.getSelectedItem());
			String queue = "/home/obs/web_pages/schedule/queue.Sci1.out";
			String mout = "/home/obs/web_pages/schedule/mksched.calendar_" + rband + ".sci1." + fs;
			mkSched(fileName,mout,rband,arrayConfig1.getSelectedItem(),"Sci1",queue);
			// now do the 1mm stuff
			rband = "1mm";
			ArrayList<ItemValue> tempIVS1 = new ArrayList<ItemValue>();
			tempIVS1.add(new ItemValue("obsblockStatus","INCOMPLETE"));
			tempIVS1.add(new ItemValue("remainingTime",minHours.getText().trim()));
			tempIVS1.add(new ItemValue("receiverBand",rband));
			tempIVS1.add(new ItemValue("arrayConfiguration",arrayConfig1.getSelectedItem()));
			tempIVS1.add(new ItemValue("notProject","commissioning"));
			tempIVS1.add(new ItemValue("priority","0.1,100000.0"));
			ItemValue[] ivSeq1 = (ItemValue[])tempIVS1.toArray(new ItemValue[tempIVS.size()]);
			projects_ = null;
			RunQuery rq2 = new RunQuery(SchedulePane.pdb_,ivSeq1);
			// issue the search in a thread to avoid indefinite hangs
			try{
				long time = 0;
				while((projects_ = rq2.checkResults()) == null){
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
			if(projects_ == null){
				new ExceptionHandler("No projects found for given criteria",false);
			}
			fs = fileStamp();
			fileName = "/home/obs/web_pages/schedule/mksched.in_" + rband + ".sci1." + fs;
			makeInputFile(fileName,rband,arrayConfig1.getSelectedItem());
			mout = "/home/obs/web_pages/schedule/mksched.calendar_" + rband + ".sci1." + fs;
			queue = "/home/obs/web_pages/schedule/queue.Sci1.out";
			mkSched(fileName,mout,rband,arrayConfig1.getSelectedItem(),"Sci1",queue);
		}
		else{
			ArrayList<ItemValue> tempIVS = new ArrayList<ItemValue>();
			tempIVS.add(new ItemValue("obsblockStatus","INCOMPLETE"));
			tempIVS.add(new ItemValue("remainingTime",minHours.getText().trim()));
			tempIVS.add(new ItemValue("receiverBand",band1.getSelectedItem()));
			tempIVS.add(new ItemValue("arrayConfiguration",arrayConfig1.getSelectedItem()));
			tempIVS.add(new ItemValue("notProject","commissioning"));
			tempIVS.add(new ItemValue("priority","0.1,100000.0"));
			ItemValue[] ivSeq = (ItemValue[])tempIVS.toArray(new ItemValue[tempIVS.size()]);
			projects_ = null;
			RunQuery rq1 = new RunQuery(SchedulePane.pdb_,ivSeq);
			// issue the search in a thread to avoid indefinite hangs
			try{
				long time = 0;
				while((projects_ = rq1.checkResults()) == null){
					if(time > 120){
						rq1.killThread();
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
			if(projects_ == null){
				new ExceptionHandler("No projects found for given criteria",false);
			}
			String fs = fileStamp();
			String fileName = "/home/obs/web_pages/schedule/mksched.in_" + band1.getSelectedItem() + ".sci1." + fs;
			makeInputFile(fileName,band1.getSelectedItem(),arrayConfig1.getSelectedItem());
			String mout = "/home/obs/web_pages/schedule/mksched.calendar_"+band1.getSelectedItem() + ".sci1."+fs;
			String queue = "/home/obs/web_pages/schedule/queue.Sci1.out";
			mkSched(fileName,mout,band1.getSelectedItem(),arrayConfig1.getSelectedItem(),"Sci1",queue);
		}
		if(band2.getSelectedItem() == "All"){
			String rband = "3mm";
			ArrayList<ItemValue> tempIVS = new ArrayList<ItemValue>();
			tempIVS.add(new ItemValue("obsblockStatus","INCOMPLETE"));
			tempIVS.add(new ItemValue("remainingTime",minHours.getText().trim()));
			tempIVS.add(new ItemValue("receiverBand",rband));
			tempIVS.add(new ItemValue("arrayConfiguration",arrayConfig2.getSelectedItem()));
			tempIVS.add(new ItemValue("notProject","commissioning"));
			tempIVS.add(new ItemValue("priority","0.1,100000.0"));
			ItemValue[] ivSeq = (ItemValue[])tempIVS.toArray(new ItemValue[tempIVS.size()]);
			projects_ = null;
			RunQuery rq1 = new RunQuery(SchedulePane.pdb_,ivSeq);
			// issue the search in a thread to avoid indefinite hangs
			try{
				long time = 0;
				while((projects_ = rq1.checkResults()) == null){
					if(time > 120){
						rq1.killThread();
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
			if(projects_ == null){
				new ExceptionHandler("No projects found for given criteria",false);
			}
			String fs = fileStamp();
			String fileName = "/home/obs/web_pages/schedule/mksched.in_" + rband + ".sci2." + fs;
			makeInputFile(fileName,rband,arrayConfig2.getSelectedItem());
			String mout = "/home/obs/web_pages/schedule/mksched.calendar_" + rband + ".sci2." + fs;
			String queue = "/home/obs/web_pages/schedule/queue.Sci2.out";
			mkSched(fileName,mout,rband,arrayConfig2.getSelectedItem(),"Sci2",queue);
			// now do the 1mm stuff
			rband = "1cm";
			ArrayList<ItemValue> tempIVS1 = new ArrayList<ItemValue>();
			tempIVS1.add(new ItemValue("obsblockStatus","INCOMPLETE"));
			tempIVS1.add(new ItemValue("remainingTime",minHours.getText().trim()));
			tempIVS1.add(new ItemValue("receiverBand",rband));
			tempIVS1.add(new ItemValue("arrayConfiguration",arrayConfig2.getSelectedItem()));
			tempIVS1.add(new ItemValue("notProject","commissioning"));
			tempIVS1.add(new ItemValue("priority","0.1,100000.0"));
			ItemValue[] ivSeq1 = (ItemValue[])tempIVS1.toArray(new ItemValue[tempIVS.size()]);
			projects_ = null;
			RunQuery rq2 = new RunQuery(SchedulePane.pdb_,ivSeq1);
			// issue the search in a thread to avoid indefinite hangs
			try{
				long time = 0;
				while((projects_ = rq2.checkResults()) == null){
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
			if(projects_ == null){
				new ExceptionHandler("No projects found for given criteria",false);
			}
			fs = fileStamp();
			fileName = "/home/obs/web_pages/schedule/mksched.in_" + rband + ".sci2." + fs;
			makeInputFile(fileName,rband,arrayConfig2.getSelectedItem());
			mout = "/home/obs/web_pages/schedule/mksched.calendar_" + rband + ".sci2." + fs;
			queue = "/home/obs/web_pages/schedule/queue.Sci2.out";
			mkSched(fileName,mout,rband,arrayConfig2.getSelectedItem(),"Sci2",queue);
		}
		else{
			ArrayList<ItemValue> tempIVS = new ArrayList<ItemValue>();
			tempIVS.add(new ItemValue("obsblockStatus","INCOMPLETE"));
			tempIVS.add(new ItemValue("remainingTime",minHours.getText().trim()));
			tempIVS.add(new ItemValue("receiverBand",band2.getSelectedItem()));
			tempIVS.add(new ItemValue("arrayConfiguration",arrayConfig2.getSelectedItem()));
			tempIVS.add(new ItemValue("notProject","commissioning"));
			tempIVS.add(new ItemValue("priority","0.1,100000.0"));
			ItemValue[] ivSeq = (ItemValue[])tempIVS.toArray(new ItemValue[tempIVS.size()]);
			projects_ = null;
			RunQuery rq1 = new RunQuery(SchedulePane.pdb_,ivSeq);
			// issue the search in a thread to avoid indefinite hangs
			try{
				long time = 0;
				while((projects_ = rq1.checkResults()) == null){
					if(time > 120){
						rq1.killThread();
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
			if(projects_ == null){
				new ExceptionHandler("No projects found for given criteria",false);
			}
			String fs = fileStamp();
			String fileName = "/home/obs/web_pages/schedule/mksched.in_" + band2.getSelectedItem() + ".sci2." + fs;
			makeInputFile(fileName,band2.getSelectedItem(),arrayConfig2.getSelectedItem());
			String mout = "/home/obs/web_pages/schedule/mksched.calendar_"+ band2.getSelectedItem() + ".sci2." + fs;
			String queue = "/home/obs/web_pages/schedule/queue.Sci2.out";
			mkSched(fileName,mout,band2.getSelectedItem(),arrayConfig2.getSelectedItem(),"Sci2",queue);
		}

	}
	
	/**
	 * function to call the scheduler program (system call)
	 * @param fileName the input file name
	 * @param mout the output file name
	 * @param rband the frequency band being scheduled
	 */
	private void mkSched(String fileName, String mout, String rband, String AC, String sci, String queue){
		String temp = sci;
		float time = Float.parseFloat(startLSTHours.getText().trim());
		time += (Float.parseFloat(startLSTMinutes.getText().trim()))/60.0;
		String[] command={"/bin/sh","-c","/opt/rt/bin/mksched file=" + fileName + " band=" + rband + " days=" + days.getText().trim() + " config=" + AC + " start=" + startDate() + " fixed=" + fixed.getText().trim() + " pivot=" + pivot.getText().trim() + " mode=" + mode.getSelectedItem() + " startLST=" + String.format("%-2.2f", time) + " subarray=" + temp.toLowerCase() + " queue=" + queue + " verbose=0 > " + mout};
		// generate the schedule
		try{
			final Process process = Runtime.getRuntime().exec(command);
			int returnCode = process.waitFor();
			if(returnCode != 0){
				new ExceptionHandler("mksched returned with exit code " + returnCode,false);
			}
		}
		catch(Exception e){
			new ExceptionHandler(e.toString(),false);
		}
		// move links
		String[] command1={"/bin/sh","-c","/bin/ln -sf " + mout + " /home/obs/web_pages/schedule/calendar." + rband + "_" + sci};
		try{
			final Process process = Runtime.getRuntime().exec(command1);
			int returnCode = process.waitFor();
			if(returnCode != 0){
				new ExceptionHandler("Error moving link for " + mout,false);
			}
		}
		catch(Exception e){
			new ExceptionHandler(e.toString(),false);
		}
		
		String fout = fileName + ".fixed_blocks";
		String[] command2={"/bin/sh","-c","/bin/ln -sf " + fout + " /home/obs/web_pages/schedule/list." + rband + "_" + sci};
		try{
			final Process process = Runtime.getRuntime().exec(command2);
			int returnCode = process.waitFor();
			if(returnCode != 0){
				new ExceptionHandler("Error moving link for " + fout,false);
			}
		}
		catch(Exception e){
			new ExceptionHandler(e.toString(),false);
		}

		String[] command3={"/bin/sh","-c","/bin/ln -sf " + fileName + " /home/obs/web_pages/schedule/input." + rband + "_" + sci};
		try{
			final Process process = Runtime.getRuntime().exec(command3);
			int returnCode = process.waitFor();
			if(returnCode != 0){
				new ExceptionHandler("Error moving link for " + fileName,false);
			}
		}
		catch(Exception e){
			new ExceptionHandler(e.toString(),false);
		}
	}
	
	/*
	 * function to generate the input file for the scheduler
	 */
	private void makeInputFile(String fileName,String rband, String AC){
		try{
		    // Create file 
		    FileWriter fstream = new FileWriter(fileName);
		    BufferedWriter out = new BufferedWriter(fstream);
		    DateFormat dateFormat = new SimpleDateFormat("EEE MMM dd  HH:mm:ss  yyyy");
            Date date = new Date();
    		float time = Float.parseFloat(startLSTHours.getText().trim());
    		time += (Float.parseFloat(startLSTMinutes.getText().trim()))/60.0;

		    out.write("# mksched input file generated by project database on " + dateFormat.format(date) + "\n");
		    out.write("# with schedule(" + AC +"," + minHours.getText().trim() + "," + rband + "," + days.getText().trim() + ",True,/array/rt/scripts/fixedblocks," + mode.getSelectedItem() + "," + pivot.getText().trim() + "," + String.format("%-2.2f", time) + ")\n#\n#\n");
		    out.write("#|  project_name    | obsblockId     |  obsblock       | Freq  | source_name          | RA        | Dec   | configuration | TAC_allocation | Flex_HA | numeric_grade  | Institution \n");
		    out.write("#|     s            |      s         | i               | GHz   | s                    | hms       | dms   | s             | r              | i       | r              | s           \n");
		    out.write("#|                  |                |                 |       |                      |           |       |               |                |         |                |             \n");

		    //Close the output stream
		    for(int i = 0; i < projects_.length; i++){
		    	String instCode = "Visitor";
		    	if(projects_[i].primaryInvestigator.affiliation.contains("CalTech") || projects_[i].primaryInvestigator.affiliation.contains("CIT")){
		    		instCode = "CIT";
		    	}
		    	else if(projects_[i].primaryInvestigator.affiliation.contains("UC Berkeley") || projects_[i].primaryInvestigator.affiliation.contains("UCB")){
		    		instCode = "UCB";
		    	}
		    	else if(projects_[i].primaryInvestigator.affiliation.contains("UMD") || projects_[i].primaryInvestigator.affiliation.contains("UIUC")){
		    		instCode = projects_[i].primaryInvestigator.affiliation;
		    	}
		    	for(int j = 0; j < projects_[i].obsblock.length; j++){
		    		int flex = 0;
		    		if(projects_[i].obsblock[j].isFlex){
		    			flex = 1;
		    		}
				    out.write(String.format("%s    %s %2d    %6.3f     %24s   %10s  %10s  %2s  %6.2f  %d  %6.2f %8s \n" ,projects_[i].projectID,projects_[i].projectID + "." + projects_[i].obsblock[j].obsblockID,j+1,projects_[i].obsblock[j].restFrequency,projects_[i].obsblock[j].subObsblock[0].trial[0].source[0].sourceName.replace(" ","_"),Conversions.RadiansToFullTime(projects_[i].obsblock[j].subObsblock[0].trial[0].source[0].ra),Conversions.RadiansToDec(projects_[i].obsblock[j].subObsblock[0].trial[0].source[0].dec),projects_[i].obsblock[j].arrayConfiguration,projects_[i].obsblock[j].remainingTime,flex,projects_[i].obsblock[j].priority,instCode));
		    	}
		    }
		    out.close();
		}
		catch (Exception e){//Catch exception if any
		    new ExceptionHandler(e.toString(),false);
	    }
	}

	private String fileStamp(){
		DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd_HHmm");
        Date date = new Date();
        return dateFormat.format(date);
	}

	private String startDate(){
		DateFormat dateFormat = new SimpleDateFormat("yyyy-MMM-dd");
        Date date = new Date();
        return dateFormat.format(date);
	}
	static Choice arrayConfig1 = null;
	static Choice arrayConfig2 = null;
	static Choice band1 = null;
	static Choice band2 = null;
	static JTextField days = null;
	static JTextField fixed = null;
	static JTextField minHours = null;
	static Choice mode = null;
	static JTextField pivot = null;
	static JTextField startLSTHours = null;
	static JTextField startLSTMinutes = null;
	static JButton mksched = null;
	static JButton reset = null;
	public static Project[] projects_ = null;
}

/**
 * class to display the current 1mm and 3mm schedules
 * @author friedel
 *
 */
@SuppressWarnings("serial")
class ScheduleResultsPane extends JPanel{
	ScheduleResultsPane(String ommFile,String tmmFile, String cmFile,String ocmFile,String nghzFile, String scionequeue, String scitwoqueue){
		tabbedPane = new JTabbedPane();
		tabbedPane.addTab("3mm Sci1", new ScheduleDisplay(tmmFile));
		tabbedPane.addTab("1mm Sci1", new ScheduleDisplay(ommFile));
		tabbedPane.addTab("1cm Sci1", new ScheduleDisplay(cmFile));
		tabbedPane.addTab("1cm Sci2", new ScheduleDisplay(ocmFile));
		tabbedPane.addTab("3mm Sci2", new ScheduleDisplay(nghzFile));
		tabbedPane.addTab("Sci1 Queue", new ScheduleDisplay(scionequeue));
		tabbedPane.addTab("Sci2 Queue", new ScheduleDisplay(scitwoqueue));
		add(tabbedPane);
	}
	JTabbedPane tabbedPane = null;
}

/**
 * class to display an individual schedule file
 * @author friedel
 *
 */
@SuppressWarnings("serial")
class ScheduleDisplay extends JPanel{
	ScheduleDisplay(String fileName){
		String fullList = "";
	    try {
	    	File tmpFile = new File(fileName);
	    	if(fileName != null && tmpFile.exists()){
	    		BufferedReader input =  new BufferedReader(new FileReader(fileName));
	    		String line = null;
	    		while (( line = input.readLine()) != null){
	    			fullList += line + "\n";
	    		}
	    		input.close();
	    	}
		    listing = new JTextArea(fullList);
		    listing.setFont(new Font("monospaced", Font.PLAIN, 12));
		    JScrollPane scrollPane = new JScrollPane(listing);
		    scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
			scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
			scrollPane.setPreferredSize(new Dimension(Globals.WIDTH-75,Globals.HEIGHT-260));
			scrollPane.setMinimumSize(new Dimension(Globals.WIDTH-75,Globals.HEIGHT-260));
			add(scrollPane);
		}
	    catch(Exception e){
	    	new ExceptionHandler(e.toString(),false);
	    }
	}
	private JTextArea listing = null; 
}