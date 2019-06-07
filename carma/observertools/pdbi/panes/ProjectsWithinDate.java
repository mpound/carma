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
 * class for the projects within date tab
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class ProjectsWithinDate extends JPanel{
	/**
	 * constructor
	 * @param pdb an instance of the project database manager
	 */
	public ProjectsWithinDate(ProjectDatabaseManager pdb){
		pwdPane = new JPanel();
		pwdPane.add(new PWDSearchPane(pdb),new GBC(0,0).setAnchor(GBC.CENTER).setWeight(100,100).setFill(GBC.HORIZONTAL));
		jtp = new TogglePane("PWD");
		pwdPane.add(jtp,new GBC(0,1).setAnchor(GBC.CENTER).setWeight(100, 100).setAnchor(GBC.CENTER).setFill(GBC.HORIZONTAL));
		pwdPane.add(new ResultsPane(null,band_,sort_,jtp,sort), new GBC(0,2).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
		pwdPane.setPreferredSize(new Dimension(Globals.WIDTH-2,Globals.HEIGHT-50));
		pwdPane.setMinimumSize(new Dimension(Globals.WIDTH-50,Globals.HEIGHT-50));
		pwdPane.setMaximumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT));
		pwdPane.revalidate();
		add(pwdPane);
	}
	
	/**
	 * method to refresh the panel if anything changes
	 */
	public static void refreshPane(){
		pwdPane.remove(2);
		pwdPane.add(new ResultsPane(projects_,band_,sort_,jtp,sort), new GBC(0,2).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
		pwdPane.revalidate();
		pwdPane.repaint();
	}

	public static void setBand(String band){
		band_ = band;
	}

	public static TogglePane jtp = null;
	public static JPanel pwdPane = null;
	public static Project[] projects_ = null;
	public static String sort_ = "";
	public static String band_ = "ALL";
	public static ResultsPane.SortType sort = ResultsPane.SortType.PRIORITY;
}

/**
 * class for the search part of the panel
 * @author friedel
 *
 */
@SuppressWarnings("serial")
class PWDSearchPane extends JPanel{
	PWDSearchPane(ProjectDatabaseManager pdb){
		this.pdb = pdb;
		setLayout(new GridBagLayout());
		setPreferredSize(new Dimension(Globals.WIDTH,100));
		setMinimumSize(new Dimension(Globals.WIDTH,100));
		setMaximumSize(new Dimension(Globals.WIDTH,100));
		// inputs for the frequency constraints
		JPanel startDate = new JPanel();
		startDate.setLayout(new GridBagLayout());
		startDate.setPreferredSize(new Dimension(200,75));
		startDate.setMinimumSize(new Dimension(200,75));
		startDate.add(new JLabel("YY"),new GBC(2,0).setAnchor(GBC.WEST).setWeight(100,100));
		startDate.add(new JLabel("MM"),new GBC(4,0).setAnchor(GBC.WEST).setWeight(100, 100));
		startDate.add(new JLabel("DD"),new GBC(6,0).setAnchor(GBC.WEST).setWeight(100,100));
		startDate.add(new JLabel("Start Date:"),new GBC(0,1).setAnchor(GBC.WEST).setWeight(100, 100));
		startDate.add(new JLabel("20"),new GBC(1,1).setAnchor(GBC.CENTER).setWeight(100, 100));
		minYearF = new JTextField("06",2);
		minYearF.getDocument().addDocumentListener(new DateFieldListener());
		minYearF.setToolTipText("Set the starting date for the search in YY-MM-DD format");
		startDate.add(minYearF,new GBC(2,1).setAnchor(GBC.WEST).setWeight(100,100));
		startDate.add(new JLabel("-"),new GBC(3,1).setAnchor(GBC.CENTER).setWeight(100, 100));
		minMonthF = new JTextField("01",2);
		minMonthF.getDocument().addDocumentListener(new DateFieldListener());
		minMonthF.setToolTipText("Set the starting date for the search in YY-MM-DD format");
		startDate.add(minMonthF,new GBC(4,1).setAnchor(GBC.WEST).setWeight(100,100));
		startDate.add(new JLabel("-"),new GBC(5,1).setAnchor(GBC.CENTER).setWeight(100, 100));
		minDayF = new JTextField("01",2);
		minDayF.getDocument().addDocumentListener(new DateFieldListener());
		minDayF.setToolTipText("Set the starting date for the search in YY-MM-DD format");
		startDate.add(minDayF,new GBC(6,1).setAnchor(GBC.WEST).setWeight(100,100));
		add(startDate, new GBC(0,0).setAnchor(GBC.EAST).setWeight(100,100));
		
		JPanel endDate = new JPanel();
		endDate.setLayout(new GridBagLayout());
		endDate.setPreferredSize(new Dimension(200,75));
		endDate.setMinimumSize(new Dimension(200,75));
		endDate.add(new JLabel("  End Date:"),new GBC(0,0).setAnchor(GBC.WEST).setWeight(100, 100));
		endDate.add(new JLabel("20"),new GBC(1,0).setAnchor(GBC.WEST).setWeight(100, 100));
		maxYearF = new JTextField("06",2);
		maxYearF.getDocument().addDocumentListener(new DateFieldListener());
		maxYearF.setToolTipText("Set the ending date for the search in YY-MM-DD format");
		endDate.add(maxYearF,new GBC(2,0).setAnchor(GBC.WEST).setWeight(100,100));
		endDate.add(new JLabel("-"),new GBC(3,0).setAnchor(GBC.CENTER).setWeight(100, 100));
		maxMonthF = new JTextField("01",2);
		maxMonthF.getDocument().addDocumentListener(new DateFieldListener());
		maxMonthF.setToolTipText("Set the ending date for the search in YY-MM-DD format");
		endDate.add(maxMonthF,new GBC(4,0).setAnchor(GBC.WEST).setWeight(100,100));
		endDate.add(new JLabel("-"),new GBC(5,0).setAnchor(GBC.CENTER).setWeight(100, 100));
		maxDayF = new JTextField("01",2);
		maxDayF.getDocument().addDocumentListener(new DateFieldListener());
		maxDayF.setToolTipText("Set the ending date for the search in YY-MM-DD format");
		endDate.add(maxDayF,new GBC(6,0).setAnchor(GBC.WEST).setWeight(100,100));
		add(endDate, new GBC(0,1).setAnchor(GBC.EAST).setWeight(100,100));
		JPanel leftP = new JPanel();
		leftP.add(new JLabel("     Include Commissioning projects:"));
		comm = new JCheckBox("",true);
		leftP.add(comm);
		add(leftP,new GBC(1,0).setAnchor(GBC.WEST).setWeight(100, 100));

		JPanel array = new JPanel();
		array.setPreferredSize(new Dimension(200,75));
		array.setMinimumSize(new Dimension(200,75));
		array.add(new JLabel("    Array Configuration:"));
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
			ProjectsWithinDate.sort = ResultsPane.SortType.RRA;
		}
		else if(selected.contains("RA")){
			ProjectsWithinDate.sort = ResultsPane.SortType.RA;
		}
		else if(selected.contains("Rev. LST")){
			ProjectsWithinDate.sort = ResultsPane.SortType.RLST;
		}
		else if(selected.contains("LST")){
			ProjectsWithinDate.sort = ResultsPane.SortType.LST;
		}
		else if(selected.contains("Rev. Time")){
			ProjectsWithinDate.sort = ResultsPane.SortType.RTIME;
		}
		else if(selected.contains("Time")){
			ProjectsWithinDate.sort = ResultsPane.SortType.TIME;
		}
		else if(selected.contains("Rev. Pri")){
			ProjectsWithinDate.sort = ResultsPane.SortType.RPRIORITY;
		}
		else if(selected.contains("Prior")){
			ProjectsWithinDate.sort = ResultsPane.SortType.PRIORITY;
		}
// only update if the value changed		
		if(lastSort != ProjectsWithinDate.sort){
			lastSort = ProjectsWithinDate.sort;
			ProjectsWithinDate.refreshPane();
		}
	}

	/**
	 * method to construct and do the actual search
	 */
	private void doSearch(){
		ArrayList<ItemValue> tempIVS = new ArrayList<ItemValue>();
		ArrayList<ItemValue> temp2IVS = new ArrayList<ItemValue>();
		temp2IVS.add(new ItemValue("exceedTAC","true"));
		tempIVS.add(new ItemValue("trialObservationDate", Conversions.dateConvert(minYearF.getText().trim(),minMonthF.getText().trim(),minDayF.getText().trim())+ "," + Conversions.dateConvert(maxYearF.getText().trim(),maxMonthF.getText().trim(), maxDayF.getText().trim())));
		if(arrayConfig.getSelectedItem() != "Any"){
			tempIVS.add(new ItemValue("arrayConfiguration",arrayConfig.getSelectedItem()));
			temp2IVS.add(new ItemValue("arrayConfiguration",arrayConfig.getSelectedItem()));
		}
		if(!comm.isSelected()){
			CommissioningProjects.excludeCommissioning(tempIVS);
			CommissioningProjects.excludeCommissioning(temp2IVS);
		}

		ItemValue[] ivSeq = (ItemValue[])tempIVS.toArray(new ItemValue[tempIVS.size()]);
		ItemValue[] ivSeq2 = (ItemValue[])temp2IVS.toArray(new ItemValue[temp2IVS.size()]);
		//Project[] ps = null;
		Project[] run1 = null;
		Project[] run2 = null;
		RunQuery rq1 = new RunQuery(pdb,ivSeq);
		RunQuery rq2 = new RunQuery(pdb,ivSeq2);
		// issue the search in a thread in order to avoid indefinite hangs
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
			while((run2 = rq1.checkResults()) == null){
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
		ProjectsWithinDate.projects_ = Operators.mergeProjects(run1,run2);
		ProjectsWithinDate.refreshPane();
	}

	private void checkDate(){
		int day;
		int month;
		int year;
		try{
			day = Integer.parseInt(minDayF.getText().trim());
			month = Integer.parseInt(minMonthF.getText().trim());
			year = Integer.parseInt(minYearF.getText().trim());
			TestDate(day,month,year);
			day = Integer.parseInt(maxDayF.getText().trim());
			month = Integer.parseInt(maxMonthF.getText().trim());
			year = Integer.parseInt(maxYearF.getText().trim());
			TestDate(day,month,year);
		}
		catch(NumberFormatException e){
			if(!(e.toString().contains("\"\"") || e.toString().contains("empty"))){
				new ExceptionHandler(e.toString(), false);
			}
		}
	}

	private void TestDate(int day, int month, int year){
		if(year < 0 || year > 99){
			throw new NumberFormatException("Invalid year (00-99).");
		}
		if(month < 0 || month > 12){
			throw new NumberFormatException("Invalid month (01-12).");
		}
		if(day < 0 || day > 31){
			badDay();
		}
		if(month == 2 && day > 28){
			if(!(year%4 == 0 && day == 29)){
				badDay();
			}
		}
		if((month == 4 || month == 6 || month == 9 || month ==11) && day == 31){
			badDay();
		}
	}
	
	private void badDay(){
		throw new NumberFormatException("Invalid day for given month.");
	}

	private class DateFieldListener implements DocumentListener{
		public void insertUpdate(DocumentEvent event){checkDate();}
		public void removeUpdate(DocumentEvent event){checkDate();}
		public void changedUpdate(DocumentEvent event){checkDate();}			
	}

	private JTextField minDayF = null;
	private JTextField minMonthF = null;
	private JTextField minYearF = null;
	private JTextField maxDayF = null;
	private JTextField maxMonthF = null;
	private JTextField maxYearF = null;
	private Choice arrayConfig = null;
	private JButton update = null;
	private JCheckBox comm = null;
	private ProjectDatabaseManager pdb = null;
	private ReorderPane rp = null;
	private static ResultsPane.SortType lastSort = ResultsPane.SortType.PRIORITY;
}

