package carma.observertools.pdbi.panes;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;

import carma.observertools.*;
import carma.observertools.pdbi.messages.*;
import carma.observertools.pdbi.utils.*;

/**
 * class to look at the status of a project visually
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class ProjectStatusPane extends JPanel{
	public ProjectStatusPane(ProjectDatabaseManager pdb){
		mainPanel = new JPanel();
		pdb_ = pdb;
		setLayout(new GridBagLayout());
		JPanel pID = new JPanel();
		pID.add(new JLabel("Project:"));
		project = new JTextField("",15);
		project.setToolTipText("The project ID (cXXXX, csXXX, cxXXX, ctXXX,...)");
		pID.add(project);
		update = new JButton("Search");
		update.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent event){
				doSearch();
			}
		});
		pID.add(update);
		pID.setPreferredSize(new Dimension(700,30));
		pID.setMinimumSize(new Dimension(700,30));
		pID.setMaximumSize(new Dimension(700,30));
		mainPanel.add(pID, new GBC(0,0).setAnchor(GBC.WEST).setFill(GBC.HORIZONTAL).setWeight(100,100));
		listPane = new JPanel();
		listPane.setBackground(Color.WHITE);
		listPane.add(new JLabel(""));
		listScrollPane = new JScrollPane(listPane);
		listScrollPane.setPreferredSize(prefSize);
		listScrollPane.setMinimumSize(minSize);
		listScrollPane.setMaximumSize(maxSize);
		mainPanel.add(listScrollPane, new GBC(0,1).setAnchor(GBC.WEST).setFill(GBC.BOTH).setWeight(100,100));
		displayPane = new JPanel();
		displayPane.setBackground(Color.WHITE);
		displayScrollPane = new JScrollPane(displayPane);
		displayScrollPane.setPreferredSize(prefSize);
		displayScrollPane.setMinimumSize(minSize);
		displayScrollPane.setMaximumSize(maxSize);
		mainPanel.add(displayScrollPane, new GBC(0,2).setAnchor(GBC.WEST).setFill(GBC.BOTH).setWeight(100,100));
		mainPanel.setPreferredSize(new Dimension(Globals.WIDTH-50,Globals.HEIGHT-50));
		mainPanel.setMinimumSize(new Dimension(Globals.WIDTH-50,Globals.HEIGHT-50));
		mainPanel.setMaximumSize(new Dimension(2100,2000));
		mainPanel.revalidate();
		add(mainPanel);
	}
	
	private static void doSearch(){
		Project[] ps = null;
		ItemValue[] ivSeq = {new ItemValue("project",project.getText())};
		RunQuery rq = new RunQuery(pdb_,ivSeq);
		// issue the query in a thread to avoid indefinite hangs
		try{
			long time = 0;
			while((ps = rq.checkResults()) == null){
				if(time > 120){
					rq.killThread();
					new ExceptionHandler("Query to PDB timed out.",false);
					break;
				}
				time++;
				Thread.sleep(250);
			}
		}
		catch(InterruptedException e){
			new ExceptionHandler(e.toString(),false);
		}
		if(ps == null){
			new ExceptionHandler("Error in query",false);
		}
		if(ps.length != 0){
			TreeListing projectList = new TreeListing(ps[0]);
			mainPanel.remove(2);
			mainPanel.remove(1);
			listPane = new JPanel();
			listPane.add(projectList);
			listScrollPane = new JScrollPane(listPane);
			listScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
			listScrollPane.setPreferredSize(prefSize);
			listScrollPane.setMinimumSize(minSize);
			listScrollPane.setMaximumSize(maxSize);
			mainPanel.add(listScrollPane,new GBC(0,1).setAnchor(GBC.WEST).setFill(GBC.BOTH).setWeight(100,100));
			displayPane = new JPanel();
			displayScrollPane = new JScrollPane(displayPane);
			displayScrollPane.setPreferredSize(prefSize);
			displayScrollPane.setMinimumSize(minSize);
			displayScrollPane.setMaximumSize(maxSize);
			mainPanel.add(displayScrollPane, new GBC(0,2).setAnchor(GBC.WEST).setFill(GBC.BOTH).setWeight(100,100));
			mainPanel.revalidate();
			mainPanel.repaint();
		}
		else{
			mainPanel.remove(2);
			mainPanel.remove(1);
			listPane = new JPanel();
			listPane.add(new JLabel("Project not found"));
			listScrollPane = new JScrollPane(listPane);
			//listScrollPane.setPreferredSize(prefSize);
			listScrollPane.setMinimumSize(minSize);
			//listScrollPane.setMaximumSize(maxSize);
			mainPanel.add(listScrollPane,new GBC(0,1).setAnchor(GBC.WEST).setFill(GBC.BOTH).setWeight(100,100));
			displayPane = new JPanel();
			displayScrollPane = new JScrollPane(displayPane);
			displayScrollPane.setPreferredSize(prefSize);
			displayScrollPane.setMinimumSize(minSize);
			displayScrollPane.setMaximumSize(maxSize);
			mainPanel.add(displayScrollPane, new GBC(0,2).setAnchor(GBC.WEST).setFill(GBC.BOTH).setWeight(100,100));
			mainPanel.revalidate();
			mainPanel.repaint();
		}
	}
	
	public static void refreshPane(){
		mainPanel.remove(2);
		mainPanel.add(displayScrollPane,new GBC(0,2).setAnchor(GBC.WEST).setWeight(100,100));
		mainPanel.revalidate();
		mainPanel.repaint();
	}

	private static class TreeListing extends JPanel implements TreeSelectionListener{
		public TreeListing(Project project){
			project_ = project;
			DefaultMutableTreeNode top = new DefaultMutableTreeNode(new nodeInfo(project_.projectID,"0",Type.PROJECT));
			buildTree(top);
			tree = new JTree(top);
	        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
	        tree.addTreeSelectionListener(this);
//	        tree.setPreferredSize(TprefSize);
	        tree.setMinimumSize(TminSize);
//	        tree.setMaximumSize(TmaxSize);
	        add(tree,new GBC(0,0).setAnchor(GBC.NORTHWEST).setFill(GBC.BOTH));
		}
		
	    public void valueChanged(TreeSelectionEvent e) {
	        DefaultMutableTreeNode node = (DefaultMutableTreeNode)
	                           tree.getLastSelectedPathComponent();

	        if (node == null) return;

	        Object nodeInfo = node.getUserObject();
	        nodeInfo nodeI = (nodeInfo)nodeInfo;
	        if(nodeI.type == Type.PROJECT){
	        	displayProject();
	        }
	        else if(nodeI.type == Type.OBSBLOCK){
	        	displayObsblock(nodeI.ID);
	        }
	        else if(nodeI.type == Type.SUBOBSBLOCK){
	        	displaySubobsblock(nodeI.ID);
	        }
	        else{
	        	displayTrial(nodeI.ID);
	        }
	    }

	    public static void displayProject(){
			displayPane = new JPanel();
			String status = "";
			if(project_.status == ProjectStatus.PSTATUS_COMPLETE){
				status = "<font color=\"#ff0000\">Complete</font>";
			}
			else if(project_.status == ProjectStatus.PSTATUS_INCOMPLETE){
				status = "Incomplete";
			}
			String category = "";
			if(project_.category == ObsCategory.CATEGORY_COMET){
				category = "Comet";
			}
			else if(project_.category == ObsCategory.CATEGORY_EXTRAGALACTIC){
				category = "Extragalactic";
			}
			else if(project_.category == ObsCategory.CATEGORY_GALACTIC){
				category = "Galactic";
			}
			else if(project_.category == ObsCategory.CATEGORY_PLANET){
				category = "Planet";
			}
			else if(project_.category == ObsCategory.CATEGORY_SOLAR){
				category = "Solar";
			}
			else{
				category = "Other";
			}
			String output = "<html><body><table border=\"0\">";
			String too = "No";
			if(project_.isTargetOfOpportunity){
				too = "<font color=\"#0000ff\">Yes</font>";
			}
			String key = "No";
			if(project_.isKeyProject){
				key = "font color=\"#0000ff\">Yes</font>";
			}
			output += "<tr><td>Project: " + project_.projectID + "</td><td>Status: " + status + "</td><td>Category: " + category + "</td><td>ToO:" + too + "</td><td>Key Project:" + key + "</td></tr>";
			output += "<tr><td colspan=\"5\">Title: " + project_.title +"</td></tr>";
			output += "<tr><td>PI Name:" + project_.primaryInvestigator.name +"</td><td>Email:" + project_.primaryInvestigator.email +"</td><td colspan=\"2\">Affil:" + project_.primaryInvestigator.affiliation + "</td></tr>";
			output += String.format("<tr><td colspan=\"4\">%s %5.1f</td></tr>","Total observed Time:",project_.totalTime);
			if(project_.parentProject.length != 0){
				output += "<tr><td colspan=\"5\">Parent projects:<br>";
				for(int i = 0; i < project_.parentProject.length; i++){
					output += project_.parentProject[i] + "<br>";
				}
				output += "</td></tr>";
			}
			if(project_.childProject.length != 0){
				output += "<tr><td colspan=\"5\">Child projects:<br>";
				for(int i = 0; i < project_.childProject.length; i++){
					output += project_.childProject[i] + "<br>";
				}
				output += "</td></tr>";
			}
			output += "<tr><td colspan=\"5\">Abstract:<br>";
			output += StringFormatter.wordWrapHTML(project_.projectAbstract,110);
			output += "</td></tr></table></body></html>";
			displayPane.add(new JLabel(output));
			displayScrollPane = new JScrollPane(displayPane);
			displayScrollPane.setPreferredSize(prefSize);
			displayScrollPane.setMinimumSize(minSize);
			displayScrollPane.setMaximumSize(maxSize);
	    	ProjectStatusPane.refreshPane();
	    }
	    
	    private void displayObsblock(String ID){
	    	Obsblock obs = getObsblock(ID);
			displayPane = new JPanel();
			String output = "<html><body><table border=\"0\">";
			String temp = "INCOMPLETE";
			if(obs.status == ProjectStatus.PSTATUS_COMPLETE){
				temp = "<font color=\"#ff0000\">COMPLETE</font>";
			}
			output += "<tr><td>Name: " + obs.obsblockID + "</td><td>Status: " + temp;
			if(obs.exceedTAC){
				output += "</td><td>exceedTAC: True</td><td></td></tr>";
			}
			else{
				output += "</td><td>exceedTAC: False</td><td></td></tr>";
			}
			if(obs.isFlex){
				temp = "<font color=\"#0000ff\">Yes</font>";
			}
			else{
				temp = "No";
			}
			output += "<tr><td>Band: " + obs.receiverBand + "</td><td>" + String.format("%s %6.2f GHZ","Rest freq.:",obs.restFrequency) + "</td><td>Array :"+ obs.arrayConfiguration + "</td><td>FlexHA :" + temp + "</td></tr>";
			if(obs.observationType == carma.observertools.ObsType.TYPE_CARMA15){
				output += "<tr><td colsapn=\"4\">Obseration Type: CARMA 15</td></tr>";
			}
			else if(obs.observationType == carma.observertools.ObsType.TYPE_CARMA8){
				output += "<tr><td colsapn=\"4\">Obseration Type: CARMA 8</td></tr>";
			}
			else if(obs.observationType == carma.observertools.ObsType.TYPE_CARMA23){
				output += "<tr><td colsapn=\"4\">Obseration Type: CARMA 23</td></tr>";
			}
			else if(obs.observationType == carma.observertools.ObsType.TYPE_PACS){
				output += "<tr><td colsapn=\"4\">Obseration Type: PACS</td></tr>";
			}
			else if(obs.observationType == carma.observertools.ObsType.TYPE_SINGLEPOL){
				output += "<tr><td colsapn=\"4\">Obseration Type: Single Polarization</td></tr>";
			}
			else if(obs.observationType == carma.observertools.ObsType.TYPE_DUALPOL){
				output += "<tr><td colsapn=\"4\">Obseration Type: Dual Polarization</td></tr>";
			}
			else if(obs.observationType == carma.observertools.ObsType.TYPE_FULLPOL){
				output += "<tr><td colsapn=\"4\">Obseration Type: Full Polarization</td></tr>";
			}
			else if(obs.observationType == carma.observertools.ObsType.TYPE_MAXSENS_DUALPOL){
				output += "<tr><td colsapn=\"4\">Obseration Type: Maximum Sensitivity Dual Polarization</td></tr>";
			}
			else if(obs.observationType == carma.observertools.ObsType.TYPE_MAXSENS_CARMA23){
				output += "<tr><td colsapn=\"4\">Obseration Type: Maximum Sensitivity CARMA 23</td></tr>";
			}
			else if(obs.observationType == carma.observertools.ObsType.TYPE_MAXSENS_LL){
				output += "<tr><td colsapn=\"4\">Obseration Type: Maximum Sensitivity LL</td></tr>";
			}
			else{
				output += "<tr><td colsapn=\"4\">Obseration Type: Single Polarization</td></tr>";
			}
			output += String.format("<tr><td>%s %5.1f</td><td>%s %5.1f </td><td>%s %5.1f</td><td></td></tr>","Allocated Time:",obs.minAllocatedTime,"Observed Time:",obs.totalObsTime,"Remaining Time:",obs.remainingTime);
			output += "<tr><td colspan=\"4\">Observable RA Range: " + Conversions.RadiansToTime(obs.lowRa) + " - " + Conversions.RadiansToTime(obs.highRa) + "</td></tr>";
			String[] hac = obs.actualHourAngleCoverage.trim().split(",");
			String cover = "";
			if(hac.length == 0 || obs.actualHourAngleCoverage.contains("NODE_NOT_FOUND") || obs.actualHourAngleCoverage == null || obs.actualHourAngleCoverage.trim().equals("")){
				cover = "None found";
			}
			else{
				double ra = Conversions.RadiansToHours(obs.subObsblock[0].trial[0].source[0].ra);
				for(int i = 0; i < hac.length; i++){
					int index = hac[i].indexOf("-",1);
					if(i != 0){
						cover += ",";
					}
					if(index == -1){
						cover += Conversions.RadiansToHours(Double.parseDouble(hac[i])+ra);
					}
					else{
						cover += Conversions.HoursToTime(Double.parseDouble(hac[i].substring(0,index)) + ra) + "-" + Conversions.HoursToTime(Double.parseDouble(hac[i].substring(index+1)) + ra);
					}
				}
			}
			output += "<tr><td colspan=\"4\">Actual RA coverage: " + cover + "</td></tr>";
			if(obs.parentObsblock.length != 0){
				output += "<tr><td colspan=\"4\">Parent obsblocks:<br>";
				for(int i = 0; i < obs.parentObsblock.length; i++){
					output += obs.parentObsblock[i] + "<br>";
				}
				output += "</td></tr>";
			}
			if(obs.childObsblock.length != 0){
				output += "<tr><td colspan=\"4\">Child obsblocks:<br>";
				for(int i = 0; i < obs.childObsblock.length; i++){
					output += obs.childObsblock[i] + "<br>";
				}
				output += "</td></tr></table></body></html>";
			}
		
			displayPane.add(new JLabel(output));
			displayScrollPane = new JScrollPane(displayPane);
			displayScrollPane.setPreferredSize(prefSize);
			displayScrollPane.setMinimumSize(minSize);
			displayScrollPane.setMaximumSize(maxSize);
	    	ProjectStatusPane.refreshPane();
	    }
	    
	    private void displaySubobsblock(String ID){
	    	SubObsblock sub = getSubObsblock(ID);
			String output = "<html><body><table border=\"0\">";
			String temp = "INCOMPLETE";
			if(sub.status == ProjectStatus.PSTATUS_COMPLETE){
				temp = "<font color=\"#ff0000\">COMPLETE</font>";
			}
			output += "<tr><td>Name: " + sub.subObsblockID + "</td><td>Status: " + temp + "</td></tr>";
			if(sub.parentSubObsblock.length != 0){
				output += "<tr><td colspan=\"2\">Parent subobsblocks:<br>";
				for(int i = 0; i < sub.parentSubObsblock.length; i++){
					output += sub.parentSubObsblock[i] + "<br>";
				}
				output += "</td></tr>";
			}
			if(sub.childSubObsblock.length != 0){
				output += "<tr><td colspan=\"2\">Child subobsblocks:<br>";
				for(int i = 0; i < sub.childSubObsblock.length; i++){
					output += sub.childSubObsblock[i] + "<br>";
				}
				output += "</td></tr></table></body></html>";
			}

			displayPane = new JPanel();
			displayPane.add(new JLabel(output));
			displayScrollPane = new JScrollPane(displayPane);
			displayScrollPane.setPreferredSize(prefSize);
			displayScrollPane.setMinimumSize(minSize);
			displayScrollPane.setMaximumSize(maxSize);
	    	ProjectStatusPane.refreshPane();
	    }
	    
	    private void displayTrial(String ID){
	    	Trial trial = getTrial(ID);
			String temp = "INCOMPLETE";
			if(trial.status == ProjectStatus.PSTATUS_COMPLETE){
				temp = "<font color=\"#ff0000\">COMPLETE</font>";
			}
	    	String output = "<html><body><table border=\"0\">";
	    	output += "<tr><td>Trial: " + trial.trialID + "</td><td>Status: " + temp + "</td></tr>";
	    	String[] time = trial.trialObservationDateStart.split("T");
	    	output += "<tr><td>Observation Length: " + trial.trialObservationLength + "</td><td>Start: " + time[0] + "    " + time[1] + " UT    (" + Conversions.RadiansToTime(trial.observedLSTstart) + " LST)</td></tr>";
	    	time = trial.trialObservationDateEnd.split("T");
	    	output += "<tr><td></td><td>End: " + time[0] + "    " + time[1] + " UT    (" + Conversions.RadiansToTime(trial.observedLSTend) + " LST)</td></tr>";
	    	output += "<tr><td>Image Quality or Signal-to-Noise ratio: " + trial.imgVsSnr + "</td></tr>";
	    	for(int i = 0; i < trial.correlator.length; i++){
	    		output += "<tr><td colspan=\"2\">Correlator Bandwidths " + i+1 + " (MHZ): ";
	    		for(int j = 0; j < trial.correlator[i].window.length; j++){
	    			if(j != 0){
	    				output += ",";
	    			}
	    			output += String.format("%7.3f",trial.correlator[i].window[j].bandwidth);
	    		}
	    		output += "</td><tr>";
	    	}
	    	output += String.format("<tr><td>Average tau: %4.2f</td><td>Avgerage RMS Phase: %4.0f</td></tr>",trial.averageOpacity,trial.averagePhase);
	    	output += String.format("<tr><td>Quality Grade: %5.1f</td><td>Observer Grade: %5.1f</td></tr>",trial.dqaOverallGrade,trial.obsGrade);
	    	output += "<tr><td colspan=\"2\">" + StringFormatter.wordWrapHTML(trial.obsComments,110) + "</td></tr></table></body></html>";
	    	displayPane = new JPanel();
			displayPane.add(new JLabel(output));
			displayScrollPane = new JScrollPane(displayPane);
			displayScrollPane.setPreferredSize(prefSize);
			displayScrollPane.setMinimumSize(minSize);
			displayScrollPane.setMaximumSize(maxSize);
	    	ProjectStatusPane.refreshPane();
	    }
		
		private void buildTree(DefaultMutableTreeNode top){
			for(int i = 0; i < project_.obsblock.length; i++){
				Obsblock obs = project_.obsblock[i];
				DefaultMutableTreeNode obsblock = new DefaultMutableTreeNode(new nodeInfo(obs.obsblockID,"" + i,Type.OBSBLOCK));
				top.add(obsblock);
				for(int j = 0; j < obs.subObsblock.length; j++){
					SubObsblock sub = obs.subObsblock[j];
					DefaultMutableTreeNode subObs = new DefaultMutableTreeNode(new nodeInfo(sub.subObsblockID,i + "." + j,Type.SUBOBSBLOCK));
					obsblock.add(subObs);
					for(int k = 0; k < sub.trial.length; k++){
						Trial trial = sub.trial[k];
						DefaultMutableTreeNode trl = new DefaultMutableTreeNode(new nodeInfo("" +trial.trialID,i + "." + j + "." + k,Type.TRIAL));
						subObs.add(trl);
					}
				}
			}
		}
		
		private class nodeInfo {
			private String name;
			private String ID;
			private Type type;
			
			public nodeInfo(String name, String ID, Type type){
				if(name.length() == 0){
					this.name = "<i>(default)</i></body></html>";
				}
				else{
					this.name = name;
				}
				this.ID = ID;
				this.type = type;
			}
			
			public String toString(){
				String header = "";
				if(type == Type.PROJECT){
					header = "Project: ";
				}
				else if(type == Type.OBSBLOCK){
					header = "Obsblock: ";
				}
				else if(type == Type.SUBOBSBLOCK){
					header = "<html><body>Subobsblock: ";
				}
				else{
					header = "Trial: ";
				}
				if(type == Type.OBSBLOCK){
					Obsblock obs = getObsblock(ID);
					return String.format("%s %-30s  %s  %5.1f/%5.1f   %s %1s   %s %s-%s",header,name,"Alloc./Remain. time:",obs.minAllocatedTime,obs.remainingTime,"Array:",obs.arrayConfiguration,"LST Range:",Conversions.RadiansToTime(obs.lowRa),Conversions.RadiansToTime(obs.highRa));
				}
				else if(type == Type.TRIAL){
					Trial trial = getTrial(ID);
					return String.format("%s %-3s  %s %s    %s %s-%s    %s %4.1f    %s %4.1f    %s %4.2f    %s %4.0f",header,name,"Date:",trial.trialObservationDateStart.split("T")[0],"LST:",Conversions.RadiansToTime(trial.observedLSTstart),Conversions.RadiansToTime(trial.observedLSTend),"Length:",trial.trialObservationLength,"Grade:",trial.obsGrade,"Tau:",trial.averageOpacity,"Phase:",trial.averagePhase);
				}
				return header + name;
			}
		}
		
		private Obsblock getObsblock(String ID){
			return project_.obsblock[Integer.parseInt(ID)];
		}
		
		private SubObsblock getSubObsblock(String ID){
			String[] ids = ID.split("\\.",2);
			return project_.obsblock[Integer.parseInt(ids[0])].subObsblock[Integer.parseInt(ids[1])];
		}
		
		private Trial getTrial(String ID){
			String[] ids = ID.split("\\.",3);
			return project_.obsblock[Integer.parseInt(ids[0])].subObsblock[Integer.parseInt(ids[1])].trial[Integer.parseInt(ids[2])];
		}
		
		private static Project project_ = null;
		private static JTree tree = null;
		private enum Type {PROJECT,OBSBLOCK,SUBOBSBLOCK,TRIAL};
	}

	
	private static ProjectDatabaseManager pdb_ = null;
	private static JTextField project = null;
	private static JButton update = null;
	private static JScrollPane listScrollPane = null;
	private static JScrollPane displayScrollPane = null;
	private static JPanel listPane = null;
	private static JPanel displayPane = null;
	private static JPanel mainPanel = null;
	private static final Dimension prefSize = new Dimension(Globals.WIDTH-50,325);
	private static final Dimension minSize = new Dimension(Globals.WIDTH-50,325);
	private static final Dimension maxSize = new Dimension(Globals.WIDTH-50,Globals.HEIGHT);
	private static final Dimension TminSize = new Dimension(Globals.WIDTH-80,300);
}

