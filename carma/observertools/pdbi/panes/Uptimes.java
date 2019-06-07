package carma.observertools.pdbi.panes;

import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import carma.observertools.*;
import carma.observertools.pdbi.messages.ExceptionHandler;
import carma.observertools.pdbi.utils.*;
import carma.observertools.pdbi.utils.comparators.RACompare;

/**
 * class for the uptimes tab
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class Uptimes extends JPanel{
	/**
	 * constructor
	 * @param pdb the project database manager instance
	 */
	public Uptimes(ProjectDatabaseManager pdb){
		pdb_ = pdb;
		upPane = new JPanel();
		upPane.add(new UPTSearchPane(),new GBC(0,0).setAnchor(GBC.CENTER).setWeight(100,100).setFill(GBC.HORIZONTAL));
		jtp = new TogglePane("UPT");
		upPane.add(jtp,new GBC(0,1).setAnchor(GBC.CENTER).setWeight(100, 100).setAnchor(GBC.CENTER).setFill(GBC.HORIZONTAL));
		upPane.add(new UPResultsPane(projects_,band_,jtp), new GBC(0,2).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
		upPane.setPreferredSize(new Dimension(Globals.WIDTH-2,Globals.HEIGHT-50));
		upPane.setMinimumSize(new Dimension(Globals.WIDTH-50,Globals.HEIGHT-50));
		upPane.setMaximumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT));
		upPane.revalidate();
		add(upPane);
	}
	
	public static void setBand(String band){
		band_ = band;
	}

	/**
	 * method to refresh the panel when things change
	 */
	public static void refreshPane(){
		upPane.remove(2);
		upPane.add(new UPResultsPane(projects_,band_,jtp), new GBC(0,2).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
		upPane.revalidate();
		upPane.repaint();
	}
	
	public static ProjectDatabaseManager pdb_ = null;
	private static JPanel upPane = null;
	public static String band_ = "ALL";
	public static Project[] projects_ = null;
	public static TogglePane jtp = null;
}

/**
 * class for the search panel
 * @author friedel
 *
 */
@SuppressWarnings("serial")
class UPTSearchPane extends JPanel{
	public UPTSearchPane(){
		JPanel array = new JPanel();
		array.add(new JLabel("Array Configuration:"));
		arrayConfig = new ArrayConfiguration();
		array.add(arrayConfig);
		add(array,new GBC(1,1).setAnchor(GBC.WEST).setWeight(100, 100));
		update = new JButton("Update");
		update.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent event){
				doSearch();
			}
		});
		add(update,new GBC(2,0,1,2).setAnchor(GBC.SOUTHWEST).setWeight(100, 100));
	}
	
	/**
	 * method to create and implement the actual search
	 */
	public static void doSearch(){
		ArrayList<ItemValue> tempIVS = new ArrayList<ItemValue>();
		tempIVS.add(new ItemValue("obsblockStatus","INCOMPLETE"));
		if(arrayConfig.getSelectedItem() != "Any"){
			tempIVS.add(new ItemValue("arrayConfiguration",arrayConfig.getSelectedItem()));
		}
		CommissioningProjects.excludeCommissioning(tempIVS);
		ItemValue[] ivSeq = (ItemValue[])tempIVS.toArray(new ItemValue[tempIVS.size()]);

		Uptimes.projects_ = null;
		RunQuery rq = new RunQuery(Uptimes.pdb_,ivSeq);
		// issue the search in a thread in order to avoid indefinite hangs
		try{
			long time = 0;
			while((Uptimes.projects_ = rq.checkResults()) == null){
				if(time > 120){
					rq.killThread();
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
		if(Uptimes.projects_ == null){
			new ExceptionHandler("Error in query",false);
		}
		Uptimes.refreshPane();
	}
	
	private static Choice arrayConfig = null;
	private static JButton update = null;
}

/**
 * class to display the results
 * @author friedel
 *
 */
@SuppressWarnings("serial")
class UPResultsPane extends JPanel{
	public UPResultsPane(Project[] projects, String band, TogglePane jtp){
		band_ = band;
		setPreferredSize(new Dimension(Globals.WIDTH,Globals.HEIGHT-200));
		setMinimumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT-200));
		setMaximumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT-200));
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(),"Obsblocks Found"));
		Tobsblock_ = new ArrayList<Obsblock_I>();
		T2obsblock_ = new ArrayList<Obsblock_I>();
		maxSize = 0;
		int omm = 0;
		int tmm = 0;
		int ocm = 0;
		int ftp = 0;
		if(projects != null){
			for(int i = 0; i < projects.length; i++){
				for(int j = 0; j < projects[i].obsblock.length; j++){
					if(projects[i].obsblock[j].exceedTAC){
					    T2obsblock_.add(new Obsblock_I(projects[i].projectID,projects[i].obsblock[j],projects[i].isKeyProject,projects[i].isFastTrack));
					}
					else{
					    Tobsblock_.add(new Obsblock_I(projects[i].projectID,projects[i].obsblock[j],projects[i].isKeyProject,projects[i].isFastTrack));
					}
					if(projects[i].obsblock[j].receiverBand.contains("1MM")){
						omm++;
					}
					else if(projects[i].obsblock[j].receiverBand.contains("1CM")){
					    ocm++;
					}
					else{
						tmm++;
					}
					if(projects[i].isFastTrack){
					    ftp++;
					}
					maxSize = Math.max(maxSize, (projects[i].obsblock[j].obsblockID.length()));
				}
			}
		}
		jtp.onefield.setText("" + omm);
		jtp.threefield.setText("" + tmm);
		jtp.onecmfield.setText("" + ocm);
		jtp.fastTrackfield.setText("" + ftp);
		jtp.allfield.setText("" + (tmm+omm+ocm));
		obsblock_ = (Obsblock_I[])Tobsblock_.toArray(new Obsblock_I[Tobsblock_.size()]);
		Arrays.sort(obsblock_,new RACompare());
		if(T2obsblock_ != null){
			Eobsblock_ = (Obsblock_I[])T2obsblock_.toArray(new Obsblock_I[T2obsblock_.size()]);
			Arrays.sort(Eobsblock_,new RACompare());
		}
		
		setLayout(new GridBagLayout());
		makePanel();
		// labels for columns
		JLabel topLabel = new JLabel("Obsblock                                                                 Time Left               0                4                 8                12               16             20 (LST)");
		topLabel.setPreferredSize(new Dimension(750,20));
		add(topLabel, new GBC(0,0).setFill(GBC.HORIZONTAL).setWeight(100,100));
		JScrollPane scrollPane = new JScrollPane(resultsList);
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		scrollPane.setPreferredSize(new Dimension(Globals.WIDTH,Globals.HEIGHT-350));
		scrollPane.setMinimumSize(new Dimension(Globals.WIDTH,Globals.HEIGHT-350));
		add(scrollPane, new GBC(0,1).setFill(GBC.BOTH).setWeight(100,100));
	}
	
	/**
	 * method that constructs the actual result entries
	 */
	private static void makePanel(){
		resultsList = new JPanel();
		resultsList.setLayout(new GridBagLayout());
		int m = 0;
		String stringFormat = "%-" + maxSize +"s";
		for(Obsblock_I obsblock : obsblock_){
		    if((obsblock.obsblock_.receiverBand.contains(band_) || band_.contains("ALL")) || (obsblock.isFastTrack && band_.contains("FT"))){
				JPanel tempPanel = new JPanel();
				if((m%2) == 0){
				    if(obsblock.isFastTrack){
					tempPanel.setBackground(Color.ORANGE.darker());
				    }
				    else{
					tempPanel.setBackground(Color.LIGHT_GRAY);
				    }
				}
				else{
				    if(obsblock.isFastTrack){
					tempPanel.setBackground(Color.ORANGE.brighter());
				    }
				    else{
					tempPanel.setBackground(Color.WHITE);
				    }
				}

				JLabel obsLabel = new JLabel(String.format(stringFormat,obsblock.pid_ + "." + obsblock.obsblock_.obsblockID));
				obsLabel.setPreferredSize(new Dimension(250,18));
				obsLabel.setMinimumSize(new Dimension(250,18));
				obsLabel.setMaximumSize(new Dimension(250,18));
				obsLabel.setToolTipText("Obsblock name <project>.<obsblock>");

				JLabel timeLabel = new JLabel(String.format("%7.1f", obsblock.obsblock_.remainingTime));
				timeLabel.setPreferredSize(new Dimension(40,18));
				timeLabel.setMinimumSize(new Dimension(40,18));
				timeLabel.setMaximumSize(new Dimension(40,18));
				timeLabel.setToolTipText("Hours remaining to be observed");
				
				tempPanel.setLayout(new GridBagLayout());
				tempPanel.add(obsLabel, new GBC(0,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100));
				tempPanel.add(timeLabel,new GBC(1,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100));
				PlotUptimes pu = new PlotUptimes(obsblock.obsblock_.lowRa,obsblock.obsblock_.highRa,obsblock.obsblock_.receiverBand);
				pu.setToolTipText("Obsblock " + obsblock.pid_ + "." + obsblock.obsblock_.obsblockID + pu.getToolTip());
				tempPanel.add(pu,new GBC(2,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100));
				resultsList.add(tempPanel, new GBC(0,m).setFill(GBC.BOTH).setAnchor(GBC.WEST).setWeight(100,100).setWeight(100,100));
				m++;
			}
		}
		if(Eobsblock_ != null){
			if(Eobsblock_.length > 0){
				JPanel tempPanel = new JPanel();
				tempPanel.setBackground(Color.BLACK);
				resultsList.add(tempPanel, new GBC(0,m).setFill(GBC.BOTH).setAnchor(GBC.WEST).setWeight(100,100));
				m++;
			}
			for(Obsblock_I obsblock : Eobsblock_){
				if(obsblock.obsblock_.receiverBand.contains(band_) || band_.contains("ALL")){
					JPanel tempPanel = new JPanel();
					if((m%2) == 0){
						tempPanel.setBackground(Color.LIGHT_GRAY);
					}
					else{
						tempPanel.setBackground(Color.WHITE);
					}

					JLabel obsLabel = new JLabel(String.format(stringFormat,obsblock.pid_ + "." + obsblock.obsblock_.obsblockID));
					obsLabel.setPreferredSize(new Dimension(250,18));
					obsLabel.setMinimumSize(new Dimension(250,18));
					obsLabel.setMaximumSize(new Dimension(250,18));
					obsLabel.setToolTipText("Obsblock name <project>.<obsblock>");

					JLabel timeLabel = new JLabel(String.format("%5.1f", obsblock.obsblock_.remainingTime));
					timeLabel.setPreferredSize(new Dimension(30,18));
					timeLabel.setMinimumSize(new Dimension(30,18));
					timeLabel.setMaximumSize(new Dimension(30,18));
					timeLabel.setToolTipText("Hours remaining to be observed");
					
					tempPanel.setLayout(new GridBagLayout());
					tempPanel.add(obsLabel, new GBC(0,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100));
					tempPanel.add(timeLabel,new GBC(1,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100));
					PlotUptimes pu = new PlotUptimes(obsblock.obsblock_.lowRa,obsblock.obsblock_.highRa,obsblock.obsblock_.receiverBand);
					pu.setToolTipText("Obsblock " + obsblock.pid_ + "." + obsblock.obsblock_.obsblockID + pu.getToolTip());
					tempPanel.add(pu,new GBC(2,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100,100));
					resultsList.add(tempPanel, new GBC(0,m).setFill(GBC.BOTH).setAnchor(GBC.WEST).setWeight(100,100).setWeight(100,100));
					m++;
				}
			}
		}
		// fill out the display if there are less then 10 entries
		if(m < 18){
			for(int i = m; i < 18; i++){
				JPanel tempPanel = new JPanel();
				if((i%2) == 0){
					tempPanel.setBackground(Color.LIGHT_GRAY);
				}
				else{
					tempPanel.setBackground(Color.WHITE);
				}
				resultsList.add(tempPanel, new GBC(0,i).setFill(GBC.BOTH).setAnchor(GBC.WEST).setWeight(100,100));
			}
		}
	}
	
	private static int maxSize = 0;
	private static ArrayList<Obsblock_I>Tobsblock_ = null;
	private static ArrayList<Obsblock_I>T2obsblock_ = null;
	private static Obsblock_I[] obsblock_ = null;
	private static Obsblock_I[] Eobsblock_ = null;
	private static JPanel resultsList = null;
	private static String band_ = "ANY";
}