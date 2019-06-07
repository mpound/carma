package carma.observertools.pdbi.messages;

import java.awt.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;

import carma.observertools.pdbi.utils.*;

/**
 * Class that displays the help panel for the Project Database Interface
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class HelpPanel extends JDialog{
	/**
	 * Main Constructor
	 * @param owner
	 */
	public HelpPanel(JFrame owner){
		super(owner,"Project Database Interface Help",true);
		mainPanel = new JPanel();
		mainPanel.setLayout(new GridBagLayout());
		HelpMenu hm = new HelpMenu();
		hm.setPreferredSize(new Dimension(185,600));
		hm.setMinimumSize(new Dimension(185,600));
		hm.setMaximumSize(new Dimension(185,600));		
		hm.setBackground(Color.WHITE);
		mainPanel.add(hm, new GBC(0,0).setAnchor(GBC.WEST).setWeight(100,100));
		helpText = new WelcomeScreen();
		helpScroll = new JScrollPane(helpText);
		helpScroll.setPreferredSize(new Dimension(415,590));
		helpScroll.setMinimumSize(new Dimension(415,590));
		helpScroll.setMaximumSize(new Dimension(415,590));
		mainPanel.add(helpScroll,new GBC(1,0).setAnchor(GBC.WEST).setWeight(100,100));
		mainPanel.revalidate();
		mainPanel.repaint();
		add(mainPanel);
		setSize(600,600);
	}
	
	/**
	 * method to refresh the pane when it changes content
	 */
	public static void refreshPane(){
		mainPanel.remove(1);
		mainPanel.add(helpScroll,new GBC(1,0).setAnchor(GBC.WEST).setWeight(100,100));
		mainPanel.revalidate();
		mainPanel.repaint();
	}

	/**
	 * private class to display the help menu
	 * @author friedel
	 *
	 */
	private static class HelpMenu extends JPanel implements TreeSelectionListener{
		public HelpMenu(){
			DefaultMutableTreeNode top = new DefaultMutableTreeNode(new HelpFile("Help Menu",new Blank()));
			top.add(new DefaultMutableTreeNode(new HelpFile("Uptimes",new Upt())));
			top.add(new DefaultMutableTreeNode(new HelpFile("Projects within Range", new PWR())));
			top.add(new DefaultMutableTreeNode(new HelpFile("Projects within Date",new PWD())));
			top.add(new DefaultMutableTreeNode(new HelpFile("Projects Starting At",new PSA())));
			top.add(new DefaultMutableTreeNode(new HelpFile("Grade Projects",new GP())));
			top.add(new DefaultMutableTreeNode(new HelpFile("ProjectStatus",new PS())));
			top.add(new DefaultMutableTreeNode(new HelpFile("Schedule",new SCH())));

			tree = new JTree(top);
	        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
	        tree.addTreeSelectionListener(this);
	        add(tree, new GBC(0,0).setAnchor(GBC.NORTHWEST).setWeight(100,100));
		}
		public void valueChanged(TreeSelectionEvent e) {
	        DefaultMutableTreeNode node = (DefaultMutableTreeNode)
	                           tree.getLastSelectedPathComponent();
	        if (node == null) return;
	        Object nodeInfo = node.getUserObject();
	        HelpFile nodeI = (HelpFile)nodeInfo;
	        helpText = nodeI.jp;
	        helpScroll = new JScrollPane(helpText);
			helpScroll.setPreferredSize(new Dimension(415,590));
			helpScroll.setMinimumSize(new Dimension(415,590));
			helpScroll.setMaximumSize(new Dimension(415,590));

	        HelpPanel.refreshPane();
	    }
		
		/**
		 * class to hold the data for each node
		 * @author friedel
		 *
		 */
		private class HelpFile{
			public HelpFile(String name, JPanel panel){
				helpName = name;
				jp = panel;
			}
			public String toString(){
				return helpName;
			}
		
			public String helpName;
			public JPanel jp;
		}
		
		/**
		 * the blank welcome screen
		 * @author friedel
		 *
		 */
		private class Blank extends JPanel{
			public Blank(){
				setBackground(Color.WHITE);
				setPreferredSize(minSize);
				setMinimumSize(minSize);
				setMaximumSize(minSize);

				add(new JLabel("<html><body><h1>Welcome to PDBI help</h1></body></html>"));
			}
		}
		
		/**
		 * the help screen for grading projects
		 * @author friedel
		 *
		 */
		private class GP extends JPanel{
			public GP(){
				setBackground(Color.WHITE);
				setMinimumSize(minSize);
				String output = "";
				output += "<html><body><h1>Grading Projects</h1>";
				output += StringFormatter.wordWrapHTML("To grade a trial type the obsblock name in the format &lt;project&gt;.&lt;obsblock&gt;.&lt;subObsblock&gt;, where the subObsblock is only necessary if there is one present, in the \"Obsblock\" field. Also put the trial number in the \"Trial\" field. Next put your initials, the length of time your are giving to the observastions, and the letter grade in the appropriate fields. If you wish to replace ALL existing comments then select \"Replace existing comments\", otherwise leave the selection on \"Append to existing comments.\" Finally type your comments into the comment box and click on \"Update\" to submit them.",58);
				output += "<br><br>";
				output += StringFormatter.wordWrapHTML("To search for trials that need grading click the \"Search\" button in the Need Grades box. Results are displayed in the list below. Any trial needing a Quality grade will have a button with \"Q\" in it. Click the button to launch a quality window. Any trial needing an observer grade will have a button with \"O\", click the button to transfer the obsblock name to the grading section.", 58);
				output += "</body></html>";
				add(new JLabel(output));
			}
		}
		
		/**
		 * the help screen for project status
		 * @author friedel
		 *
		 */
		private class PS extends JPanel{
			public PS(){
				setBackground(Color.WHITE);
				setMinimumSize(minSize);
				String output = "";
				output += "<html><body><h1>Project Status</h1>";
				output += StringFormatter.wordWrapHTML("The project status tab is used to look at the status of all obsblocks from a given project.",50);
				output += "<br><br>" + StringFormatter.wordWrapHTML("Simply type the project id (i.e. c0001) into the \"Project\" field and click on \"Search\". The results will be displayed in the upper panel. Highlighting (by clicking) on any of the entries will give more detailed information in the lower panel. You can also expand each obsblock and subObsblock by clicking on the  expansion icon next to the folder icon. The results are displayed in increasing depth (project, obsblock, subObsblock, trial).", 58);
				output += "</body></html>";
				add(new JLabel(output));
			}
		}
		
		/**
		 * the help screen for projects starting at
		 * @author friedel
		 *
		 */
		private class PSA extends JPanel{
			public PSA(){
				setLayout(new GridBagLayout());
				setBackground(Color.WHITE);
				setMinimumSize(minSize);
				String output = "";
				output += "<html><body><h1>Projects Starting At</h1>";
				output += StringFormatter.wordWrapHTML("The projects starting at tab is used to search for any observable project within X hours of a given LST. The LST is specified in the \"Start LST\" boxes (hours:minutes) and the hour range (LST-HA to LST+HA) is specified in the \"HA Limit\" box. You can also specify to only have projects with at least \"Min. hours left\" hours remaining to be observed. Also specify the array configuration in the pull down menu and whether to include Flex HA projects in the results. Click \"Search\" to search the database. The results are currently in priority order. The results can be sorted by frequency band with the \"1 MM\", \"3 MM\" and \"All\" buttons. The number of obsblocks in each frequency band are given in the boxes below each button.",58);
				output += "<br><br>" + StringFormatter.wordWrapHTML("The results are presented with the following columns:", 58);
				output += "<ol><li>" + StringFormatter.wordWrapHTML("", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Obsblock ID in the format &lt;project&gt;.&lt;obsblock&gt;", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Frequency band (1MM or 3MM)", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Time Left - number of hours remaining to be observed", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("RA - RA of the source", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("LST Range - the LST times during which the source can be observed", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Flex - whether the obsblock is FlexHA (green box) or not (red box)", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Scal - whether the source is selfcalibratable (green box) or not (red box)", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Script - whether the script to run this obsblock is (green box) or is not (red box) present in the current script directory. Note that it assumes the typical naming convention of &lt;project&gt;_&lt;obsblock&gt;.obs.", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("An graphical overview of when the source can be and has been observed. The LST is marked every 4 hours with a black line. The <font color=\"#ff0000\">red</font> line marks when the source can be observed and the <font color=\"#0000ff\">blue</font> bar(s) represent when the source has already been observed by previous tracks. In the example below the source is observable from 05:00 to 16:00 LST and has been observed during 06:00-07:00 LST and 09:00-14:00 LST. ", 45);
				output += "</ol></body></html>";
				add(new JLabel(output), new GBC(0,0).setAnchor(GBC.NORTH).setWeight(100,100));
				add(new PlotCoverage(Conversions.twoPI*5.0/24.0,Conversions.twoPI*16.0/24.0,Conversions.twoPI*10.5/24.0,"-4.5--3.5,-0.5-4.5"), new GBC(0,1).setAnchor(GBC.NORTH).setWeight(100,100));

			}
		}
		
		/**
		 * the help screen for projects within date
		 * @author friedel
		 *
		 */
		private class PWD extends JPanel{
			public PWD(){
				setLayout(new GridBagLayout());
				setBackground(Color.WHITE);
				setMinimumSize(minSize);
				String output = "<html><body><h1>Projects Within Date</h1>";
				output += "The projects within date panel is used to dispaly all projects run during the given date range. The date range is given in the \"Start Date\" and \"End Date\" entries in the format of YY-MM-DD (all numeric). The search can be narrowed to a single array configuration by selecting one from the pull down menu. Also commissioning projects can be included by checking the appropriate box. Click on \"Search\" to search the database. The results can be sorted by frequency band with the \"1 MM\", \"3 MM\" and \"All\" buttons. The number of obsblocks in each frequency band are given in the boxes below each button.";
				output += "<br><br>" + StringFormatter.wordWrapHTML("The results are presented with the following columns:", 58);
				output += "<ol><li>" + StringFormatter.wordWrapHTML("", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Obsblock ID in the format &lt;project&gt;.&lt;obsblock&gt;", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Frequency band (1MM or 3MM)", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Time Left - number of hours remaining to be observed", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("RA - RA of the source", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("LST Range - the LST times during which the source can be observed", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Flex - whether the obsblock is FlexHA (green box) or not (red box)", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Scal - whether the source is selfcalibratable (green box) or not (red box)", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Script - whether the script to run this obsblock is (green box) or is not (red box) present in the current script directory. Note that it assumes the typical naming convention of &lt;project&gt;_&lt;obsblock&gt;.obs.", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("An graphical overview of when the source can be and has been observed. The LST is marked every 4 hours with a black line. The <font color=\"#ff0000\">red</font> line marks when the source can be observed and the <font color=\"#0000ff\">blue</font> bar(s) represent when the source has already been observed by previous tracks. In the example below the source is observable from 05:00 to 16:00 LST and has been observed during 06:00-07:00 LST and 09:00-14:00 LST. ", 45);
				output += "</ol></body></html>";
				add(new JLabel(output), new GBC(0,0).setAnchor(GBC.NORTH).setWeight(100,100));
				add(new PlotCoverage(Conversions.twoPI*5.0/24.0,Conversions.twoPI*16.0/24.0,Conversions.twoPI*10.5/24.0,"-4.5--3.5,-0.5-4.5"), new GBC(0,1).setAnchor(GBC.NORTH).setWeight(100,100));
			}
		}
		
		/**
		 * the help screen for projects within range
		 * @author friedel
		 *
		 */
		private class PWR extends JPanel{
			public PWR(){
				setLayout(new GridBagLayout());
				setBackground(Color.WHITE);
				setMinimumSize(minSize);
				String output = "";
				output += "<html><body><h1>Projects Within Range</h1>";
				output += StringFormatter.wordWrapHTML("The projects within range tab is used to search for any observable project during the given LST range. The LST range is specified in the \"Start LST\" and \"End LST\" boxes (hours:minutes). You can also specify to only have projects with at least \"Min. hours left\" hours remaining to be observed. Also specify the array configuration in the pull down menu and whether to include Flex HA projects in the results. Click \"Search\" to search the database. The results are currently in priority order. The results can be sorted by frequency band with the \"1 MM\", \"3 MM\" and \"All\" buttons. The number of obsblocks in each frequency band are given in the boxes below each button.",58);
				output += "<br><br>" + StringFormatter.wordWrapHTML("The results are presented with the following columns:", 58);
				output += "<ol>" + StringFormatter.wordWrapHTML("", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Obsblock ID in the format &lt;project&gt;.&lt;obsblock&gt;", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Frequency band (1MM or 3MM)", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Time Left - number of hours remaining to be observed", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("RA - RA of the source", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("LST Range - the LST times during which the source can be observed", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Flex - whether the obsblock is FlexHA (green box) or not (red box)", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Scal - whether the source is selfcalibratable (green box) or not (red box)", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("Script - whether the script to run this obsblock is (green box) or is not (red box) present in the current script directory. Note that it assumes the typical naming convention of &lt;project&gt;_&lt;obsblock&gt;.obs.", 45);
				output += "<li>" + StringFormatter.wordWrapHTML("An graphical overview of when the source can be and has been observed. The LST is marked every 4 hours with a black line. The <font color=\"#ff0000\">red</font> line marks when the source can be observed and the <font color=\"#0000ff\">blue</font> bar(s) represent when the source has already been observed by previous tracks. In the example below the source is observable from 05:00 to 16:00 LST and has been observed during 06:00-07:00 LST and 09:00-14:00 LST. ", 45);
				output += "</ol></body></html>";
				add(new JLabel(output), new GBC(0,0).setAnchor(GBC.NORTH).setWeight(100,100));
				add(new PlotCoverage(Conversions.twoPI*5.0/24.0,Conversions.twoPI*16.0/24.0,Conversions.twoPI*10.5/24.0,"-4.5--3.5,-0.5-4.5"), new GBC(0,1).setAnchor(GBC.NORTH).setWeight(100,100));
			}
		}
		
		/**
		 * the help screen for uptimes plot
		 * @author friedel
		 *
		 */
		private class Upt extends JPanel{
			public Upt(){
				setLayout(new GridBagLayout());
				setBackground(Color.WHITE);
				setMinimumSize(minSize);
				String output = "";
				output += "<html><body><h1>Uptimes Plot</h1>";
				output += StringFormatter.wordWrapHTML("The uptimes plot will display all available projects in increasing RA order.",58);
				output += "<br><br>" + StringFormatter.wordWrapHTML("Select the array configuration from the pull down menu and click on 'Update' to display the results. The results are currently in RA order. The three buttons '1 MM', '3 MM', and 'ALL' can be used to filter the results based on observation frequency. The boxes below each button indicate how many obsblocks are in each range.",58);
				output += "<br><br>The results will have three parts:";
				output += "<ol><li>Obsblock name in the form of &lt;project&gt;.&lt;obsblock&gt;";
				output += "<li>Amount of time remaining for the obsblock in hours.";
				output += "<li>" + StringFormatter.wordWrapHTML("A graphical representation of when the obsblock can be observed. The LST is marked every 4 hours by the black marks and are labeled above the plots. Obsblocks with <font color=\"#ff0000\">red</font> bars are 1 mm projects, projects with <font color=\"#0000ff\">blue</font> bars are 3 mm projects, and those with <font color=\"#aaaaaa\">grey</font> bars do not have a band specified. In the example below the 3mm track can be observed between 20:00 LST and 04:00 LST",45);
				output += "</ol>";
				output += "</body></html>";
				add(new JLabel(output), new GBC(0,0).setAnchor(GBC.NORTH).setWeight(100,100));
				add(new PlotUptimes(Conversions.twoPI*20.0/24.0,Conversions.twoPI*4.0/24.0,"3MM"), new GBC(0,1).setAnchor(GBC.NORTH).setWeight(100,100));
			}
		}
		
		private class SCH extends JPanel{
			public SCH(){
				setBackground(Color.WHITE);
				setMinimumSize(minSize);
				String output = "";
				output += "<html><body><h1>Schedule</h1>";
				output += StringFormatter.wordWrapHTML("The schedule pane is a graphical interface to the scheduler that is similar to the SAC commands <i>schedule</i>, <i>schedule3mm</i>, <i>schedule1mm</i>, and <i>scheduleAll</i>. It displays the resulting schedule in the lower panel (which can also be seen on the web page).",58);
				add(new JLabel(output));
			}
		}
	}
	
	/**
	 * blank opening screen
	 * @author friedel
	 *
	 */
	private class WelcomeScreen extends JPanel{
		public WelcomeScreen(){
			setBackground(Color.WHITE);
			setPreferredSize(minSize);
			setMinimumSize(minSize);
			setMaximumSize(minSize);
		}
	}
	
	private static final Dimension minSize = new Dimension(400,525);
	private static JPanel helpText = null;
	private static JScrollPane helpScroll = null;
	private static JPanel mainPanel = null;
	private static JTree tree = null;
}

