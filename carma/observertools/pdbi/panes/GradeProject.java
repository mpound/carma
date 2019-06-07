package carma.observertools.pdbi.panes;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.text.*;

import carma.observertools.*;
import carma.observertools.pdbi.messages.*;
import carma.observertools.pdbi.utils.*;
import carma.observertools.pdbi.utils.comparators.PIDCompare;
import carma.observertools.pdbi.config.*;

/**
 * class to submit the observer grade to the pdb
 * @author friedel
 *
 */
@SuppressWarnings("serial")
public class GradeProject extends JPanel{
    /**
     * constructor
     * @param pdb an instance of the Project database manager
     * @param mainView 
     */
    public GradeProject(ProjectDatabaseManager pdb){
	pdb_ = pdb;
	// layout the inputs
	setLayout(new GridBagLayout());
	JPanel obsID = new JPanel();
	obsID.add(new JLabel("Obsblock:"));
	obsblock = new JTextField("",40);
	obsblock.setToolTipText("Fully qualified obsblock name <project>.<obsblock>");
	trialBox = new JTextField("",3);
	trialBox.setToolTipText("Trial number to be graded");
	trialBox.getDocument().addDocumentListener(new IntListener());
	obsID.add(obsblock);
	obsID.add(new JLabel("  Trial:"));
	obsID.add(trialBox);
	add(obsID, new GBC(0,0,2,1).setAnchor(GBC.WEST).setWeight(100,100));
	
	JPanel gradeC = new JPanel();
	grade = new Choice();
	grade.add("A+");
	grade.add("A");
	grade.add("A-");
	grade.add("B+");
	grade.add("B");
	grade.add("B-");
	grade.add("C+");
	grade.add("C");
	grade.add("C-");
	grade.add("D");
	grade.add("E");
	grade.add("F");
	gradeC.add(new JLabel("Name:"));
	name = new JTextField("",10);
	name.setToolTipText("Your name/initials");
	gradeC.add(name);
	gradeC.add(new JLabel("   Length(hours)"));
	lengthBox = new JTextField("0.0",4);
	lengthBox.setToolTipText("The length of time for this trial in hours");
	lengthBox.getDocument().addDocumentListener(new FloatListener());
	gradeC.add(lengthBox);
	gradeC.add(new JLabel("  Grade:"));
	gradeC.add(grade);
	add(gradeC,new GBC(0,1,1,1).setAnchor(GBC.WEST).setWeight(100, 100));
	ButtonGroup buttons = new ButtonGroup();
	replace = new JRadioButton("Replace existing comments");
	append = new JRadioButton("Append to existing comments");
	append.setSelected(true);
	buttons.add(replace);
	buttons.add(append);
	JPanel buttonPanel = new JPanel();
	buttonPanel.setLayout(new GridBagLayout());
	buttonPanel.add(replace,new GBC(0,0).setAnchor(GBC.WEST).setWeight(100,100));
	buttonPanel.add(append,new GBC(0,1).setAnchor(GBC.WEST).setWeight(100,100));
	add(buttonPanel,new GBC(0,2,1,1).setAnchor(GBC.WEST).setWeight(100,100));
	add(new NeedGrades(), new GBC(1,1,0,2).setAnchor(GBC.EAST).setWeight(100,100));
	add(new JLabel("Comments:"),new GBC(0,3,2,1).setAnchor(GBC.SOUTHWEST).setWeight(100,100));
	commentText = new JTextArea();
	commentText.setLineWrap(true);
	commentText.setWrapStyleWord(true);
	textBar = new JScrollPane(commentText);
	textBar.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
	textBar.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	textBar.setPreferredSize(new Dimension(600,200));
	textBar.setMaximumSize(new Dimension(600,200));
	textBar.setMinimumSize(new Dimension(600,200));		
	add(textBar, new GBC(0,4,2,1).setAnchor(GBC.NORTHWEST).setWeight(100, 100));
	update = new JButton("Update");
	update.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent event){
		    setGrade();
		}
	    });
	add(update, new GBC(0,5,2,1).setAnchor(GBC.WEST).setWeight(100, 100));
	
    }
    
    /**
     * method to check the validity of integer input
     */
    private static void checkInt(){
	int trial_;
	try{
	    trial_ = Integer.parseInt(trialBox.getText().trim());
	    if(trial_ < 1){
		throw new NumberFormatException("Trial number must be positive.");
	    }
	    trial = (short)trial_;
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
	float length_;
	try{
	    length_ = Float.parseFloat(lengthBox.getText().trim());
	    if(length_ < 0.0){
		new ExceptionHandler("Length must be >= 0.0",false);
	    }
	    length = length_;
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
     * method to set the grade in the pdb and also update the web page
     */
    private static void setGrade(){
	// check and make sure the obsblock exists first
	String[] pid = Conversions.splitPid(obsblock.getText());
	ArrayList<ItemValue> tempIVS = new ArrayList<ItemValue>();
	tempIVS.add(new ItemValue("project",pid[0]));
	tempIVS.add(new ItemValue("obsblock",pid[1]));
	tempIVS.add(new ItemValue("subObsblock",pid[2]));
	ItemValue[] ivSeq = (ItemValue[])tempIVS.toArray(new ItemValue[tempIVS.size()]);
	Project[] ps = null;
	RunQuery rq = new RunQuery(pdb_,ivSeq);
	// issue the search in a thread to avoid hangs
	try{
	    long time = 0;
	    while((ps = rq.checkResults()) == null){
		if(time > 360){
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
	if(ps == null){
	    new ExceptionHandler("Error in query",false);
	}
	if(ps.length == 0){
	    new ExceptionHandler("Obsblock not found in database.",false);
	}
	if(ps.length != 1){
	    new ExceptionHandler("More than one result was found (this should never happen",false);
	}
	if(ps[0].obsblock[0].subObsblock[0].trial[ps[0].obsblock[0].subObsblock[0].trial.length - 1].trialID < trial){
	    new ExceptionHandler("Trial " + trial + " does not exist in " + obsblock.getText(),false);
	}
	// create the edit items by reading the inputs
	Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
	SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm");
	String time = sdf.format(cal.getTime());
	String comments = "[" + time + "]" + name.getText() +" : " + commentText.getText();
	if(append.isSelected() && !ps[0].obsblock[0].subObsblock[0].trial[trial-1].obsComments.contains("NODE_NOT_FOUND")){
	    comments = ps[0].obsblock[0].subObsblock[0].trial[trial-1].obsComments + "\n" + comments;
	}
	if(Float.parseFloat(lengthBox.getText().trim()) < 0.0){
	    new ExceptionHandler("Length must be >= 0.0",false);
	}
	ItemValue[] EivSeq = {new ItemValue("obsGrade",grade.getSelectedItem()), new ItemValue("trialObservationLength",lengthBox.getText()), new ItemValue("comments",comments)};
	RunEdit re = new RunEdit(pdb_,pid[0], pid[1], pid[2], trial, EditStatus.ESTATUS_EDIT, EivSeq);
	String result = "NOT_DONE";
	// do the edit in a thread to avoid hangs
	try{
	    long timeE = 0;
	    while((result = re.checkResults()) == "NOT_DONE"){
		if(timeE > 360){
		    rq.killThread();
		    new ExceptionHandler("Query to PDB timed out.",false);
		    break;
		}
		timeE++;
		Thread.sleep(1000);
	    }
	}
	catch(InterruptedException e){
	    new ExceptionHandler(e.toString(),false);
	}
	catch(Exception e){
	    new ExceptionHandler(e.toString() + e.getMessage(),false);
	}
	// update the web file
	if(result.contains("NOT_DONE") || result.contains("FALSE")){
	    new ExceptionHandler("The edit completed in an unexpected manor.",false);
	}
	else{
	    try{
		ArrayList<String> wrappedComments = StringFormatter.wordWrap(comments,60);
		SimpleDateFormat sdf2 = new SimpleDateFormat("yyyy-MM-dd");
		String time2 = sdf2.format(cal.getTime());
		FileWriter fw = new FileWriter(PdbiConfig.getWeb(),true);
		fw.write(String.format("%s  %30s.%d  %4.1f  %2s\n",time2,obsblock.getText(),trial,length,grade.getSelectedItem()));
		fw.write("                                Comments:\n");
		if(ps[0].obsblock[0].subObsblock[0].trial[trial - 1].obsComments != "NODE_NOT_FOUND"){
		    ArrayList<String> oldComments = StringFormatter.wordWrap(ps[0].obsblock[0].subObsblock[0].trial[trial - 1].obsComments,60);
		    for(String line : oldComments){
			fw.write("                                " + line + "\n");
		    }
		}
		for(String line : wrappedComments){
		    fw.write("                                " + line + "\n");
		}
		fw.close();
	    }
	    catch(IOException e){
		new ExceptionHandler(e.toString(),false);
	    }
	}
    }
    
    private class NeedGrades extends JPanel{
	public NeedGrades(){
	    needGrades = new JPanel();
	    TitledBorder title;
	    title = BorderFactory.createTitledBorder("Need Grades");
	    needGrades.setBorder(title);
	    needGrades.setSize(new Dimension(350,300));
	    needGrades.setMinimumSize(new Dimension(350,300));
	    needGrades.setMaximumSize(new Dimension(350,300));
	    needGrades.setLayout(new GridBagLayout());
	    needGrades.add(new JLabel("Array:"),new GBC(0,0,1,1).setAnchor(GBC.EAST).setWeight(100,100));
	    arrayConfig = new ArrayConfiguration();
	    needGrades.add(arrayConfig, new GBC(1,0,1,1).setAnchor(GBC.WEST).setWeight(100,100));
	    update = new JButton("Search");
	    update.addActionListener(new ActionListener(){
		    public void actionPerformed(ActionEvent event){
			doSearch();
		    }
		});
	    needGrades.add(update,new GBC(2,0,1,1).setAnchor(GBC.SOUTHWEST).setWeight(100, 100));
	    needGrades.add(new JLabel("     Obsblock                                    Flex  Q  G"), new GBC(0,1,3,1).setFill(GBC.BOTH).setWeight(100,100));
	    needGrades.add(new NGResultsPane(),new GBC(0,2,3,1).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
	    add(needGrades);
	}
	
	private void refreshPanel(){
	    needGrades.remove(4);
	    needGrades.add(new NGResultsPane(), new GBC(0,2,3,1).setFill(GBC.BOTH).setAnchor(GBC.CENTER).setWeight(100,100));
	    needGrades.revalidate();
	    needGrades.repaint();
	}
	
	private void doSearch(){
	    ArrayList<ItemValue> tempIVS = new ArrayList<ItemValue>();
        tempIVS.add(new ItemValue("TOR",""));
	    tempIVS.add(new ItemValue("dqaOverallGrade","0.0,0.0"));
        tempIVS.add(new ItemValue("obsGrade","0.0,0.0"));
        tempIVS.add(new ItemValue("ENDTOR",""));
	    tempIVS.add(new ItemValue("obsblockStatus","INCOMPLETE"));
	    tempIVS.add(new ItemValue("trialStatus","RUNNING"));
	    tempIVS.add(new ItemValue("notProject","commissioning"));
	    if(arrayConfig.getSelectedItem() != "Any"){
		tempIVS.add(new ItemValue("arrayConfiguration",arrayConfig.getSelectedItem()));
	    }
	    CommissioningProjects.excludeCommissioning(tempIVS);
	    ItemValue[] ivSeq = (ItemValue[])tempIVS.toArray(new ItemValue[tempIVS.size()]);
	    projects = null;
	    RunQuery rq = new RunQuery(pdb_,ivSeq);
	    // issue the search in a thread in order to avoid indefinite hangs
	    try{
		long time = 0;
		while((projects = rq.checkResults()) == null){
		    if(time > 360){
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
            return;
	    }
	    if(projects == null){
            new ExceptionHandler("Error in query",false);
            return;
	    }

        projects_ = combineProjects();
	    
	    refreshPanel();
	}
	
	private NeedGradesResult[] combineProjects(){
	    ArrayList<NeedGradesResult>results = new ArrayList<NeedGradesResult>();
        for(Project p : projects){
            for(Obsblock o : p.obsblock){
                for(SubObsblock s : o.subObsblock){
                    for(Trial t : s.trial){
                        if((t.source.length != 0 && t.source[0].dataFile.contains("astro")) || (t.calibrator.length != 0 && t.calibrator[0].dataFile.contains("astro"))
                           && t.obsGrade < 0.01)
                            results.add(new NeedGradesResult(p.projectID,o.obsblockID,s.subObsblockID,t.trialID,true,false,o.isFlex));
                    }
                }
            }
        }
        for(Project p : projects){
            for(Obsblock o : p.obsblock){
                for(SubObsblock s : o.subObsblock){
                    for(Trial t : s.trial){
                        NeedGradesResult ng1 = new NeedGradesResult(p.projectID,o.obsblockID,s.subObsblockID,t.trialID,false,true,o.isFlex);
                        boolean match = false;
                        for(int i = 0; i < results.size(); i++){
                            NeedGradesResult ng2 = results.get(i);
                            if(ng1.equals(ng2)){
                                ng2.setQG(true);
                                results.set(i, ng2);
                                match = true;
                                break;
                            }
                        }
                        if(!match)
                            if((t.source.length != 0 && t.source[0].dataFile.contains("astro")) || (t.calibrator.length != 0 && t.calibrator[0].dataFile.contains("astro"))
                               && t.dqaOverallGrade < 0.01)
                                results.add(ng1);
                    }
                }
            }
        }
	    NeedGradesResult[] ngr = (NeedGradesResult[])results.toArray(new NeedGradesResult[results.size()]);
	    Arrays.sort(ngr, new PIDCompare());
	    return ngr;
	}
	
	private class NGResultsPane extends JPanel{
	    public NGResultsPane(){
		setSize(new Dimension(330,290));
		setMinimumSize(new Dimension(330,290));
		setMaximumSize(new Dimension(330,290));
		JPanel resultsList = new JPanel();
		resultsList.setLayout(new GridBagLayout());
		int m = 0;
		String stringFormat = "%-" + 30 + "s";
		for(final NeedGradesResult ngr : projects_){
		    JPanel tempPanel = new JPanel();
		    // alternate the colors of each line to make them easier to track
		    if((m%2) == 0){
			tempPanel.setBackground(Color.LIGHT_GRAY);
		    }
		    else{
			tempPanel.setBackground(Color.WHITE);
		    }
		    // put in the different components
		    JTextField obsLabel;
		    String obsName = "";
		    if(ngr.soid.equals("")){
			obsName = ngr.pid + "." + ngr.oid;
			obsLabel = new JTextField(String.format(stringFormat,obsName + "." + ngr.tid));
		    }
		    else{
			obsName = ngr.pid + "." + ngr.oid + "." + ngr.soid;
			obsLabel = new JTextField(String.format(stringFormat,obsName + "." + ngr.tid));
		    }
		    obsLabel.setPreferredSize(new Dimension(200,YSIZE));
		    obsLabel.setMinimumSize(new Dimension(200,YSIZE));
		    obsLabel.setMaximumSize(new Dimension(200,YSIZE));
		    obsLabel.setEditable(false);
		    obsLabel.setDisabledTextColor(Color.black);
		    obsLabel.setBackground(null);
		    obsLabel.setBorder(null);
		    tempPanel.add(obsLabel,new GBC(0,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
		    if(ngr.flex){
			GreenBox gb = new GreenBox();
			gb.setPreferredSize(new Dimension(10,YSIZE));
			gb.setMinimumSize(new Dimension(10,YSIZE));
			gb.setMaximumSize(new Dimension(10,YSIZE));
			gb.setToolTipText("This obsblock is a flexHA obsblock");
			tempPanel.add(gb,new GBC(1,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
		    }
		    else{
			RedBox rb = new RedBox();
			rb.setPreferredSize(new Dimension(10,YSIZE));
			rb.setMinimumSize(new Dimension(10,YSIZE));
			rb.setMaximumSize(new Dimension(10,YSIZE));
			rb.setToolTipText("This obsblock is not a flexHA obsblock");
			tempPanel.add(rb,new GBC(1,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
		    }
		    
		    if(ngr.qg){
			JButton rb = new JButton("Q"){
				public Dimension getPreferredSize() {
				    return getSize();
				}
			    };
			rb.setMargin(new Insets(1,1,1,1));
			rb.setSize(new Dimension(20,15));;
			final String on = obsName;
			rb.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent event){
				    trialBox.setText("" + ngr.tid);
				    obsblock.setText(on);
				    commentText.setText("");
				    lengthBox.setText("0.0");
				    rq = new RunQuality(on + "." + ngr.tid);
				    (new Thread(rq)).start();
				}
			    });
			tempPanel.add(rb,new GBC(2,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
		    }
		    else{
			GreenBox gb = new GreenBox();
			gb.setPreferredSize(new Dimension(10,YSIZE));
			gb.setMinimumSize(new Dimension(10,YSIZE));
			gb.setMaximumSize(new Dimension(10,YSIZE));
			gb.setToolTipText("");
			tempPanel.add(gb,new GBC(2,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
		    }
		    if(ngr.og){
			JButton rb = new JButton("G"){
				public Dimension getPreferredSize() {
		                    return getSize();
				}
			    };
			rb.setMargin(new Insets(1,1,1,1));
			rb.setSize(new Dimension(20,15));
			final String on = obsName;
			rb.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent event){
				    trialBox.setText("" + ngr.tid);
				    obsblock.setText(on);
				    commentText.setText("");
				    lengthBox.setText("0.0");
				}
			    });
			tempPanel.add(rb,new GBC(3,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
		    }
		    else{
			GreenBox gb = new GreenBox();
			gb.setPreferredSize(new Dimension(10,YSIZE));
			gb.setMinimumSize(new Dimension(10,YSIZE));
			gb.setMaximumSize(new Dimension(10,YSIZE));
			gb.setToolTipText("");
			tempPanel.add(gb,new GBC(3,0).setFill(GBC.NONE).setAnchor(GBC.WEST).setWeight(100, 100).setIpad(0, 0));
		    }
		    resultsList.add(tempPanel, new GBC(0,m).setFill(GBC.BOTH).setAnchor(GBC.WEST).setWeight(100,100));
		    m++;
		}
		if(m < 8){
		    for(int i = m; i < 8; i++){
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
		
		JScrollPane scrollPane = new JScrollPane(resultsList);
		scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);		
		scrollPane.setPreferredSize(new Dimension(330,290));
		scrollPane.setMinimumSize(new Dimension(330,290));
		add(scrollPane, new GBC(0,1).setFill(GBC.BOTH).setWeight(100,100));
	    }
	}
	
	private NeedGradesResult[] projects_ = new NeedGradesResult[0];
        //private Project[] oprojects = null;
        //private Project[] qprojects = null;
    private Project[] projects = null;
	private JPanel needGrades = null;
	private Choice arrayConfig = null;
	private JButton update = null;
	private static final int YSIZE = 15;
    }
    private RunQuality rq = null;
    private static JTextField name = null;
    private static JTextArea commentText = null;
    private static JTextField lengthBox = null;
    private static JButton update = null;
    private static Choice grade = null;
    private static JTextField trialBox = null;
    private static short trial = 0;
    private static float length = 0.0f;
    private static JRadioButton replace = null;
    private static JRadioButton append = null;
    private static JScrollPane textBar = null;
    private static JTextField obsblock = null;
    private static ProjectDatabaseManager pdb_ = null;
}
