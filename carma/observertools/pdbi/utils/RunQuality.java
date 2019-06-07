package carma.observertools.pdbi.utils;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;

import carma.observertools.pdbi.messages.ExceptionHandler;

@SuppressWarnings("serial")
public class RunQuality extends JFrame implements ActionListener,Runnable{
	public void run(){
		QualityRun();
	}
	public RunQuality(String fB){
		setTitle("Project Database Interface");
		setSize(new Dimension(770,250));
		//super(owner,"Quality",true);
		fileBase = fB;
	}
	private void QualityRun(){	
	    Dimension size = new Dimension(780,400);
	    setPreferredSize(size);
	    setMinimumSize(size);
	    setMaximumSize(size);

		mainPanel = new JPanel();
	    Filter nf = new Filter (fileBase);
	    // current directory
	    File dir = new File ("/opt/sdp/sciencedata");
	    String[] files = dir.list(nf);
	    if(files.length == 0){
	    	new ExceptionHandler("No miriad file found for obsblock " + fileBase,false);
	    }
	    else if(files.length != 1){
	    	new ExceptionHandler("More than one miriad file found, this should not happen",false);
	    }
	    fileName = files[0];
	    commandLineArgs = new JTextField("",50);
	    	    
	    sources = new JTextField("",10);
	    sources.getDocument().addDocumentListener(new docListener());
	    
	    gaincals = new JTextField("",10);
	    gaincals.getDocument().addDocumentListener(new docListener());
	    
	    passcals = new JTextField("",10);
	    passcals.getDocument().addDocumentListener(new docListener());
	    
	    fluxcal = new JTextField("",10);
	    fluxcal.getDocument().addDocumentListener(new docListener());
	    
	    prnY = new JRadioButton();
	    prnN = new JRadioButton();
	    prnN.setSelected(true);
	    ButtonGroup prnG = new ButtonGroup();
	    prnG.add(prnY);
	    prnG.add(prnN);
	    prnY.addActionListener(this);
	    prnN.addActionListener(this);
	    
	    seeY = new JRadioButton();
	    seeN = new JRadioButton();
	    seeY.setSelected(true);
	    ButtonGroup seeG = new ButtonGroup();
	    seeG.add(seeY);
	    seeG.add(seeN);
	    seeY.addActionListener(this);
	    seeN.addActionListener(this);
	    
	    tmax = new JTextField("1000",6);
	    tmax.getDocument().addDocumentListener(new docListener());
	    
	    tauz = new JTextField("",6);
	    tauz.getDocument().addDocumentListener(new docListener());
	    
	    banduse = new JTextField("",3);
	    banduse.getDocument().addDocumentListener(new docListener());
	    
	    refant = new JTextField("9",3);
	    refant.getDocument().addDocumentListener(new docListener());
	    
	    solint = new JTextField("5",3);
	    solint.getDocument().addDocumentListener(new docListener());
	    
	    ampgainsolint = new JTextField("0.5",4);
	    ampgainsolint.getDocument().addDocumentListener(new docListener());
	    
	    smavarpltY = new JRadioButton();
	    smavarpltN = new JRadioButton();
	    smavarpltY.setSelected(true);
	    ButtonGroup smavarpltG = new ButtonGroup();
	    smavarpltG.add(smavarpltY);
	    smavarpltG.add(smavarpltN);
	    smavarpltY.addActionListener(this);
	    smavarpltN.addActionListener(this);
	    
	    mainPanel.setLayout(new GridBagLayout());
	    mainPanel.setPreferredSize(size);
	    mainPanel.setMinimumSize(size);
	    mainPanel.setMaximumSize(size);
	    
	    mainPanel.add(new JLabel("Command"),new GBC(0,0).setAnchor(GBC.EAST).setWeight(100,100));
	    mainPanel.add(commandLineArgs, new GBC(1,0).setAnchor(GBC.WEST).setWeight(100,100));
	    mainPanel.add(new JLabel("Optional Arguments:"),new GBC(0,1,2,1).setAnchor(GBC.WEST).setWeight(100,100));
	    
	    JPanel options = new JPanel();
	    options.setLayout(new GridBagLayout());
	    Dimension osize = new Dimension(770,250);
	    options.setPreferredSize(osize);
	    options.setMinimumSize(osize);
	    options.setMaximumSize(osize);
	    options.add(new JLabel("Sources:"), new GBC(0,0).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(sources, new GBC(1,0).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Gaincals:"), new GBC(2,0).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(gaincals, new GBC(3,0).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Passcals:"), new GBC(4,0).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(passcals, new GBC(5,0).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Fluxcal:"), new GBC(6,0).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(fluxcal, new GBC(7,0).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Print: Y"), new GBC(0,1).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(prnY, new GBC(1,1).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("N"), new GBC(2,1).setAnchor(GBC.EAST).setWeight(100,100));
	    options.add(prnN, new GBC(3,1).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("See:   Y"), new GBC(4,1).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(seeY, new GBC(5,1).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("N"), new GBC(6,1).setAnchor(GBC.EAST).setWeight(100,100));
	    options.add(seeN, new GBC(7,1).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Tmax:"), new GBC(0,2).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(tmax, new GBC(1,2).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Tauz:"), new GBC(2,2).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(tauz, new GBC(3,2).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Banduse:"), new GBC(4,2).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(banduse, new GBC(5,2).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Refant:"), new GBC(6,2).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(refant, new GBC(7,2).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Solint:"), new GBC(0,3).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(solint, new GBC(1,3).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Ampsolint:"), new GBC(2,3).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(ampgainsolint, new GBC(3,3).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("Smavarplt: Y"), new GBC(4,3).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(smavarpltY, new GBC(5,3).setAnchor(GBC.WEST).setWeight(100,100));
	    options.add(new JLabel("N"), new GBC(6,3).setAnchor(GBC.EAST).setWeight(100,100));
	    options.add(smavarpltN, new GBC(7,3).setAnchor(GBC.WEST).setWeight(100,100));
	    mainPanel.add(options, new GBC(0,2,2,1).setAnchor(GBC.WEST).setWeight(100,100));
	    go = new JButton("Run");
		go.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent event){
				String command = "xterm -hold -e quality " + commandLine;
				try {
					//@SuppressWarnings("unused")
					Process quality = Runtime.getRuntime().exec(command);
				} catch (IOException e) {
					new ExceptionHandler(e.toString(),false);
				}
			}
		});
		mainPanel.add(go, new GBC(0,3,2,1).setAnchor(GBC.WEST).setWeight(100,100));
	    createCommandLine();
		mainPanel.revalidate();
		mainPanel.repaint();
		add(mainPanel);
		setVisible(true);
	}
	
	public void actionPerformed(ActionEvent e) {
	    createCommandLine();
	}
	private class docListener implements DocumentListener{
		public void insertUpdate(DocumentEvent event){createCommandLine();}
		public void removeUpdate(DocumentEvent event){createCommandLine();}
		public void changedUpdate(DocumentEvent event){createCommandLine();}		
	}

	/**
	 * subroutine to write the command line arguments for quality
	 * default arguments are not passed.
	 */
	private void createCommandLine(){
		commandLine = "proj=" + fileName;
		if(sources.getText().trim().length() != 0){
			commandLine += " sources=" + sources.getText().trim();
		}
		if(gaincals.getText().trim().length() != 0){
			commandLine += " gaincals=" + gaincals.getText().trim();
		}
		if(passcals.getText().trim().length() != 0){
			commandLine += " passcals=" + passcals.getText().trim();
		}
		if(fluxcal.getText().trim().length() != 0){
			commandLine += " fluxcal=" + fluxcal.getText().trim();
		}
		if(prnY.isSelected()){
			commandLine += " prn=y";
		}
		else{
			commandLine += " prn=n";
		}
		if(seeN.isSelected()){
			commandLine += " see=n";
		}
		else{
			commandLine += " see=y";
		}
		if(!tmax.getText().trim().equals("1000") && tmax.getText().trim().length() != 0){
			commandLine += " tmax=" + tmax.getText().trim();
		}
		if(tauz.getText().trim().length() != 0){
			commandLine += " tauz=" + tauz.getText().trim();
		}
		if(banduse.getText().trim().length() != 0){
			commandLine += " banduse=" + banduse.getText().trim();
		}
		if(refant.getText().trim().length() != 0 && !refant.getText().trim().equals("9")){
			commandLine += " refant=" + refant.getText().trim();
		}
		if(solint.getText().trim().length() != 0 && !solint.getText().trim().equals("5")){
			commandLine += " solint=" + solint.getText().trim();
		}
		if(ampgainsolint.getText().trim().length() != 0 && !ampgainsolint.getText().trim().equals("0.5")){
			commandLine += " ampgainsolint=" + ampgainsolint.getText().trim();
		}
		if(smavarpltN.isSelected()){
			commandLine += " smavarplt=0";
		}
		commandLineArgs.setText(commandLine);
	}

	public class Filter implements FilenameFilter {
		protected String pattern;
		public Filter (String str) {
			pattern = str;
		}
	  
		public boolean accept (File dir, String name) {
			return name.toLowerCase().contains(pattern.toLowerCase());
		}
	}
	private String fileName = "";
	private String commandLine = "";
	private JRadioButton prnY = null;
	private JRadioButton prnN = null;
	private JRadioButton seeY = null;
	private JRadioButton seeN = null;
	private JTextField commandLineArgs = null;
	private JTextField tmax = null;
	private JTextField tauz = null;
	private JTextField banduse = null;
	private JTextField refant = null;
	private JTextField solint = null;
	private JTextField ampgainsolint = null;
	private JRadioButton smavarpltY = null;
	private JRadioButton smavarpltN = null;
	private JTextField sources = null;
	private JTextField gaincals = null;
	private JTextField passcals = null;
	private JTextField fluxcal = null;
	private JPanel mainPanel = null;
	private JButton go = null;
	private String fileBase = null;
}
