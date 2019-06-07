package carma.observertools.pdbi.view;

import java.awt.*;
import java.awt.event.*;
import org.omg.CORBA.*;
import org.omg.CosNaming.*;
import java.io.IOException;
import java.util.*;
import javax.swing.*;

import carma.observertools.*;
import carma.observertools.pdbi.panes.*;
import carma.observertools.pdbi.messages.*;
import carma.observertools.pdbi.config.*;
import carma.observertools.pdbi.utils.Globals;
import carma.util.*;

public class ProjectDatabaseInterface {

    private NameComponent[] _nc;

    @SuppressWarnings("unchecked")
	public ProjectDatabaseInterface(String[] args) {
        PdbiConfig pdbiConfig = null;
        String configFileName = "";

        // if only 1 argument, assume it is the config filename
        if (args.length == 1 && args[0].endsWith("xml"))
            configFileName = args[0];
        for (int i = 0; i < args.length - 1; i++) {
            if ( args[i].equalsIgnoreCase("-XMLFile") &&
                 args[i + 1].endsWith("xml") )
                configFileName = args[i + 1];
        }

        try {
            pdbiConfig = PdbiConfig.getInstance(configFileName);
        } catch (IOException e) {
            JOptionPane.showMessageDialog(null,
                                          "Unable to open XML config file. Try using -XMLFile <filename>",
                                          "Project Database Interface", JOptionPane.ERROR_MESSAGE);
            e.printStackTrace();
            System.exit(1);
        }
        _nc = null;
        _nc = getStartNameComponent(pdbiConfig);
        if (_nc != null) {
            Hashtable h = pdbiConfig.getProperties();
            Properties p = System.getProperties();
            Enumeration e = h.keys();
            while (e.hasMoreElements()) {
                String key = (String)e.nextElement();
                p.setProperty(key, (String)h.get(key));
            }
        }
		MainView mainView = new MainView();
		mainView.setVisible(true);
    }
    

    /**
     *  Get the NameComponent to use on startup
     */
    private NameComponent[] getStartNameComponent(CarmaConfig conf) {
        java.lang.Object[] ids = conf.getNameComponentIDs();
        java.lang.Object[] kinds = conf.getNameComponentKinds();
        if (ids != null && kinds != null && ids.length == kinds.length) {
            NameComponent[] nc = new NameComponent[ids.length];
            for (int i = 0; i < nc.length; i++) {
                nc[i] = new NameComponent();
                nc[i].id = (String)ids[i];
                nc[i].kind = (String)kinds[i];
            }
            return nc;
        }
        return null;
    }

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		@SuppressWarnings("unused")
		ProjectDatabaseInterface pdbi = new ProjectDatabaseInterface(args);
	}

}

@SuppressWarnings("serial")
class MainView extends JFrame{
    public MainView(){
	_pdb = getPdb();
	setTitle("Project Database Interface");
	setSize(Globals.WIDTH,Globals.HEIGHT);
	addWindowListener(new WindowAdapter(){
		public void windowClosing(WindowEvent we){
		    System.exit(0);
		}
	    });
	JMenu fileMenu = new JMenu("File");
	fileMenu.add(new AbstractAction("Exit"){
		public void actionPerformed(ActionEvent event){
		    System.exit(0);
		}
	    });
	JMenu helpMenu = new JMenu("Help");
	JMenuItem aboutItem = new JMenuItem("About");
	aboutItem.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent event){
		    if(dialog==null)dialog=new AboutDialog(MainView.this);
		    dialog.setVisible(true);
		}
	    });
	JMenuItem helpItem = new JMenuItem("Help Dialog");
	helpItem.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent event){
		    if(help == null)help = new HelpPanel(MainView.this);
		    help.setVisible(true);
		}
	    });
	helpMenu.add(helpItem);
	helpMenu.add(aboutItem);
	
	JMenuBar menuBar = new JMenuBar();
	setJMenuBar(menuBar);
	menuBar.add(fileMenu);
	menuBar.add(helpMenu);
	
	tabbedPane = new JTabbedPane();
	tabbedPane.addTab("Uptimes", new Uptimes(_pdb));
	tabbedPane.addTab("Projects within Range", new ProjectsWithinRange(_pdb));
	tabbedPane.addTab("Projects within Date",new ProjectsWithinDate(_pdb));
	tabbedPane.addTab("Projects Starting At", new ProjectsStartingAt(_pdb));
	tabbedPane.addTab("Grade Projects",new GradeProject(_pdb));
	tabbedPane.addTab("Project Status",new ProjectStatusPane(_pdb));
	tabbedPane.addTab("Scheduling", new Scheduling(_pdb));
	add(tabbedPane,"Center");
    }
    
    private ProjectDatabaseManager getPdb(){
	ProjectDatabaseManager pdb = null;
	// get ORB using defaults found in configuration file
	_orb = CorbaUtils.getNewORB(null, null, null);
	if(_orb == null){
	    new ExceptionHandler("Cannot get ORB.",true);
	}
	org.omg.CORBA.Object tmpObj = null;
	try{
	    CorbaUtils cu = new CorbaUtils(_orb);
	    tmpObj = cu.resolveName(PROJECT_DATABASE_MANAGER_NAME.value);
	}
	catch(org.omg.CosNaming.NamingContextPackage.InvalidName e){
	    new ExceptionHandler(e.toString() + e.getMessage(),true);
	}
	catch(org.omg.CosNaming.NamingContextPackage.CannotProceed e){
	    new ExceptionHandler(e.toString() + e.getMessage(),true);
	}
	catch(org.omg.CosNaming.NamingContextPackage.NotFound e){
	    new ExceptionHandler(e.toString() + e.getMessage(),true);
	}
	catch(Exception e){
	    new ExceptionHandler(e.toString() + e.getMessage(),true);
	}

	if(tmpObj == null){
	    new ExceptionHandler("Cannot resolve " + PROJECT_DATABASE_MANAGER_NAME.value,true);
	}
	pdb = ProjectDatabaseManagerHelper.narrow(tmpObj);
	return pdb;
	
    }
    
    private AboutDialog dialog = null;
    private HelpPanel help = null;
    private JTabbedPane tabbedPane;
    private ProjectDatabaseManager _pdb = null;
    private ORB _orb;
}

/**
 * About dialog box from help menu
 */
@SuppressWarnings("serial")
class AboutDialog extends JDialog{
    public AboutDialog(JFrame owner){
	super(owner,"About Project Database Interface",true);
	add(new JLabel("<html><h1><B>Project Database Interface<B></h1><font size=-1>Version 1.2</font></html>"));
	JButton ok = new JButton("Ok");
	ok.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent event){
		    setVisible(false);
		}
	    });
	JPanel panel = new JPanel();
	panel.add(ok);
	add(panel,BorderLayout.SOUTH);
	setSize(300,200);
    }
}
