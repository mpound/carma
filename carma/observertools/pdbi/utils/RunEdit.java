package carma.observertools.pdbi.utils;

import carma.observertools.*;
import carma.observertools.pdbi.messages.ExceptionHandler;

/**
 * class to run edits in a thread
 * @author friedel
 *
 */
public class RunEdit implements Runnable{

	public void run(){
		try{
            org.omg.CORBA.BooleanHolder bh = new org.omg.CORBA.BooleanHolder();
			pdb.projectEditInOut(project,obsblock,subObsblock,trial,query,estatus,bh);
            result = bh.value;
		}
		catch(Exception e){ 	 
		        new ExceptionHandler(e.toString(),false); 	 
		}
		done = true;
	}
	
	public RunEdit(ProjectDatabaseManager pdb,String project, String obsblock, String subObsblock, Short trial, EditStatus estatus,ItemValue[] query){
		this.project = project;
		this.obsblock = obsblock;
		this.subObsblock = subObsblock;
		this.trial = trial;
		this.estatus = estatus;
		this.pdb = pdb;
		this.query = query;
		runner = new Thread(this);
		runner.start();
	}
	
	public String checkResults(){
		if(done){
			if(result){
				return "TRUE";
			}
			else{
				return "FALSE";
			}
		}
		else{
			return "NOT_DONE";
		}
	}
	
	@SuppressWarnings("deprecation")
	public void killThread(){
		runner.stop();
	}

	Thread runner;
	private ItemValue[] query = null;
	private ProjectDatabaseManager pdb = null;
	private String project = "";
	private String obsblock = "";
	private String subObsblock = "";
	private Short trial = -1;
	private EditStatus estatus = null;
	private boolean done = false;
	private boolean result;
}

