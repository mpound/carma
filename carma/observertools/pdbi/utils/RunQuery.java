package carma.observertools.pdbi.utils;

import carma.observertools.*;
import carma.observertools.pdbi.messages.ExceptionHandler;

/**
 * class to run queries in a thread
 * @author friedel
 *
 */
public class RunQuery implements Runnable{

	public void run(){
		try{
            ProjectSequenceHolder psh = new ProjectSequenceHolder();
			pdb.projectQueryInOut(query,psh);
            projects = psh.value;
		}
		catch(Exception e){ 	 
		        new ExceptionHandler(e.toString(),false); 	 
		}
		done = true;
	}
	
	public RunQuery(ProjectDatabaseManager pdb,ItemValue[] query){
		this.pdb = pdb;
		this.query = query;
		runner = new Thread(this);
		runner.start();
	}
	
	public Project[] checkResults(){
		if(done){
			return projects;
		}
		else{
			return null;
		}
	}
	
	@SuppressWarnings("deprecation")
	public void killThread(){
		runner.stop();
	}
	
	Thread runner;
	private ItemValue[] query = null;
	private ProjectDatabaseManager pdb = null;
	private Project[] projects = null;
	private boolean done = false;
}
