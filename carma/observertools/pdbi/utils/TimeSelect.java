package carma.observertools.pdbi.utils;

import java.util.ArrayList;

import carma.observertools.*;

/**
 * class for sorting out which projects meet time range criteria
 * this class will not be needed one the dbxml query issue is fixed
 * @author friedel
 *
 */
public class TimeSelect {
	/**
	 * method for time matching, will only return obsblocks that match the criteria
	 * @param projects a list of projects
	 * @param start the start time in radians
	 * @param end the end time in radians
	 * @return the projects that meet the criteria
	 */
	public static Project[] matchTime(Project[] projects, double start, double end){
		ArrayList<Project> prj = new ArrayList<Project>();
		for(int i = 0; i < projects.length; i++){
			boolean found = false;
			for(int j = 0; j < projects[i].obsblock.length; j++){
				Obsblock o = projects[i].obsblock[j];
				if(!((o.lowRa <= end && o.lowRa >= start) || (o.highRa <= end && o.highRa >= start) || (start <= o.highRa && start >= o.lowRa) || ((o.highRa < o.lowRa) && ((start <= o.highRa) || (end <= o.highRa) || (start >= o.lowRa) || (end >= o.lowRa))))){
					projects[i].obsblock[j] = null;
				}
				else{
					found = true;
				}
			}
			if(found){
				prj.add(projects[i]);
			}
		}
		return (Project[])prj.toArray(new Project[prj.size()]);
	}
}
