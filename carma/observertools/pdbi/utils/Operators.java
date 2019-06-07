/**
 * 
 */
package carma.observertools.pdbi.utils;
import java.util.ArrayList;
import java.util.Arrays;
import carma.observertools.pdbi.utils.comparators.TrialCompare;
import carma.observertools.*;

/**
 * @author friedel
 *
 */
public final class Operators {
	public static Project[] mergeProjects(Project[] p1, Project[] p2){
		ArrayList<Project>projects = new ArrayList<Project>();
		for(Project p : p1){
			projects.add(p);
		}
		for(Project p : p2){
			boolean match = false;
			for(Project newp : projects){
				if(p.projectID.equals(newp.projectID)){
					newp.obsblock = mergeObsblocks(newp.obsblock,p.obsblock);
					match = true;
					break;
				}
			}
			if(!match){
				projects.add(p);
			}
		}
		return (Project[])projects.toArray(new Project[projects.size()]);
	}
	
	public static Obsblock[] mergeObsblocks(Obsblock[] o1, Obsblock[] o2){
		ArrayList<Obsblock>obsblocks = new ArrayList<Obsblock>();
		for(Obsblock o : o1){
			obsblocks.add(o);
		}
		for(Obsblock o : o2){
			boolean match = false;
			for(Obsblock newo : obsblocks){
				if(o.obsblockID.equals(newo.obsblockID)){
					newo.subObsblock = mergeSubObsblocks(newo.subObsblock,o.subObsblock);
					match = true;
					break;
				}
			}
			if(!match){
				obsblocks.add(o);
			}
		}
		return (Obsblock[])obsblocks.toArray(new Obsblock[obsblocks.size()]);
	}
	
	public static SubObsblock[] mergeSubObsblocks(SubObsblock[] s1, SubObsblock[] s2){
		ArrayList<SubObsblock>subObsblocks = new ArrayList<SubObsblock>();
		for(SubObsblock s : s1){
			subObsblocks.add(s);
		}
		for(SubObsblock s : s2){
			boolean match = false;
			for(SubObsblock news : subObsblocks){
				if(s.subObsblockID.equals(news.subObsblockID)){
					news.trial = mergeTrials(news.trial,s.trial);
					match = true;
					break;
				}
			}
			if(!match){
				subObsblocks.add(s);
			}
		}
		return (SubObsblock[])subObsblocks.toArray(new SubObsblock[subObsblocks.size()]);
	}
	
	public static Trial[] mergeTrials(Trial[] t1, Trial[] t2){
		ArrayList<Trial>trials = new ArrayList<Trial>();
		for(Trial t : t1){
			trials.add(t);
		}
		for(Trial t : t2){
			boolean match = false;
			for(Trial newt : trials){
				if(t.trialID == newt.trialID){
					match = true;
					break;
				}
			}
			if(!match){
				trials.add(t);
			}
		}
		Trial[] sortTrial = (Trial[])trials.toArray(new Trial[trials.size()]);
		Arrays.sort(sortTrial,new TrialCompare());
		return sortTrial;
	}
}
