package carma.observertools.pdbi.utils.comparators;
import carma.observertools.*;

import java.util.Comparator;

public class TrialCompare implements Comparator<Trial>{
	public int compare(Trial A, Trial B){
		return (int)(B.trialID - A.trialID);
	}
}
