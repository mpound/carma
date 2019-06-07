package carma.observertools.pdbi.utils.comparators;

import carma.observertools.pdbi.utils.NeedGradesResult;

import java.util.Comparator;

public class PIDCompare implements Comparator<NeedGradesResult>{
	public int compare(NeedGradesResult A, NeedGradesResult B){
		if(A.pid.equals(B.pid)){
			if(A.oid.equals(B.oid)){
				if(A.soid.equals(B.soid)){
					return (int)(B.tid - A.tid);
				}
				return A.soid.compareTo(B.soid);
			}
			return A.oid.compareTo(B.oid);
		}
		return A.pid.compareTo(B.pid);
	}
}
