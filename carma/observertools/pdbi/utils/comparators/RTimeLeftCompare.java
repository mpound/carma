package carma.observertools.pdbi.utils.comparators;

import carma.observertools.pdbi.utils.*;

import java.util.Comparator;

public class RTimeLeftCompare implements Comparator<Obsblock_I>{
	public int compare(Obsblock_I A, Obsblock_I B){
		return (int)(A.obsblock_.remainingTime - B.obsblock_.remainingTime);
	}
}
