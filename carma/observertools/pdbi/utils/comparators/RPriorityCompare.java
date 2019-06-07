package carma.observertools.pdbi.utils.comparators;

import carma.observertools.pdbi.utils.*;

import java.util.Comparator;

/**
 * class to compare 2 obsblocks and sort them according to priority
 * @author friedel
 *
 */
public class RPriorityCompare implements Comparator<Obsblock_I>{
	public int compare(Obsblock_I A, Obsblock_I B){
		return (int)(A.obsblock_.priority - B.obsblock_.priority);
	}
}
