package carma.observertools.pdbi.utils.comparators;

import carma.observertools.pdbi.utils.*;

import java.util.Comparator;

/**
 * class to compare 2 obsblocks and order them according to RA
 * @author friedel
 *
 */
public class RACompare implements Comparator<Obsblock_I>{
	public int compare(Obsblock_I A, Obsblock_I B){
		return (int)(100*(A.obsblock_.subObsblock[0].trial[A.obsblock_.subObsblock[0].trial.length - 1].source[0].ra - B.obsblock_.subObsblock[0].trial[B.obsblock_.subObsblock[0].trial.length - 1].source[0].ra));
	}
}