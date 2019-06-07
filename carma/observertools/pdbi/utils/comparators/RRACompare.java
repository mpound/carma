package carma.observertools.pdbi.utils.comparators;

import java.util.Comparator;

import carma.observertools.pdbi.utils.*;

/**
 * class to compare 2 obsblocks and order them according to RA
 * @author friedel
 *
 */
public class RRACompare implements Comparator<Obsblock_I>{
	public int compare(Obsblock_I A, Obsblock_I B){
		return (int)(100*(B.obsblock_.subObsblock[0].trial[B.obsblock_.subObsblock[0].trial.length - 1].source[0].ra - A.obsblock_.subObsblock[0].trial[A.obsblock_.subObsblock[0].trial.length - 1].source[0].ra));
	}
}