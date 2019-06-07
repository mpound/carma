package carma.observertools.pdbi.utils.comparators;

import carma.observertools.pdbi.utils.*;

import java.util.Comparator;

public class LSTCompare implements Comparator<Obsblock_I>{
	public int compare(Obsblock_I A, Obsblock_I B){
		return (int)(100*(A.obsblock_.lowRa - B.obsblock_.lowRa));
	}
}
