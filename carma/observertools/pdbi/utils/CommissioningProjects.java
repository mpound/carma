package carma.observertools.pdbi.utils;

import java.util.*;

import carma.observertools.*;

/**
 * class to add removal of commissioning projects from queries
 * @author friedel
 *
 */
public class CommissioningProjects {
	public static void excludeCommissioning(ArrayList<ItemValue> ivSeq){
		ivSeq.add(new ItemValue("notProject","commissioning"));
	}
}
