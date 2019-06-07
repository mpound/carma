package carma.observertools.pdbi.utils;

import carma.observertools.*;

/**
 * class used to convert a project to a obsblock holding the pid
 * @author friedel
 *
 */
public class Obsblock_I {
    public Obsblock_I(String pid, Obsblock obs, boolean keyProject, boolean fastTrack){
		pid_ = pid;
		obsblock_ = obs;
		isKeyProject = keyProject;
		isFastTrack = fastTrack;
	}
	
	public String pid_ = "";
	public Obsblock obsblock_ = null;
	public boolean isKeyProject = false;
        public boolean isFastTrack = false;
}
