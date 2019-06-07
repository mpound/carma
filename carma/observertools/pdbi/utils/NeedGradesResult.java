package carma.observertools.pdbi.utils;

/**
 * Class to hold results from a need grades results
 * @author D. N. Friedel
 *
 */
public class NeedGradesResult {
    public NeedGradesResult(String pid, String oid, String soid, int tid, boolean og, boolean qg, boolean flex){
	this.pid = pid;
	this.oid = oid;
	this.soid = soid;
	this.tid = tid;
	this.og = og;
	this.qg = qg;
	this.flex = flex;
    }
    
    public void setQG(boolean value){
	qg = value;
    }
    
    public void setOQ(boolean value){
	og = value;
    }
    public void setFlex(boolean value){
	flex = value;
    }
    public boolean equals(NeedGradesResult ng2){
	if(pid.equals(ng2.pid) && oid.equals(ng2.oid) && soid.equals(ng2.soid) && tid == ng2.tid && flex == ng2.flex)
	    return true;
	return false;
    }
    public String pid = null;
    public String oid = null;
    public String soid = null;
    public int tid = 0;
    public boolean og = false;
    public boolean qg = false;
    public boolean flex = false;
}
