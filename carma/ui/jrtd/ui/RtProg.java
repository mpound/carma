package carma.ui.jrtd.ui;

/** 
 * Interface for a Realtime Program (the basis for a RtDisplay)
 *
 * @author Steve Scott
 * @version 0.5 07-July-1997
 */
public interface RtProg {
    public boolean update();
    public int getSleepTime();
    public void setSleepTime(int st);
}
