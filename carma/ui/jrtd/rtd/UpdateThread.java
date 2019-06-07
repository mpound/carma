package carma.ui.jrtd.rtd;

import carma.ui.jrtd.ui.*;
import carma.ui.jrtd.util.*;

import java.lang.*;
import java.util.*;

/**
 * This thread is responsible for getting the info from the server
 * and then doing any update of the application.  The actual work
 * to update displays is done in the specific program class that has
 * been instantiated.  This thread implements the neat feature that
 * it adjusts its sleeptime to try and achieve the request sample
 * rate by measuring the actual rate.
 *
 * @author Steve Scott
 * @version 1.2 02-Mar-1999
 * <br>History:
 * <br> v1.2 - mwp made constructor public, added isRunning() method.
 * <br> version 1.1 09-Nov-1998
 *
 * @version $Revision: 1.5 $, $Date: 2013/11/19 03:47:27 $, $Author: iws $
 *
 */

public class UpdateThread extends Thread {
    private RtProg   rtp;  // This pointer allows retrieval of the update method
    static final boolean debug = false;
    private boolean keepGoing  = true;
    private int     sleepTime  = 2000; // If this is a local (method) variable,
    // the JRE1.1.7 JIT will never ever let its value change!!

    public UpdateThread(RtProg rtp) {
        this.rtp = rtp;
    }

    public void endLoop() {
        //Util.spew("UpdateThread endLoop");
        keepGoing = false;       // Exit loop next time through
        interrupt();             // Wake it up from its sleep
    }

    public void run() {
        int passes = 1;
        MJD last;
        MJD now = new MJD();
        double secs;

        // This determines whether the screen is updated all the time
        // Only MIN_PRIORITY will give continuous updates
        // It is about 13% faster to do it just once per screen
        setPriority(NORM_PRIORITY);

        MJD start    = new MJD();
        last         = start;

        // We calculate the actual update rate versus the desired over the last
        // N updates and use this to tweak the sleeptime.
        final int Nupdates   = 10;  // Number of updates to average over.
        final int iniNupdates = 4;  // First time is fast...
        int       Nup = iniNupdates;
        double    oldSleepTime  = 0;
        double oldSampleTime = 0;
        double oldMJD        = 0;
        int  oldReqSleepTime = 0;
        double sampleTime    = 0;
        double oldPasses     = 0;

        while (keepGoing) {
            //spew("UPD start:"+passes);
            int reqSleepTime = rtp.getSleepTime();
            // See if user changed sleeptime
            if (reqSleepTime != oldReqSleepTime) {
                sleepTime   = oldReqSleepTime = reqSleepTime;
                sampleTime  = sleepTime * 0.001;
                if(debug) {
                    Util.spew("Change reqSleepTime:"+reqSleepTime+
                            " sleepTime:"+sleepTime+" sampleTime:"+sampleTime);
                }
            }
            passes++;
            if (debug)Util.spew("Passes:"+passes);

            boolean tweak = ((passes%Nup) == 2);
            if (tweak) now = new MJD();
            // Don't try to tweak 1st time through or if sampleTime has changed,
            // or if the sampleTime is > 30seconds.
            if (tweak && (passes > Nup) && 
                    (sampleTime == oldSampleTime) &&
                    (sampleTime < 31) && ((passes-oldPasses)>=Nup/2)) {
                double actSampTime = 86400*(now.mjd-oldMJD)/(passes-oldPasses);
                //Util.spew("passes:" + passes +
                //	 " sampInterval:"+Util.printf(actualSampleTime,2)+
                //	 "  sleepTime:"+sleepTime);
                double correction = actSampTime - sampleTime;
                // Limit correction
                if (correction >  0.3)correction =  0.3;
                if (correction < -0.3)correction = -0.3;
                sleepTime -= 1000*correction;
                if (sleepTime < 100)sleepTime = 100; // Don't let it hammer the machine
                Nup = Nupdates;
                    }
            if (tweak) {
                oldMJD        = now.mjd;
                oldSleepTime  = sleepTime;
                oldSampleTime = sampleTime;
                oldPasses     = passes;
                if(debug) {
                    Util.spew("Tweak sleepTime:"+sleepTime+
                            " sampleTime:"+sampleTime);
                }
            }
            if (!keepGoing) return;
            if (!rtp.update()) {
                if (!keepGoing) return;
                System.out.println("Update failed with error");
                return;
            }
            // Snooze...
            if (sleepTime > 0) {
                try {
                    sleep(sleepTime);
                }
                catch(InterruptedException e){}
            }
            //spew("UPD end:"+passes);
        }

        now = new MJD();
        double rate = passes/((now.mjd - start.mjd)*86400);
        //spew("Update rate:" + Util.printf(rate, 2));
        //spew("Exiting update thread...");
    }

    /**
     * Is the UpdateThread currently running?
     * @return <code>true</code> if the thread has been started and
     * <code>endLoop()</code> has not yet been called.
     */
    public final boolean isRunning() {
        return (isAlive() && keepGoing);
    }

    void spew(String s) {
        Util.spew(s);
    }
}

/* vim: set ts=4 sts=4 sw=4 et: */
