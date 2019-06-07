// $Id: EasyTimer.java,v 1.1 2011/04/06 18:16:54 iws Exp $
// vim: set ts=4 sts=4 sw=4 noet:

package carma.ui.cdv.util;

public class EasyTimer {
        private long start = 0;
        private long stop = 0;

        public void start() {
                start = System.currentTimeMillis();
        }

        public void stop() {
                stop = System.currentTimeMillis();
        }

        public String toString() {
                long elapsed = stop - start;
                return "" + elapsed + " ms";
        }
}
