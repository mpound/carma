\begin{verbatim}
/** 
 * THIS IS JUST EXAMPLE CODE FOR THE LOGGER DOC. IT IS NOT MEANT
 * TO COMPILE NOR RUN.
 *
 * $Id: Log2Syslog.cc,v 1.1 2003/07/24 18:13:01 mpound Exp $
 * Example usage of CARMA logging to syslog. 
 * @file Log2Syslog.cc
 * @author Marc Pound
 */

#include "carma/util/Logger.h"
using log4cpp;

int main(int argc, char* argv[])
{

    // I think every program should define this.
    string progName = "Antenna Calibrator Control";

    // Get a reference to a Category (Logger) object, assigning it the
    // name of the calling program, the computer on which the
    // syslogd is running, and a hierarchical namespace for
    // this Logger.  The layout will already be set to SyslogLayout and
    // the appender additivity will be set to false.
    Category& myLogger = carma::util::Logger.getSyslogger(
                              progName,
                              "rdbms.mmarray.org",
                              "carma.antenna.control");

    // Push the diagnostic context of this program on the
    // NDC stack.  I've just made up this config call, but
    // note the NDC is obtained in an antenna-independent way.
    string whoami = carma::util::Config.getConfiguration().getAntennaName();
    NDC::push(whoami); 

    // Set up the priority for the Category, in this example
    // the INFO priority (so attempts to log DEBUG messages 
    // will fail, since DEBUG is lower priority than INFO.)
    myLogger.setPriority(Priority::INFO);

    // Log an info message.
    myLogger.info("Information is a good thing.");
    // Show an example of syslog message decimation by repeating a message.
    for (int i = 0; i < 21; i++)  {
        myLogger
            .warn("You are surrounded by twisty little passages, all alike.");
    }

    // You can also use the stream methods, but be sure to
    // to include a Priority level (runtime error if absent).
    std::string message = "I'll be gone ";
    int value = 500;
    myLogger << Priority::WARN 
             << message 
             << value 
             << " miles when the day is done.";
   
    // Log an emergency message.
    myLogger.emerg("Abandon ship!");

    // Clean up: remove all appenders
    myLogger.shutdown();
    return 0;
}
\end{verbatim}
