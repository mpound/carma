\begin{verbatim}
/** 
 * THIS IS JUST EXAMPLE CODE FOR THE LOGGER DOC. IT IS NOT MEANT
 * TO COMPILE NOR RUN.
 *
 * $Id: Log2File.cc,v 1.1 2003/07/24 18:13:01 mpound Exp $
 * Example usage of log4cpp logging to a (local) file.
 * @file Log2File.cc
 * @author Marc Pound
 */

#include "carma/util/Logger.h"
using namespace std;
using namespace log4cpp;

int main(int argc, char* argv[])
{

    // I think every program should define this.
    string progName = "Antenna Calibrator Control";

    // Get a reference to a Category (Logger) object, assigning it the
    // name of the calling program, the output file, and the 
    // a hierarchical namespace for this Logger.  
    // The layout will already be set to FileLayout and
    // the appender additivity will be set to false.
    Category& myLogger = carma::util::Logger.getFilelogger(
                              progName,
                              "/home/mpound/acp.log",
                              "carma.antenna.control");

    // Push the diagnostic context of this program on the
    // NDC stack.  I've just made up this config call, but
    // note the NDC is obtained in an antenna-independent way.
    string whoami = carma::util::Config.getConfiguration().getAntennaName();
    NDC::push(whoami); 

    // Allow messages of DEBUG level and above to be passed
    // to appenders.  But set the FileAppender's threshold to
    // INFO.
    myLogger.setPriority(Priority::DEBUG);
    myLogger
        .getAppender("Antenna Control Program")->setThreshold(Priority::INFO);

    // Here we will be tricky and add another appender to the
    // Category.  The second appender will be an output stream
    // appender that will catch DEBUG and above messages, while
    // the FileAppender instantiated as part of myLogger catches
    // INFO priority and above.  Ideally, we'd like a log4cpp analog
    // of log4j's LevelRangeFilter, which would allow us to
    // catch ONLY debug messages in the OstreamAppender. I will
    // likely build this eventually.
    OstreamAppender* osappender = new OstreamAppender("Antenna Control Debug",
		                                      &cout);
    osappender->setThreshold(Priority::DEBUG);

    // An Appender when added to Category becomes an additional output 
    // destination unless additivity is set to false. When it is false,
    // the appender added to the category replaces all previously 
    // existing appenders. Make myLogger accept multiple appenders.
    myLogger.setAdditivity(true);

    // A side-effect of setting additivity to true is that the
    // root appender of the Category (every Category has a default
    // root appender) will get passed the messages.  The root appender
    // appends to stderr, which is annoying. So we remove the
    // root appender.
    myLogger.getRoot().removeAllAppenders();

    // Remember, NDC is a stack, so we can push multiple levels 
    // of diagnostic context onto it.
    NDC::push("Root logger has been removed.");
    
    // Instantiate a layout will tells how to format the output stream file.
    // In this example, we use a layout that comes predefined with 
    // log4cpp. The log4cpp::BasicLayout includes a simple timestamp.
    Layout* layout = new BasicLayout();

    // Attach the layout object to ostream appender
    // and add it to myLogger.
    osappender->setLayout(layout);
    myLogger.addAppender(osappender);

    // Log an info message. This will go only to the output file.
    myLogger.info("Information is a good thing.");
    // Debug messages will go only to output stream appender.
    myLogger.debug("You are surrounded by twisty little passages, all alike.");

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
