/*
 * rtdtest.cc
 *
 * Gets data from the carma test subsystem and displays.
 * Extra tracing is added to help debug problems.
 *
 * @author Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>
#include <cerrno>
#include <cstring>

#include <pthread.h>

#include <log4cpp/NDC.hh>

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/TestSubsystem.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"


using namespace std;
using namespace log4cpp;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

static std::string makeHelp()
{
	ostringstream oss;
	oss << "       TEST SUBSYSTEM HELP\n\n"
		<< "The test subsystem is a subsystem used for testing and "
		<< "its monitor values are driven by a simulator program. ";
	return oss.str();
}

int
Program::main()
{
    log4cpp::NDC::push( "Main thread" );

    if ( true ) {
        const pthread_t me = pthread_self( );

        CARMA_CPTRACE( Trace::TRACE7, "pthread_self() = " << me );

        int policy;
        struct sched_param param;

        const int r = pthread_getschedparam( me, &policy, &param );

        CARMA_CPTRACE( Trace::TRACE7, "pthread_getschedparam returned " << r
                                      << " (" << strerror( r ) << ")" );
    }

    //cout << "Rtdtest, confdir:" << Program::getConfDir() << endl;

    // Create a dislay
    MonitorDisplay display("TestSubsystem status");
    display.setSpecificHelp("Test Help", makeHelp());
    CARMA_CPTRACE(Trace::TRACE6, "MonitorDisplay created");

    // Create a folder (folders are the third dimension)
    // Even though we only have one we must use this construct
    RtFolderPtr folder(new RtFolder("testing"));
    // Add the folder to the display
    display.add(folder);


    // This is a 3 pixel spacer that we will use in the layouts below
    RtSpacer sp3(3);
    // This is a 5 pixel spacer used at the end of lines
    RtSpacer endsp(5);
    // This is a spring with a minimum width of 10 pixels and a springiness=1.0
    RtSpring spring(10, 1.0);


    TestSubsystem::Box& box = display.cms().test().box();

    //******* Layout box2
    // The layout used is the "BoxLayout" (boxes, springs, & spacers)

    // This box is the top container, and will contain some horizontal lines,
    // so it is a vertical box.
    // The string arg is just a label for debugging
    RtVBoxPtr box2(new RtVBox("box2"));
    // Set border to be a line below it to separate from next box
    box2->setBorder(ONE_PIXEL_ABOVE_BORDER);
    // Add it to the folder
    folder->add(box2);


    // Auto generated table
    RtTablePtr table2(new RtTable("table2"));
    table2->setBorder(ONE_PIXEL_ABOVE_BORDER);
    folder->add(table2);
    table2->addCol(RtColumn::makeColumn("Ave"));

    // Put all of mp's from Box into a vector
    vector<MonitorPoint*> v;
    // Specifying maxLevel=1 limits the mp to just this container
    int maxLevels = 1;
    MonitorPointIterator mpi(box, maxLevels);
    while (mpi++) v.push_back(&mpi.getMonitorPoint());

    for (::size_t i=0; i< v.size(); i++) {
        table2->addRow(RtRow::makeRow(v[i]->getShortName()));
    }

    int width = 20;
    for (::size_t i=0; i< v.size(); i++) {
        // Increase mp value string width
        if (v[i]->getWidth() < width)v[i]->setWidth(width);
        // Add the monitor cells to the table
        table2->add(MonitorCell::makeCell(width, *(v[i])));
    }

    CARMA_CPTRACE(Trace::TRACE6, "Folders completely constructed");

    while (display.serveData()) {
    }

    return 0;
}
