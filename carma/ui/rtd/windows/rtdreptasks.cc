/**
 * @file
 *
 * Loberotator display window
 *
 * @author Steve Scott
 *
 * $id:$
 *
 * $CarmaCopyright$
 */

#include <iostream>
#include <sstream>
#include <vector>

#include "carma/util/Program.h"

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/ControlSubsystemExt.h"

#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorTable.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

static std::string makeHelp()
{
    ostringstream oss;
    oss << "Repetitive Tasks Help" << endl << endl
        << "Repetitive tasks should be run on a recurring basis to "
        << "ensure smooth operations. "
        << "The repeat interval is unique for each task. " 
        << "There is also a 'reminder' interval that is used by the "
        << "fault system to send an email or sound an alarm "
        << "and an 'auto' time that can be used to trigger automatic "
        << "execution of the task by the standard script or queue. "
        << "All intervals displayed are in days. "
        << "The list of tasks that are displayed is generated when the "
        << "window is started and is not dynamic. "
        << "To update the task list the window needs to be restarted "
        << "(a morph will do this). "
        ;
    return oss.str();
}

int carma::util::Program::main()
{
    // Create a dislay
    MonitorDisplay display("Repetitive Tasks Status");
    display.setSpecificHelp("Repetitive Tasks Help", makeHelp());
    // Need values of some MP's to determine layout
    display.cms().read();
        
    ControlSubsystem& cs = display.cms().control();
    
    vector<int> taskIndices;
    int l = cs.repTaskCount();
    
    for (int i=0; i<l; i++) {    
        ControlSubsystem::RepTask& rt = cs.repTask(i);
        double last = rt.timeLastDone().getValue();
        string tname = rt.taskName().getValue();
        //cout << "Last:" << last << "  tname:" << tname << endl;
        if ((last < 1000) | (last > 100000)) continue;
        if (tname.size() > 0) taskIndices.push_back(i);
    }
    int nrows = taskIndices.size();    

    vector<MonitorContainer*> cols;
    vector<string>            labels; // Labels for the columns

    labels.push_back("Task name");
    labels.push_back("Last completed");
    labels.push_back("Since");
    labels.push_back("Interval");
    labels.push_back("Remind");
    labels.push_back("Auto");
    
    MonitorContainer* tname = new MonitorContainer("");
    cols.push_back(tname);
    MonitorContainer* lastcomp = new MonitorContainer("Last");
    cols.push_back(lastcomp);
    MonitorContainer* since = new MonitorContainer("Since");
    cols.push_back(since);
    MonitorContainer* interval = new MonitorContainer("Interval");
    cols.push_back(interval);
    MonitorContainer* remind = new MonitorContainer("Remind");
    cols.push_back(remind);
    MonitorContainer* autom = new MonitorContainer("Auto");
    cols.push_back(autom);
    for (int j = 0; j< nrows; j++) {
        int i = taskIndices[j];
        ControlSubsystem::RepTask& rti = cs.repTask(i);
        tname->add(rti.taskName());
        lastcomp->add(rti.timeLastDone());
        since->add(rti.since());
        interval->add(rti.repeatInterval());
        remind->add(rti.reminderInterval());
        autom->add(rti.autoInterval());
    }
    MonitorSingleTableFolderPtr taskFolder(new MonitorSingleTableFolder(
                "Tasks", labels, cols, AUTO_SIZE_EACH_COLUMN, 1,0,false,
                MonitorTableVisitorPtr(),false,false));
    display.add(taskFolder);

    while ( display.serveData() ) {}

    return 0;
}
