/*
 * @file
 *
 * Gets data from the ovro subsystem Yig and Gunn modules and displays it.
 *
 * @author Original: Andy Beard (based on code from Steve Scott)
 * $id: $
 *
 * $CarmaCopyright$
 */


#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorTable.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/services/Global.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::ui::rtd;
using namespace carma::util;

static std::string makeHelp()
{
    ostringstream oss;
    oss << "       10M LO HELP\n\n"
        << "Status of the 10m LO CAN modules. "
        << "Fill in more info here... "
        ;
    return oss.str();
}

int
Program::main()
{

    // Create a dislay
    MonitorDisplay display("10m Antenna LO Component Status");
    display.setSpecificHelp("10m Antenna LO Component Help", makeHelp());

    const int width = AUTO_SIZE;
    vector<string> columnLabel = RtTable::getOvroAntNames();
    vector<MonitorContainer*> yigContainer;
    vector<MonitorContainer*> gunn1mmContainer;
    vector<MonitorContainer*> gunn3mmContainer;
    vector<MonitorContainer*> gunn1cmContainer;
    vector<MonitorContainer*> xac1mmContainer;
    vector<MonitorContainer*> xac3mmContainer;
    vector<MonitorContainer*> xac1cmContainer;
    vector<MonitorContainer*> xacYigContainer;
    vector<MonitorContainer*> lorefs;
    vector<MonitorContainer*> lorefxacs;
    MonitorContainer * xac;
    MonitorContainer * gunn;
    MonitorContainer * loref;
    for (unsigned int i = 0; i < Global::nOvroAntennas(); ++i) {

        yigContainer.push_back(&display.cms().ovro(i).yig());

        gunn = new MonitorContainer( "gunn1mm" );
        gunn->add( display.cms().ovro(i).gunn1mm().state() );
        gunn->add( display.cms().ovro(i).gunn1mm().gunnPll() );
        gunn1mmContainer.push_back( gunn );

        xac = new MonitorContainer("xacgunn1mm");
        xac->add( display.cms().ovro(i).gunn1mm().state() );
        xac->add( display.cms().ovro(i).gunn1mm().xac() );
        xac1mmContainer.push_back(xac);

        gunn = new MonitorContainer( "gunn3mm" );
        gunn->add( display.cms().ovro(i).gunn3mm().state() );
        gunn->add( display.cms().ovro(i).gunn3mm().gunnPll() );
        gunn3mmContainer.push_back( gunn );

        xac = new MonitorContainer("xacgunn3mm");
        xac->add( display.cms().ovro(i).gunn3mm().state() );
        xac->add( display.cms().ovro(i).gunn3mm().xac() );
        xac3mmContainer.push_back(xac);

        gunn = new MonitorContainer("gunn1cm" );
        gunn->add( display.cms().ovro(i).gunn1cm().state() );
        gunn->add( display.cms().ovro(i).gunn1cm().varactor() );
        gunn1cmContainer.push_back( gunn );

        xac = new MonitorContainer("xacgunn1cm");
        xac->add( display.cms().ovro(i).gunn1cm().state() );
        xac->add( display.cms().ovro(i).gunn1cm().xac() );
        xac1cmContainer.push_back(xac);

        xac = new MonitorContainer("dumberer");
        xac->add( display.cms().ovro(i).yig().state() );
        xac->add( display.cms().ovro(i).yig().xac() );
        xacYigContainer.push_back(xac);

        loref = new MonitorContainer( "loref" );
        loref->add( display.cms().ovro(i).loReferenceContainer().state() );
        loref->add(
            display.cms().ovro(i).loReferenceContainer().loReference() );
        lorefs.push_back( loref );

        xac = new MonitorContainer("dumbererer");
        xac->add( display.cms().ovro(i).loReferenceContainer().state() );
        xac->add( display.cms().ovro(i).loReferenceContainer().xac() );
        lorefxacs.push_back(xac);
    }

    MonitorSingleTableFolderPtr yigFolder(new MonitorSingleTableFolder(
                "  yig  ", columnLabel, yigContainer, width, 1));
    MonitorSingleTableFolderPtr g1mmFolder(new MonitorSingleTableFolder(
                "gunn 1mm", columnLabel, gunn1mmContainer, width, 2));
    MonitorSingleTableFolderPtr g3mmFolder(new MonitorSingleTableFolder(
                "gunn 3mm", columnLabel, gunn3mmContainer, width, 2));
    MonitorSingleTableFolderPtr g1cmFolder(new MonitorSingleTableFolder(
                "gunn 1cm", columnLabel, gunn1cmContainer, width, 2));
    MonitorSingleTableFolderPtr xacYigFolder(new MonitorSingleTableFolder(
                "  yig (xac)  ", columnLabel, xacYigContainer, width, 2));
    MonitorSingleTableFolderPtr xac1mmFolder(new MonitorSingleTableFolder(
                "gunn 1mm (xac)", columnLabel, xac1mmContainer, width, 2));
    MonitorSingleTableFolderPtr xac3mmFolder(new MonitorSingleTableFolder(
                "gunn 3mm (xac)", columnLabel, xac3mmContainer, width, 2));
    MonitorSingleTableFolderPtr xac1cmFolder(new MonitorSingleTableFolder(
                "gunn 1cm (xac)", columnLabel, xac1cmContainer, width, 2));
    MonitorSingleTableFolderPtr lorefFolder(new MonitorSingleTableFolder(
                "lo ref", columnLabel, lorefs, AUTO_SIZE, 2));
    MonitorSingleTableFolderPtr lorefXacFolder(new MonitorSingleTableFolder(
                "lo ref (xac)", columnLabel, lorefxacs, AUTO_SIZE, 2));

    // Add the folders to the display
    display.add(lorefFolder);
    display.add(yigFolder);
    display.add(g1cmFolder);
    display.add(g3mmFolder);
    display.add(g1mmFolder);
    display.add(lorefXacFolder);
    display.add(xacYigFolder);
    display.add(xac1cmFolder);
    display.add(xac3mmFolder);
    display.add(xac1mmFolder);

    // Loop forever serving data to the client
    while (display.serveData()) {
    }

    return 0;
}
