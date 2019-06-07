/*
 * @file
 *
 * Gets data from the carma test subsystem and displays.
 *
 * @author Original: Steve Scott
 * $id: $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/services/Global.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

static std::string makeHelp()
{
	std::ostringstream oss;
	oss << "10M ANTENNA CRYO HELP\n\n"
		<< "Status of the 10m antenna cryo subsystem. "
		<< "Fill in more info here... ";
	return oss.str();
}

int carma::util::Program::main()
{
    // Create a dislay
    MonitorDisplay display("10m Antenna Cryo Status");
    display.setSpecificHelp("10m Cryo Help", makeHelp());

    vector<string> columnLabel = RtTable::getOvroAntNames();
    vector<MonitorContainer*> dewars;
    vector<MonitorContainer*> compressors;
    vector<MonitorContainer*> dewarXacs;
    vector<MonitorContainer*> compressorXacs;
    MonitorContainer* xac;
    for (int i=0; i < Global::nOvroAntennas(); ++i) {
        dewars.push_back(&display.cms().ovro(i).cryo().dewar());
        compressors.push_back(&display.cms().ovro(i).cryo().compressor());
        xac = new MonitorContainer("dumb"); // Dummy to add state to xac pgs.
        xac->add( display.cms().ovro(i).cryo().dewar().state() );
        xac->add( display.cms().ovro(i).cryo().dewar().xac() );
        dewarXacs.push_back(xac);
        xac = new MonitorContainer("dumber"); // Dummy to add state to xac pgs.
        xac->add( display.cms().ovro(i).cryo().compressor().state() );
        xac->add( display.cms().ovro(i).cryo().compressor().xac() );
        compressorXacs.push_back(xac);
    }

    MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
                " compressor ", columnLabel, compressors));
    MonitorSingleTableFolderPtr folder2(new MonitorSingleTableFolder(
                "   dewar   ", columnLabel, dewars));
    MonitorSingleTableFolderPtr folder3(new MonitorSingleTableFolder(
                "dewar (xac)", columnLabel, dewarXacs, AUTO_SIZE, 2));
    MonitorSingleTableFolderPtr folder4(new MonitorSingleTableFolder(
                "compressor (xac)", columnLabel, compressorXacs, AUTO_SIZE, 2));

    // Turn on audio on all  fridgeDriveState cells
    int i = 0;
    vector<int> fridgeIndex;
    for (int a=0; a < Global::nOvroAntennas(); ++a) {
        MonitorPointIterator mpi(display.cms().ovro(a).cryo().compressor(), 1);
        while (mpi++) {
            string name = mpi.getMonitorPoint().getName();
            if (name == "fridgeDriveState") {
                fridgeIndex.push_back(i);
            }
            //cout << i << "  " << fridgeIndex.size() << "   "<< name << endl;
            i++;
         }
    }
    RtTable& compressorTable = folder1->getTable();
    for(unsigned int i=0; i < fridgeIndex.size(); i++) {
        //cout << i << "  " << compressorTable.getNumCells() << " " << fridgeIndex.at(i) << endl;
        compressorTable.getCell(fridgeIndex.at(i)).setAudio('E');
    }

    // Add the folders to the display
    display.add(folder1);
    display.add(folder2);
    display.add(folder3);
    display.add(folder4);

    // Loop forever serving data to the client
    while (display.serveData()) {
    }

    return 0;
}
