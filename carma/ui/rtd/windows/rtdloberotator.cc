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

#include <sstream>
#include <vector>

#include "carma/util/Program.h"

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/LoberotatorSubsystem.h"

#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorTable.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

#define LRBOARD_STR	"LRBoard "

static std::string makeHelp()
{
    ostringstream oss;
    oss << "Loberotator Help" << endl << endl
        << ""
        << "" ;

    return oss.str();
}

int carma::util::Program::main()
{
    // Create a dislay
    MonitorDisplay display("Loberotator Status");
    display.setSpecificHelp("Loberotator Help", makeHelp());
    const int nBoards = display.cms().loberotator().boardCount();
    LoberotatorSubsystem& lr = display.cms().loberotator();

    vector<MonitorContainer*> cols;
    vector<string>            labels; // Labels for the columns
    int width = 11;

    for ( int i = 0; i < nBoards; i++ ) {
        ostringstream os;
        os << "Board" << i+1;
        labels.push_back(os.str());
        cols.push_back(&lr.board(i));
    }
    width = MonitorTable::autoFitColumnWidth(cols, 1, 3, MonitorTableVisitorPtr());
    MonitorSingleTableFolderPtr boardFolder(new MonitorSingleTableFolder("Boards", labels, cols, width));

    labels.clear();
    cols.clear();
    for ( int i = 0; i < nBoards; i++ ) {
        ostringstream os;
        os << "XAC" << i+1;
        labels.push_back(os.str());
        cols.push_back(&lr.board(i).xac());
    }
    width = MonitorTable::autoFitColumnWidth(cols, 1, 3, MonitorTableVisitorPtr());
    MonitorSingleTableFolderPtr xacFolder(new MonitorSingleTableFolder("XACs", labels, cols, width));

    const int nChanPerBoard = 4;  // Characteristic of the hardware
    // Our choice of how many boards to put in a folder
    const int nBoardPerFolder = 2;
    const int nFolders = (nBoards + nBoardPerFolder - 1)/nBoardPerFolder;
    const int nChanPerFolder = nChanPerBoard*nBoardPerFolder;
    for ( int f = 0; f < nFolders; f++ ) {
        labels.clear();
        cols.clear();
        for ( int c = 0; c < nChanPerFolder; c++ ) {
            ostringstream os;
            int chanIdx = f*nChanPerFolder + c;
            os << "Chan" << chanIdx+1;
            labels.push_back(os.str());
            cols.push_back(&lr.channel(chanIdx));
        }
        ostringstream folderLabel;
        folderLabel << "Chans " << f*nChanPerFolder+1 << "-"
                    << (f+1)*nChanPerFolder;
        string folderTitle = folderLabel.str();
        width = MonitorTable::autoFitColumnWidth(cols, 1, 3, MonitorTableVisitorPtr());
        MonitorSingleTableFolderPtr mstf(new MonitorSingleTableFolder(folderTitle, labels, cols, width));
        display.add(mstf);
    }
    display.add(boardFolder);
    display.add(xacFolder);

    MonitorSingleTableFolderPtr hostFolder(new MonitorSingleTableFolder("Host", "Values", lr.can().host(), 15));
    display.add(hostFolder);
    MonitorSingleTableFolderPtr busFolder(new MonitorSingleTableFolder("Bus", "Values", lr.can().bus(), 15));
    display.add(busFolder);

    while ( display.serveData() ) {}

    return 0;
}
