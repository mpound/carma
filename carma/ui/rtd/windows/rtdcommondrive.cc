/*
 * rtdcommondrives.cc
 *
 * Gets and displays data about all drive systems
 *
 * @author Colby Gutierrez-Kraybill
 * @author Chul Gwon
 * $Id: rtdcommondrive.cc,v 1.14 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include <boost/foreach.hpp>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/ZebraVisitor.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"

#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/AntennaCommon.h"

#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

namespace {

typedef AntennaCommon::ApertureCoefficients APC;
typedef AntennaCommon::SelectedApertMonitorPointEnum SelectedApertMPE;

const string title           = "Common Antenna Drives";
const string helpTitle       = "Common Antenna Drives";
const string helpPageHeading = "Common Antenna Drives HELP\n\n";
const string helpSummary     = "Status of the Common Antenna Drives. ";

string makeHelp() {
    ostringstream ost;
    ost
    << helpPageHeading
    << helpSummary
    ;
    return ost.str();
}

string
apertureToString( const SelectedApertMPE::SELECTEDAPERT aperture )
{
    switch ( aperture ) {
        case SelectedApertMPE::OPTICAL:
            return "Optical ";
        case SelectedApertMPE::RADIO1MM:
            return "Radio 1mm ";
        case SelectedApertMPE::RADIO3MM:
            return "Radio 3mm ";
        case SelectedApertMPE::RADIO1CM:
            return "Radio 1cm ";
        default:
            return "Unknown ";
    }
}

void
prependApertureToAPCShortNames( AntennaCommon::Constants & constants )
{
    for (int apInt = 0; apInt < SelectedApertMPE::SELECTEDAPERT_COUNT; ++apInt)
    {
        const SelectedApertMPE::SELECTEDAPERT ap =
            static_cast<SelectedApertMPE::SELECTEDAPERT>( apInt );
        const string apString = apertureToString( ap );
        APC & apc = constants.apertureCoefficients( ap );
        apc.elCollErr().setShortName( apString +
                                      apc.elCollErr().getShortName() );
        apc.crossElCollErr().setShortName( apString +
                                           apc.crossElCollErr().getShortName());
        apc.sag().setShortName( apString + apc.sag().getShortName() );
    }
}

MonitorContainer *
cloneContainer(const MonitorContainer &container, const int depth)
{
    MonitorContainer *c = new MonitorContainer(container.getName());
    MonitorPointIterator it(container, depth);

    while (it++)
        c->add(it.getMonitorPoint());

    return c;
}

} // namespace <unnamed>

typedef std::vector<MonitorContainer*> MCVector;
typedef std::map<std::string, MCVector> TabDataType;

static void
dataHelper(MCVector &vec, const MonitorContainer &container, const int depth, ZebraVisitorPtr visitor)
{
	MonitorContainer *c = cloneContainer(container, depth);
	vec.push_back(c);
	visitor->addMonitorContainer(*c);
}

// Replace tracking error cells with new that use other cell for bckgrnd clr 
void modAxisBackground(MonitorContainer* mc, MonitorTable& table, 
        CarmaMonitorSystem& cms, int nants)
{  
    int numRows = table.getNumRows();
    MonitorPointIterator mpi(*mc, 1);
    int azerrIndex = 0;
    while( mpi++ ) {
	    const string name = mpi.getMonitorPoint().getName();
        if (name == "errorAzimuthSky") break;
        azerrIndex++;
    }
    int elerrIndex = 0;
    MonitorPointIterator mpit(*mc, 1);
    while( mpit++ ) {
	    const string name = mpit.getMonitorPoint().getName();
        if (name == "errorElevation") break;
        elerrIndex++;
    }
    for (int i=0; i < nants ; i++) {
        const int antIdx     = i;
        const AntennaCommon& antCommon = cms.antennaCommon(antIdx);
        const AntennaCommon::Track& track = antCommon.drive().track();
        MonitorPointFloat& mpa      = track.errorAzimuthSky() ;
        MonitorPoint&      mpastate = track.azimuthAxisState() ;
        MonitorPointFloat& mpe      = track.errorElevation() ;
        MonitorPoint&      mpestate = track.elevationAxisState() ;
        CellPtr ca = MonitorCell::makeCell(mpa, mpastate);
        table.replaceCell(i*numRows+azerrIndex, ca);
        CellPtr ce = MonitorCell::makeCell(mpe, mpestate);
        table.replaceCell(i*numRows+elerrIndex, ce);
    }
}    
 
int carma::util::Program::main()
{
	// Create a dislay
	MonitorDisplay display(title);
	display.setSpecificHelp(helpTitle, makeHelp());


	// initialize tabs (names are in display order!)
	std::vector<std::string> tabNames;
	tabNames.push_back("Summary");
	tabNames.push_back("Tracking");
	tabNames.push_back("Pointing");
	tabNames.push_back("Constants");
	tabNames.push_back("Limits");

	TabDataType tabData;
	BOOST_FOREACH(const std::string &tabName, tabNames)
		tabData[tabName] = MCVector();

	// for each antenna, add data to each vector
	ZebraVisitorPtr zebra(new ZebraVisitor());
	const std::vector<std::string> colLabels = RtTable::getAntNames();
	int nants = colLabels.size();
	for (int i = 0 ; i < nants; i++) {
		const AntennaCommon::Drive& drive = 
		        display.cms().antennaCommon(i).drive();

		// minor fixups
		prependApertureToAPCShortNames(drive.point().constants());

		// add tab data
		dataHelper(tabData["Summary"], drive, 1, zebra);
		dataHelper(tabData["Tracking"], drive.track(), 1, zebra);
		dataHelper(tabData["Pointing"], drive.point(), 1, zebra);
		dataHelper(tabData["Constants"], drive.point().constants(), 2, zebra);
		dataHelper(tabData["Limits"], drive.limit(), 1, zebra);
	}

	// create a tab for each container, add it to the display
	BOOST_FOREACH(const std::string &tabName, tabNames) {
		const MCVector& tabContainers = tabData[tabName];

		MonitorSingleTableFolderPtr folder(new MonitorSingleTableFolder(tabName,
				colLabels, tabContainers, 8, 1, 0, true, zebra));
        if (tabName == "Tracking") {
            modAxisBackground(tabContainers.at(0), folder->getTable(), 
                    display.cms(), nants);
        }
		display.add(folder);
	}

	while (display.serveData()) {
		/* nothing */
	}

	return 0;
}
