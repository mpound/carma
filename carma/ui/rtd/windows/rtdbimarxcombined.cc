/*
 * rtdbimadrive.cc
 *
 * Gets and displays summary data about the BIMA Rx's
 *
 * @author Colby Gutierrez-Kraybill
 * $Id: rtdbimarxcombined.cc,v 1.21 2013/11/19 03:41:14 iws Exp $
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
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/LoRefSubsystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace log4cpp;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

static const std::string title           = "BIMA Rx Summary";
static const std::string helpTitle       = "BIMA Rx Summary";
static const std::string helpPageHeading = "BIMA Rx Summary HELP\n\n";
static const std::string helpSummary     = "Status of the BIMA receivers subsystem. ";

static std::string makeHelp()
{
	std::ostringstream oss;
	oss << helpPageHeading << helpSummary ;
	return oss.str();
}

static MonitorContainer *
createSISContainer( unsigned int bimaAntNum, MonitorDisplay &display )
{
  BimaSubsystem::BimaSpecific &bs = display.cms().bima(bimaAntNum).bimaSpecific();
  MonitorContainer *c = new MonitorContainer("SIS Mixers");

  // sis
  c->add( bs.sIS().sisband() );
  c->add( bs.sIS().sisbiasout() );
  c->add( bs.sIS().sismode() );
  c->add( bs.sIS().sisreadback() );
  c->add( bs.sIS().sisreadbacku() );

  // wba
  c->add( bs.wBA().idrain());
  c->add( bs.wBA().vgate());
  c->add( bs.wBA().vgatecmded());

  return c;
}

static MonitorContainer *
createPhaseLocksContainer( unsigned int bimaAntNum, MonitorDisplay &display )
{
  const int antIndex = bimaAntNum + 6; // A carma antenna number index
  AntennaCommon &ac = display.cms().bima(bimaAntNum).antennaCommon();
  BimaSubsystem::BimaSpecific &bs = display.cms().bima(bimaAntNum).bimaSpecific();
  ControlSubsystem &control = display.cms().control();

  MonitorContainer *c = new MonitorContainer("PhaseLocks");

  // Frequencies
  c->add( ac.lO().loFreq());
  c->add( ac.lO().oscFreq());
  c->add( control.antenna(antIndex).harmonic());
  c->add( ac.lO().yigFreq());
  c->add( bs.refs().yigCommanded());
  c->add( control.antenna(antIndex).refLoFreq());

  // 10MHz/50MHz
  c->add( bs.refs().tenOptPwr());
  c->add( bs.refs().tenStatus());
  c->add( bs.refs().fiftyOptPwr());

  // LOTerm
  c->add( bs.lOTerm().optPower());
  c->add( bs.lOTerm().rFin());
  c->add( bs.lOTerm().atten());
  c->add( bs.lOTerm().rFout());

  // X Lock
  c->add( ac.lO().yigState());
  c->add( ac.lO().yigIFLevel());
  c->add( ac.lO().yigError());

  // mm Lock
  c->add( bs.mMlock().state());
  c->add( bs.mMlock().error());
  c->add( bs.mMlock().iFLevel());
  c->add( bs.mMlock().loopgain());
  c->add( bs.mMlock().mmband());
  c->add( bs.mMlock().fiftyMHzRef());
  c->add( bs.mMlock().sweep());
  c->add( bs.mMlock().osc());
  c->add( bs.mMlock().phasenoise());

  return c;
}

static MonitorContainer *
createCryoContainer( unsigned int bimaAntNum, MonitorDisplay &display )
{
  BimaSubsystem::BimaSpecific &bs = display.cms().bima(bimaAntNum).bimaSpecific();
  MonitorContainer *c = new MonitorContainer("Cryogenics");

  // Dewar
  c->add( bs.dewar().stage1() );
  c->add( bs.dewar().stage2() );
  c->add( bs.dewar().stage3() );
  c->add( bs.dewar().stage4() );
  c->add( bs.dewar().stage5() );
  c->add( bs.dewar().heater3v() );
  c->add( bs.dewar().heater3ma() );

  // Compressor
  c->add( bs.dewar().tinlet() );
  c->add( bs.dewar().tdisch() );
  c->add( bs.dewar().texch() );
  c->add( bs.dewar().tsump() );

  // He Pressures
  c->add( bs.dewar().psupply() );
  c->add( bs.dewar().preturn() );

  return c;
}

static MonitorContainer *
createTempsContainer( unsigned int bimaAntNum, MonitorDisplay &display )
{
  AntennaIF& aif = display.cms().bima(bimaAntNum).antennaIfContainer(0).antennaIF();
  BimaSubsystem::BimaSpecific &bs = display.cms().bima(bimaAntNum).bimaSpecific();
  MonitorContainer *c = new MonitorContainer("Temperatures");

  // Temps
  c->add( bs.plate().temp() );
  c->add( bs.plate().heater() );
  c->add( bs.lOTerm().temp() );

  // Wrok around PamTemp shortname daftness
  const string n = string( "PAM" );
  aif.pamTemp().setShortName( n );
  c->add( aif.pamTemp() );

  c->add( bs.calPlate().tempAmb() );
  c->add( bs.temperatures().tcabinTop() );
  c->add( bs.temperatures().tcabinFan() );
  c->add( bs.temperatures().twaterIn() );
  c->add( bs.temperatures().twaterOut() );
  c->add( bs.temperatures().tsubrefl() );

  return c;
}

static MonitorContainer *
createOpticsContainer( unsigned int bimaAntNum, MonitorDisplay &display )
{
  AntennaCommon& ac = display.cms().bima(bimaAntNum).antennaCommon();
  BimaSubsystem::BimaSpecific &bs = display.cms().bima(bimaAntNum).bimaSpecific();
  MonitorContainer *c = new MonitorContainer("Optics");

  c->add( ac.calibrator().calState() );
  c->add( bs.optics().beamSelect() );
  c->add( bs.optics().opticsCntrl() );
  // the next 3 monitor points are no longer used at Cedar Flat, sep 2012
  // c->add( bs.optics().calpos() );
  // c->add( ac.optics().polarization() );
  // c->add( bs.optics().polpos() );
  c->add( bs.calPlate().tempAmb() );

  return c;
}

typedef std::vector<MonitorContainer *> MCVector;
typedef std::map<std::string, MCVector> TabDataType;

static void
dataHelper(MCVector &vec, MonitorContainer *container, ZebraVisitorPtr visitor)
{
    vec.push_back(container);
    visitor->addMonitorContainer(*container);
}

int carma::util::Program::main()
{
    // Create a dislay
    MonitorDisplay display(title);
    display.setSpecificHelp(helpTitle, makeHelp());

    // initialize tabs (names are in display order!)
    std::vector<std::string> tabNames;
    tabNames.push_back("PhaseLock");
    tabNames.push_back("SIS");
    tabNames.push_back("Cryo");
    tabNames.push_back("Temps");
    tabNames.push_back("Optics");

    TabDataType tabData;
    BOOST_FOREACH(const std::string &tabName, tabNames)
        tabData[tabName] = MCVector();

    // add data for each BIMA antenna
    ZebraVisitorPtr zebra(new ZebraVisitor());
    const std::vector<std::string> colLabels = RtTable::getBimaAntNames();
    for (size_t i = 0; i < colLabels.size(); i++) {
        dataHelper(tabData["PhaseLock"], createPhaseLocksContainer(i, display), zebra);
        dataHelper(tabData["SIS"], createSISContainer(i, display), zebra);
        dataHelper(tabData["Cryo"], createCryoContainer(i, display), zebra);
        dataHelper(tabData["Temps"], createTempsContainer(i, display), zebra);
        dataHelper(tabData["Optics"], createOpticsContainer(i, display), zebra);
    }

    // create a tab for each container, add it to the display
    BOOST_FOREACH(const std::string &tabName, tabNames) {
        const std::vector<MonitorContainer *> &tabContainers = tabData[tabName];

        MonitorSingleTableFolderPtr folder(new MonitorSingleTableFolder(tabName,
                colLabels, tabContainers, AUTO_SIZE, 1, 0, true, zebra));

        display.add(folder);
    }

    while (display.serveData()) {
        /* none */
    }

    return 0;
}
