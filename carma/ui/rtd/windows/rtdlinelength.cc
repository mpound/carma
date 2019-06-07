/*
 * @file
 *
 * RTD definition for Line Length window
 *
 * @author Chul Gwon (from Steve Scott)
 * $Id: rtdlinelength.cc,v 1.15 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/ZebraVisitor.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/LineLengthSubsystem.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui::rtd;

static MonitorContainer *
createChannelsContainer( unsigned int chanNum, ZebraVisitorPtr visitor, MonitorDisplay &display )
{
  LineLengthSubsystem &ll = display.cms().lineLength();
  MonitorContainer *c = new MonitorContainer("Channels");

  c->add( ll.initialized(chanNum) );
  c->add( ll.lineLength(chanNum) );
  c->add( ll.phase(chanNum) );
  c->add( ll.phaseRms(chanNum) );

  ll.phaseComplex(chanNum).setShortName("Amplitude (V)");
  ll.phaseComplex(chanNum).setStringRepresentation( MonitorPointComplex::AMP );
  c->add( ll.phaseComplex(chanNum) );

  c->add( ll.optPwr(chanNum) );
  c->add( ll.agc(chanNum) );
  c->add( ll.refChan(chanNum) );

  visitor->addMonitorContainer(*c);
  return c;
}

static MonitorContainer *
createRefsContainer( unsigned int chanNum, ZebraVisitorPtr visitor, MonitorDisplay &display )
{
  LineLengthSubsystem &ll = display.cms().lineLength();
  MonitorContainer *c = new MonitorContainer("Channels");

  ll.phaseRef(chanNum).setShortName("Phase");
  c->add( ll.phaseRef(chanNum) );
  ll.phaseRefRms(chanNum).setShortName("Phase RMS");
  c->add( ll.phaseRefRms(chanNum) );

  ll.phaseRefComplex(chanNum).setShortName("Amplitude (V)");
  ll.phaseRefComplex(chanNum).setStringRepresentation( MonitorPointComplex::AMP );
  c->add( ll.phaseRefComplex(chanNum) );

  ll.agcRef(chanNum).setShortName("AGC");
  c->add( ll.agcRef(chanNum) );
  ll.refRFPower(chanNum).setShortName("RF Power");
  c->add( ll.refRFPower(chanNum) );
  ll.offsetFreqLocked(chanNum).setShortName("Offset Freq Locked");
  c->add( ll.offsetFreqLocked(chanNum) );

  visitor->addMonitorContainer(*c);
  return c;
}

static std::string makeHelp()
{
	std::ostringstream oss;
	oss << "LINE LENGTH HELP\n\n"
		<< "Status of the line length subsystem.\n"
		<< "The Line Length subsystem is used to monitor the small changes in "
		<< "fiber length caused by thermal expansion and contraction. It works "
		<< "by measuring the round trip phases of the LO reference signals which "
		<< "each antenna reflects back through the fiber. In addition to measuring"
		<< " the line length phases, the Line Length subsystem also computes the "
		<< "phase correction for the LO1 freqeuncies.\n"
		<< "The columns are labelled by linelength channel number, "
		<< "which is not necessarily the same as CARMA antenna number. ";
	return oss.str();
}

int carma::util::Program::main()
{
	const std::string title = "Line Length";
	const std::string helpTitle = "Line Length";
	ZebraVisitorPtr zebraVisitor(new ZebraVisitor());

    // Create a dislay
    MonitorDisplay display(title);
    display.setSpecificHelp(helpTitle, makeHelp());

    RtFolder folder( "LineLength" );
    vector<string> labels1to15;
    vector<string> labels16to24;
    ostringstream oss;
    map<string, vector<MonitorContainer*> > tabData;
    for ( int i = 0; i < 15; i++ ) {
      oss << "LL" << i+1;
      labels1to15.push_back(string(oss.str()));
      oss.seekp(0);
      tabData["LL1-15"].push_back(
        createChannelsContainer( i, zebraVisitor, display ));
    }

    for ( int i = 15; i < 24; i++ ) {
      oss << "LL" << i+1;
      labels16to24.push_back(string(oss.str()));
      oss.seekp(0);
      tabData["LL16-24"].push_back(
        createChannelsContainer( i, zebraVisitor, display ));
    }

    display.add(MonitorSingleTableFolderPtr(new MonitorSingleTableFolder( string("LL1-15"), labels1to15,
	  tabData["LL1-15"], 0, 1, 0, true, zebraVisitor)));
    display.add(MonitorSingleTableFolderPtr(new MonitorSingleTableFolder( string("LL16-24"), labels16to24,
	  tabData["LL16-24"], 0, 1, 0, true, zebraVisitor)));

    vector<string> labelsRefs;
    for ( int i = 0; i < 3; i++ )
    {
      oss << "Reference " << i+1;
      labelsRefs.push_back(string(oss.str()));
      oss.seekp(0);
      tabData["References"].push_back(
	  createRefsContainer( i, zebraVisitor, display ));
    }

    display.add(MonitorSingleTableFolderPtr(new MonitorSingleTableFolder( string("References"), labelsRefs,
	  tabData["References"], 0, 1, 0, true, zebraVisitor)));

    // Loop forever serving data to the client
    while (display.serveData()) {
    }

    return EXIT_SUCCESS;
}
