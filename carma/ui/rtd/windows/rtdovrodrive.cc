/*
 *
 * Display status of ovro drive system.
 * More details can be found in the tracking or pointing window.
 *
 * @author Original: Steve Scott
 * $Id: rtdovrodrive.cc,v 1.24 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/monitor/OvroSubsystem.h"
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
	oss << "OVRO ANTENNA DRIVES HELP\n\n"
		<< "Status of the OVRO antenna drive subsystem. "
		<< "More details can be found in the tracking or pointing window. ";
	return oss.str();
}

int carma::util::Program::main()
{
    // Create a dislay
    MonitorDisplay display("OVRO Antenna Drives Status");
    display.setSpecificHelp("OVRO Drives Help", makeHelp());

    vector<string> columnLabel = RtTable::getOvroAntNames();
    vector<MonitorContainer*> summary;
    vector<MonitorContainer*> tracking;
    vector<MonitorContainer*> pointing;
    vector<MonitorContainer*> constants;
    vector<MonitorContainer*> limits;
    vector<MonitorContainer*> weather;
    vector<MonitorContainer*> system;
    vector<MonitorContainer*> driveModule;
    vector<MonitorContainer*> azEncoder;
    vector<MonitorContainer*> elEncoder;
    vector<MonitorContainer*> driveModuleXac;
    vector<MonitorContainer*> azEncoderXac;
    vector<MonitorContainer*> elEncoderXac;

    for (int i=0; i < Global::nOvroAntennas( ); ++i) {
        summary.push_back(&display.cms().ovro(i).drive());
        tracking.push_back(&display.cms().ovro(i).drive().track());
        pointing.push_back(&display.cms().ovro(i).drive().point());
        constants.push_back(&display.cms().ovro(i).drive().point().constants());
        limits.push_back(&display.cms().ovro(i).drive().limit());
        weather.push_back(&display.cms().ovro(i).drive().weather());
        system.push_back(&display.cms().ovro(i).drive().system());
        driveModule.push_back(&display.cms().ovro(i).drive().driveModule());

        // Encoder modules need special attention in order to tack state on.
        MonitorContainer * encoderContainer = new MonitorContainer( "dumb" );
        encoderContainer->add(
            display.cms().ovro(i).drive().azEncoder().state() );
        encoderContainer->add(
            display.cms().ovro(i).drive().azEncoder().encoder() );
        azEncoder.push_back( encoderContainer );
        encoderContainer = new MonitorContainer( "dumber" );
        encoderContainer->add(
            display.cms().ovro(i).drive().elEncoder().state() );
        encoderContainer->add(
            display.cms().ovro(i).drive().elEncoder().encoder() );
        elEncoder.push_back( encoderContainer );

        MonitorContainer * driveContainer = new MonitorContainer( "drivedumb" );
        driveContainer->add(
            display.cms().ovro(i).drive().driveModule().state() );
        driveContainer->add(
            display.cms().ovro(i).drive().driveModule().xac() );
        driveModuleXac.push_back( driveContainer );

        encoderContainer = new MonitorContainer( "dumberer" );
        encoderContainer->add(
            display.cms().ovro(i).drive().azEncoder().state() );
        encoderContainer->add(
            display.cms().ovro(i).drive().azEncoder().xac() );
        azEncoderXac.push_back( encoderContainer );

        encoderContainer = new MonitorContainer( "dumbererer" );
        encoderContainer->add(
            display.cms().ovro(i).drive().elEncoder().state() );
        encoderContainer->add(
            display.cms().ovro(i).drive().elEncoder().xac() );
        elEncoderXac.push_back( encoderContainer );
    }

    // Create folders with tables
    MonitorSingleTableFolderPtr summaryFolder(new MonitorSingleTableFolder(
                "summary", columnLabel, summary));
    MonitorSingleTableFolderPtr trackingFolder(new MonitorSingleTableFolder(
                "tracking", columnLabel, tracking));
    MonitorSingleTableFolderPtr pointingFolder(new MonitorSingleTableFolder(
                "pointing", columnLabel, pointing));
    MonitorSingleTableFolderPtr constantsFolder(new MonitorSingleTableFolder(
                "pointConstants", columnLabel, constants));
    MonitorSingleTableFolderPtr limitsFolder(new MonitorSingleTableFolder(
                "limits", columnLabel, limits));
    MonitorSingleTableFolderPtr weatherFolder(new MonitorSingleTableFolder(
                "weather", columnLabel, weather));
    MonitorSingleTableFolderPtr systemFolder(new MonitorSingleTableFolder(
                "system", columnLabel, system));
    MonitorSingleTableFolderPtr driveModuleFolder(new MonitorSingleTableFolder(
                "drive module", columnLabel, driveModule));
    MonitorSingleTableFolderPtr azEncoderFolder(new MonitorSingleTableFolder(
                "az encoder", columnLabel, azEncoder, AUTO_SIZE, 2));
    MonitorSingleTableFolderPtr elEncoderFolder(new MonitorSingleTableFolder(
                "el encoder", columnLabel, elEncoder, AUTO_SIZE, 2));
    MonitorSingleTableFolderPtr driveModuleXacFolder(new MonitorSingleTableFolder(
                "drive (xac)", columnLabel, driveModuleXac, AUTO_SIZE, 2 ));
    MonitorSingleTableFolderPtr azEncoderXacFolder(new MonitorSingleTableFolder(
                "az encoder (xac)", columnLabel, azEncoderXac, AUTO_SIZE, 2 ));
    MonitorSingleTableFolderPtr elEncoderXacFolder(new MonitorSingleTableFolder(
                "el encoder (xac)", columnLabel, elEncoderXac, AUTO_SIZE, 2 ));

    // Add the folders to the display
    display.add(summaryFolder);
    display.add(trackingFolder);
    display.add(pointingFolder);
    display.add(constantsFolder);
    display.add(limitsFolder);
    display.add(weatherFolder);
    display.add(systemFolder);
    display.add(driveModuleFolder);
    display.add(azEncoderFolder);
    display.add(elEncoderFolder);
    display.add(driveModuleXacFolder);
    display.add(azEncoderXacFolder);
    display.add(elEncoderXacFolder);

    while (display.serveData()) {
    }

    return 0;
}
