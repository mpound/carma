/**
 * @file
 *
 * Gets data from either the wbdc or sldc subsystem and displays.
 *
 * @author Original: Steve Scott
 * @author Andy Beard
 * $Id: rtddcsystem.cc,v 1.23 2013/11/19 03:41:14 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/monitor/LoMonitor.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/services/Global.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorTable.h"
#include "carma/util/Program.h"

using namespace std;
using namespace log4cpp;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

namespace {

    typedef std::vector< MonitorContainer * > MonitorContainerVec;
    typedef std::vector< std::string > StringVec;

    template < typename S >
    void populateCommonContainers(
            S                   & dcMonSubsys,
            StringVec           & qmColumnLabels,
            StringVec           & loMonChanColumnLabels,
            MonitorContainer    & nsContainer,
            MonitorContainer    & nsXacContainer,
            MonitorContainerVec & quadModContainer,
            MonitorContainerVec & qmXacContainer,
            MonitorContainer    & loMonContainer,
            MonitorContainer    & loMonXacContainer,
            MonitorContainerVec & loMonChanContainer )
    {
        // Populate monitor containers common to both sldc and wbdc systems.

        nsContainer.add( dcMonSubsys.noiseSourceContainer().state() );
        nsContainer.add( dcMonSubsys.noiseSourceContainer().noiseSource() );

        nsXacContainer.add( dcMonSubsys.noiseSourceContainer().state() );
        nsXacContainer.add( dcMonSubsys.noiseSourceContainer().xac() );

        loMonContainer.add( dcMonSubsys.loMonitorContainer().state() );
        loMonContainer.add( dcMonSubsys.loMonitorContainer().loMonitor() );

        loMonXacContainer.add( dcMonSubsys.loMonitorContainer().state());
        loMonXacContainer.add( dcMonSubsys.loMonitorContainer().xac());

        // Quad Mods
        const int numQuadMods = S::quadModContainerCount();
        for (int i = 0; i < numQuadMods; ++i) {

            ostringstream os;
            os << "Input " << (i + 1);
            qmColumnLabels.push_back( os.str() );

            // Yes, it's deliberately leaked
            MonitorContainer * mc = new MonitorContainer(
                    "QuadMod" + os.str( ) );
            MonitorContainer * mcxac = new MonitorContainer(
                    "QuadModXAC" + os.str( ) );

            mc->add( dcMonSubsys.quadModContainer(i).state() );
            mc->add( dcMonSubsys.quadModContainer(i).quadMod() );
            quadModContainer.push_back( mc );

            mcxac->add( dcMonSubsys.quadModContainer(i).state() );
            mcxac->add( dcMonSubsys.quadModContainer(i).xac() );
            qmXacContainer.push_back( mcxac );
        }

        // Lo Monitor channels
        const int numChans = LoMonitor::loChanCount();
        for ( int i = 0; i < numChans; ++i ) {

            ostringstream os;
            os << "Channel " << (i + 1);
            loMonChanColumnLabels.push_back(os.str());

            loMonChanContainer.push_back(
                &dcMonSubsys.loMonitorContainer().loMonitor().loChan(i) );
        }

    } // populateCommonContainers

    void populateSldcSpecificContainers(
            carma::monitor::SldcSubsystem & sldcMonSubsys,
            StringVec                     & loControlChanColumnLabels,
            StringVec                     & blockDcColumnLabels,
            MonitorContainer              & loControlContainer,
            MonitorContainer              & loControlXacContainer,
            MonitorContainerVec           & loControlChanContainer,
            MonitorContainerVec           & blockDcContainer,
            MonitorContainerVec           & blockDcXacContainer )
    {
        // Namely, populate the locontrol and block downconverter containers.

        // Lo Control
        SldcSubsystem::LoControlContainer & loControlMonContainer =
            sldcMonSubsys.loControlContainer( );

        loControlContainer.add( loControlMonContainer.state( ) );
        loControlContainer.add( loControlMonContainer.loControl( ) );

        loControlXacContainer.add( loControlMonContainer.state( ) );
        loControlXacContainer.add( loControlMonContainer.xac( ) );

        // Lo Control Channels
        const int numLoChans = LoControl::loCount( );
        for ( int i = 0; i < numLoChans; ++i ) {

            ostringstream os;
            os << "Channel " << (i + 1);
            loControlChanColumnLabels.push_back( os.str( ) );

            loControlChanContainer.push_back(
                &loControlMonContainer.loControl().lo( i ) );
        }

        // Block Downconverters
        const int numBdcs = SldcSubsystem::blockDownconverterContainerCount( );
        for ( int i = 0; i < numBdcs; ++i ) {
            ostringstream os;
            os << "Input " << (i + 1);
            blockDcColumnLabels.push_back( os.str() );

            // Yes, they're deliberately leaked
            MonitorContainer * blockDcMc =
                new MonitorContainer( "BlockDc" + os.str() );
            MonitorContainer * blockDcMcXac =
                new MonitorContainer( "BlockDcXac" + os.str() );

            SldcSubsystem::BlockDownconverterContainer & bdcMonContainer =
                sldcMonSubsys.blockDownconverterContainer( i );

            blockDcMc->add( bdcMonContainer.state( ) );
            blockDcMc->add( bdcMonContainer.blockDownconverter( ) );
            blockDcContainer.push_back( blockDcMc );

            blockDcMcXac->add( bdcMonContainer.state( ) );
            blockDcMcXac->add( bdcMonContainer.xac() );
            blockDcXacContainer.push_back( blockDcMcXac );
        }
    }

} // namespace < unnamed >


static std::string makeHelp()
{
    ostringstream oss;
    oss << "       NOISE AND 2ND LO HELP\n\n"
        << "Status of the noise source, quadrature modulators and "
        << "2nd LO subsystems.\n Fill in more info here... " ;
    return oss.str();
}

int
Program::main()
{
    // If you're using this as a template, just remember that RTD memory
    // management is a nightmare.  Basically just keep everything in scope
    // or you risk deleting something out from underneath RTD internals.
    // Said another way, never assume a deep copy!!!
    // On a related note, we deliberately leak certain data structures just
    // for convenience sake.  These are clearly marked in the code.
    // Don't worry as they're all one shot creations and the system will
    // clean 'em up in the end.

    const string string1Param = getStringParameter( "string1" );
    const bool sldc = ( string1Param.find("sldc") != string::npos );
    const bool wbdc = !sldc;
    const int width = AUTO_SIZE;  // Autoresize column width

    const string title = ( sldc ? string( "Spectral Line" ) :
                                  string( "Wideband" ) ) +
                           " Noise and 2nd LO Status";

    MonitorDisplay display( title );
    display.setSpecificHelp( "Noise and 2nd LO Help", makeHelp( ) );

    // Elements common between the sldc and wbdc systems.
    StringVec qmColumnLabels;
    StringVec loMonChanColumnLabels;
    MonitorContainer nsContainer( "NoiseSource" );
    MonitorContainer nsXacContainer( "NoiseSourceXAC" );
    MonitorContainerVec quadModContainer;
    MonitorContainerVec qmXacContainer;
    MonitorContainer loMonContainer( "LoMonitor" );
    MonitorContainer loMonXacContainer( "LoMonitorXAC" );
    MonitorContainerVec loMonChanContainer;

    if ( sldc ) {

        populateCommonContainers( display.cms().sldc(),
                                  qmColumnLabels,
                                  loMonChanColumnLabels,
                                  nsContainer,
                                  nsXacContainer,
                                  quadModContainer,
                                  qmXacContainer,
                                  loMonContainer,
                                  loMonXacContainer,
                                  loMonChanContainer );
    } else { // wbdc

        populateCommonContainers( display.cms().wbdc(),
                                  qmColumnLabels,
                                  loMonChanColumnLabels,
                                  nsContainer,
                                  nsXacContainer,
                                  quadModContainer,
                                  qmXacContainer,
                                  loMonContainer,
                                  loMonXacContainer,
                                  loMonChanContainer );
    }

    // Create folders common to both systems
    MonitorSingleTableFolderPtr nsFolder(new MonitorSingleTableFolder(
            "Noise Source", "Value", nsContainer, width, 2));
    MonitorSingleTableFolderPtr nsXacFolder(new MonitorSingleTableFolder(
            "Noise Source XAC", "Value", nsXacContainer, width, 2));

    MonitorSingleTableFolderPtr qmFolder(new MonitorSingleTableFolder(
            "Quad Mods", qmColumnLabels, quadModContainer, width, 2));
    MonitorSingleTableFolderPtr qmXacFolder(new MonitorSingleTableFolder(
            "Quad Mod XACs", qmColumnLabels, qmXacContainer, width, 2));

    // Tables are deliberately leaked
    MonitorTablePtr loTable = MonitorTable::makeTable( "LoMonitor",
                                                      loMonContainer,
                                                      width,
                                                      2,
                                                      0,
                                                      true,
                                                      false,
                                                      MonitorTableVisitorPtr() );
    MonitorTablePtr loChanTable = MonitorTable::makeTable( loMonChanColumnLabels,
                                                          loMonChanContainer,
                                                          width,
                                                          1,
                                                          0,
                                                          true,
                                                          false,
                                                          MonitorTableVisitorPtr() );
    RtFolderPtr loMonFolder(new RtFolder("LO Monitor"));
    loMonFolder->add(loTable);
    loMonFolder->add(loChanTable);
    MonitorSingleTableFolderPtr loMonXacFolder(new MonitorSingleTableFolder(
                                             "Lo Monitor XAC",
                                             "Lo Monitor",
                                             loMonXacContainer,
                                             width, 2));

    // Now bifurcate, add in any subsystem dependent windows and serve data.
    if ( wbdc ) {

        // wbdc contains no additional folders - form display and run

        // Build display, tabs are ordered based on order in which added
        display.add( nsFolder );
        display.add( qmFolder );
        display.add( loMonFolder );
        display.add( nsXacFolder );
        display.add( qmXacFolder );
        display.add( loMonXacFolder );

        while ( display.serveData( ) ); // Serve the data out.

    } else { // sldc

        // The sldc system adds the block downconverters and lo control modules.

        StringVec loControlChanColumnLabels;
        StringVec blockDcColumnLabels;
        MonitorContainerVec blockDcContainer;
        MonitorContainerVec blockDcXacContainer;
        MonitorContainer loControlContainer( "LoControl" );
        MonitorContainer loControlXacContainer( "LoControlXAC" );
        MonitorContainerVec loControlChanContainer;

        populateSldcSpecificContainers( display.cms().sldc(),
                                        loControlChanColumnLabels,
                                        blockDcColumnLabels,
                                        loControlContainer,
                                        loControlXacContainer,
                                        loControlChanContainer,
                                        blockDcContainer,
                                        blockDcXacContainer );

        MonitorSingleTableFolderPtr blockDcFolder(new MonitorSingleTableFolder( "Block Downconverters",
                                                blockDcColumnLabels,
                                                blockDcContainer, width, 2 ));
        MonitorSingleTableFolderPtr blockDcXacFolder(new MonitorSingleTableFolder( "Block Dc XAC",
                                                   blockDcColumnLabels,
                                                   blockDcXacContainer,
                                                   width, 2 ));
        MonitorSingleTableFolderPtr loControlXacFolder(new MonitorSingleTableFolder( "Lo Control XAC",
                                                     "Lo Control",
                                                     loControlXacContainer,
                                                     width, 2 ));

        RtFolderPtr loControlFolder(new RtFolder( "LO Control" ));
        MonitorTablePtr loControlTable = MonitorTable::makeTable(
                "LoControl",
                loControlContainer,
                width,
                2,
                0,
                true,
                false,
                MonitorTableVisitorPtr() );
        MonitorTablePtr loControlChanTable = MonitorTable::makeTable(
                loMonChanColumnLabels,
                loControlChanContainer,
                width,
                1,
                0,
                true,
                false,
                MonitorTableVisitorPtr() );
        loControlFolder->add( loControlTable );
        loControlFolder->add( loControlChanTable );

        // Build display, tabs are ordered based on order in which added
        display.add( nsFolder );
        display.add( qmFolder );
        display.add( blockDcFolder );
        display.add( loMonFolder );
        display.add( loControlFolder );
        display.add( nsXacFolder );
        display.add( qmXacFolder );
        display.add( blockDcXacFolder );
        display.add( loMonXacFolder );
        display.add( loControlXacFolder );

        while ( display.serveData( ) ); // Serve the data out.

    }

    return 0;
}
