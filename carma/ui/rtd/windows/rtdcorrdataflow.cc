/** @file
 * Retrieves and displays data for the spectral line correlator subsystems.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.5 $
 * $Date: 2014/06/20 15:55:47 $
 * $Id: rtdcorrdataflow.cc,v 1.5 2014/06/20 15:55:47 mpound Exp $
 * 
 * $CarmaCopyright$
 */

// C++ Standard Library includes
#include <iostream>
#include <sstream>
#include <vector>

// Carma includes
#include "carma/monitor/WbDataflowSubsystem.h"
#include "carma/monitor/SlDataflowSubsystem.h"
#include "carma/monitor/C3gDataflowSubsystem.h"
#include "carma/monitor/WbRemapperSubsystem.h"
#include "carma/monitor/SlRemapperSubsystem.h"
#include "carma/monitor/C3gRemapperSubsystem.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;
    
namespace { // anonymous 

string
makeHelp( void ) 
{
    ostringstream os;
    os <<"     CORRELATOR DATA FLOW HELP\n\n";
    os << "This window displays information about data received from"
       << " the correlator before it gets to the pipeline."
       << " There are two Tabs one for data received from the "
       << " correlator by correlator/transport/CorrDataUpdater.cc "
       << " and one for data received from CorrDataUpdater by "
       << " correlator/transport/CorrDataRemapper.cc."
       << " The values (e.g. number of baselines) should be the "
       << " same in these two sections. If not, that can tell you "
       << " where data may be dropped."
        ;
    return os.str();
}

} // end anonymous namespace

int 
Program::main() 
{
    try {

        typedef enum { SL, WB, C3G } CorrType;
        // Retrieve band from input options (no command line for rtds)
        const string typeString = Program::getStringParameter("string1");
        CorrType ctype;
        if ( typeString.find("sl") != string::npos ) ctype=SL;
        else if ( typeString.find("wb") != string::npos ) ctype=WB;
        else if ( typeString.find("c3g") != string::npos ) ctype=C3G;
        else ThrowCarmaError("Unrecognized correlator-type string:" << typeString <<". Should be one of {sl, wb,  c3g}. Check CarmaDisplay.cc");

        ostringstream titlebar;
        vector <MonitorContainer *> dataupdaterflows;
        vector <MonitorContainer *> remapperflows;
        vector <string> columnLabel;
        string corrTypeString;
        MonitorDisplay display("");
        switch ( ctype ) {
          case SL:
            {
            corrTypeString="Spectral Line";
            titlebar <<  corrTypeString << " Data Flow Status";
            const size_t msize = SlDataflowSubsystem::COUNT ;
            dataupdaterflows.reserve(msize);
            remapperflows.reserve(msize);
            columnLabel.reserve(msize);
                for ( size_t i = 0 ; i < msize; ++i ) {
                    ostringstream os;
                    os << "Band " << i+1;
                    columnLabel.push_back(os.str());
                    os.str("");
                    os << "Spectral DataUpdater flow " << columnLabel.at(i);
                    SlDataflowSubsystem & s = display.cms().slDataflow(i);
                    MonitorContainer * f = new MonitorContainer(os.str());
                    s.receivedTime().setFormat(MonitorPointAbstime::TIME);
                    s.publishedTime().setFormat(MonitorPointAbstime::TIME);
                    // show tenths of a second
                    s.receivedTime().setPrecision(1);
                    s.publishedTime().setPrecision(1);
                    f->add( s );
                    dataupdaterflows.push_back( f );

                    os.str("");
                    os << "Spectral Remapper flow " << columnLabel.at(i);
                    MonitorContainer * r = new MonitorContainer(os.str());
                    monitor::SlRemapperSubsystem & rm = display.cms().slRemapper(i);
                    rm.receivedTime().setFormat(MonitorPointAbstime::TIME);
                    rm.publishedTime().setFormat(MonitorPointAbstime::TIME);
                    rm.receivedTime().setPrecision(1);
                    rm.publishedTime().setPrecision(1);
                    r->add( rm );
                    remapperflows.push_back( r );
                }
            }
            
            break;
          case WB:
            {
                corrTypeString="Wideband ";
                titlebar <<  corrTypeString << " Data Flow Status";
                const size_t msize =  WbDataflowSubsystem::COUNT;
                dataupdaterflows.reserve(msize);
                remapperflows.reserve(msize);
                columnLabel.reserve(msize);
                for ( size_t i = 0 ; i < msize; ++i ) {
                    ostringstream os;
                    os << "Band " << i+9;
                    columnLabel.push_back(os.str());
                    //cout << "creating WB container for " << columnLabel.at(i) << endl;
                    os.str("");
                    os << "Wideband Dataflow " << columnLabel.at(i);
                    WbDataflowSubsystem & w = display.cms().wbDataflow(i);
                    MonitorContainer * f = new MonitorContainer(os.str());
                    w.receivedTime().setFormat(MonitorPointAbstime::TIME);
                    w.publishedTime().setFormat(MonitorPointAbstime::TIME);
                    w.receivedTime().setPrecision(1);
                    w.publishedTime().setPrecision(1);
                    f->add( w );
                    dataupdaterflows.push_back( f );

                    os.str("");
                    os << "Wideband Remapper flow " << columnLabel.at(i);
                    MonitorContainer * r = new MonitorContainer(os.str());
                    monitor::WbRemapperSubsystem & rm = display.cms().wbRemapper(i);
                    rm.receivedTime().setFormat(MonitorPointAbstime::TIME);
                    rm.publishedTime().setFormat(MonitorPointAbstime::TIME);
                    rm.receivedTime().setPrecision(1);
                    rm.publishedTime().setPrecision(1);
                    r->add( rm );
                    remapperflows.push_back( r );
                }
             }
             break;
            case C3G:
             {
                corrTypeString="C3G";
                titlebar <<  corrTypeString << " Data Flow Status";
                const size_t msize =  C3gDataflowSubsystem::COUNT;
                dataupdaterflows.reserve(msize);
                remapperflows.reserve(msize);
                columnLabel.reserve(msize);
                for ( size_t i = 0 ; i < msize; ++i ) {
                    ostringstream os;
                    os << "Band " << i+25;
                    columnLabel.push_back(os.str());
                    os.str("");
                    os << "C3G Dataflow " << columnLabel.at(i);
                    C3gDataflowSubsystem & w = display.cms().c3gDataflow(i);
                    MonitorContainer * f = new MonitorContainer(os.str());
                    w.receivedTime().setFormat(MonitorPointAbstime::TIME);
                    w.publishedTime().setFormat(MonitorPointAbstime::TIME);
                    w.receivedTime().setPrecision(1);
                    w.publishedTime().setPrecision(1);
                    f->add( w );
                    dataupdaterflows.push_back( f );

                    os.str("");
                    os << "C3G Remapper flow " << columnLabel.at(i);
                    MonitorContainer * r = new MonitorContainer(os.str());
                    monitor::C3gRemapperSubsystem & rm = display.cms().c3gRemapper(i);
                    rm.receivedTime().setFormat(MonitorPointAbstime::TIME);
                    rm.publishedTime().setFormat(MonitorPointAbstime::TIME);
                    rm.receivedTime().setPrecision(1);
                    rm.publishedTime().setPrecision(1);
                    r->add( rm );
                    remapperflows.push_back( r );
                }
            }
            break;
        }

        display.setTitle( titlebar.str());
        display.setSpecificHelp( corrTypeString + " Correlator Data Flow Help",
                                 makeHelp() );
        MonitorSingleTableFolderPtr folder1(new MonitorSingleTableFolder(
                "Stats for Data Received from Correlator",columnLabel,dataupdaterflows,20,2));
        display.add(folder1);
        MonitorSingleTableFolderPtr folder2(new MonitorSingleTableFolder(
                 "Stats for Data Received by CorrDataRemapper",columnLabel,remapperflows,AUTO_SIZE,2));
        display.add(folder2);

        // Serve the data out.
        while ( display.serveData() ) { }

        return EXIT_SUCCESS;

    } catch ( const ErrorException & ex ) {
        programLogErrorIfPossible( ex.getMessage());
        cerr << "Caught exception: " << ex.getMessage() << endl;
        return EXIT_FAILURE;
    } catch ( ... ) {
        const string err = "Caught unclassified exception";
        programLogErrorIfPossible(err);
        cerr << err << endl;
        return EXIT_FAILURE;
    }

} // End main

