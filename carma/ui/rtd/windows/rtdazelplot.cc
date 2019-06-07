/*
 * @file
 * 
 * Displays azel plots for each subarray.
 *
 * @author Tom Costa
 *
 * @version $Revision: 1.9 $
 *
 * $CarmaCopyright$
 */

#include <iosfwd>
#include <sstream>
#include <vector>

#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/windows/AzelPlotManager.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;


namespace {


class MyDisplay : public MonitorDisplay {
    public:
        MyDisplay( const string & subtitle,
                   bool           visibleTimePanel,
                   int            numAnts,
                   int            numSas );
        
        ~MyDisplay( );
            
        RtAzelPlotPtr getAzelPlot( const int saNo );
            
    protected:
        virtual void internalUpdate( );
        
    private:
        AzelPlotManager azelPlotManager_;
};


MyDisplay::MyDisplay( const string & subtitle,
                      const bool     visibleTimePanel,
                      const int      numAnts,
                      const int      numSas ) :
MonitorDisplay( subtitle, visibleTimePanel ),
azelPlotManager_( cms(), numAnts, numSas )
{
}


MyDisplay::~MyDisplay( )
try {
} catch ( ... ) {
    return;
}


RtAzelPlotPtr
MyDisplay::getAzelPlot( const int saNo )
{
    return azelPlotManager_.getAzelPlot( saNo );
}


void
MyDisplay::internalUpdate( )
{
    azelPlotManager_.update();
}


void
addSaAzelPlotFolder( MyDisplay &            display,
                     const int              saNo)
{
    const string saName =
        ControlSubsystem::getSubarrayAlphanumericName( saNo );

    RtAzelPlotPtr azelPlot = display.getAzelPlot( saNo );

    RtFolderPtr folder(new RtFolder( saName ));
    folder->add( azelPlot );

    display.add( folder );
}


string
makeHelp( )
{
    ostringstream oss;
    
    oss
    << "       AZEL PLOT HELP\n\n" 
    << "You're on your own for now ";

    return oss.str();
}


void
runDisplay( )
{
    const int kNumAnts = 15;
    const int kNumSubarrays = 5;
    
    // Create a display
    MyDisplay display( "AzEl Plot", true, kNumAnts, kNumSubarrays );

    display.setSpecificHelp( "AzEl Plot Help", makeHelp() );

    for ( int saNo = 1; saNo <= kNumSubarrays; ++saNo )
        addSaAzelPlotFolder( display, saNo );
    
    while ( display.serveData() ) {
        // nothing
    }
}


} // namespace < anonymous >


int
Program::main( )
{
    {
        const string baseLogname = getLogname();
        
        setInstanceLogname( baseLogname + ".azelplot" );
    }

    runDisplay();

    return EXIT_SUCCESS;
}
