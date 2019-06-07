#include "carma/ui/rtd/common/GrayedOutVisitor.h"
#include "carma/monitor/MonitorPoint.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;


GrayedOutVisitor::GrayedOutVisitor( )
{

}


GrayedOutVisitor::~GrayedOutVisitor( )
try {
} catch ( ... ) {
    // Just stifle any exception
}


void
GrayedOutVisitor::addToEffectedSet( const MonitorPoint & mp ) 
{
    effected_.insert( mp.getCanonicalName( ) );
}


void
GrayedOutVisitor::postprocessMonitorCell( const MonitorPoint & mp,
                                          MonitorCell &        cell ) const 
{
    if ( effected_.find( mp.getCanonicalName( ) ) != effected_.end( ) ) 
    {
        cell.setGrayedOut( true );
        cell.setGoodColor( LIGHT_GRAY_CELL_COLOR );
        cell.setWarnColor( LIGHT_GRAY_CELL_COLOR );
        cell.setColor( LIGHT_GRAY_CELL_COLOR );
    }
}
