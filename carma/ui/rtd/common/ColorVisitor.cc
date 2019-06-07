#include "carma/ui/rtd/common/ColorVisitor.h"
#include "carma/monitor/MonitorPoint.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;


ColorVisitor::ColorVisitor( CellColor goodColor, CellColor warnColor, 
	                    CellColor errorColor ) 
    : goodColor_(goodColor),
      warnColor_(warnColor),
      errorColor_(errorColor)
{

}


ColorVisitor::~ColorVisitor( )
try {
} catch ( ... ) {
    // Just stifle any exception
}


void
ColorVisitor::addToEffectedSet( const MonitorPoint & mp ) {
    effected_.insert( mp.getCanonicalName( ) );
}


void
ColorVisitor::postprocessMonitorCell( const MonitorPoint & mp,
                                            MonitorCell &        cell ) const {
    if ( effected_.find( mp.getCanonicalName( ) ) != effected_.end( ) ) 
    {
        cell.setGoodColor( goodColor_ );
        cell.setWarnColor ( warnColor_  );
        cell.setErrorColor ( errorColor_  );
    }
}
