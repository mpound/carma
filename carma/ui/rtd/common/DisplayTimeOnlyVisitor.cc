/** @file
 * Implementation for DisplayTimeOnlyVisitor class.
 * 
 * $Id: DisplayTimeOnlyVisitor.cc,v 1.1 2006/01/31 01:16:05 abeard Exp $
 */

#include "carma/ui/rtd/common/DisplayTimeOnlyVisitor.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/monitorPointSpecializations.h"

namespace {
    const int DEFAULT_CELL_WIDTH     = 12;
    const int DEFAULT_PRECISION      =  2;
} // End namespace <unnamed>

using namespace carma::monitor;
using namespace carma::ui::rtd;
using namespace std;

DisplayTimeOnlyVisitor::DisplayTimeOnlyVisitor( ) {
}

DisplayTimeOnlyVisitor::~DisplayTimeOnlyVisitor( ) 
try {
} catch ( ... ) {
    // Ignore
}

void 
DisplayTimeOnlyVisitor::addToEffectedSet( const MonitorPoint & mp ) {
    effected_.insert( mp.getCanonicalName( ) );
}

void 
DisplayTimeOnlyVisitor::postprocessMonitorCell( const MonitorPoint & mp,
                                                MonitorCell &        cell ) const
{ 
    if ( effected_.find( mp.getCanonicalName( ) ) != effected_.end( ) ) {
        // Downcast to MonitorPointAbstime reference, if it fails, dynamic_cast
        // throws a bad_cast exceptions since references can't dangle. 
        MonitorPoint & tmp = const_cast<MonitorPoint &>( mp ); // Cast away 
        MonitorPointAbstime & timecell = dynamic_cast<MonitorPointAbstime &>(tmp);
        timecell.setWidth( DEFAULT_CELL_WIDTH );
        timecell.setFormat( MonitorPointAbstime::TIME );
        timecell.setPrecision( DEFAULT_PRECISION );
    }
}
