#include "carma/ui/rtd/common/MonitorTableVisitor.h"

#include "carma/monitor/MonitorPoint.h"
#include "carma/ui/rtd/common/MonitorTable.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;


MonitorTableVisitor::MonitorTableVisitor( )
{
}


MonitorTableVisitor::~MonitorTableVisitor( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


bool
MonitorTableVisitor::acceptMonitorPoint( const MonitorPoint & mp ) const
{
    return true;
}


int
MonitorTableVisitor::calcCellWidth( const MonitorPoint & mp ) const
{
    return mp.getWidth();
}


void
MonitorTableVisitor::postprocessMonitorCell( const MonitorPoint & mp,
                                             MonitorCell &        cell ) const
{
}


string
MonitorTableVisitor::generateRowLabel( const MonitorPoint & mp,
                                       const bool           showUnits ) const
{
    return MonitorTable::generateRowLabel( mp, showUnits );
}
