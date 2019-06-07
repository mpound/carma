#include "carma/ui/rtd/common/CompositeMonitorTableVisitor.h"
#include <boost/foreach.hpp>

using namespace carma;
using namespace carma::ui::rtd;
using namespace std;

CompositeMonitorTableVisitor::CompositeMonitorTableVisitor( 
        const vector<MonitorTableVisitorPtr > & visitors ) :
    visitors_( visitors )
{
    // Nothing
}

CompositeMonitorTableVisitor::~CompositeMonitorTableVisitor( ) 
{
    // Nothing
}

bool
CompositeMonitorTableVisitor::acceptMonitorPoint( 
    const monitor::MonitorPoint & mp ) const
{
    BOOST_FOREACH(MonitorTableVisitorPtr v, visitors_) {
        if (v->acceptMonitorPoint( mp ))
            return true;
    }

    return false;
}
    
int
CompositeMonitorTableVisitor::calcCellWidth( 
    const monitor::MonitorPoint & mp ) const
{
    int maxCellWidth = 0;

    BOOST_FOREACH(MonitorTableVisitorPtr v, visitors_) {
        maxCellWidth = std::max( maxCellWidth,  v->calcCellWidth( mp ) );
    }

    return maxCellWidth;
}
    
void 
CompositeMonitorTableVisitor::postprocessMonitorCell(
    const monitor::MonitorPoint & mp,
    MonitorCell &                 monitorCell ) const
{
    BOOST_FOREACH(MonitorTableVisitorPtr v, visitors_) {
        v->postprocessMonitorCell( mp, monitorCell );
    }
}
    
::std::string 
CompositeMonitorTableVisitor::generateRowLabel(
    const monitor::MonitorPoint & mp,
    bool                          showUnits ) const
{
    return MonitorTableVisitor::generateRowLabel( mp, showUnits );
}



