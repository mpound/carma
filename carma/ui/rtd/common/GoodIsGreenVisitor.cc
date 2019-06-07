#include "carma/ui/rtd/common/GoodIsGreenVisitor.h"

#include "carma/monitor/MonitorPoint.h"
#include "carma/ui/rtd/common/MonitorCell.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;


GoodIsGreenVisitor::GoodIsGreenVisitor( )
{
}


GoodIsGreenVisitor::~GoodIsGreenVisitor( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
GoodIsGreenVisitor::addToEffectedSet( const MonitorPoint & mp )
{
    effected_.insert( mp.getCanonicalName() );
}


void
GoodIsGreenVisitor::postprocessMonitorCell( const MonitorPoint & mp,
                                            MonitorCell &        cell ) const
{
    if ( effected_.find( mp.getCanonicalName() ) != effected_.end() )
        cell.setGoodColor( GREEN_CELL_COLOR );
}
