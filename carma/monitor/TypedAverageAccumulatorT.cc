#include "carma/monitor/TypedAverageAccumulatorT.h"

#include <map>

#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;


namespace {


void
addContainerMps(
    const MonitorContainer &                 container,
    const MonitorComponent::ARCHIVE_PRIORITY priority,
    map< tagIDType, MonitorPoint * > &       m )
{
    if ( priority == MonitorComponent::DONTARCHIVE )
        return;
        
    const int numChildren = container.getNumChildren();
    
    for ( int i = 0; i < numChildren; ++i ) {
        const MonitorContainer::Child child = container.getChild( i );
        
        if ( child.isMp() ) {
            MonitorPoint * const mp = child.mpPtr();
            
            // add the point to the list of those to be accumulated only
            // if it has a high enough (low enough in the enum) priority
            if ( priority >= mp->getArchivePriority() ) {
                m.insert( make_pair( mp->getTagID(), mp ) );
            }
        } else if ( child.isContainer() ) {
            addContainerMps( child.containerRef(), priority, m );
        } else {
            if ( child.isNull() )
                throw CARMA_ERROR( "getChild return NULL child" );
            else
                throw CARMA_ERROR( "child was neither point nor container" );
        }
    }
}


} // namespace < anonymous >


void
AverageAccumulatorBase::setMonSys(
    const MonitorSystem &                    monSys,
    const MonitorComponent::ARCHIVE_PRIORITY priority )
{
    vector< MonitorPoint * > mpVec;
    {
        map< tagIDType, MonitorPoint * > m;
        
        addContainerMps( monSys, priority, m );
        
        mpVec.reserve( m.size() );
        
        map< tagIDType, MonitorPoint * >::const_iterator i = m.begin();
        const map< tagIDType, MonitorPoint * >::const_iterator iEnd = m.end();
        
        for ( ; i != iEnd; ++i )
            mpVec.push_back( i->second );
    }
    
    setSeq( mpVec );
}
