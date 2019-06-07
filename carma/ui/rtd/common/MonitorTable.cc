//!
//! An automatically generated table for a MonitorDisplay
//!
//! @author Steve Scott
//!
//! $CarmaCopyright$
//!

#include <iostream>

#include "carma/ui/rtd/common/MonitorTable.h"

#include "carma/monitor/MonitorContainer.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorTableVisitor.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;


namespace {

const int kMinColumnWidth = 3;

}  // namespace < anonymous >


MonitorColumnInfo::MonitorColumnInfo(
    const ::std::string&             inHeading,
    const monitor::MonitorContainer& inContainer) :
heading( inHeading ),
container( &inContainer ) {
}


MonitorTable::MonitorTable(
    const vector<MonitorColumnInfo>&  columns, 
    const int                         columnWidth,
    const int                         hierarchyDepth,
    const int                         sampleNo,
    const bool                        unitsInRowLabels,
    const bool                        switchRowsAndColumns,
    const MonitorTableVisitorPtr      visitor,
    const bool                        displayRowLabels) :
RtTable( "autotable" ),
hierarchyDepth_( hierarchyDepth ),
sampleNo_( sampleNo ) {
    const ::size_t numColumns = columns.size();

    if ( switchRowsAndColumns )
        setReverseOrder();
    
    int defaultWidth;
    vector<int> individualWidths;
    
    if ( columnWidth == AUTO_SIZE ) {
        defaultWidth = autoFitColumnWidth( columns,
                                           hierarchyDepth,
                                           kMinColumnWidth,
                                           visitor );
    } else if ( columnWidth == AUTO_SIZE_EACH_COLUMN ) {
        if ( switchRowsAndColumns ) {
            defaultWidth = 7;
            
            if ( columns.empty() == false ) {
                const MonitorContainer * const container0 =
                    columns.at( 0 ).container;
                
                if ( container0 == 0 )
                    throw logic_error( "NULL MonitorContainer pointer" );
                
                individualWidths = getWidths( *container0,
                                              hierarchyDepth,
                                              kMinColumnWidth,
                                              visitor);
            }
        } 
        else {
            defaultWidth = 7;
            MonitorContainer* c = new MonitorContainer("x");
            for (unsigned int i=0; i<numColumns; i++) {
                const MonitorContainer* mc = columns.at(i).container;
                MonitorPointIterator mpi(*mc, hierarchyDepth);
                mpi++;
                MonitorPoint& mp = mpi.getMonitorPoint();
                c->add(mp);
                individualWidths.push_back(mp.getWidth());
                //cout<<"Name:"<<mp.getName()<<"  w:" << mp.getWidth() << endl;
            }
        }
    } else
        defaultWidth = columnWidth;

    for ( ::size_t i = 0; i < numColumns; ++i ) {
        if ( switchRowsAndColumns ) 
            addRow( RtRow::makeRow( columns.at( i ).heading ) );
        else 
            addCol( RtColumn::makeCol( columns.at( i ).heading ) );
    }

    // Get the row labels from the monitor points in the first column
    if (numColumns > 0) {
        const MonitorContainer* const container0 = columns.at(0).container;
        
        if (container0 == 0)
            throw logic_error( "NULL MonitorContainer pointer" );
            
        addRowLabels(*container0, unitsInRowLabels, switchRowsAndColumns, 
                      visitor, displayRowLabels );
    }
    
    // And the monitor points in each column
    for ( ::size_t i = 0; i < numColumns; ++i ) {
        const MonitorContainer* const container = columns.at( i ).container;
        
        if ( container == 0 )
            throw logic_error( "NULL MonitorContainer pointer" );

        if (switchRowsAndColumns || (individualWidths.size() == 0)) {
            addContainerMonitorPoints(*container,
                                      visitor,
                                      defaultWidth,
                                      individualWidths);
        }
        else {
            vector <int> dummy;
            int w = individualWidths.at(i);
            addContainerMonitorPoints(*container, visitor, w, dummy);
        }
    }
}


MonitorTable::~MonitorTable( )
try {
} catch ( ... ) {
    // Just stifle any exception
}


string
MonitorTable::generateRowLabel( const MonitorPoint & mp,
                                const bool           showUnits )
{
    string rowLabel = mp.getShortName();
    
    if ( showUnits ) {
        const string units = mp.getUnits();        
        if ( units.empty( ) == false ) {
            rowLabel += " (";
            rowLabel += units;
            rowLabel += ")";
        }
    }    
    return rowLabel;
}


void
MonitorTable::addRowLabels(
    const MonitorContainer&           container,
    const bool                        unitsInRowLabels,
    const bool                        switchRowsAndColumns,
    const MonitorTableVisitorPtr      visitor,
    const bool                        displayRowLabels) 
{
    MonitorPointIterator mpi( container, hierarchyDepth_ );

    while ( mpi++ ) {
        const string name = mpi.getMonitorPoint().getShortName();
    }

    mpi.reset( );
    
    while ( mpi++ ) {
        const MonitorPoint & mp = mpi.getMonitorPoint();

        if ( (visitor.get() == 0) || visitor->acceptMonitorPoint( mp ) ) {
            string rowLabel;
            
            if ( visitor.get() == 0 )
                rowLabel = generateRowLabel( mp, unitsInRowLabels );
            else
                rowLabel = visitor->generateRowLabel( mp, unitsInRowLabels );
            
            if (!displayRowLabels) rowLabel = "";
            if ( switchRowsAndColumns ) 
                addCol( RtColumn::makeCol( rowLabel ) );
            else 
                addRow( RtRow::makeRow( rowLabel ) );
                
        }
    }
}


void
MonitorTable::addContainerMonitorPoints(
    const MonitorContainer&           container,
    const MonitorTableVisitorPtr      visitor,
    const int                         defaultWidth,
    const vector< int > &             individualWidths )
{
    ::size_t cellsAdded = 0;

    vector< int >::const_iterator iw = individualWidths.begin();
    const vector< int >::const_iterator iwEnd = individualWidths.end();
    
    MonitorPointIterator mpi( container, hierarchyDepth_ );

    int row = 1;
    while ( mpi++ ) {
        MonitorPoint & mp = mpi.getMonitorPoint( );

        if ( (visitor.get() == 0) || visitor->acceptMonitorPoint( mp ) ) {
            // Modify mp value string width
            int width;
            
            if ( iw == iwEnd )
                width = defaultWidth;
            else {
                width = *iw;
                ++iw;
            }
            mp.setWidth(width);
    
            // Add the monitor cells to the table
            MonitorCellPtr const monitorCell =
                MonitorCell::makeCell( width, mp, sampleNo_ );
                
            addCell(monitorCell);

            ++cellsAdded;
            
            if ( visitor.get() != 0 )
                visitor->postprocessMonitorCell( mp, *monitorCell );
            row++;    
        }
    }
    
    if ( cellsAdded != getNumRows( ) ) {
        ostringstream oss;
        
        oss << "ERROR: " << cellsAdded << " cells were added for container \""
            << container.getCanonicalName( ) << " but the table has "
            << getNumRows( ) << " rows.";
            
        CARMA_CPTRACE( Trace::TRACE1, oss.str( ) );
    }
}


vector< int >
MonitorTable::getWidths(
    const MonitorContainer &          container,
    const int                         levels,
    const int                         minWidth,
    const MonitorTableVisitorPtr      visitor ) {
    vector< int > widths;

    MonitorPointIterator mpi( container, levels );
    
    while ( mpi++ ) {
        const MonitorPoint & mp = mpi.getMonitorPoint();

        if ( (visitor.get() == 0) || visitor->acceptMonitorPoint( mp ) ) {
            int mpWidth;
            
            if ( visitor.get() == 0 )
                mpWidth = mp.getWidth( );
            else
                mpWidth = visitor->calcCellWidth( mp );
                
            const int width = max( minWidth, mpWidth );
            
            widths.push_back( width );

            CARMA_CPTRACE( Trace::TRACE6,
                           mp.getCanonicalName() << ": " <<
                           mpWidth << " " << width );
        }
    }

    return widths;
}


int
MonitorTable::autoFitColumnWidth(
    const vector< const MonitorContainer * > & containers,
    const int                                  levels,
    const int                                  minWidth,
    const MonitorTableVisitorPtr               visitor ) {
    int columnWidth = minWidth;

    {
        vector< const MonitorContainer * >::const_iterator i =
            containers.begin();

        const vector< const MonitorContainer * >::const_iterator iEnd =
            containers.end();
            
        for ( ; i != iEnd; ++i ) {
            const MonitorContainer * const container = *i;
                
            if ( container == 0 )
                throw logic_error( "NULL MonitorContainer pointer" );
                
            MonitorPointIterator mpi( *container, levels );
            
            while ( mpi++ ) {
                const MonitorPoint & mp = mpi.getMonitorPoint( );

                if ( (visitor.get() == 0) || visitor->acceptMonitorPoint( mp ) ) {
                    int mpWidth;
                    
                    if ( visitor.get() == 0 )
                        mpWidth = mp.getWidth( );
                    else
                        mpWidth = visitor->calcCellWidth( mp );
                        
                    columnWidth = max( columnWidth, mpWidth );
        
                    CARMA_CPTRACE( Trace::TRACE6,
                                   mp.getCanonicalName() << ": " <<
                                   mpWidth << " " << columnWidth );
                }
            }
        }
    }

    {
        vector< const MonitorContainer * >::const_iterator i =
            containers.begin();

        const vector< const MonitorContainer * >::const_iterator iEnd =
            containers.end();
            
        for ( ; i != iEnd; ++i ) {
            const MonitorContainer * const container = *i;
            
            if ( container == 0 )
                throw logic_error( "NULL MonitorContainer pointer" );
                
            MonitorPointIterator mpi( *container, levels );
            
            while ( mpi++ ) {
                MonitorPoint & mp = mpi.getMonitorPoint( );

                if ( (visitor.get() == 0) || visitor->acceptMonitorPoint( mp ) )
                    mp.setWidth( columnWidth );
            }
        }
    }

    return columnWidth;
}


int
MonitorTable::autoFitColumnWidth( const MonitorContainer &          container,
                                  const int                         levels,
                                  const int                         minWidth,
                                  const MonitorTableVisitorPtr      visitor ) {
    vector< const MonitorContainer * > containers;
    
    containers.push_back( &container );
    
    return autoFitColumnWidth( containers, levels, minWidth, visitor );
}


int
MonitorTable::autoFitColumnWidth(
    const vector< MonitorContainer * > & containers,
    const int                            levels,
    const int                            minWidth,
    const MonitorTableVisitorPtr         visitor ) {
    const vector< const MonitorContainer * >
        containers2( containers.begin(), containers.end() );

    return autoFitColumnWidth( containers2, levels, minWidth, visitor );
}


int
MonitorTable::autoFitColumnWidth(
    const vector< MonitorColumnInfo > & columns,
    const int                           levels,
    const int                           minWidth,
    const MonitorTableVisitorPtr        visitor ) {
    vector< const MonitorContainer * > containers;
    
    {
        vector< MonitorColumnInfo >::const_iterator i = columns.begin();
        const vector< MonitorColumnInfo >::const_iterator iEnd = columns.end();
        
        for ( ; i != iEnd; ++i )
            containers.push_back( i->container );
    }
    
    return autoFitColumnWidth( containers, levels, minWidth, visitor );
}


int
MonitorTable::autoFitColumnWidth( const MonitorColumnInfo &         column,
                                  const int                         levels,
                                  const int                         minWidth,
                                  const MonitorTableVisitorPtr      visitor ) {
    vector< const MonitorContainer * > containers;
    
    containers.push_back( column.container );
    
    return autoFitColumnWidth( containers, levels, minWidth, visitor );
}


MonitorTablePtr
MonitorTable::makeTable(
    const vector< MonitorColumnInfo > & columns,
    const int                           columnWidth,
    const int                           hierarchyDepth,
    const int                           sampleNo,
    const bool                          unitsInRowLabels,
    const bool                          switchRowsAndColumns,
    const MonitorTableVisitorPtr        visitor,
    const bool                          displayRowLabels ) 
{ 
    return MonitorTablePtr(new MonitorTable( columns,
                             columnWidth,
                             hierarchyDepth,
                             sampleNo,
                             unitsInRowLabels,
                             switchRowsAndColumns,
                             visitor, 
                             displayRowLabels ));
}


MonitorTablePtr
MonitorTable::makeTable(
    const MonitorColumnInfo &         column,
    const int                         columnWidth,
    const int                         hierarchyDepth,
    const int                         sampleNo,
    const bool                        unitsInRowLabels,
    const bool                        switchRowsAndColumns,
    const MonitorTableVisitorPtr      visitor,
    const bool                         displayRowLabels) 
{
    vector<MonitorColumnInfo> columns;

    columns.push_back( column );

    return makeTable( columns,
                      columnWidth,
                      hierarchyDepth,
                      sampleNo,
                      unitsInRowLabels,
                      switchRowsAndColumns,
                      visitor,
                      displayRowLabels);
}


MonitorTablePtr
MonitorTable::makeTable(
    const string &                    heading,
    const MonitorContainer &          container,
    const int                         columnWidth,
    const int                         hierarchyDepth,
    const int                         sampleNo,
    const bool                        unitsInRowLabels,
    const bool                        switchRowsAndColumns,
    const MonitorTableVisitorPtr      visitor,
    const bool                        displayRowLabels ) {
    vector< MonitorColumnInfo > columns;
    
    columns.push_back( MonitorColumnInfo( heading, container ) );

    return makeTable( columns,
                      columnWidth,
                      hierarchyDepth,
                      sampleNo,
                      unitsInRowLabels,
                      switchRowsAndColumns,
                      visitor,
                      displayRowLabels );
}


MonitorTablePtr
MonitorTable::makeTable(
    const vector< string > &                   headings,
    const vector< const MonitorContainer * > & containers,
    const int                                  columnWidth,
    const int                                  hierarchyDepth,
    const int                                  sampleNo,
    const bool                                 unitsInRowLabels,
    const bool                                 switchRowsAndColumns,
    const MonitorTableVisitorPtr               visitor,
    const bool                                 displayRowLabels ) 
{
    const ::size_t numColumns = headings.size( );

    if ( numColumns != containers.size( ) ) {
        ostringstream oss;
        
        oss << " The number of column headings (" << numColumns
            << "), does not equal the number of containers ("
            << containers.size( ) << ")";
            
        throw CARMA_ERROR( oss );
    }

    vector< MonitorColumnInfo > columns;
    
    for ( ::size_t i = 0; i < numColumns; ++i ) {
        const MonitorContainer * const container = containers.at( i );
        
        if ( container == 0 )
            throw logic_error( "NULL MonitorContainer pointer" );
            
        columns.push_back( MonitorColumnInfo( headings.at( i ), *container ) );
    }
    
    return makeTable( columns,
                      columnWidth,
                      hierarchyDepth,
                      sampleNo,
                      unitsInRowLabels,
                      switchRowsAndColumns, 
                      visitor,
                      displayRowLabels);
}

MonitorTablePtr
MonitorTable::makeTable(
    const vector< string > &             headings,
    const vector< MonitorContainer * > & containers,
    const int                            columnWidth,
    const int                            hierarchyDepth,
    const int                            sampleNo,
    const bool                           unitsInRowLabels,
    const bool                           switchRowsAndColumns,
    const MonitorTableVisitorPtr         visitor,
    const bool                           displayRowLabels) {
    const vector< const MonitorContainer * >
        containers2( containers.begin(), containers.end() );

    return makeTable( headings,
                      containers2,
                      columnWidth,
                      hierarchyDepth,
                      sampleNo,
                      unitsInRowLabels,
                      switchRowsAndColumns,
                      visitor,
                      displayRowLabels);
}
