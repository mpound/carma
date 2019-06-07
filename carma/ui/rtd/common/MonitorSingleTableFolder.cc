/*
 *
 * A folder with a single automatically generated table for a MonitorDisplay
 *
 * @author Steve Scott 
 * $id: $
 *
 * $CarmaCopyright$
 *
 */

  
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/monitor/MonitorPointIterator.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;


MonitorSingleTableFolder::MonitorSingleTableFolder(
    const string &                      folderName,
    const vector< MonitorColumnInfo > & columns,
    const int                           columnWidth,
    const int                           hierarchyDepth,
    const int                           sampleNo,
    const bool                          unitsInRowLabels,
    const bool                          switchRowsAndColumns,
    const MonitorTableVisitorPtr        visitor,
    const bool                          displayRowLabels) :
RtFolder( folderName ) 
{
    table_ = MonitorTable::makeTable(columns,
                                     columnWidth,
                                     hierarchyDepth,
                                     sampleNo,
                                     unitsInRowLabels,
                                     switchRowsAndColumns,
                                     visitor,
                                     displayRowLabels);
    add(table_);
    add(RtObjectPtr(new RtSpring( 4, 1.0 )));
}


MonitorSingleTableFolder::MonitorSingleTableFolder(
    const string &                      folderName,
    const vector< MonitorColumnInfo > & columns,
    const int                           columnWidth,
    const int                           hierarchyDepth,
    const int                           sampleNo,
    const bool                          unitsInRowLabels,
    const MonitorTableVisitorPtr        visitor,
    const bool                          displayRowLabels) :
RtFolder( folderName ) 
{
    table_ = MonitorTable::makeTable(columns,
                                     columnWidth,
                                     hierarchyDepth,
                                     sampleNo,
                                     unitsInRowLabels,
                                     false,
                                     visitor,
                                     displayRowLabels);
    add(table_);
    add(RtObjectPtr(new RtSpring( 4, 1.0 )));
}


MonitorSingleTableFolder::MonitorSingleTableFolder(
    const string &                    folderName,
    const MonitorColumnInfo &         column,
    const int                         columnWidth,
    const int                         hierarchyDepth,
    const int                         sampleNo,
    const bool                        unitsInRowLabels,
    const MonitorTableVisitorPtr      visitor,
    const bool                         displayRowLabels) :
RtFolder( folderName ) 
{
    table_ = MonitorTable::makeTable(column,
                                     columnWidth,
                                     hierarchyDepth,
                                     sampleNo,
                                     unitsInRowLabels,
                                     false,
                                     visitor,
                                     displayRowLabels);

    add(table_);
    add(RtObjectPtr(new RtSpring( 4, 1.0 )));
}


MonitorSingleTableFolder::MonitorSingleTableFolder(
    const string &                    folderName,
    const string &                    heading,
    const MonitorContainer &          container,
    const int                         columnWidth,
    const int                         hierarchyDepth,
    const int                         sampleNo,
    const bool                        unitsInRowLabels,
    const MonitorTableVisitorPtr      visitor,
    const bool                        displayRowLabels) :
RtFolder( folderName ) 
{
    table_ = MonitorTable::makeTable(heading,
                                     container,
                                     columnWidth,
                                     hierarchyDepth,
                                     sampleNo,
                                     unitsInRowLabels,
                                     false,
                                     visitor,
                                     displayRowLabels);

    add(table_);
    add(RtObjectPtr(new RtSpring( 4, 1.0 )));
}


MonitorSingleTableFolder::MonitorSingleTableFolder(
    const string &                             folderName,
    const vector< string > &                   headings,
    const vector< const MonitorContainer * > & containers,
    const int                                  columnWidth,
    const int                                  hierarchyDepth,
    const int                                  sampleNo,
    const bool                                 unitsInRowLabels,
    const MonitorTableVisitorPtr               visitor,
    const bool                                 switchRowsAndColumns,
    const bool                                 displayRowLabels) :
RtFolder( folderName ) {
    table_ = MonitorTable::makeTable(headings,
                                     containers,
                                     columnWidth,
                                     hierarchyDepth,
                                     sampleNo,
                                     unitsInRowLabels,
                                     switchRowsAndColumns,
                                     visitor,
                                     displayRowLabels);
    add(table_);
    add(RtObjectPtr(new RtSpring( 4, 1.0 )));
}


MonitorSingleTableFolder::MonitorSingleTableFolder(
    const string &                       folderName,
    const vector< string > &             headings,
    const vector< MonitorContainer * > & containers,
    const int                            columnWidth,
    const int                            hierarchyDepth,
    const int                            sampleNo,
    const bool                           unitsInRowLabels,
    const MonitorTableVisitorPtr         visitor,
    const bool                           switchRowsAndColumns,
    const bool                           displayRowLabels) :
RtFolder( folderName ) {
    table_ = MonitorTable::makeTable(headings,
                                     containers,
                                     columnWidth,
                                     hierarchyDepth,
                                     sampleNo,
                                     unitsInRowLabels,
                                     switchRowsAndColumns, 
                                     visitor,
                                     displayRowLabels);

    add(table_);
    add(RtObjectPtr(new RtSpring(4, 1.0 )));
}

MonitorTable& MonitorSingleTableFolder::getTable() const {
    return *table_;
}


MonitorSingleTableFolder::~MonitorSingleTableFolder( )
try {
} catch ( ... ) {
    // Just stifle any exception   
    return;
}
