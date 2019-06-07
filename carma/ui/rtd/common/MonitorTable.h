#ifndef CARMA_UI_RTD_MONITORTABLE_H
#define CARMA_UI_RTD_MONITORTABLE_H


/*
 * @file
 *
 * Create a table to include in a MonitorDisplay.
 *
 * @author Steve Scott 
 * $id: $
 *
 * $CarmaCopyright$
 *
 */


#include <vector>
#include <boost/shared_ptr.hpp>

#include <carma/ui/rtd/common/RtDisplay.h>


namespace carma {


namespace monitor {

class MonitorContainer;
class MonitorPoint;

}  // namespace carma::monitor


namespace ui {
namespace rtd { 


/**
 * Indicate to MonitorSingleTableFolder that we want column widths to be 
 * automatically calculated with all columns equal width.
 */
const int AUTO_SIZE = 0;

/**
 * Indicate to MonitorSingleTableFolder that we want column widths to be 
 * automatically calculated with each column sized individually.
 */
const int AUTO_SIZE_EACH_COLUMN = -1;

class MonitorTableVisitor;
typedef boost::shared_ptr<MonitorTableVisitor> MonitorTableVisitorPtr;

struct MonitorColumnInfo {
    ::std::string                     heading;
    const monitor::MonitorContainer * container;

    MonitorColumnInfo( const ::std::string &             inHeading,
                       const monitor::MonitorContainer & inContainer );
};

class MonitorTable;
typedef boost::shared_ptr<MonitorTable> MonitorTablePtr;

class MonitorTable : public RtTable {
    public:
        /**
         * Factory for a multi-column table
         * @param headings strings to use for the column headings
         * @param containers containers in which to find the monitor points
         *        The monitor points in each column will be found by descending
         *        the tree below this container, to a depth defined by the 
         *        hierarchyDepth parameter. This is usually just one level.
         * @param columnWidth in characters (default autosizes).
         * @param hierarchyDepth how many levels below the container to get MPs
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  Default is zero, which means use
         *        the average of all samples.
         * @param unitsInRowLabels <tt>true/tt> if this table should append 
         *        units to the monitor point names use as row labels, 
         *        <tt>false</tt> to use only names with no units.
         */
        static MonitorTablePtr makeTable(
            const ::std::vector<MonitorColumnInfo>& columns, 
            int                                       columnWidth= AUTO_SIZE,
            int                                       hierarchyDepth=1,
            int                                       sampleNo=0,
            bool                                      unitsInRowLabels=true,
            bool                                      switchRowsAndColumns=false,
            const MonitorTableVisitorPtr              visitor=MonitorTableVisitorPtr(),
            bool                                      displayRowLabels=true);

        /**
         * Factory for a single column table
         * @param heading string to use for the column heading
         * @param containers container in which to find the monitor points
         *        The monitor points in the column will be found by descending
         *        the tree below this container, to a depth defined by the 
         *        hierarchyDepth parameter. This is usually just one level.
         * @param columnWidth in characters (default autosizes)
         * @param hierarchyDepth how many levels below the container to get MPs
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  Default is zero, which means use
         *        the average of all samples.
         * @param unitsInRowLabels <tt>true/tt> if this table should append 
         *        units to the monitor point names use as row labels, 
         *        <tt>false</tt> to use only names with no units.
         */
        static MonitorTablePtr makeTable(
            const MonitorColumnInfo&    column, 
            int                         columnWidth= AUTO_SIZE,
            int                         hierarchyDepth=1,
            int                         sampleNo=0,
            bool                        unitsInRowLabels=true,
            bool                        switchRowsAndColumns=false,
            const MonitorTableVisitorPtr visitor=MonitorTableVisitorPtr(),
            const bool                  displayRowLabels=true);
        
        /**
         * Factory for a single column table
         * @param heading string to use for the column heading
         * @param container container in which to find the monitor points
         *        The monitor points in the column will be found by descending
         *        the tree below this container, to a depth defined by the 
         *        hierarchyDepth parameter. This is usually just one level.
         * @param columnWidth in characters (default autosizes)
         * @param hierarchyDepth how many levels below the container to get MPs
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  Default is zero, which means use
         *        the average of all samples.
         * @param unitsInRowLabels <tt>true/tt> if this table should append 
         *        units to the monitor point names use as row labels, 
         *        <tt>false</tt> to use only names with no units.
         */
        static MonitorTablePtr makeTable(
            const ::std::string&              heading, 
            const monitor::MonitorContainer&  container,
            int                               columnWidth = AUTO_SIZE,
            int                               hierarchyDepth=1,
            int                               sampleNo=0,
            bool                              unitsInRowLabels=true,
            bool                              switchRowsAndColumns=false,
            const MonitorTableVisitorPtr      visitor=MonitorTableVisitorPtr(),
            const bool                        displayRowLabels=true);
        
        /**
         * Factory for a multi-column table
         * @param headings strings to use for the column headings
         * @param containers containers in which to find the monitor points
         *        The monitor points in each column will be found by descending
         *        the tree below this container, to a depth defined by the 
         *        hierarchyDepth parameter. This is usually just one level.
         * @param columnWidth in characters (default autosizes).
         * @param hierarchyDepth how many levels below the container to get MPs
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  Default is zero, which means use
         *        the average of all samples.
         * @param unitsInRowLabels <tt>true/tt> if this table should append 
         *        units to the monitor point names use as row labels, 
         *        <tt>false</tt> to use only names with no units.
         */
        static MonitorTablePtr makeTable(
            const std::vector<std::string>&              headings, 
            const std::vector<const monitor::MonitorContainer*>& containers,
            int                                          columnWidth = AUTO_SIZE,
            int                                          hierarchyDepth =1,
            int                                          sampleNo = 0,
            bool                                         unitsInRowLabels=true,
            bool                                         switchRowsAndColumns=false,
            const MonitorTableVisitorPtr                 visitor=MonitorTableVisitorPtr(),
            bool                                         displayRowLabels=true);
        
        /**
         * Factory for a multi-column table
         * @param headings strings to use for the column headings
         * @param containerss containers in which to find the monitor points
         *        The monitor points in each column will be found by descending
         *        the tree below this container, to a depth defined by the 
         *        hierarchyDepth parameter. This is usually just one level.
         * @param columnWidth in characters (default autosizes).
         * @param hierarchyDepth how many levels below the container to get MPs
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  Default is zero, which means use
         *        the average of all samples.
         * @param unitsInRowLabels <tt>true/tt> if this table should append 
         *        units to the monitor point names use as row labels, 
         *        <tt>false</tt> to use only names with no units.
         */
        static MonitorTablePtr makeTable(
            const std::vector< std::string >&        headings, 
            const std::vector<monitor::MonitorContainer*>& containers,
            int                                      columnWidth=AUTO_SIZE,
            int                                      hierarchyDepth=1,
            int                                      sampleNo=0,
            bool                                     unitsInRowLabels=true,
            bool                                     switchRowsAndColumns=false,
            const MonitorTableVisitorPtr             visitor=MonitorTableVisitorPtr(),
            bool                                     displayRowLabels=true);
        
        virtual ~MonitorTable();
        
        /**
         * Goes through all monitor points in a vector of containers 
         *  and finds max width
         * @param container vector of containers 
         * @param levels number of levels below the container to search for MPs
         *        defaults to 1
         * @param minWidth minimum width for the column
         *        defaults to 3
         * @return the column width
         */
        static int autoFitColumnWidth(
            const ::std::vector< const monitor::MonitorContainer * > & containers,
            int                                                        levels,
            int                                                        minWidth,
            const MonitorTableVisitorPtr                               visitor ); 
         
        /**
         * Goes through all monitor points in a container and finds max width
         * Used to find column width
         * @param container 
         * @param levels number of levels below the container to search for MPs
         *        defaults to 1
         * @param minWidth minimum width for the column
         *        defaults to 3
         * @return the column width
         */
        static int autoFitColumnWidth(
            const monitor::MonitorContainer & container,
            int                               levels,
            int                               minWidth,
            const MonitorTableVisitorPtr      visitor );
        
        /**
         * Goes through all monitor points in a vector of containers 
         *  and finds max width
         * @param container vector of containers 
         * @param levels number of levels below the container to search for MPs
         *        defaults to 1
         * @param minWidth minimum width for the column
         *        defaults to 3
         * @return the column width
         */
        static int autoFitColumnWidth(
            const ::std::vector< monitor::MonitorContainer * > & containers,
            int                                                  levels,
            int                                                  minWidth,
            const MonitorTableVisitorPtr                         visitor );
         
        /**
         * Goes through all monitor points in a vector of columns 
         *  and finds max width
         * @param container vector of containers 
         * @param levels number of levels below the container to search for MPs
         *        defaults to 1
         * @param minWidth minimum width for the column
         *        defaults to 3
         * @return the column width
         */
        static int autoFitColumnWidth(
            const ::std::vector< MonitorColumnInfo > & columns,
            int                                        levels,
            int                                        minWidth,
            const MonitorTableVisitorPtr               visitor );

        /**
         * Goes through all monitor points in a vector of columns 
         *  and finds max width
         * @param container vector of containers 
         * @param levels number of levels below the container to search for MPs
         *        defaults to 1
         * @param minWidth minimum width for the column
         *        defaults to 3
         * @return the column width
         */
        static int autoFitColumnWidth(
            const MonitorColumnInfo &   column,
            int                         levels,
            int                         minWidth,
            const MonitorTableVisitorPtr visitor );

        static ::std::string
        generateRowLabel( const monitor::MonitorPoint & mp,
                          bool                          showUnits );

    protected:
        /**
         * Constructor for a multi-column table
         * @param headings strings to use for the column headings
         * @param containers containers in which to find the monitor points
         *        The monitor points in each column will be found by descending
         *        the tree below this container, to a depth defined by the 
         *        hierarchyDepth parameter. This is usually just one level.
         * @param columnWidth in characters (default autosizes).
         * @param hierarchyDepth how many levels below the container to get MPs
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  Default is zero, which means use
         *        the average of all samples.
         * @param unitsInRowLabels <tt>true/tt> if this table should append 
         *        units to the monitor point names use as row labels, 
         *        <tt>false</tt> to use only names with no units.
         */
        MonitorTable(
            const std::vector<MonitorColumnInfo>& columns, 
            int                                   columnWidth,
            int                                   hierarchyDepth,
            int                                   sampleNo,
            bool                                  unitsInRowLabels,
            bool                                  switchRowsAndColumns,
            const MonitorTableVisitorPtr          visitor,
            bool                                  displayRowLabels);

    private:
        static ::std::vector< int > getWidths(
            const monitor::MonitorContainer & container,
            int                               levels,
            int                               minWidth,
            const MonitorTableVisitorPtr      visitor );

        void addRowLabels(const monitor::MonitorContainer& container,
                          bool                             unitsInRowLabels,
                          bool                             switchRowsAndColumns,
                          const MonitorTableVisitorPtr     visitor,
                          const bool                       displayRowLabels);
        
        void addContainerMonitorPoints(
            const monitor::MonitorContainer & container,
            const MonitorTableVisitorPtr      visitor,
            const int                         defaultWidth,
            const ::std::vector< int > &      individualWidths );

        const int hierarchyDepth_;
        const int sampleNo_;
};


}  // namespace carma::ui::rtd
}  // namespace carma::ui


}  // namespace carma


#endif
