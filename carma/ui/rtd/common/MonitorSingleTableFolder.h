#ifndef CARMA_UI_RTD_MONITORSINGLETABLEFOLDER_H
#define CARMA_UI_RTD_MONITORSINGLETABLEFOLDER_H


/*
 * @file
 *
 * Create a folder with a single automatically generated table 
 * to include in a MonitorDisplay.
 *
 * @author Steve Scott 
 *
 * $CarmaCopyright$
 *
 */
 
#include <vector>

#include <carma/monitor/MonitorContainer.h>
#include <carma/ui/rtd/common/RtDisplay.h>
#include <carma/ui/rtd/common/MonitorTable.h>

#include <boost/shared_ptr.hpp>

namespace carma {
namespace ui {
namespace rtd { 

class MonitorSingleTableFolder : public RtFolder {
    public:
        /**
         * Constructor for a multi-column table
         * @param folderName name for the folder, will also appear on tab
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
         */
        MonitorSingleTableFolder(
            const ::std::string &                      folderName,
            const ::std::vector< MonitorColumnInfo > & columns, 
            int                                        columnWidth,
            int                                        hierarchyDepth,
            int                                        sampleNo,
            bool                                       unitsInRowLabels,
            bool                                       switchRowsAndColumns,
            const MonitorTableVisitorPtr               visitor,
            bool                                       displayRowLabels=true 
            );
        
        /**
         * Constructor for a multi-column table
         * @param folderName name for the folder, will also appear on tab
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
         */
        MonitorSingleTableFolder(
            const ::std::string &                      folderName,
            const ::std::vector< MonitorColumnInfo > & columns, 
            int                                        columnWidth = AUTO_SIZE,
            int                                        hierarchyDepth = 1,
            int                                        sampleNo = 0,
            bool                                       unitsInRowLabels = true,
            const MonitorTableVisitorPtr               visitor = MonitorTableVisitorPtr(),
            bool                                       displayRowLabels =true
        );
        
        /**
         * Constructor for a single column table
         * @param folderName name for the folder, will also appear on tab
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
         */
        MonitorSingleTableFolder(
            const ::std::string &       folderName,
            const MonitorColumnInfo &   column,
            int                         columnWidth = AUTO_SIZE,
            int                         hierarchyDepth = 1,
            int                         sampleNo = 0,
            bool                        unitsInRowLabels = true,
            const MonitorTableVisitorPtr visitor = MonitorTableVisitorPtr(),
            bool                        displayRowLabels=true
        );
        
        /**
         * Constructor for a single column table
         * @param folderName name for the folder, will also appear on tab
         * @param heading string to use for the column heading
         * @param container container in which to find the monitor points
         *        The monitor points in the column will be found by descending
         *        the tree below this container, to a depth defined by the 
         *        hierarchyDepth parameter. This is usually just one level.
         * @param columnWidth in characters (default autosizes).
         * @param hierarchyDepth how many levels below the container to get MPs
         * @param sampleNo For monitor points with multiple samples, the sample
         *        number to use in the display.  Default is zero, which means use
         *        the average of all samples.
         */
        MonitorSingleTableFolder(
            const ::std::string &             folderName,
            const ::std::string &             heading, 
            const monitor::MonitorContainer & container,
            int                               columnWidth = AUTO_SIZE,
            int                               hierarchyDepth = 1,
            int                               sampleNo = 0,
            bool                              unitsInRowLabels = true,
            const MonitorTableVisitorPtr      visitor = MonitorTableVisitorPtr(),
            bool                              displayRowLabels=true
        );
        
        /**
         * Constructor for a multi-column table
         * @param folderName name for the folder, will also appear on tab
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
         */
        MonitorSingleTableFolder(
            const std::string&                      folderName,
            const std::vector< ::std::string >&     headings, 
            const std::vector<const monitor::MonitorContainer*>& containers,
            int                                     columnWidth = AUTO_SIZE,
            int                                     hierarchyDepth = 1,
            int                                     sampleNo = 0,
            bool                                    unitsInRowLabels = true,
            const MonitorTableVisitorPtr            visitor = MonitorTableVisitorPtr(),
            bool                                    switchRowsAndColumns=false,
            bool                                    displayRowLabels=true
        );
        
        /**
         * Constructor for a multi-column table
         * @param folderName name for the folder, will also appear on tab
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
         */
        MonitorSingleTableFolder(
            const std::string &                            folderName,
            const std::vector< ::std::string > &           headings, 
            const std::vector<monitor::MonitorContainer*>& containers,
            int                                            columnWidth = AUTO_SIZE,
            int                                            hierarchyDepth = 1,
            int                                            sampleNo = 0,
            bool                                           unitsInRowLabels = true,
            const MonitorTableVisitorPtr                   visitor = MonitorTableVisitorPtr(),
            bool                                           switchRowsAndColumns=false,
            bool                                           displayRowLabels=true
        );
        
        virtual ~MonitorSingleTableFolder( );
        
        /// Get a reference to the table
        MonitorTable& getTable() const;
    private:
        boost::shared_ptr<MonitorTable> table_;
};

typedef boost::shared_ptr<MonitorSingleTableFolder> MonitorSingleTableFolderPtr;

}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma

#endif
