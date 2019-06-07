#ifndef CARMA_UI_RTD_ROW_VISITOR_H
#define CARMA_UI_RTD_ROW_VISITOR_H

#include <set>
#include <string>

#include "carma/monitor/MonitorPoint.h"
#include "carma/ui/rtd/common/MonitorTableVisitor.h"
#include "carma/ui/rtd/common/ColorVisitor.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/RtDisplay.h"


namespace carma {
namespace ui {
namespace rtd { 


//! @brief Visitor that uses the long monitor point names for 
//!        row labels instead of the short ones.  Also allows
//!        setting of good and bad colors set to something other than 
//!        the default.

// this could have equally been done by adding yet another bool
// to the MonitorTableFolder constructor
class RowVisitor : public ColorVisitor {
    public:
        explicit RowVisitor( CellColor goodColor, 
                     CellColor warnColor  = YELLOW_CELL_COLOR, 
                 CellColor errorColor = RED_CELL_COLOR );
        
        virtual ~RowVisitor( );

        ::std::string generateRowLabel(
            const monitor::MonitorPoint & mp,
            bool                          showUnits ) const;

};

RowVisitor::RowVisitor( CellColor goodColor, 
                        CellColor warnColor,
                        CellColor errorColor) 
: ColorVisitor( goodColor, warnColor, errorColor )
{
}

RowVisitor::~RowVisitor( )
try {
} catch ( ... ) {
    // Just stifle any exception
}

::std::string
RowVisitor::generateRowLabel( const monitor::MonitorPoint & mp,
                              const bool showUnits ) const
{
    ::std::string rowLabel = mp.getLongName();
    
    if ( showUnits ) {
        const ::std::string units = mp.getUnits();
        
        if ( units.empty( ) == false ) {
            rowLabel += " (";
            rowLabel += units;
            rowLabel += ")";
        }
    }
    
    return rowLabel;
}

}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma

#endif
