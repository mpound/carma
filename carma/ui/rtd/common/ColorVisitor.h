#ifndef CARMA_UI_RTD_COLOR_VISITOR_H
#define CARMA_UI_RTD_COLOR_VISITOR_H

#include <set>
#include <string>

#include "carma/ui/rtd/common/MonitorTableVisitor.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/RtDisplay.h"


namespace carma {
namespace ui {
namespace rtd { 


//! @brief Visitor that post processes a set of MonitorCell to have 
//!        good and bad colors set to something other than the default.
class ColorVisitor : public MonitorTableVisitor {
    public:
        explicit ColorVisitor( CellColor goodColor, 
                               CellColor warnColor  = YELLOW_CELL_COLOR, 
                               CellColor errorColor = RED_CELL_COLOR);
        
        virtual ~ColorVisitor( );

        void addToEffectedSet( const monitor::MonitorPoint & mp );

        void postprocessMonitorCell(
            const monitor::MonitorPoint & mp,
            MonitorCell &                 monitorCell ) const;

    private:
        ::std::set< ::std::string > effected_;
        CellColor goodColor_;
        CellColor warnColor_;
        CellColor errorColor_;
};


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma

#endif
