#ifndef CARMA_UI_RTD_GRAYEDOUTVISITOR_H
#define CARMA_UI_RTD_GRAYEDOUTVISITOR_H

#include <set>
#include <string>

#include "carma/ui/rtd/common/MonitorTableVisitor.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/RtDisplay.h"


namespace carma {
namespace ui {
namespace rtd { 


//! @brief Visitor that post processes a set of MonitorCell to be
//!        grayed out.
class GrayedOutVisitor : public MonitorTableVisitor {
    public:
        GrayedOutVisitor( );
        
        virtual ~GrayedOutVisitor( );

        void addToEffectedSet( const monitor::MonitorPoint & mp );

        void postprocessMonitorCell(
            const monitor::MonitorPoint & mp,
            MonitorCell &                 monitorCell ) const;

    private:
        ::std::set< ::std::string > effected_;
};


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma

#endif
