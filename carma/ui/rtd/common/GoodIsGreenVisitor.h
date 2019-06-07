#ifndef CARMA_UI_RTD_GREENISGOODVISITOR_H
#define CARMA_UI_RTD_GREENISGOODVISITOR_H

#include <set>
#include <string>

#include "carma/ui/rtd/common/MonitorTableVisitor.h"
//@todo make this derived from ColorVisitor -- mwp


namespace carma {
namespace ui {
namespace rtd { 


//! @brief Visitor that post processes a set of MonitorCell to have green as
//!        their good color
class GoodIsGreenVisitor : public MonitorTableVisitor {
    public:
        explicit GoodIsGreenVisitor( );
        
        ~GoodIsGreenVisitor( );

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
