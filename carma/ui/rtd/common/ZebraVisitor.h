#ifndef CARMA_UI_RTD_ZEBRA_VISITOR_H
#define CARMA_UI_RTD_ZEBRA_VISITOR_H

#include <set>
#include <string>

#include "carma/ui/rtd/common/ColorVisitor.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/RtDisplay.h"

#include "carma/monitor/MonitorContainer.h"

#include <boost/shared_ptr.hpp>

namespace carma {
namespace ui {
namespace rtd {

//! @brief Visitor that post processes a set of MonitorCell to have
//!        zebra-like coloring (alternating white and gray rows)
class ZebraVisitor : public ColorVisitor {
    public:
        explicit ZebraVisitor(const CellColor &goodColor = LIGHT_GRAY_CELL_COLOR);
        virtual ~ZebraVisitor();

        void addMonitorContainer(const carma::monitor::MonitorContainer &c, const int mod = 2);
        void addMonitorContainer(carma::monitor::MonitorContainer *c, const int mod = 2);
};

typedef boost::shared_ptr<ZebraVisitor> ZebraVisitorPtr;

}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma

#endif
