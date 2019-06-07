#ifndef CARMA_UI_RTD_REACHABLECELLUTILS_H
#define CARMA_UI_RTD_REACHABLECELLUTILS_H

namespace carma {
namespace ui {
namespace rtd {


class MonitorCellBool;


int calcReachableCellWidth( );

void postprocessReachableCell( MonitorCellBool & cell );


}  // namespace carma::ui::rtd
}  // namespace carma::ui
}  // namespace carma


#endif
