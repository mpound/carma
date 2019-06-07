#include "carma/ui/rtd/common/reachableCellUtils.h"

#include "carma/ui/rtd/common/MonitorCell.h"

using namespace ::std;
using namespace carma;
using namespace carma::ui;
using namespace carma::ui::rtd;


namespace {


const string kReachableTrueText  = "reachable";
const string kReachableFalseText = "UNREACHABLE";


}  // namespace < anonymous >


int
carma::ui::rtd::calcReachableCellWidth( )
{
    return max( kReachableTrueText.size( ), kReachableFalseText.size( ) );
}


void
carma::ui::rtd::postprocessReachableCell( MonitorCellBool & cell )
{
    cell.overrideStateAppearances( kReachableTrueText,  GREEN_CELL_COLOR,
                                   kReachableFalseText, RED_CELL_COLOR );
                                   
    cell.setLayout( EOL_CENTERED_LAYOUT );
}


