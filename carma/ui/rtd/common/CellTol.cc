#include "carma/ui/rtd/common/CellTol.h"

#include <cmath>

using namespace ::std;
using namespace ::carma;
using namespace ::carma::ui;
using namespace ::carma::ui::rtd;


// And doubles with a location specified that contains an absolute tolerance
CellFloatTol::CellFloatTol
   (const char* fmt, int r, const char* leg, const float& _data, const float& _tolerance):
   CellFloat(fmt,  r, leg, _data), tolerance(_tolerance)
{ } 

CellFloatTol::CellFloatTol
   (const char* fmt, int r, const float& _data, const float& _tolerance):
   CellFloat(fmt,  r, _data), tolerance(_tolerance)
{ } 

void CellFloatTol::updateColor() {
    if (isReplaceText())CellFloat::updateColor(); 
    else if (fabs(data) > tolerance) setColor( RED_CELL_COLOR ); 
    else setColor( WHITE_CELL_COLOR );
} 


// And doubles with a location specified that contains an absolute tolerance
CellDbleTol::CellDbleTol
   (const char* fmt, int r, const char* leg, const double& _data, const double& _tolerance):
   CellDble(fmt,  r, leg, _data), tolerance(_tolerance)
{ } 

CellDbleTol::CellDbleTol
   (const char* fmt, int r, const double& _data, const double& _tolerance):
   CellDble(fmt,  r, _data), tolerance(_tolerance)
{ } 

void CellDbleTol::updateColor() 
{
    if (isReplaceText())CellDble::updateColor(); 
    else if (fabs(data) > tolerance) setColor( RED_CELL_COLOR ); 
    else setColor( WHITE_CELL_COLOR );
} 
