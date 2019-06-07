#include "carma/ui/rtd/common/CellMM.h"


using namespace ::std;
using namespace ::carma;
using namespace ::carma::ui;
using namespace ::carma::ui::rtd;


// And short with a max and min (for colors) specified
CellShortMM::CellShortMM(const char* _fmt, const char* leg, const short& _data):
   CellShort(_fmt, _data), 
   limSet(0), warnColor_(YELLOW_CELL_COLOR), errColor_(RED_CELL_COLOR)
{ } 
CellShortMM::CellShortMM(const char* _fmt, const short& _data):
   CellShort(_fmt, _data), 
   limSet(0), warnColor_(YELLOW_CELL_COLOR), errColor_(RED_CELL_COLOR)
{ } 

void CellShortMM::setLimits(short Yhi, short Ylo, short Rhi, short Rlo) {
   limSet = 1;
   warnHi = Yhi;
   warnLo = Ylo;
   errHi  = Rhi;
   errLo  = Rlo;
} 
void CellShortMM::setErrColor(const CellColor color){errColor_ = color; }
void CellShortMM::setWarnColor(const CellColor color){warnColor_ = color; }
void CellShortMM::updateColor() {
   if (!*legit || alternativeIsActive( ) || !limSet) setColor( WHITE_CELL_COLOR ); 
   else if ((data > errHi)  || (data < errLo) ) setColor( errColor_ ); 
   else if ((data > warnHi) || (data < warnLo)) setColor( warnColor_ );
   else setColor( WHITE_CELL_COLOR );
}     


// And doubles with a max and min (for colors) specified
CellFloatMM::CellFloatMM(const char* _fmt, int r, const char* leg, const float& _data):
   CellFloat(_fmt,  r, _data), 
   limSet(0), warnColor_(YELLOW_CELL_COLOR), errColor_(RED_CELL_COLOR)
{ } 

// And doubles with a max and min (for colors) specified
CellFloatMM::CellFloatMM(const char* _fmt, int r, const float& _data):
   CellFloat(_fmt,  r, _data), 
   limSet(0), warnColor_(YELLOW_CELL_COLOR), errColor_(RED_CELL_COLOR)
{ } 

void CellFloatMM::setLimits(float Yhi, float Ylo, float Rhi, float Rlo) 
{
   limSet = 1;
   warnHi = Yhi;
   warnLo = Ylo;
   errHi  = Rhi;
   errLo  = Rlo;
} 


void
CellFloatMM::setErrColor( const CellColor color ) {
    errColor_ = color;
}


void
CellFloatMM::setWarnColor( const CellColor color ) {
    warnColor_ = color;
}


void CellFloatMM::updateColor() 
{
   if (!*legit || alternativeIsActive( ) || !limSet) setColor( WHITE_CELL_COLOR ); 
   else if ((data > errHi)  || (data < errLo) ) setColor( errColor_ ); 
   else if ((data > warnHi) || (data < warnLo)) setColor( warnColor_ );
   else setColor( WHITE_CELL_COLOR );
}     


// And doubles with a max and min (for colors) specified
CellDbleMM::CellDbleMM(const char* _fmt, int r, const char* leg, const double& _data):
   CellDble(_fmt,  r, _data), 
   limSet(0), warnColor_(YELLOW_CELL_COLOR), errColor_(RED_CELL_COLOR)
{ } 

// And doubles with a max and min (for colors) specified
CellDbleMM::CellDbleMM(const char* _fmt, int r, const double& _data):
   CellDble(_fmt,  r, _data), 
   limSet(0), warnColor_(YELLOW_CELL_COLOR), errColor_(RED_CELL_COLOR)
{ } 

void CellDbleMM::setLimits(double Yhi, double Ylo, double Rhi, double Rlo) 
{
   limSet = 1;
   warnHi = Yhi;
   warnLo = Ylo;
   errHi  = Rhi;
   errLo  = Rlo;
} 


void
CellDbleMM::setErrColor( const CellColor color ) {
    errColor_ = color;
}


void
CellDbleMM::setWarnColor( const CellColor color ) {
    warnColor_ = color;
}


void CellDbleMM::updateColor() 
{
   if (!*legit || alternativeIsActive( ) || !limSet) setColor( WHITE_CELL_COLOR ); 
   else if ((data > errHi)  || (data < errLo) ) setColor( errColor_ ); 
   else if ((data > warnHi) || (data < warnLo)) setColor( warnColor_ );
   else setColor( WHITE_CELL_COLOR );
}
