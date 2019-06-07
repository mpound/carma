#include "carma/antenna/sza/antenna/control/QuadShiftBoard.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor for the board with a passed name
 */
QuadShiftBoard::QuadShiftBoard(SzaShare* share, string name) : 
  Board(share, name)
{
  // Non-pointer members

  quadRequested_   = 0;
  quadLast_        = 0;

  // Pointer members

  share_ = share;

  loQuadActual_    = 0;
  loQuadRequested_ = 0;

  loQuadActual_    = findReg("lo_quad_actual");
  loQuadRequested_ = findReg("lo_quad");
}

/**.......................................................................
 * Constructor with an AntNum container
 */
QuadShiftBoard::QuadShiftBoard(SzaShare* share, AntNum ant) : Board(share, ant)
{
  // Non-pointer members

  quadRequested_   = 0;
  quadLast_        = 0;

  // Pointer members

  share_ = share;

  loQuadActual_    = 0;
  loQuadRequested_ = 0;

  loQuadActual_    = findReg("lo_quad_actual");
  loQuadRequested_ = findReg("lo_quad");
}
