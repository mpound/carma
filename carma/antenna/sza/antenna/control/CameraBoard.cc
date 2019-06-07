#include "carma/antenna/sza/antenna/control/CameraBoard.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor function for the CameraBoard class
 */
CameraBoard::CameraBoard(SzaShare* share, string name) :
  Board(share, name)
{
  angle_ = 0;
  angle_ = findReg("angle");
}

/**.......................................................................
 * Record a new value of the zero_angle in the register database
 */
void CameraBoard::recordAngle(long angle)
{
  signed value = angle;
  writeReg(angle_, 0, 1, (unsigned *) &value);
}
