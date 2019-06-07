#include "carma/szautil/LoBand.h"

using namespace std;
using namespace sza::util;

Attenuation LoBand::loTermAttens_[AntNum::NANT];

/**.......................................................................
 * Return the LO Terminator attenuation associated with this antenna
 */
Attenuation LoBand::getLoTermAttenuation(AntNum::Id antId)
{
  unsigned iant = AntNum::idToInt(antId);
  return loTermAttens_[iant];
}

/**.......................................................................
 * Return the LO Terminator attenuation associated with this antenna
 */
void LoBand::setLoTermAttenuation(AntNum::Id antId, Attenuation atten)
{
  AntNum antennas(antId);

  // Iterate over antennas, checking which ones were requested

  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {
    if(antennas.isSet(antiter)) {
      unsigned iant = antiter.getIntId();
      loTermAttens_[iant] = atten;
    }
  }
}
