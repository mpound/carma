#include "carma/szautil/IFAtten.h"

using namespace std;

using namespace sza::util;

#include "carma/szautil/IFAtten.h"

using namespace std;
using namespace sza::util;

float IFAtten::inputAttens_[AntNum::NANT];
float IFAtten::outputAttens_[AntNum::NANT];
float IFAtten::totalAttens_[AntNum::NANT];

const double IFAtten::attenMax_ = 63;

/**.......................................................................
 * Return the input IF attenuation associated with this antenna
 */
float IFAtten::getDefaultInputAttenuation(AntNum::Id antId)
{
  unsigned iant = AntNum::idToInt(antId);
  return inputAttens_[iant];
}

/**.......................................................................
 * Return the output IF attenuation associated with this antenna
 */
float IFAtten::getDefaultOutputAttenuation(AntNum::Id antId)
{
  unsigned iant = AntNum::idToInt(antId);
  return outputAttens_[iant];
}

/**.......................................................................
 * Return the total IF attenuation associated with this antenna
 */
float IFAtten::getDefaultTotalAttenuation(AntNum::Id antId)
{
  unsigned iant = AntNum::idToInt(antId);
  return totalAttens_[iant];
}

/**.......................................................................
 * Set the default input attenuation
 */
void IFAtten::setDefaultInputAttenuation(AntNum::Id antId, float atten)
{
  AntNum antennas(antId);

  // Iterate over antennas, checking which ones were requested

  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {
    if(antennas.isSet(antiter)) {
      unsigned iant = antiter.getIntId();
      inputAttens_[iant] = atten;
    }
  }
}

/**.......................................................................
 * Set the default output attenuation
 */
void IFAtten::setDefaultOutputAttenuation(AntNum::Id antId, float atten)
{
  AntNum antennas(antId);

  // Iterate over antennas, checking which ones were requested

  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {
    if(antennas.isSet(antiter)) {
      unsigned iant = antiter.getIntId();
      outputAttens_[iant] = atten;
    }
  }
}

/**.......................................................................
 * Set the default total attenuation
 */
void IFAtten::setDefaultTotalAttenuation(AntNum::Id antId, float atten)
{
  AntNum antennas(antId);

  // Iterate over antennas, checking which ones were requested

  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {
    if(antennas.isSet(antiter)) {
      unsigned iant = antiter.getIntId();
      totalAttens_[iant] = atten;
    }
  }
}

