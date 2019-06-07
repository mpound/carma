#include "carma/szautil/Frequency.h"
#include "carma/szautil/SzaFitsIo.h"

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
SzaFitsIo::SzaFitsIo() 
{
  // Set up for writing out wideband data

  setTelescopeName("SZA");
  setInstrument("Ka-Band Receivers");
  setNumberOfStokesParameters(1);
  setNumberOfChannelsPerIf(1);

  // Set the bandwidth to 500 MHz

  Frequency dfreq;
  dfreq.setGHz(0.5);
  setDeltaIfFrequency(dfreq);
}

/**.......................................................................
 * Destructor.
 */
SzaFitsIo::~SzaFitsIo() {}
