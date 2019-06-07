#include "carma/szautil/Length.h"
#include "carma/szautil/SzaMiriadIo.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
SzaMiriadIo::SzaMiriadIo() 
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

  // Set the longitude and latitude
  
  Angle longitude;
  longitude.setDegrees("-118:17:45.9");
  setLongitude(longitude);

  Angle latitude;
  latitude.setDegrees("37:13:57.5");
  setLatitude(latitude);

  // Set the telescope diameters.
  //
  // Note we have to do this roundabout declaration or else versions
  // of gcc < 3.4 will complain

  Length diameter = Length(Length::Meters(), 3.5);
  setTelescopeDiameter(diameter);
  setTelescopeApertureEfficiency(0.55);

  Time time = Time(Time::Seconds(), 20);
  setIntTime(time);
}

/**.......................................................................
 * Destructor.
 */
SzaMiriadIo::~SzaMiriadIo() {}
