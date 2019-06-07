#include "carma/szautil/Constants.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/VisIo.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
VisIo::VisIo() 
{
  nIf_         = 0;
  nFrame_      = 0;
  nBaseline_   = 0;
  nStokes_     = 0;
  nTelescope_  = 0;
  doConj_      = false;
  firstTelescopeNum_ = 0;
}

/**.......................................................................
 * Destructor.
 */
VisIo::~VisIo() {}

/**.......................................................................
 * Set index number of the first telescope
 */
void VisIo::setFirstTelescopeNum(unsigned iTel)
{
  firstTelescopeNum_=iTel;
}

/**.......................................................................
 * Return the antenna indices associated with a given visibility index.
 */
void VisIo::getTelescopeIndices(unsigned baselineIndex, 
				unsigned* iRow, unsigned* iCol, 
				unsigned nTel)
{
  int i,iStart,iStop;

  if(baselineIndex > nTel*(nTel-1)-1) {
    ThrowError("Received invalid index: " << baselineIndex);
  }

  // Convert an index in a real array into the corresponding real
  // index of a complex array.

  baselineIndex *= 2;
  
  // Convert an odd index of a complex array to the index of the
  // corresponding real component.
  
  if(baselineIndex%2 > 0)
    baselineIndex -= 1;
  
  // First find out which row this index corresponds to

  for(i=0;i < nTel;i++) {
    iStart = 2*(i*nTel - ((i+1)*(i+1) - (i+1))/2);
    iStop = iStart + 2*(nTel-2-i);
    if(baselineIndex >= iStart && baselineIndex <= iStop) {
      *iRow = i;
      break;
    }
  }
  
  // Now determine which column in the row.
  
  *iCol = *iRow+1 + (baselineIndex - iStart)/2;
}


void VisIo::checkParameters()
{
  // Check the validity of the frequency parameters

  if(nIf_ == 0)
    ThrowError("No IFs were specified (use setIfFrequencies() "
	       "or setStartingIfFrequency() and setDeltaIfFrequency()");

  // Check the bandwidth parameters

  if(deltaIfFrequencies_.size()==0 && !(deltaIfFrequency_.Hz() > 0.0))
    ThrowError("No finite IF bandwidth was specified "
	       "(use setDeltaIfFrequency() or setDeltaIfFrequencies())");

  // Check the validity of the antenna parameters

  if(nTelescope_ == 0)
    ThrowError("No antennas were given (use setTelescopeLocations())");

  // Check consistency with telescope parameters


  if(diameters_.size() > 0 && diameters_.size() != nTelescope_)
    ThrowError("Invalid diameter array length");

  if(apeffs_.size() > 0 && apeffs_.size() != nTelescope_)
    ThrowError("Invalid aperture efficieny length");
}

//-----------------------------------------------------------------------
// Methods to deal with telescope parameters
//-----------------------------------------------------------------------

/**.......................................................................
 * Specify the number of telescopes
 */
void VisIo::setNumberOfTelescopes(unsigned nTel)
{
  nTelescope_ = nTel;
  nTelescopePar_.update();
}

/**.......................................................................
 * Check nTel against the current number of telescopes
 */
void VisIo::checkNumberOfTelescopes(unsigned nTel)
{
  if(nTelescopePar_.isSet()) {
    if(nTel != nTelescope_) {
      ThrowError("Invalid number of telescopes: " << nTel);
    }
  } else {
    setNumberOfTelescopes(nTel);
  }
}

/**.......................................................................
 * Set the telescope azimuths
 */
void VisIo::setTelescopeAzimuth(std::vector<Angle>& az)
{
  checkNumberOfTelescopes(az.size());
  az_ = az;
  azPar_.update();
}

/**.......................................................................
 * Set the telescope elevations
 */
void VisIo::setTelescopeElevation(std::vector<Angle>& el)
{
  checkNumberOfTelescopes(el.size());
  el_ = el;
  elPar_.update();
}

/**.......................................................................
 * Install a vector of antenna locations
 */
void VisIo::setTelescopeLocations(std::vector<std::vector<Length> >& locations)
{
  if(locations.size() == 0)
    ThrowError("Received zero-sized vector");

  if(locations[0].size() != 3)
    ThrowError("Second axis should have dimension: 3");

  checkNumberOfTelescopes(locations.size());
  locations_  = locations;
  locationsPar_.update();
}

/**.......................................................................
 * Set the telescope diameters
 */
void VisIo::setTelescopeDiameter(Length& diameter)
{
  diameters_.resize(0);
  diameter_ = diameter;
  diameterPar_.update();
}

/**.......................................................................
 * Set the telescope diameters
 */
void VisIo::setTelescopeDiameters(std::vector<Length>& diameters)
{
  checkNumberOfTelescopes(diameters.size());
  diameters_ = diameters;
  diameterPar_.update();
}

/**.......................................................................
 * Set the telescope aperture efficiencies
 */
void VisIo::setTelescopeApertureEfficiency(float apeff)
{
  apeffs_.resize(0);
  apeff_ = apeff;
  apEffPar_.update();
}

/**.......................................................................
 * Set the telescope aperture efficiencies
 */
void VisIo::setTelescopeApertureEfficiencies(std::vector<float>& apeffs)
{
  checkNumberOfTelescopes(apeffs.size());
  apeffs_ = apeffs;
  apEffPar_.update();
}

//-----------------------------------------------------------------------
// Methods to deal with frequency
//-----------------------------------------------------------------------

/**.......................................................................
 * Set the number of IF frequencies
 */
void VisIo::setNumberOfIfs(unsigned nIf) 
{
  nIf_ = nIf;

  // And reset the delta IF frequency

  setDeltaIfFrequency(deltaIfFrequency_);

  nIfPar_.update();
}

/**.......................................................................
 * Install a vector of IF frequencies
 */
void VisIo::setIfFrequencies(std::vector<Frequency>& frequencies)
{
  if(frequencies.size() == 0)
    ThrowError("Received zero-sized vector");

  // And store relevant information needed for the header

  setNumberOfIfs(frequencies.size());

  startingIfFrequency_ = frequencies[0];
  ifFrequencies_       = frequencies;

  ifFreqPar_.update();

  // Set up the center frequency too, since Miriad will need this

  double freqGHz = 0.0;
  for(unsigned iFreq=0; iFreq < frequencies.size(); iFreq++) {
    freqGHz += (frequencies[iFreq].GHz() - freqGHz)/(iFreq+1);
  }

  ifCenterFrequency_.setGHz(freqGHz);
}

/**.......................................................................
 * An alternate method for specifying IF frequencies is to use a
 * starting frequency and a delta
 */
void VisIo::setStartingIfFrequency(Frequency frequency) 
{
  startingIfFrequency_ = frequency;
  ifFreqPar_.update();
}

/**.......................................................................
 * Set a vector of delta IF Frequencies
 */
void VisIo::setDeltaIfFrequency(Frequency& frequency)
{
  // Set the frequency

  deltaIfFrequency_ = frequency;

  // And install in the vector

  deltaIfFrequencies_.resize(nIf_);
  for(unsigned iFreq=0; iFreq < nIf_; iFreq++)
    deltaIfFrequencies_[iFreq] = frequency;

  deltaIfFreqPar_.update();
}

/**.......................................................................
 * Install a vector of IF bandwidths
 */
void VisIo::setDeltaIfFrequencies(std::vector<Frequency>& frequencies)
{
  if(frequencies.size() == 0)
    ThrowError("Received zero-sized vector");

  // Iterate over the vector:

  for(unsigned iFreq=0; iFreq < frequencies.size(); iFreq++)
    if(!(frequencies[iFreq].Hz() > 0.0))
      ThrowError("Received zero bandwidth");

  // Finally, store the bandwidths

  deltaIfFrequencies_       = frequencies;

  deltaIfFreqPar_.update();
}

void VisIo::setNumberOfChannelsPerIf(unsigned nChan) 
{
  nChannel_ = nChan;
  nChannelPar_.update();
}

void VisIo::setDeltaChannelFrequency(Frequency frequency) 
{
  deltaChannelFrequency_ = frequency;
  deltaChannelFrequencyPar_.update();
}

/**.......................................................................
 * Set the integration time
 */
void VisIo::setIntTime(Time& intTime)
{
  intTime_ = intTime;
  intTimePar_.update();
}

//------------------------------------------------------------
// Methods to set information about the array
//------------------------------------------------------------

void VisIo::setInstrument(std::string instrument) 
{
  instrument_ = instrument;
  instrumentPar_.update();
} 

void VisIo::setTelescopeName(std::string telescope) 
{
  telescope_ = telescope;
  telescopePar_.update();
}

void VisIo::setLatitude(Angle& lat) 
{
  latitude_ = lat;
  latitudePar_.update();
}

void VisIo::setLongitude(Angle& longitude) 
{
  longitude_ = longitude;
  longitudePar_.update();
}

//-----------------------------------------------------------------------
// Source/Pointing information
//-----------------------------------------------------------------------

void VisIo::setSourceName(std::string srcName) 
{
  srcName_ = srcName;
  srcNamePar_.update();
}

void VisIo::setRa(HourAngle ra) 
{
  ra_ = ra;
  raPar_.update();
}

void VisIo::setDec(DecAngle dec) 
{
  dec_ = dec;
  decPar_.update();
}

void VisIo::setRaApp(HourAngle ra) 
{
  raApp_ = ra;
  raAppPar_.update();
}

void VisIo::setDecApp(DecAngle dec) 
{
  decApp_ = dec;
  decAppPar_.update();
}

void VisIo::setDRaApp(HourAngle ra) 
{
  dRaApp_ = ra;
  dRaAppPar_.update();
}

void VisIo::setDDecApp(DecAngle dec) 
{
  dDecApp_ = dec;
  dDecAppPar_.update();
}

void VisIo::setRaRef(HourAngle ra)
{
  raRef_ = ra;
  raRefPar_.update();
}

void VisIo::setDecRef(DecAngle dec)
{
  decRef_ = dec;
  decRefPar_.update();
}

void VisIo::setUvw(double* uvw)
{
  uvw_ = uvw;
  uvwPar_.update();
}

void VisIo::setVisWide(double* visRe, double* visIm)
{
  visWideRe_ = visRe;
  visWideIm_ = visIm;
  visWidePar_.update();
}

void VisIo::setVisSpec(double* visRe, double* visIm)
{
  visSpecRe_ = visRe;
  visSpecIm_ = visIm;
  visSpecPar_.update();
}

void VisIo::setRms(double* rms)
{
  rms_ = rms;
  rmsPar_.update();
}

void VisIo::setVisFlags(bool* visFlags)
{
  visFlags_ = visFlags;
  visFlagsPar_.update();
}

void VisIo::setMjd(double* mjd)
{
  mjd_ = mjd;
  mjdPar_.update();
}

void VisIo::setLst(double* lst)
{
  lst_ = lst;
  lstPar_.update();
}

//-----------------------------------------------------------------------
// Utility methods
//-----------------------------------------------------------------------

float VisIo::jyPerK(Length& diameter, float apeff)
{
  double k = Constants::kBoltzCgs_;
  float d  = diameter_.centimeters();

  return 2*k/(apeff * M_PI/4 * d * d) * Constants::JyPerCgs_;
}

float VisIo::jyPerK()
{
  return jyPerK(diameter_, apeff_);
}

void VisIo::setAirTemperature(Temperature& temp)
{
  airTemperature_ = temp;
  airTemperaturePar_.update();
}

void VisIo::setWindDirection(Angle& windDirection)
{
  windDirection_ = windDirection;
  windDirectionPar_.update();
}

void VisIo::setWindSpeed(Speed& windSpeed)
{
  windSpeed_ = windSpeed;
  windSpeedPar_.update(); 
}

void VisIo::setPressure(Pressure& pressure)
{
  pressure_ = pressure;
  pressurePar_.update();
}

void VisIo::setRelativeHumidity(double relativeHumidity)
{
  relativeHumidity_ = relativeHumidity;
  relativeHumidityPar_.update();
}

void VisIo::setPurpose(char purpose) 
{
  purpose_ = purpose;
  purposePar_.update();
}

void VisIo::conjugateBaselines(bool conj)
{
  doConj_ = conj;
}
