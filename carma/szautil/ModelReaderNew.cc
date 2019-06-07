#include "carma/szaarrayutils/input.h"

#include "carma/szautil/ModelReaderNew.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include <iomanip>
#include <cmath>

using namespace std;

using namespace sza::util;

const double ModelReaderNew::arcSecPerRad_ = 206265;

/**.......................................................................
 * Constructors
 */
ModelReaderNew::ModelReaderNew()
{
  freqs_.resize(0);
  nFreq_ = 0;
  t_.resize(0);
  tInterp_.resize(0);
}

ModelReaderNew::ModelReaderNew(std::string dir, std::string fileName, std::vector<Frequency> freqs)
{
  initialize(freqs);
  readFile(dir, fileName);
}

void ModelReaderNew::initialize(std::vector<Frequency> freqs)
{
  freqs_ = freqs;
  t_.resize(freqs.size());
  tInterp_.resize(freqs.size());
  nFreq_ = freqs.size();
}

/**.......................................................................
 * Destructor
 */
ModelReaderNew::~ModelReaderNew() {}

void ModelReaderNew::readFile(std::string dir, std::string fileName) 
{
  InputStream* stream=0;
  
  stream = new_InputStream();
  
  if(stream==0) 
    ThrowError("Unable to allocate a new input stream");
  
  try {
    
    if(open_FileInputStream(stream, (char*) dir.c_str(),
			    (char*)fileName.c_str())) 
      
      ThrowError("Unable to connect input stream to file: " 
		 << dir.c_str() << fileName.c_str());
    
    // Locate the first entry.
    
    if(input_skip_white(stream, 0, 0)) 
      ThrowError("Error");
    
    // Read to the end of the stream or error.
    
    while(stream->nextc != EOF) 
      readRecord(stream);
    
  } catch(const Exception& err) {
    
    // Close the file.
    
    if(stream != 0)
      del_InputStream(stream);
    
    throw err;
  }
  
  // Close the file.
  
  if(stream != 0)
    del_InputStream(stream);
}

/**.......................................................................
 * Read the next line of parameters from the file
 */
void ModelReaderNew::readRecord(InputStream* stream) 
{
  // Read the date and time, and skip them
  
  for(unsigned i=0; i < 2; i++)
    readItem(stream);

  // Now read the MJD
  
  readItem(stream);
  mjd_.push_back(atof(stream->work));
  
  // Read the Ediam
  
  readItem(stream);
  eDiam_.push_back(atof(stream->work));
  
  // Read the pdiam
  
  readItem(stream);
  pDiam_.push_back(atof(stream->work));
  
  // Now read the nFreq_ brightness temperatures 

  for(unsigned iFreq = 0; iFreq < nFreq_; iFreq++) {
    readItem(stream);
    t_[iFreq].push_back(atof(stream->work));
  }

}

/**.......................................................................
 * Read a single item from a model file
 */ 
void ModelReaderNew::readItem(InputStream* stream)
{
  if(input_word(stream, 0, 1)) 
    ThrowError("Error in input_word()");
  
  if(input_skip_white(stream, 1, 0))
    ThrowError("Error in input_skip_space()");
}

/**.......................................................................
 * Find the nearest point to the passed MJD
 */
void ModelReaderNew::fillInterpolationContainers(TimeVal& time, Frequency& freq)
{
  double mjd = time.getMjd();
  unsigned iMjdStart, iMjdStop;
  unsigned iFreqStart, iFreqStop;

  findMjdIndices(mjd,   iMjdStart,  iMjdStop);
  findFreqIndices(freq, iFreqStart, iFreqStop);

  // Fill the interpolation containers for physical parameters

  eDiamInterp_.empty();
  pDiamInterp_.empty();

  for(unsigned iMjd=iMjdStart; iMjd <= iMjdStop; iMjd++) {
    eDiamInterp_.extend(mjd_[iMjd], eDiam_[iMjd]);
    pDiamInterp_.extend(mjd_[iMjd], pDiam_[iMjd]);
  }

  // Fill the frequency interpolation containers too

  valInterp_.empty();
  for(unsigned iFreq=iFreqStart; iFreq <= iFreqStop; iFreq++) {

    // Interpolate brightness temperature, at this frequency, over mjd

    tInterp_[iFreq].empty();

    for(unsigned iMjd=iMjdStart; iMjd <= iMjdStop; iMjd++) {
      tInterp_[iFreq].extend(mjd_[iMjd], t_[iFreq].at(iMjd));
    }

    // Now interpolate over frequency at this mjd

    valInterp_.extend(freqs_[iFreq].GHz(), tInterp_[iFreq].evaluate(mjd));
  }

}

/**.......................................................................
 * Find the nearest point to the passed MJD
 */
void ModelReaderNew::fillInterpolationContainers(TimeVal& time)
{
  double mjd = time.getMjd();
  unsigned iMjdStart, iMjdStop;

  findMjdIndices(mjd, iMjdStart, iMjdStop);

  // Fill the interpolation containers for physical parameters

  eDiamInterp_.empty();
  pDiamInterp_.empty();

  for(unsigned iMjd=iMjdStart; iMjd <= iMjdStop; iMjd++) {
    eDiamInterp_.extend(mjd_[iMjd], eDiam_[iMjd]);
    pDiamInterp_.extend(mjd_[iMjd], pDiam_[iMjd]);
  }
}

/**.......................................................................
 * Find the starting index in the array of frequencies corresponding
 * to this frequency
 */
void ModelReaderNew::findMjdIndices(double mjd, unsigned& iStart, unsigned& iStop) 
{
  unsigned size = mjd_.size();

  if(size == 0)
    ThrowError("No data have been read in");
  
  // If the mjd is before the first date, fill in with the three first values
  
  if(mjd <= mjd_[0]) {
    iStart = 0;
    iStop  = (size > 2 ? 2 : size-1);
  } else if(mjd >= mjd_[size-1]) {
    iStop  = size-1;
    iStart = (size > 2 ? size-3 : 0);
  } else {
    
    // Else binary search for the bracketing pair, and return the nearest one.

    unsigned lo = 0, mi = size/2, hi = size-1;
    
    while (hi-lo > 1) {
      
      if(mjd > mjd_[mi]) 
	lo = mi;
      else
	hi = mi;
      
      mi = (hi+lo)/2;
    }
    
    // If the point is nearest to the lower bound, go one element
    // lower, unless there aren't any
    
    if(fabs(mjd-mjd_[lo]) < fabs(mjd-mjd_[hi])) {
      iStart = (lo == 0 ? lo : lo-1);
      iStop  = hi;
    } else {
      iStart = lo;
      iStop  = (hi == size-1 ? hi : hi+1);
    }
  }
}

/**.......................................................................
 * Find the starting index in the array of frequencies corresponding
 * to this frequency
 */
void ModelReaderNew::findFreqIndices(Frequency& freq, unsigned& iStart, unsigned& iStop) 
{
  if(nFreq_ == 0)
    ThrowError("No data have been read in");
  
  unsigned lo=0, hi=nFreq_-1, mid = nFreq_/2;
  
  while(hi - lo > 1) {

    if(freq > freqs_[mid]) {
      lo = mid;
    } else {
      hi = mid;
    }

    mid = (hi + lo)/2;
  }

  // Use the closest point as the midpoint of a three-point
  // interpolation, or if it is at the ends of the array, use an
  // appropriate starting point

  if(fabs(freq.GHz()-freqs_[lo].GHz()) < fabs(freq.GHz()-freqs_[hi].GHz())) { 
    iStart = (lo == 0 ? lo : lo-1);
    iStop  = hi;
  } else {
    iStart = lo;
    iStop  = (hi == nFreq_-1 ? hi : hi+1);
  }

}

/**.......................................................................
 * Interpolate the planck T from the Ts at appropriate frequencies,
 * for the appropriate mjd.
 */
Temperature ModelReaderNew::brightnessTemperature(TimeVal& time, Frequency& freq, 
						  unsigned int& error)
{
  double mjd = time.getMjd();
  double GHz = freq.GHz();

  error = ERR_NONE;

  try {
    fillInterpolationContainers(time, freq);
  } catch(...) {
    error |= ERR_NO_DATA;
  }

  // If this time stamp is outside the range which can be bracketed,
  // flag it as such

  if(!eDiamInterp_.canBracket(mjd))
    error |= ERR_OUTSIDE_MJD;

  // If this frequency is outside the range which can be bracketed,
  // flag it as such

  if(!valInterp_.canBracket(GHz)) {
    error |= ERR_OUTSIDE_FREQ;
  }

  return Temperature(Temperature::Kelvin(), valInterp_.evaluate(GHz));
}

/**.......................................................................
 * Return the solid angle of the source
 */
SolidAngle ModelReaderNew::solidAngle(TimeVal& time, unsigned int& error)
{
  double mjd = time.getMjd();

  error = ERR_NONE;

  try {
    fillInterpolationContainers(time);
  } catch(...) {
    error |= ERR_NO_DATA;
  }

  if(!eDiamInterp_.canBracket(mjd))
    error |= ERR_OUTSIDE_MJD;

  return SolidAngle(SolidAngle::Steradians(), 
		    (M_PI/4 * eDiamInterp_.evaluate(mjd) * pDiamInterp_.evaluate(mjd))/(arcSecPerRad_*arcSecPerRad_));
}

/**.......................................................................
 * Return the equatorial diameter of the source
 */
Angle ModelReaderNew::eDiam(TimeVal& time, unsigned int& error)
{
  double mjd = time.getMjd();

  error = ERR_NONE;

  try {
    fillInterpolationContainers(time);
  } catch(...) {
    error |= ERR_NO_DATA;
  }

  if(!eDiamInterp_.canBracket(mjd))
    error |= ERR_OUTSIDE_MJD;

  return Angle(Angle::ArcSec(), eDiamInterp_.evaluate(mjd));
}

/**.......................................................................
 * Return the polar diameter of the source
 */
Angle ModelReaderNew::pDiam(TimeVal& time, unsigned int& error)
{
  double mjd = time.getMjd();

  error = ERR_NONE;

  try {
    fillInterpolationContainers(time);
  } catch(...) {
    error |= ERR_NO_DATA;
  }

  if(!pDiamInterp_.canBracket(mjd))
    error |= ERR_OUTSIDE_MJD;

  return Angle(Angle::ArcSec(), pDiamInterp_.evaluate(mjd));
}

/**.......................................................................
 * Interpolate the planck T from the Ts at 26, 31 and 36 GHz, for the
 * appropriate time
 */
Flux ModelReaderNew::flux(TimeVal& time, Frequency& freq, 
		       unsigned int& error)
{
  Temperature temp  = brightnessTemperature(time, freq, error);
  unsigned int tmp;
  SolidAngle  omega = solidAngle(time, tmp);

  error |= tmp;

  return Flux(freq, temp, omega);
}
