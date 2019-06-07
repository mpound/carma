#include "carma/szaarrayutils/input.h"

#include "carma/szautil/ModelReader.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include <iomanip>
#include <cmath>

using namespace std;

using namespace sza::util;

const double ModelReader::arcSecPerRad_ = 206265;

/**.......................................................................
 * Constructors
 */
ModelReader::ModelReader() : 
  t26Interp_(0.0), t31Interp_(0.0), t36Interp_(0.0), eDiamInterp_(0.0), pDiamInterp_(0.0) {}

ModelReader::ModelReader(std::string dir, std::string fileName) :
  t26Interp_(0.0), t31Interp_(0.0), t36Interp_(0.0), eDiamInterp_(0.0), pDiamInterp_(0.0) 
{
  readFile(dir, fileName);
}

/**.......................................................................
 * Destructor
 */
ModelReader::~ModelReader() {}

void ModelReader::readFile(std::string dir, std::string fileName) 
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

#if 0
  COUT("Read: " << mjd_.size() << " records"
       << "Starting Mjd: " << std::setprecision(12) << mjd_[0]
       << "Starting Mjd: " << std::setprecision(12) << mjd_[1]
       << " Ending Mjd: " << std::setprecision(12) << mjd_[mjd_.size()-1]);
#endif
}

void ModelReader::readRecord(InputStream* stream) 
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
  
  // Read the T26GHz
  
  readItem(stream);
  t26_.push_back(atof(stream->work));
  
  // Read the T31GHz
  
  readItem(stream);
  t31_.push_back(atof(stream->work));
  
  // Read the T36GHz
  
  readItem(stream);
  t36_.push_back(atof(stream->work));
}

void ModelReader::readItem(InputStream* stream)
{
  if(input_word(stream, 0, 1)) 
    ThrowError("Error in input_word()");
  
  if(input_skip_white(stream, 1, 0))
    ThrowError("Error in input_skip_space()");
}

/**.......................................................................
 * Find the nearest point to the passed MJD
 */
void ModelReader::fillInterpContainers(double mjd)
{
  unsigned size = mjd_.size();
  unsigned iStart=0, iStop=0;

  if(size == 0)
    ThrowError("No dates have been read in");
  
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

  // Fill the interpolation containers

  t26Interp_.empty();
  t31Interp_.empty();
  t36Interp_.empty();

  eDiamInterp_.empty();
  pDiamInterp_.empty();

  for(unsigned i=iStart; i <= iStop; i++) {

    t26Interp_.extend(mjd_[i], t26_[i]);
    t31Interp_.extend(mjd_[i], t31_[i]);
    t36Interp_.extend(mjd_[i], t36_[i]);

    eDiamInterp_.extend(mjd_[i], eDiam_[i]);
    pDiamInterp_.extend(mjd_[i], pDiam_[i]);

  }

}

/**.......................................................................
 * Interpolate the planck T from the Ts at 26, 31 and 36 GHz, for the
 * appropriate time
 */
Temperature ModelReader::brightnessTemperature(TimeVal& time, Frequency& freq, 
					       unsigned int& error)
{
  double mjd = time.getMjd();
  double GHz = freq.GHz();

  error = ERR_NONE;
  fillInterpContainers(mjd);

  // If this time stamp is outside the range which can be bracketed,
  // flag it as such

  if(!t26Interp_.canBracket(mjd))
    error |= ERR_OUTSIDE_MJD;

  static QuadraticInterpolatorNormal tInterp(0.0);

  tInterp.empty();

  tInterp.extend(26.0, t26Interp_.evaluate(mjd));
  tInterp.extend(31.0, t31Interp_.evaluate(mjd));
  tInterp.extend(36.0, t36Interp_.evaluate(mjd));

  // If this frequency is outside the range which can be bracketed,
  // flag it as such

  if(!tInterp.canBracket(GHz)) {
    error |= ERR_OUTSIDE_FREQ;
  }

  return Temperature(Temperature::Kelvin(), tInterp.evaluate(GHz));
}

/**.......................................................................
 * Return the solid angle of the source
 */
SolidAngle ModelReader::solidAngle(TimeVal& time, unsigned int& error)
{
  double mjd = time.getMjd();

  error = ERR_NONE;
  fillInterpContainers(mjd);

  if(!eDiamInterp_.canBracket(mjd))
    error |= ERR_OUTSIDE_MJD;

  return SolidAngle(SolidAngle::Steradians(), 
		    (M_PI/4 * eDiamInterp_.evaluate(mjd) * pDiamInterp_.evaluate(mjd))/(arcSecPerRad_*arcSecPerRad_));
}

/**.......................................................................
 * Return the equatorial diameter of the source
 */
Angle ModelReader::eDiam(TimeVal& time, unsigned int& error)
{
  double mjd = time.getMjd();

  error = ERR_NONE;
  fillInterpContainers(mjd);

  if(!eDiamInterp_.canBracket(mjd))
    error |= ERR_OUTSIDE_MJD;

  return Angle(Angle::ArcSec(), eDiamInterp_.evaluate(mjd));
}

/**.......................................................................
 * Return the polar diameter of the source
 */
Angle ModelReader::pDiam(TimeVal& time, unsigned int& error)
{
  double mjd = time.getMjd();

  error = ERR_NONE;
  fillInterpContainers(mjd);

  if(!pDiamInterp_.canBracket(mjd))
    error |= ERR_OUTSIDE_MJD;

  return Angle(Angle::ArcSec(), pDiamInterp_.evaluate(mjd));
}

/**.......................................................................
 * Interpolate the planck T from the Ts at 26, 31 and 36 GHz, for the
 * appropriate time
 */
Flux ModelReader::flux(TimeVal& time, Frequency& freq, 
		       unsigned int& error)
{
  Temperature temp  = brightnessTemperature(time, freq, error);
  unsigned int tmp;
  SolidAngle  omega = solidAngle(time, tmp);

  error |= tmp;

  return Flux(freq, temp, omega);
}
