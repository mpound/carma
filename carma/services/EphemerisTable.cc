
#include "carma/services/Table.h"
#include "carma/services/Types.h"
#include "carma/services/EphemerisTable.h"
#include "carma/util/ErrorException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/util/Trace.h"


#include "carma/util/QuadraticInterpolatorPositiveAngle.h"
#include "carma/util/QuadraticInterpolatorSignedAngle.h"
#include "carma/util/QuadraticInterpolatorNormal.h"

#include <fstream>
#include <sstream>
#include <cstdlib>
#include <cctype>
#include <cmath>

using namespace std;
using namespace carma::services;
using namespace carma::util;


// Default constructor 
EphemerisTable::EphemerisTable() {     
  ncols_ = nrows_ = 0;
  idx_      = -1;
  ra_       = 0.0;
  dec_      = 0.0;
  doppler_  = 0.0;
  distance_ = 0.0;
  valid_    = false;
  ett_      = EPHEM_UNKNOWN;
}


EphemerisTable::EphemerisTable(const string& filename, int nrows) {
  open(filename,nrows);
}


// Destructor does nothing
EphemerisTable::~EphemerisTable() { }


ephemTableType  
EphemerisTable::getEphemType(const string& fileName)
{
  if (ett_ == EPHEM_UNKNOWN) setEphemType(fileName);
  return ett_;
}


void
EphemerisTable::setEphemType(const string& fileName)
{
  Table t(fileName,0);          // throws if table does not exist
  int ncols = t.getNcols();
  if (ncols == 7)
    ett_                 = EPHEM_SZA;     // 0 
  else if (ncols == 10) 
    ett_                 = EPHEM_VECTORS; // 1
  else if (ncols == 11)
    ett_                 = EPHEM_RADEC;   // 2
  else
    ett_                 = EPHEM_UNKNOWN; // 3
}


// Open a file
// it will parse any optional intelligent header
// and read all lines as ASCII lines. Any row or column accessors
// will be parsing these lines when get getCol* and getRow* functions
// are called

void  
EphemerisTable::open(const string& fileName, int maxRows)
{
  Table t(fileName,maxRows);          // will throw if not exist
  setEphemType(fileName);

  nrows_ = t.getNrows();
  ncols_ = t.getNcols();
  mjd_   = 0.0;
  
  setEphemType(fileName);

#if 0
  if (ett_ == EPHEM_SZA) {               // cheat, and use sza format (bad, remember, they use app. RA/DEC, not j2000)
    open_sza(fileName,maxRows);          // also they do not support doppler; use EPHEM_RADEC
    return;
  }
#endif

  if (ett_ != EPHEM_RADEC) {
    std::ostringstream os;
    os << "EphemerisTable::open(" << fileName << ") has only " << ncols_ << " columns; 11 expected for VECTORS";
    const string errorStr = os.str();
    carma::util::Program::getLogger().error(errorStr);
    throw CARMA_EXCEPTION(ErrorException, errorStr);
  }

  if (nrows_ == 0) {
    std::ostringstream os;
    os << "EphemerisTable::open(" << fileName << ") has  " << nrows_ << "rows. Probably not initialized";
    const string errorStr = os.str();
    carma::util::Program::getLogger().error(errorStr);
    throw CARMA_EXCEPTION(ErrorException, errorStr);
  }

  // this is from a Horizons table with QUANTITIES=2,20 date/time 
  // e.g. see CARMA/conf/catalogs/horizons_sun.job for a template
  vmjd_ = t.getDoubleColumn(2);
  vra_  = t.getHMSColumn(3,4,5);
  vdec_ = t.getDMSColumn(6,7,8);
  vdistance_  = t.getDoubleColumn(9);
  vdoppler_   = t.getDoubleColumn(10);

  valid_ = false;

  // Horizons gives dates in in JD, not MJD
  // also convert km/s to m/s
  for (int i=0; i<nrows_; i++) {
    vmjd_[i] -= 2400000.5;
    if (i>0 && vmjd_[i] < vmjd_[i-1]) {
      std::ostringstream os;
      os << "EphemerisTable::open(" << fileName << ") is not time sorted";
      const string errorStr = os.str();
      carma::util::Program::getLogger().error(errorStr);
      throw CARMA_EXCEPTION(ErrorException, errorStr);
    }
    vdoppler_[i] *= 1000.0;    // we want it in m/s
  }
  min_mjd_ = vmjd_[0];
  max_mjd_ = vmjd_[nrows_-1];

  CPTRACE( Trace::TRACE2,
	   "Opened EphemerisTable " << fileName << " with " << nrows_ << " rows x " << ncols_ << " cols" 
	   << " mjd range: " << min_mjd_ << " " << max_mjd_ << endl );

  if (nrows_ == 1) {      // special quick case with no interpolation needed
    ra_       = vra_[0];
    dec_      = vdec_[0];
    doppler_  = vdoppler_[0];
    distance_ = vdistance_[0];
    return;
  }

}

void  
EphemerisTable::open_sza(const string& fileName, int maxRows)
{
  Table t(fileName,maxRows);

  nrows_ = t.getNrows();
  ncols_ = t.getNcols();
  
  // cerr << "Opened sza " << fileName << " with " << nrows_ << " rows x " << ncols_ << " cols" << endl;

  if (ncols_ != 7) {
    // cerr << "not 7 columns, probably not an sza ephemeris" << endl;
    throw "not 7 columns, probably not an sza ephemeris";
  }

  if (nrows_ == 0) { 
    // cerr << "0 rows in ephemeris table" << endl;
    throw "0 rows in ephemeris table";
  }

  // these are SZA ephem files, but note they're not in ra2000/dec2000, but in current epoch
  vmjd_ = t.getDoubleColumn(0);
  vra_  = t.getHMSColumn(1);
  vdec_ = t.getDMSColumn(2);
  vdistance_  = t.getDoubleColumn(3);
  // doppler could be derived from  d(distance)/dt

  if (nrows_ == 1) {      // special case with no interpolation needed
    ra_       = vra_[0];
    dec_      = vdec_[0];
    distance_ = vdistance_[0]; 
    doppler_  = 0.0;
    return;
  }

  // setup Quad interpolator etc.
}

void  
EphemerisTable::setMJD(double mjd)
{
  // normally the tables are given in TT, so make sure you get TT's in here, not UTC's.

  // cout << "setMJD(ephem table) = " << mjd << endl;
  if (mjd == mjd_) return;    // don't do any work if you call it with the same mjd_
  mjd_ = mjd;
  if (nrows_ == 0) {
    // this could happen if setMJD() is called *before* setSource() in ephemeris
    // throw CARMA_EXCEPTION(ErrorException, "EphemerisTable::setMJD: EphemerisTable not intialized yet, setSource too late?");
    // could warn here.... lets hope another setMJD() will follow
    valid_ = false;
  }
  valid_ = true;
  if (nrows_ == 1) return;


  if (mjd < min_mjd_ || mjd > max_mjd_) {
      std::ostringstream os;
      os << "EphemerisTable::setMJD(" << mjd << ") : bad timerange; "
	 << " not in " << min_mjd_ << " - "  << max_mjd_;
      const string errorStr = os.str();
      carma::util::Program::getLogger().error(errorStr);
      throw CARMA_EXCEPTION(ErrorException, errorStr);
  } 

  // @todo    reuse idx_ if set and time checks ok.

  for (int i = 1; i < nrows_-1; i++) {
    if (mjd < vmjd_[i]) {
      idx_ = i;
      carma::util::QuadraticInterpolatorPositiveAngle raqi(0.0);
      carma::util::QuadraticInterpolatorSignedAngle   decqi(0.0);
      carma::util::QuadraticInterpolatorNormal        disqi(0.0);
      carma::util::QuadraticInterpolatorNormal        dopqi(0.0);

      raqi.extend(vmjd_[i-1],vra_[i-1]);
      raqi.extend(vmjd_[i],  vra_[i]);
      raqi.extend(vmjd_[i+1],vra_[i+1]);

      decqi.extend(vmjd_[i-1],vdec_[i-1]);
      decqi.extend(vmjd_[i],  vdec_[i]);
      decqi.extend(vmjd_[i+1],vdec_[i+1]);

      disqi.extend(vmjd_[i-1],vdistance_[i-1]);
      disqi.extend(vmjd_[i],  vdistance_[i]);
      disqi.extend(vmjd_[i+1],vdistance_[i+1]);

      dopqi.extend(vmjd_[i-1],vdoppler_[i-1]);
      dopqi.extend(vmjd_[i],  vdoppler_[i]);
      dopqi.extend(vmjd_[i+1],vdoppler_[i+1]);

#if 0
      // if not sure if we're in range, check this and throw an error 
      if (!raqi.canBracket(mjd))
	throw "bad";
#endif

      ra_       =  raqi.evaluate(mjd);
      dec_      = decqi.evaluate(mjd);
      distance_ = disqi.evaluate(mjd);
      doppler_  = dopqi.evaluate(mjd);

#if 0
      cout <<  setw(18) << setprecision(17)
	   << "table index = " << i << endl
	   << vmjd_[i-1] << " " << vra_[i-1] << " " << vdec_[i-1] << endl
	   << vmjd_[i]   << " " << vra_[i]   << " " << vdec_[i]   << endl
	   << vmjd_[i+1] << " " << vra_[i+1] << " " << vdec_[i+1] << endl
	   << mjd_       << " " << ra_       << " " << dec_       << endl;
#endif

      break;
    }
  }
}

double
EphemerisTable::getMJD() const
{
  if (!valid_) throw CARMA_EXCEPTION(ErrorException, "EphemerisTable::getMJD: invalid time");
  return mjd_;
}

double
EphemerisTable::getRa() const
{
  if (!valid_) throw CARMA_EXCEPTION(ErrorException, "EphemerisTable::getRa: invalid time");
  return ra_;
}

double
EphemerisTable::getDec() const
{
  if (!valid_) throw CARMA_EXCEPTION(ErrorException, "EphemerisTable::getDec: invalid time");
  return dec_;
}

double
EphemerisTable::getDoppler() const
{
  if (!valid_) throw CARMA_EXCEPTION(ErrorException, "EphemerisTable::getDoppler: invalid time");
  return doppler_;
}

double
EphemerisTable::getDistance() const
{
  if (!valid_) throw CARMA_EXCEPTION(ErrorException, "EphemerisTable::getDistance: invalid time");
  return distance_;
}
