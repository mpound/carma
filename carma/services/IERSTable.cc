//
//  $Id: IERSTable.cc,v 1.11 2014/08/29 20:17:24 mpound Exp $ 
//
#include "carma/services/Table.h"
#include "carma/services/IERSTable.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <fstream>
#include <sstream>
#include <cstdlib>
#include <cctype>
#include <cmath>

using namespace std;
using namespace carma::util;
using namespace carma::services;


const double IERSTable::MAX_ALLOWABLE_DAYS_OUT_OF_DATE = 28.0;

IERSTable::IERSTable(const string& filename)
{  
  if (filename.size() > 0) {
      filename_ = filename;
  } else {
      filename_ = Program::getConfFile("catalogs/IERS.tab");
  }
  open(filename_);
}

IERSTable::~IERSTable() { }

void
IERSTable::open(const string& filename)
{
  CARMA_CPTRACE(::carma::util::Trace::TRACE2, "IERSTable open: " << filename);
  Table t(filename);
  mjd_    = t.getDoubleColumn("mjd");
  dut1_   = t.getDoubleColumn("dut1");
  xpolar_ = t.getDoubleColumn("xpolar");
  ypolar_ = t.getDoubleColumn("ypolar");
  last_mjd_ = 0.0;
  last_idx_ = -1;
  int n=mjd_.size();
  
  for (int i=1; i<n; i++) {
    if (mjd_[i] < mjd_[i-1]) {
      ostringstream os;
      os << "[IERSTtable::open] " << filename << " badly sorted: @ "
	 << i << " mdj=" << mjd_[i];
      throw CARMA_ERROR(os);
    }
  }
  // record min and max valid times
  min_mjd_ = mjd_[0];
  max_mjd_ = mjd_[n-1];
}

void
IERSTable::close(void)
{
  last_idx_ = -1;
}

void 
IERSTable::reload(void) {
    open(filename_);
}

// accessors

double 
IERSTable::dut1(const double mjd)
{
  setIndex(mjd);
  if (last_idx_ < 0) return 0.0;
  return dut1_[last_idx_];
}

double 
IERSTable::xpolar(const double mjd)
{
  setIndex(mjd);
  if (last_idx_ < 0) return 0.0;
  return xpolar_[last_idx_];
}

double 
IERSTable::ypolar(const double mjd)
{
  setIndex(mjd);
  if (last_idx_ < 0) return 0.0;
  return ypolar_[last_idx_];
}

double 
IERSTable::age(void) 
{
    return ( Time::MJD() - getMinMJD() );
}

bool 
IERSTable::isOutOfDate( void ) 
{
    return ( age() > MAX_ALLOWABLE_DAYS_OUT_OF_DATE );
}

// private functions

void
IERSTable::setIndex(const double mjd)
{
  if (mjd == last_mjd_) return;
  last_mjd_ = mjd;

  int n = mjd_.size();
  // cerr << "MJD range: " << mjd_[0] << " : " << mjd_[n-1] << endl;
  if (mjd < mjd_[0] || mjd > mjd_[n-1]) {    // time out of range
    // or throw range exception?
    last_idx_ = -1;   // this will trigger returning 0's for all quantities
    return;
  }
  // a slow linear search, also assumes array is sorted
  // (which IERS tables are)
  // @todo Quad interpolator needed here
  for (int i=0; i<n; i++) {
    if (mjd < mjd_[i]) {
      // cerr << "last_idx: " << i << endl;
      last_idx_ = i-1;
      return;
    }
  } 
}

