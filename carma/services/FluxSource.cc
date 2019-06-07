/**
 * @file FluxSource.cc
 * $Id: FluxSource.cc,v 1.6 2008/02/20 20:33:29 mpound Exp $
 *
 * @author Chul Gwon
 */

#include "carma/services/FluxSource.h"
#include "carma/util/ErrorException.h"
//#include "carma/util/StopWatch.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include <iostream>
#include <sstream>
#include <iomanip>
#include <math.h>
#include <stdlib.h>
#include <string>

using namespace std;
using namespace carma::services;
using namespace carma::util;

const string FluxSource::fmt_ = "%Y-%b-%d %H:%M:%S";

FluxSource::FluxSource() :
// t3(0.0),
  mjd_(0.0),
  freq_( Frequency(86.2, "GHz") ),
  flux_( FluxDensity(0.0, "Jy") ),
  rms_( 0.0 ),
  refAnt_()
{
}

FluxSource::~FluxSource() {}

void
FluxSource::setMJD(double mjd)
{
  mjd_ = mjd;
}

void
FluxSource::setMJD(const string &date) 
{
  setMJD( parseDate( date ) );
}

const string
FluxSource::getDate() 
{
  return parseMJD ( mjd_ );
}

double FluxSource::parseDate(const string & date) {
    //StopWatch sw(StopWatch::CPU_TIME);
    string::size_type pos = date.find_last_of(".");
    string stripped = date.substr(0,pos);
    // must change case on letters of month so
    // that they fit the %b specification.  CARMA flux catalog format has
    // all upper, e.g. "MAY", and %b requires "May"; Do all three just to
    // be sure and to be flexible.
    stripped[5] = toupper(stripped[5]);
    stripped[6] = tolower(stripped[6]);
    stripped[7] = tolower(stripped[7]);
    // note we must have HMS on the date or computeMJD() returns wildly
    // wrong answers! most likely because strptime() has undef'd fields
    // somewhere its internal tm struct.
    stripped += " 00:00:00";
    //sw.start();

    double roundedMJD = Time::computeMJD(stripped, fmt_, Time::UTC);

    //sw.stop();
    //t3 = sw.getElapsedTime();
    if ( roundedMJD == 0 ) {
	ostringstream os;
	os << "Unable to parse date " << stripped << " for source "
	    << getName();
	throw CARMA_EXCEPTION(ErrorException, os.str() );
    } else {
	ostringstream os;
	os << "Parsed date " << stripped << " for source "
	    << getName() << " as MJD= " << roundedMJD;
	CARMA_CPTRACE(Trace::TRACE1, os.str() );
    }
    const string dayPart = date.substr(pos);
    const char* ps = dayPart.c_str();
    double partialDay = atof(ps);
    return ( roundedMJD+partialDay );
}

double
FluxSource::getMJD() const
{
    return mjd_;
}

const string FluxSource::parseMJD(double mjd)
{
    int imjd = static_cast<int>( floor(mjd) );
    string date = Time::getDateString(imjd,"%Y-%b-%d");
    double partialDay = mjd - imjd;
    // remove leading zeroes
    std::ostringstream os;
    os << setprecision(1) 
       << setiosflags(ios::fixed)
       << partialDay;
    string pday = os.str();
    const string zero("0");
    string::size_type startpos = pday.find_first_not_of( zero );
    pday = pday.substr( startpos );

    std::ostringstream dayOS;
    dayOS << date << pday ;
    const string daystr = dayOS.str();
    return daystr;
}



void
FluxSource::setFrequency(const carma::services::Frequency& freq) {
  freq_ = freq;
}

carma::services::Frequency
FluxSource::getFrequency() const {
  return freq_;
}

void
FluxSource::setFlux(const carma::services::FluxDensity& flux) {
  flux_ = flux;
}

carma::services::FluxDensity
FluxSource::getFlux() const {
  return flux_;
}

void
FluxSource::setRms(const float rms) {
  rms_ = rms;
}

float
FluxSource::getRms() const {
  return rms_;
}

void
FluxSource::setRefAnt(const string &ref) {
  refAnt_ = ref;
}

string
FluxSource::getRefAnt() const {
  return refAnt_;
}

