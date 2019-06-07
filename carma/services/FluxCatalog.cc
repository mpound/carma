/**
 * @file
 * $Id: FluxCatalog.cc,v 1.8 2009/06/01 21:05:22 mpound Exp $
 *
 * @author Chul Gwon
 */

#include "carma/services/stringConstants.h"
#include "carma/services/Types.h"
#include "carma/services/FluxCatalog.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/Frequency.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include <limits>
#include <cmath>
#include <iomanip>

using namespace std;
using namespace carma::services;
using namespace carma::util;

const string FluxCatalog::DEFAULT_CATALOG = "catalogs/FluxSource.cat";

FluxCatalog::FluxCatalog() : 
    zeroFreq_(0.0,"GHz")
{ 
}

FluxCatalog::~FluxCatalog() {}

void
FluxCatalog::open(const std::string& fileName) {
  Catalog::open(fileName);

  vector<string> sourceName, date;
  vector<double> frequency, flux, rms;
  vector<string> refAnt;

  try {
    sourceName = catalogTable_.getColumn("Source");
    date       = catalogTable_.getColumn("Day.UT");
    frequency  = catalogTable_.getDoubleColumn("Freq");
    flux       = catalogTable_.getDoubleColumn("Flux");
    rms        = catalogTable_.getDoubleColumn("Rms");
    refAnt     = catalogTable_.getColumn("Ref.");
  } catch (const carma::util::ErrorException &ex) {
      programLogError( ex.getErrorMessage() );
      throw;
  }

  int nEntries = sourceName.size();

  const string FREQCOL("Freq");
  const string FLUXCOL("Flux");
  FluxSource fluxEntry;
  for (int i = 0; i < nEntries; i++) {
    fluxEntry.setName(sourceName[i]);
    fluxEntry.setMJD(date[i]);
    int colNo = catalogTable_.getColumnNumber( FREQCOL );
    fluxEntry.setFrequency( 
	    Frequency( frequency[i], catalogTable_.getColumnUnit(colNo) )
	                  );
    colNo = catalogTable_.getColumnNumber( FLUXCOL );
    fluxEntry.setFlux( 
	    FluxDensity( flux[i],    catalogTable_.getColumnUnit(colNo) )
	    );
    fluxEntry.setRms(rms[i]);
    if ( refAnt[i].size() >  0 )
	fluxEntry.setRefAnt(refAnt[i]);
    else
	fluxEntry.setRefAnt( UNKNOWN );

    fluxSources_.insert( make_pair(sourceName[i], fluxEntry) );
  }
}
    
const FluxSource &
FluxCatalog::lookup(const std::string& sourceName) 
{
    return lookup(sourceName, zeroFreq_, zeroFreq_, 0.0);
}

const FluxSource &
FluxCatalog::lookup(const std::string& sourceName,
	            const Frequency& freq, const Frequency& deltaFreq,
		    float deltaDays) 
{

    // There is probably a cleverer way to do this in fewer steps,
    // but this way is fairly quick and transparent.
    
  ScopedLogNdc("FluxCatalog::lookup");
  if (! finalMatches_.empty() ) 
      finalMatches_.clear();
  string nameUC = util::StringUtils::lowASCIIAlphaNumericToUpper(sourceName);

  vector<FluxSource> matched;
  // parse this in reverse since entries added chronologically
  multimap<string, FluxSource>::const_reverse_iterator sourceRend 
      = fluxSources_.rend();
  for ( multimap<string, FluxSource>::const_reverse_iterator i 
	  = fluxSources_.rbegin();
	i != sourceRend; i++ ) {
    if ( (*i).first == nameUC ) {
      matched.push_back( (*i).second );
    }
  }

  int size = matched.size();
  CARMA_CPTRACE(Trace::TRACE1,"size of NAME matched is " << size);
  if ( size == 0 ) {
      // if it gets here, that means that nothing was found
      ostringstream os;
      os << " Flux calibration source " << nameUC
	 << " not found in catalog " << fileName_;
      throw CARMA_EXCEPTION(SourceNotFoundException, os.str().c_str());
  }


  // @TODO? Allow reference time to be any date, not just "now"?
  double now = Time::MJD();
  // zero delta days means match anything
  double timeLimit;
  if ( deltaDays == 0 ) 
      timeLimit = 1.0;
  else 
      timeLimit = now - deltaDays;
  // 1. vector is sorted by time already with most recent time in
  // at the lowest index.
  int timeIndex = -1;
  // @todo a binary_search() would be quicker or lower_bound()
  for (int i = 0; i < size; i++ ) {
      FluxSource f = matched[i];
      double mjd = f.getMJD();
      if ( mjd < timeLimit ) {
	  break;
      }
      timeIndex = i;
  }

  if ( timeIndex == -1 ) {
      ostringstream os;
      os << " No measurement for flux calibration source " << nameUC
	 << " matching time range MJD [" << timeLimit
	 << "," << now << "] "
	 << " found in catalog " << fileName_;
      throw CARMA_EXCEPTION(SourceNotFoundException, os.str().c_str());
  }


  // timeIndex now points to the last entry which has date of
  // measurement within (up to and including) the timeLimit specified.
  // Erase the rest.
  for( int i =timeIndex+1;i<size;i++)
      matched.pop_back();

  // 2. from the extracted, find all that are within frequency range.
  size = matched.size();
  CARMA_CPTRACE(Trace::TRACE1,"size of NAME and TIME matched is " << size);

  const double fghz = freq.gigahertz();

  if ( fghz == 0 ) {
      finalMatches_.push_back(matched[0]);
  CARMA_CPTRACE(Trace::TRACE1,"Ignoring frequency matching" );
      return finalMatches_.at(0);
  }

  double dghz = deltaFreq.gigahertz();
  double lowFreq, hiFreq;

  if ( dghz == 0 ) {
      lowFreq = 0.0;
      hiFreq = 300.0;
  } else {
      lowFreq = fghz - dghz;
      hiFreq  = fghz + dghz;
  }

  {
      ostringstream xx;
      xx << "Searching in frequency range ["
	  << lowFreq << ","
	  << hiFreq << "]"
	  ;
      CARMA_CPTRACE(Trace::TRACE1, xx.str());
  }

  for (int i=0; i<size; i++ ) {
      FluxSource f = matched[i];
      double fsfreq = f.getFrequency().gigahertz();
      if (  fsfreq >= lowFreq && fsfreq <= hiFreq )
              finalMatches_.push_back(f);
  }


  // 3. for each of these, compute the distance in time-frequency
  //    phase space.
  
  int closest = -1;
  double min_distance = numeric_limits<float>::max();
  int nsize = finalMatches_.size();
  if ( nsize == 0 ) {
      ostringstream os;
      os << " No measurement for flux calibration source " << nameUC
	 << " matching frequency range [" << lowFreq
	 << "," << hiFreq << "] GHz"
	 << " found in catalog " << fileName_;
      throw CARMA_EXCEPTION(SourceNotFoundException, os.str().c_str());
  }
  CARMA_CPTRACE(Trace::TRACE1,"size of NAME TIME FREQ matched is " << nsize);
  for ( int i=0; i < nsize ; i++ ) {
      FluxSource src = finalMatches_[i];
      double distance = pow( ( now - src.getMJD() ), 2.0) 
	              + pow( ( fghz - src.getFrequency().gigahertz() ), 2.0); 
      if ( distance < min_distance ) {
	  min_distance = distance;
	  closest = i;
      }
  }


  if ( closest == -1 ) {
      ostringstream os;
      os << " No measurement for flux calibration source " << nameUC
	 << " matching frequency range [" << lowFreq
	 << "," << hiFreq << "] GHz"
	 << " found in catalog " << fileName_;
      throw CARMA_EXCEPTION(SourceNotFoundException, os.str().c_str());
  }

  // 4. Return the entry with the smallest value.
  return finalMatches_.at(closest);

}
