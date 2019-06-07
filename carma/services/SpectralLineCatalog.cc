/**
 * @file
 * $Id: SpectralLineCatalog.cc,v 1.15 2006/10/03 21:16:30 mpound Exp $
 */
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>

#include "carma/services/SpectralLineCatalog.h"
#include "carma/services/SpectralLineNotFoundException.h"
#include "carma/services/Table.h"
#include "carma/services/Types.h"
#include "carma/util/StringUtils.h"

using namespace std;
using namespace carma::services;
 
SpectralLineCatalog::SpectralLineCatalog(void) {
}

SpectralLineCatalog::~SpectralLineCatalog(void) {
}


void SpectralLineCatalog::open(const string& filename) {

  Catalog::open(filename);

  // get information from Table
  vector<string> lineName = catalogTable_.getColumn("Line");
  vector<double> frequencyValue = catalogTable_.getDoubleColumn("Frequency");
  vector<string> transitions = catalogTable_.getColumn("Transition");

  int nSpectralLines = lineName.size();

  // throw everything into a map
  SpectralLine lineEntry;
  for (int i = 0; i < nSpectralLines; i++) {
    string lineUC = 
      carma::util::StringUtils::lowASCIIAlphaNumericToUpper(lineName[i]);
    lineEntry.setName(lineUC);
    // currently, the lovas table is in MHz, but this could change, so beware
    lineEntry.setFrequency(Frequency(frequencyValue[i], "MHz"));
    lineEntry.setTransition(transitions[i]);
    lines_.insert(make_pair(lineUC, lineEntry));
  }
}

vector<SpectralLine> 
SpectralLineCatalog::lookup(const string &lineName,
			    const carma::services::Frequency & freqLo,
			    const carma::services::Frequency & freqHi) {
  vector<SpectralLine> foundLines;
  
  //convert to upper case
  string lineUC = 
    carma::util::StringUtils::lowASCIIAlphaNumericToUpper(lineName);
  
  SpectralLineIterator lines_End = lines_.end();
  for (SpectralLineIterator i = lines_.begin(); i != lines_End; i++) {
    // make sure the line name matches
    if ((*i).first == lineUC) {
        Frequency frequency = (*i).second.getFrequency();
      // make sure frequency is within the requested range
        if ( (frequency > freqLo) && (frequency < freqHi) ) {
	  foundLines.push_back((*i).second);
        }
    }
  }

  if ( foundLines.empty() ) {
    ostringstream os;
    os << " Spectral line " << lineUC
       << " not found in catalog " << fileName_
       << " in the frequency range of " << freqLo
       << " to " << freqHi;
    throw CARMA_EXCEPTION(SpectralLineNotFoundException, os.str().c_str());
  }
  
  return foundLines;
}

const SpectralLine &
SpectralLineCatalog::lookup(const string &name, const string &transition) {
  return lookup (name+":"+transition);
}

const SpectralLine &
SpectralLineCatalog::lookup(const string &name) {

  string nameUC = 
    carma::util::StringUtils::lowASCIIAlphaNumericToUpper(name);

  // split line and transition
  vector<string> lineInfo = 
    carma::util::StringUtils::tokenize(nameUC, ":");

  if (lineInfo.size() < 2) {
    throw CARMA_ERROR("SpectralLineCatalog::lookup - must specify a line AND a transition with a ':' delimiter");
  }

  SpectralLineIterator lines_End = lines_.end();
  for (SpectralLineIterator i = lines_.begin(); i != lines_End; i++) {
    // check to make sure both the line name and transition match
    if ((*i).first == lineInfo[0]) {
      if((*i).second.getTransition() == lineInfo[1]) {
	return (*i).second;
      }
    }
  }

  // if it gets here, that means that nothing was found
  ostringstream os;
  os << " Spectral line " << nameUC
     << " not found in catalog " << fileName_;
  throw CARMA_EXCEPTION(SpectralLineNotFoundException, os.str().c_str());
   
}
