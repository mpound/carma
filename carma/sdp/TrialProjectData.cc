/**
 *
 * @file TrialProjectData.cc
 *
 * A class to accumulate project data for an obsblock trial.
 *
 * $Id: TrialProjectData.cc,v 1.30 2014/04/09 02:07:36 friedel Exp $
 */

// Carma includes
#include "carma/corba/Client.h"
#include "carma/sdp/TrialProjectData.h"
#include "carma/sdp/SDPUtil.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/observertools/ProjectDatabaseManager.h"
#include "carma/observertools/ProjectDatabaseManager_skel.h"

// Carma tools includes
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLStringTokenizer.hpp>
#include <xercesc/util/XMLDouble.hpp>
#include <xercesc/util/XMLFloat.hpp>

// C++ standard library or system includes
#include <fstream>
#include <iostream>
#include <cmath>
#include <sstream>
#include <iomanip>
#include <sys/stat.h>
#include <sys/types.h>

// Namespace using directives
using namespace XERCES_CPP_NAMESPACE;
using namespace std;
using namespace carma::observertools;
using namespace carma::util;
using carma::sdp::SDPUtil;

namespace carma {
namespace sdp {

  //==========================================================================

  TrialProjectData::TrialProjectData() :
    obsBlockId_p(""),
    astroHdrFile_p(""),
    obsBlockIdComponents_p(),
    antennas_p(),
    time_p(0),
    lst_p(0),
    ra_p(0),
    dec_p(0),
    veldop_p(0),
    inttime_p(0),
    dra_p(0),
    ddec_p(0),
    vsource_p(0),
    fswitch_p(0),
    selfcal_p(0),
    source_p(""),
    veltype_p(""),
    purpose_p(""),
    obsline_p(""),
    astroHdrValidity_p(),
    sfreq_p(),
    sdf_p(),
    nschan_p(),
    numspw_p(0),
    correlatorSetup_p(),
    sources_p(),
    calibrators_p(),
    targets_p(),
    trialObservedLST_p(2,0),
    pointingOffsets_p(),
    trialObservationDate_p(),
    currentCorrelatorSetup_p(-1),
    fastSwitch_p(0),
    numberOfAntennas_p(0),
    correlatorValid_p(false),
    firstTime(true),
    processIntegrationCalled_(false)
  {
    // Null constructor
  };

  //==========================================================================

  TrialProjectData::TrialProjectData(const std::string& obsBlockId) :
    obsBlockId_p(obsBlockId),
    astroHdrFile_p(SDPUtil::astroHdrFileName("",obsBlockId)),
    obsBlockIdComponents_p(SDPUtil::parseObsBlockId(obsBlockId)),
    antennas_p(),
    time_p(0),
    lst_p(0),
    ra_p(0),
    dec_p(0),
    veldop_p(0),
    inttime_p(0),
    dra_p(0),
    ddec_p(0),
    vsource_p(0),
    fswitch_p(0),
    selfcal_p(0),
    source_p(""),
    veltype_p(""),
    purpose_p(""),
    obsline_p(""),
    astroHdrValidity_p(),
    sfreq_p(),
    sdf_p(),
    nschan_p(),
    numspw_p(0),
    correlatorSetup_p(),
    sources_p(),
    calibrators_p(),
    targets_p(),
    trialObservedLST_p(2,0),
    pointingOffsets_p(),
    trialObservationDate_p(),
    currentCorrelatorSetup_p(-1),
    fastSwitch_p(0),
    numberOfAntennas_p(0),
    correlatorValid_p(false),
    firstTime(true),
    processIntegrationCalled_(false)
  {
    // Construct from an obs. block id.

  };

  //==========================================================================
    
  TrialProjectData::~TrialProjectData()
  {
    // Destructor
  };

  //==========================================================================

  void TrialProjectData::resetIntegrationData()
  {
    // Reset at start of current integration.

    // Initialize astro header variables.
    antennas_p.resize(0);
    astroHdrValidity_p.clear();
    astroHdrValidity_p["ANTENNAS"] = false;
    time_p = 0;
    astroHdrValidity_p["TIME"] = false;
    lst_p = 0;
    astroHdrValidity_p["LST"] = false;
    ra_p = 0;
    astroHdrValidity_p["RA"] = false;
    dec_p = 0;
    astroHdrValidity_p["DEC"] = false;
    veldop_p = 0;
    astroHdrValidity_p["VELDOP"] = false;
    inttime_p = 0;
    astroHdrValidity_p["INTTIME"] = false;
    dra_p = 0;
    astroHdrValidity_p["DRA"] = false;
    ddec_p = 0;
    astroHdrValidity_p["DDEC"] = false;
    vsource_p = 0;
    astroHdrValidity_p["VSOURCE"] = false;
    fswitch_p = 0;
    astroHdrValidity_p["FSWITCH"] = false;
    selfcal_p = 0;
    astroHdrValidity_p["SELFCAL"] = false;
    source_p = "";
    astroHdrValidity_p["SOURCE"] = false;
    veltype_p = "";
    astroHdrValidity_p["VELTYPE"] = false;
    purpose_p = "";
    astroHdrValidity_p["PURPOSE"] = false;
    obsline_p = "";
    astroHdrValidity_p["OBSLINE"] = false;
    sfreq_p.clear();
    astroHdrValidity_p["SFREQ"] = false;
    sdf_p.clear();
    astroHdrValidity_p["SDF"] = false;
    nschan_p.clear();
    astroHdrValidity_p["NSCHAN"] = false;
    numspw_p = -1;
    astroHdrValidity_p["NUMSPW"] = false;
    correlatorValid_p = false;

    return;
  };
    
  //==========================================================================

  bool TrialProjectData::addAstroHdrElement(const Attributes& attr)
  {
    // Accumulate any project data from the current astro header element.

    // Initialization
    bool retval = false;

    try {
      // Extract element name, value, and length
      const XMLCh* kwName = XMLString::transcode("name");
      const XMLCh* kwValue = XMLString::transcode("value");
      const XMLCh* kwLength = XMLString::transcode("length");
      std::string name(XMLString::transcode(attr.getValue(kwName)));
      const XMLCh* val = attr.getValue(kwValue);
      const XMLCh* length = attr.getValue(kwLength);
      const int nitems = XMLString::parseInt(length);
      const double radToArcsec = 57.29577951 * 3600.0;

      // Check if this element contains project data
      if (name == "time") {
	time_p = XMLDouble(val).getValue();
	astroHdrValidity_p["TIME"] = true;
	
      } else if (name == "inttime") {
	inttime_p = XMLFloat(val).getValue();
	astroHdrValidity_p["INTTIME"] = true;
	
      } else if (name == "lst") {
	lst_p = XMLDouble(val).getValue();
	astroHdrValidity_p["LST"] = true;

      } else if (name == "fswitch") {
	fswitch_p = XMLString::parseInt(val);
	astroHdrValidity_p["FSWITCH"] = true;

      } else if (name == "dra") {
	dra_p = XMLFloat(val).getValue() * radToArcsec;
	astroHdrValidity_p["DRA"] = true;

      } else if (name == "ddec") {
	ddec_p = XMLFloat(val).getValue() * radToArcsec;
	astroHdrValidity_p["DDEC"] = true;

      } else if (name == "antennas") {
	XMLStringTokenizer tokens(val);
	for (int i=0; i < nitems; i++) {
	  const XMLCh* v = tokens.nextToken();
	  antennas_p.push_back(XMLString::parseInt(v));
	};
	astroHdrValidity_p["ANTENNAS"] = true;

      } else if (name == "source") {
	source_p = XMLString::transcode(val);
	astroHdrValidity_p["SOURCE"] = true;

      } else if (name == "ra") {
	ra_p = XMLDouble(val).getValue();
	astroHdrValidity_p["RA"] = true;

      } else if (name == "dec") {
	dec_p = XMLDouble(val).getValue();
	astroHdrValidity_p["DEC"] = true;

      } else if (name == "veldop") {
	veldop_p = XMLDouble(val).getValue();
	astroHdrValidity_p["VELDOP"] = true;

      } else if (name == "veltype") {
	veltype_p = XMLString::transcode(val);
	astroHdrValidity_p["VELTYPE"] = true;

      } else if (name == "vsource") {
	vsource_p = XMLFloat(val).getValue();
	astroHdrValidity_p["VSOURCE"] = true;

      } else if (name == "selfcal") {
	selfcal_p = XMLString::parseInt(val);
	astroHdrValidity_p["SELFCAL"] = true;

      } else if (name == "purpose") {
	purpose_p = XMLString::transcode(val);
	if(purpose_p == "UNSET"){
	  purpose_p = "O";
	}
	astroHdrValidity_p["PURPOSE"] = true;

      } else if (name == "obsline") {
	obsline_p = XMLString::transcode(val);
	astroHdrValidity_p["OBSLINE"] = true;
      };
      retval = true;

    } catch ( const XMLException& ex) {
      retval = false;
    };
    return retval;
  };

  //==========================================================================

  bool TrialProjectData::addFreqInfo(const std::vector<double>& sfreq,
				     const std::vector<double>& sdf,
				     const std::vector<int>& nschan)
  {
    // Accumulate correlator setup frequency configuration information.

    // Initialization
    bool retval = false;

    // Update correlator setup frequency configuration information.
    sfreq_p = sfreq;
    astroHdrValidity_p["SFREQ"] = true;
    sdf_p = sdf;
    astroHdrValidity_p["SDF"] = true;
    nschan_p = nschan;
    astroHdrValidity_p["NSCHAN"] = true;
    numspw_p = std::min(std::min(sfreq.size(), sdf.size()), nschan.size());
    astroHdrValidity_p["NUMSPW"] = true;

    return retval;
  };

  //==========================================================================

  bool TrialProjectData::processIntegration()
  {
    // Process accumulated project data at end of the current integration.

    // Initialization
    bool retval = false;

    // Process correlator setup information.
    processCorrelatorSetupInfo();

    // Process source information.
    processSourceInfo();

    // Process calibrator information.
    processCalibratorInfo();

    // Process target information.
    processTargetInfo();

    // Process pointing offset information.
    processPointingOffsetInfo();

    // Process antenna information.
    processAntennaInfo();

    // Process fast switching information.
    processSwitchingInfo();

    // Process time information.
    processTimeInfo();

    processIntegrationCalled_ = true;
    return retval;
  };

  //==========================================================================

  void TrialProjectData::processCorrelatorSetupInfo()
  {
    // Process correlator setup information from the current integration.

    // Initialization
    const double freqTol = 1e-5;
    const double C = 299792.458;
    // Check validity of required frequency elements
    if ((correlatorValid_p = valid("SFREQ SDF NSCHAN NUMSPW VELDOP"))) {
      // Search for a matching correlator setup frequency configuration
      bool found = false;
      uint sndx;
      for (sndx=0; (sndx < correlatorSetup_p.size()) && (!found); sndx++) {
	// Validity check
	if (correlatorSetup_p.count(sndx) > 0) {
	  ProjectCorrelatorSetup& cs = correlatorSetup_p[sndx];
	  // Match setup number and number of spectral windows
	  if ((cs.setupNumber == static_cast<int>(sndx+1)) && 
	      (numspw_p == cs.numberOfWindows)) {
	    found = true;
	    // Loop over spectral window
	    for (int jspw = 0; jspw < numspw_p; jspw++) {
	      int spw = cs.windowNumber[jspw] - 1;
	      found = (found) && (rdiff(sdf_p[spw], cs.resolution[jspw])
				  < freqTol) &&
		(nschan_p[spw] == cs.numberOfChannels[jspw]) &&
		(rdiff(sfreq_p[spw]*(1 + veldop_p/C), cs.minfreq[jspw])
		 < freqTol);
	    }; // for (jspw)
	  }; // if (cs.setupNumber)
	}; // if (cs.count)
      }; // for (sndx)

      // If match not found then add new entry, else update
      // current correlator index to existing matching entry.
      if (!found) {
	ProjectCorrelatorSetup cs;
	cs.setupNumber = correlatorSetup_p.size() + 1;
	cs.numberOfWindows = numspw_p;
	for (int jspw = 0; jspw < numspw_p; jspw++) {
	  cs.windowNumber.push_back(jspw + 1);
	  cs.resolution.push_back(sdf_p[jspw]);
	  cs.numberOfChannels.push_back(nschan_p[jspw]);
	  cs.minfreq.push_back(sfreq_p[jspw]*(1 + veldop_p/C));
	};
	correlatorSetup_p[correlatorSetup_p.size()] = cs;
	currentCorrelatorSetup_p = correlatorSetup_p.size();

      } else {
	currentCorrelatorSetup_p = sndx;
      };
    }; 

    return;
  };

  //==========================================================================

  void TrialProjectData::processSourceInfo()
  {
    // Process source information from the current integration.

    // Initialization
    double velTol = 1e-6;
    std::string::size_type sloc = 0;

    // Check validity of required astro header elements
    if (valid("SOURCE RA DEC VSOURCE VELTYPE SELFCAL INTTIME PURPOSE") &&
	(sloc = StringUtils::lowASCIIAlphaNumericToUpper(purpose_p).find("S"))
	!= std::string::npos && correlatorValid_p) {
      // remove the "S" from the purpose
      purpose_p.erase(sloc,1);
      // Search for a matching source project data entry
      bool found = false;
      uint i;
      for (i=0; (i < sources_p.size()) && (!found); i++) {
	if((found = (source_p == sources_p[i].sourceName))){
// none of the following should ever happen but we should check anyway
	  if(rdiff(vsource_p, sources_p[i].vsource) > velTol){
	    cout << "Warning: source velocity has changed." << endl;
	  }
	  if(veltype_p != sources_p[i].veltype){
	    cout << "Warning: Source velocity type has changed." << endl;
	  }
	  if(selfcal_p != sources_p[i].selfcalabratable){
	    cout << "Warning: Source selfcalibration state has changed."
		 << endl;
	  }
	  if(astroHdrFile_p != sources_p[i].file){
	    throw CARMA_EXCEPTION(
		      observertools::ProjectDatabaseException,
		      "Source astroheader file has changed, only 1 can be specified.");
	  }
	  bool corFound = false;
	  for(uint j = 0; (j < sources_p[i].correlatorSetup.size()) 
		&& (!corFound); j++){
	    corFound = (currentCorrelatorSetup_p
			== sources_p[i].correlatorSetup[j]);
	  }
	  if(!corFound){
	    sources_p[i].correlatorSetup
	      .push_back(currentCorrelatorSetup_p);
	  }
	}
      };
      // If match not found then add new source entry, else
      // increment cumulative observing time of matching entry.
      if (!found) {
	ProjectSourceData src;
	src.sourceName = source_p;
	src.ra = ra_p;
	src.dec = dec_p;
	src.file = astroHdrFile_p;
	src.vsource = vsource_p;
	src.veltype = veltype_p;
	src.selfcalabratable = selfcal_p;
	src.observationLength = inttime_p;
	src.correlatorSetup.push_back(currentCorrelatorSetup_p);
	sources_p.push_back(src);
      } else {
	sources_p[i - 1].observationLength += inttime_p;
      };
    };

    return;
  };

  //==========================================================================

  void TrialProjectData::processCalibratorInfo()
  {
    // Process calibrator information from the current integration.

    // Check validity of the required elements
    if (valid("SOURCE PURPOSE RA DEC INTTIME") && purpose_p.length() != 0 &&
	correlatorValid_p) {
      // Search for matching calibrator project data entry
      bool found = false;
      uint i;
      for (i=0; (i < calibrators_p.size()) && (!found); i++) {
	if((found = (source_p == calibrators_p[i].calibratorName))){
// merge the types
	  compareType(purpose_p, calibrators_p[i].type);
	  bool corFound = false;
	  for(uint j = 0; (j < calibrators_p[i].correlatorSetup.size()) 
		&& (!corFound); j++){
	    corFound = (currentCorrelatorSetup_p
			== calibrators_p[i].correlatorSetup[j]);
	  }
	  if(!corFound){
	    calibrators_p[i].correlatorSetup
	      .push_back(currentCorrelatorSetup_p);
	  }
	  if(astroHdrFile_p != calibrators_p[i].file){
	    throw CARMA_EXCEPTION(
		     observertools::ProjectDatabaseException,
		     "Calibrator astroheader file has changed, only 1 can be specified.");
	  }
	}
      };
      // If match not found then add new calibrator entry
      if (!found) {
	ProjectCalibratorData cal;
	cal.calibratorName = source_p;
	cal.type = purpose_p;
	cal.ra = ra_p;
	cal.dec = dec_p;
	cal.file = astroHdrFile_p;
	cal.observationLength = inttime_p;
	cal.correlatorSetup.push_back(currentCorrelatorSetup_p);
	calibrators_p.push_back(cal);
      } else {
	calibrators_p[i - 1].observationLength += inttime_p;
      };
    };
   return;
  };

  //==========================================================================

  void TrialProjectData::processTargetInfo()
  {
    // Process target information from the current integration.

    // Check validity of required elements
    if (valid("OBSLINE") && correlatorValid_p) {
      // Search for matching target entry
      bool found = false;
      for (uint i=0; (i < targets_p.size()) && (!found); i++) {
	found = (obsline_p == targets_p[i].transition);
      };
      // If match not found then add new target entry
      if (!found) {
	ProjectTargetData target;
	target.molecule = obsline_p;
	target.transition = obsline_p;
	targets_p.push_back(target);
      };
    };
    return;
  };

  //==========================================================================

  void TrialProjectData::processPointingOffsetInfo()
  {
    // Process pointing offset information from the current integration.

    // Initialization
    double pointingTol = 1e-6;

    // Check validity of required astro header elements
    if (valid("DRA DDEC") && correlatorValid_p) {
      // Search for a matching pointing offset entry
      bool found = false;
      for (uint i=0; (i < pointingOffsets_p.size()/2) && (!found); i++) {
	found = (rdiff(dra_p, pointingOffsets_p[2*i]) < pointingTol) &&
	  (rdiff(ddec_p, pointingOffsets_p[2*i+1]) < pointingTol);
      };
      // If match not found, then add new pointing offsets entry
      if (!found) {
	pointingOffsets_p.push_back(dra_p);
	pointingOffsets_p.push_back(ddec_p);
      };
    };
    return;
  };

  //==========================================================================

  void TrialProjectData::processAntennaInfo()
  {
    // Process antenna information from the current integration.

    // Check validity of the required elements
    if (valid("ANTENNAS") && correlatorValid_p) {
      int nant = 0;
      for (uint i=0; i < antennas_p.size(); i++) {
	if (antennas_p[i] > 0) {
	  nant = nant + 1;
	};
      };
      if (nant > numberOfAntennas_p) {
	numberOfAntennas_p = nant;
      };
    };
    return;
  };

  //==========================================================================

  void TrialProjectData::processSwitchingInfo()
  {
    // Process fast switching information from the current integration.

    // Check validity of the required elements
    if (valid("FSWITCH") && correlatorValid_p) {
      fastSwitch_p = std::max(fswitch_p, fastSwitch_p);
    };
    return;
  };

  //==========================================================================

  void TrialProjectData::processTimeInfo()
  {
    // Process time information from the current integration.

    // Check validity of required astro header elements
    if (valid("TIME LST INTTIME") && correlatorValid_p) {
	// the first lst entry will be the start of the track
      if(firstTime){
	  trialObservedLST_p[0] = lst_p;
	  firstTime = false;
      }
      // Observation date (MJD)
      bool found = false;
      for (uint i=0; (i < trialObservationDate_p.size()) && (!found); i++) {
	found = (trialObservationDate_p[i] == (time_p - 2400000.5));
      };
      if (!found) {
	trialObservationDate_p.push_back(time_p - 2400000.5);
      };

      // Observed LST
      // keep changing the end lst until we are finished with the track
      trialObservedLST_p[1] = lst_p;
    };
    return;
  };

  //==========================================================================

  bool TrialProjectData::valid(const std::string& names)
  {
    // Check collective validity of a list of element names.

    // Initialization
    bool retval = true;
    std::string::size_type indx = names.find_first_not_of(" ");
    std::string::size_type jndx = names.find_first_of(" ", indx);

    // Extract and test validity of each element name in the list.
    while (indx != std::string::npos) {
      std::string elem = (jndx == std::string::npos) ? names.substr(indx) : 
	names.substr(indx, jndx-indx);
      retval = (retval && astroHdrValidity_p[elem]);
      // Increment cursors
      indx = names.find_first_not_of(" ", jndx);
      jndx = names.find_first_of(" ", indx);
    };
    return retval;
  };

  //==========================================================================

  double TrialProjectData::rdiff(const double& a, const double& b)
  {
    // Compute difference ratio between two floating point numbers.

    double maxabs = std::max(std::abs(a), std::abs(b));
    return (maxabs == 0) ? 0 : std::abs((a-b)/maxabs);
  };

  //==========================================================================


  void TrialProjectData::compareType(const std::string& str1,
				     std::string& str2)
  {
    // Compare two calibrator type strings.

    // Capitalize, trim white-space, delete duplicate characters,
    // and sort in lexigraphic order
    std::string work1 = 
      carma::util::StringUtils::lowASCIIAlphaNumericToUpper(str1);
    work1 = carma::util::StringUtils::trimWhiteSpace(work1);
    work1 = carma::util::StringUtils::reallyUniq(work1);
    work1 = carma::util::StringUtils::sort(work1);

    str2 = carma::util::StringUtils::lowASCIIAlphaNumericToUpper(str2);
    str2 = carma::util::StringUtils::trimWhiteSpace(str2);
    str2 = carma::util::StringUtils::reallyUniq(str2);
    str2 = carma::util::StringUtils::sort(str2);

    if(work1 != str2){
	str2 += work1;
	str2 = carma::util::StringUtils::reallyUniq(str2);
    }
  };

  //==========================================================================
    bool TrialProjectData::updateProjectDatabase(){

      //if processIntegration never got called, it probably means no useful
      // data was found
      if(!processIntegrationCalled_)
	{ ostringstream os;
	   os << 
	     "PDB update not done because no valid project data was found.";
	   programLogErrorIfPossible( os.str() );
	   return false;
	}


	// This shouldn't happen any more because of the
	// processIntegrationCalled_ check, but just in case.
	if(trialObservationDate_p.size() == 0)
	{ostringstream os;
	 os << "PDB update not done because start & end dates were not found.";
	   programLogErrorIfPossible( os.str() );
	   return false;
	}

	{ostringstream os;
	 os << "Beginning update.";
	   programLogInfoIfPossible( os.str() );
	}

	std::string project = "";
	std::string obsblock = "";
	std::string subObsblock = "";
	CORBA::Short trial = 0;
	
	std::string::size_type start = 0;
	std::string::size_type end = 0;
	std::string::size_type last = 0;
	bool valid = true;
	last = obsBlockId_p.find_last_of(".");
	end = obsBlockId_p.find(".",end);
	if((end = obsBlockId_p.find(".",end)) != std::string::npos){
	    project = obsBlockId_p.substr(0,end);
	    start = end + 1;
	    if((end = obsBlockId_p.find(".",start)) != std::string::npos){
		obsblock = obsBlockId_p.substr(start,end-start);
		start = end + 1;
// we have no subObsblock name
		if(start >= last){
		    subObsblock = "";
		}
// we do have a subobsblock name
		else{
		    subObsblock = obsBlockId_p.substr(start,last-start);
		}
		trial = static_cast<short>
		    (atoi(obsBlockId_p.substr(last+1).c_str()));
		if(trial <= 0)valid = false;
	    }
	    else{
		valid = false;
	    }
	}
	else{
	    valid = false;
	}
	if(!valid){
	    cerr << "Invalid project format, project databse not updated" 
		 << endl;
	    return false;
	}

	ProjectDatabaseManager_var pdm;
// try to get a connection to the pdm a few times and sleep between tries
    for(int i = 0; i < 3; i++){
        programLogInfoIfPossible("resolveName to get pdm.");
        corba::Client & client = Program::getProgram().getCorbaClient();
        pdm = client.resolveName<ProjectDatabaseManager>(
            "carma.projectDatabaseManager.projectDatabaseManagerControl");
        if(!(CORBA::is_nil(pdm))){
            // jump out of the for loop if we have a connection
            break;
        } else if(i == 2) {
            // if we have tried 3 times then quit with an error
            programLogErrorIfPossible(
                   "System reference not found for ProjectDatabaseManager." );
        return false;
        } else {
            sleep(10); //WHY 10 HERE AND 3 IN OTHER LOOP??
        }
    }

	programLogInfoIfPossible("Have pdm");

	carma::observertools::ItemValueSequence ivs;
	CORBA::ULong len = 0;
// get pointing offsets
	ostringstream offsets;
	offsets << setiosflags(ios_base::dec | ios_base::showpoint)
		<< setprecision(6);
	for(size_t i = 0; i < pointingOffsets_p.size(); i++){
	    if(i != 0){
		offsets << "," << setprecision(6) << pointingOffsets_p[i];
	    }
	    else{
		offsets << setprecision(6) << pointingOffsets_p[i];
	    }
	}
	ivs.length(len + 6);
	ivs[len].name = "numberOfPointings";
	ostringstream numPnt;
	numPnt << static_cast<int>(pointingOffsets_p.size()/2);
	ivs[len].value = numPnt.str().c_str();
	ivs[len + 1].name = "pointingOffsets";
	ivs[len + 1].value = offsets.str().c_str();
// number of antennas
	ivs[len + 2].name = "numberOfAntennas";
	ostringstream ants;
	ants << numberOfAntennas_p;
	ivs[len + 2].value = ants.str().c_str();
// fast switching
	ivs[len + 3].name = "fastSwitch";
	if(fastSwitch_p > 0){
	    ivs[len + 3].value = "TRUE";
	}
	else{
	    ivs[len + 3].value = "FALSE";
	}
// observation date
	ivs[len + 4].name = "trialObservationDate";
	std::string startDate =
	    Time::getDateTimeString(trialObservationDate_p[0],0,"%C%y-%m-%dT");
	startDate.erase(11,1);
	std::string endDate =
	    Time::getDateTimeString(
		trialObservationDate_p[trialObservationDate_p.size() - 1],0,
		"%C%y-%m-%dT");
	endDate.erase(11,1);
	ostringstream dateStream;
	dateStream << startDate << "," << endDate;
	ivs[len + 4].value = dateStream.str().c_str();
// observed LST
	ivs[len + 5].name = "trialObservedLST";
	ostringstream lstStream;
	lstStream << setiosflags(ios_base::dec | ios_base::showpoint)
		  << setprecision(6) << trialObservedLST_p[0] << ","
		  << setprecision(6) << trialObservedLST_p[1];
	ivs[len + 5].value = lstStream.str().c_str();
	len += 6;
// correltor data
	map<int, ProjectCorrelatorSetup>::iterator iter;   
	for(iter = correlatorSetup_p.begin(); iter != correlatorSetup_p.end();
	    iter++ ) {
	    uint windowLen = iter->second.windowNumber.size();
	    ivs.length(len + (4 * windowLen) + 1);
	    ostringstream corrStream;
	    corrStream << iter->second.setupNumber;
	    ivs[len].name = "correlator";
	    ivs[len].value = corrStream.str().c_str();
// do the window stuff next
	    for(uint i = 0; i < windowLen; i++){
		ivs[len + (i * 4) + 1].name = "window";
		ostringstream tempStream;
		tempStream << iter->second.windowNumber[i];
		ivs[len + (i * 4) + 1].value = tempStream.str().c_str();
		tempStream.str("");
		ivs[len + (i * 4) + 2].name = "numberOfChannels";
		tempStream << iter->second.numberOfChannels[i];
		ivs[len + (i * 4) + 2].value = tempStream.str().c_str();
		tempStream.str("");
		tempStream << setiosflags(ios_base::dec | ios_base::showpoint)
			   << setprecision(8);
		ivs[len + (i * 4) + 3].name = "resolution";
		tempStream << (iter->second.resolution[i] * 1000.0);
		ivs[len + (i * 4) + 3].value = tempStream.str().c_str();
		tempStream.str("");
		tempStream << setiosflags(ios_base::dec | ios_base::showpoint)
			   << setprecision(8);
		ivs[len + (i * 4) + 4].name = "minFreq";
		tempStream << iter->second.minfreq[i];
		ivs[len + (i * 4) + 4].value = tempStream.str().c_str();
	    }
	    len += (4 * windowLen) + 1;
	}
// source data
//QUALITY
	ostringstream qualityStream;
//	qualityStream.open("/tmp/tmp.quality",ios::app);
	qualityStream << "quality proj=" << obsBlockId_p << ".mir sources=";
// END QUALITY
	for(size_t i = 0; i < sources_p.size(); i++){
	    ostringstream tempStream;
	    ivs.length(len + 9);
	    ivs[len].name = "source";
	    ivs[len].value = sources_p[i].sourceName.c_str();
// QUALITY
	    if(i != 0)
		qualityStream << ",";
	    qualityStream << sources_p[i].sourceName;
// END QUALITY
	    ivs[len + 3].name = "srcFile";
	    ivs[len + 3].value = sources_p[i].file.c_str();
	    ivs[len + 4].name = "velocity";
	    tempStream << sources_p[i].vsource;
	    ivs[len + 4].value = tempStream.str().c_str();
	    tempStream.str("");
	    ivs[len + 5].name = "veltype";
	    ivs[len + 5].value = sources_p[i].veltype.c_str();
	    ivs[len + 6].name = "selfcalibratable";
	    if(sources_p[i].selfcalabratable > 0){
		ivs[len + 6].value = "TRUE";
	    }
	    else{
		ivs[len + 6].value = "FALSE";
	    }
	    tempStream.str("");
	    ivs[len + 8].name = "srcCorrelatorSetup";
	    tempStream << sources_p[i].correlatorSetup[0];
	    for(uint j = 1; j < sources_p[i].correlatorSetup.size(); j++){
		tempStream << "," << sources_p[i].correlatorSetup[j];
	    }
	    ivs[len + 8].value = tempStream.str().c_str();
	    tempStream.str("");
	    ivs[len + 7].name = "srcObservationLength";
	    tempStream << setiosflags(ios_base::dec | ios_base::showpoint)
		       << setprecision(6) 
		       << (sources_p[i].observationLength / 3600.0);
	    ivs[len + 7].value = tempStream.str().c_str();
	    tempStream.str("");
	    ivs[len + 1].name = "srcRA";
	    tempStream << setiosflags(ios_base::dec | ios_base::showpoint)
		       << setprecision(10) << sources_p[i].ra;
	    ivs[len + 1].value = tempStream.str().c_str();
	    tempStream.str("");
	    ivs[len + 2].name = "srcDEC";
	    tempStream << setiosflags(ios_base::dec | ios_base::showpoint)
		       << setprecision(10) << sources_p[i].dec;
	    ivs[len + 2].value = tempStream.str().c_str();
	    len = len + 9;
	}
// calibrator data
// QUALITY
	vector<std::string> gaincals;
	vector<std::string> passcals;
	vector<std::string> fluxcals;
// END QUALITY
	for(size_t i = 0; i < calibrators_p.size(); i++){
	    ivs.length(len + 7);
	    ivs[len].name = "calibrator";
	    ivs[len].value = calibrators_p[i].calibratorName.c_str();
	    ivs[len + 1].name = "type";
	    ivs[len + 1].value = calibrators_p[i].type.c_str();
//QUALITY
	    if(calibrators_p[i].calibratorName != "NOISE" &&
	       calibrators_p[i].calibratorName != "noise"){
		if(calibrators_p[i].type.find("F") != std::string::npos ||
		   calibrators_p[i].type.find("f") != std::string::npos)
		    fluxcals.push_back(calibrators_p[i].calibratorName);
		if(calibrators_p[i].type.find("G") != std::string::npos ||
		   calibrators_p[i].type.find("g") != std::string::npos)
		    gaincals.push_back(calibrators_p[i].calibratorName);
		if(calibrators_p[i].type.find("B") != std::string::npos ||
		   calibrators_p[i].type.find("b") != std::string::npos)
		    passcals.push_back(calibrators_p[i].calibratorName);
	    }
// END QUALTIY
	    ivs[len + 2].name = "calFile";
	    ivs[len + 2].value = calibrators_p[i].file.c_str();
	    ivs[len + 3].name = "calCorrelatorSetup";
	    ostringstream tempStream;
	    tempStream << calibrators_p[i].correlatorSetup[0];
	    for(uint j = 1; j < calibrators_p[i].correlatorSetup.size(); j++){
		tempStream << "," << calibrators_p[i].correlatorSetup[j];
	    }
	    ivs[len + 3].value = tempStream.str().c_str();
	    tempStream.str("");
	    ivs[len + 6].name = "calObservationLength";
	    tempStream << setiosflags(ios_base::dec | ios_base::showpoint)
		       << setprecision(6)
		       << (calibrators_p[i].observationLength / 3600.0);
	    ivs[len + 6].value = tempStream.str().c_str();
	    ivs[len + 4].name = "calRA";
	    tempStream.str("");
	    tempStream << setiosflags(ios_base::dec | ios_base::showpoint)
		       << setprecision(10) << calibrators_p[i].ra;
	    ivs[len + 4].value = tempStream.str().c_str();
	    tempStream.str("");
	    ivs[len + 5].name = "calDEC";
	    tempStream << setiosflags(ios_base::dec | ios_base::showpoint)
		       << setprecision(10) << calibrators_p[i].dec;
	    ivs[len + 5].value = tempStream.str().c_str();
	    len = len + 7;
	}
// QUALITY
	qualityStream << " gaincals=";
	bool first = true;
	for(size_t i = 0; i < gaincals.size(); i++){
	    if(gaincals[i] != "URANUS" && gaincals[i] != "NEPTUNE"
	       && gaincals[i] != "MARS"){
		if(!first)
		    qualityStream << ",";
		first = false;
		qualityStream << gaincals[i];
	    }
	}
	qualityStream << " passcals=";
	for(size_t i = 0; i < passcals.size(); i++){
	    if(i != 0)
		qualityStream << ",";
	    qualityStream << passcals[i];
	}
	qualityStream << " fluxcal=";
	if(fluxcals.size() > 0)
	    qualityStream << fluxcals[0];
	qualityStream << " see=n db=y imr=ncsa-4-usernet240-224";
//	system(qualityStream.str().c_str());
//	qualityStream.close();
// END QUALITY

	{ostringstream os;
	 os << "Calling pdm::projectEdit.";
	 programLogInfoIfPossible( os.str() );
	}
// don't update the pdb for rpnt
	if(project == "rpnt")
	    return true;
#if 0
	bool success;
    pdm->projectEditInOut(project.c_str(),obsblock.c_str(),
                     subObsblock.c_str(),trial,ivs,ESTATUS_REPLACE,success);
#else
	// Ask the project database manager to update the database. If the
	// PDM isn't available, try multiple times in case it's starting up.
	bool success=false;
	int sleeptime=0;

	static const int MAXSLEEPTIME=300;
	static const int SLEEPINTERVAL=15;

	while(1) {
	  string errMsg = "Problem with project database: ";
	  try {
          pdm->projectEditInOut(project.c_str(),obsblock.c_str(),
                           subObsblock.c_str(),trial,ivs,ESTATUS_REPLACE,success);
	    break;
	    // stife all exceptions, log and continue
	  } catch ( const carma::util::ErrorException& exc) {
	    errMsg += exc.getErrorMessage();
	    errMsg += " ErrorException";
	  } catch ( const carma::observertools::ProjectDatabaseException& exc) {
	    errMsg += exc.errorMsg;
	    errMsg += " PDBException";
	  } catch ( const CORBA::SystemException& exc) {
	    errMsg += exc._name();
	    errMsg += " SystemException";
	  } catch ( const carma::util::UserException& exc) {
	    errMsg += exc.errorMsg;
	    errMsg += " UserException";
	  } catch (const std::exception& exc) {
	    errMsg += exc.what();
	    errMsg += " StdException";
	  } catch (...) {
	    errMsg += "Unidentified exception";
	  };

	  if(sleeptime < MAXSLEEPTIME) {
	    errMsg += " - Waiting";
	    programLogErrorIfPossible( errMsg );
	    sleep(SLEEPINTERVAL);	// Wait for problem to go away.
	    sleeptime += SLEEPINTERVAL;
	  }
	  else{
	      ostringstream oss;
	      oss << "Could not update pdb: " << errMsg;
	      SDPUtil::mailRTS(oss.str());
	      break;
	  }
	};
#endif

	return success;
    }
  //==========================================================================
  void TrialProjectData::addScriptToMiriad(std::string miriadFile){
    std::string project = "";
    std::string obsblock = "";
    std::string subObsblock = "";
    std::string trial = "";

    std::string::size_type start = 0;
    std::string::size_type next = 0;
    std::string::size_type last = 0;
    last = miriadFile.find_last_of(".mir");
    next = miriadFile.find_last_of("/");
    std::string mFile = miriadFile.substr(next+1,last-next-5);
    start = mFile.find(".");
    project = mFile.substr(0,start);
    if(project.find("fringe") == string::npos ||
       project.find("flux") == string::npos ||
       project.find("base") == string::npos ||
       project.find("rpnt") == string::npos ||
       project.find("opnt") == string::npos ||
       project.find("tilt") == string::npos ||
       project.find("test") == string::npos ||
       project.find("bandpass") == string::npos ||
       project.find("blank") == string::npos ||
       project.find("vlbi") == string::npos)
      return;
    next = mFile.find(".",start+1);
    obsblock = mFile.substr(start+1,next-start-1);
    last = mFile.find_last_of(".");
    trial = mFile.substr(last+1);
    CARMA_CPTRACE(carma::util::Trace::TRACE4,"HERE" << project << " " << obsblock << " " << subObsblock << " " << trial << " " << miriadFile << " " << mFile);
    if(next != last)
      subObsblock = mFile.substr(next+1,last-next-1);

    ProjectDatabaseManager_var pdm;
// try to get a connection to the pdm a few times and sleep between tries
    for(int i = 0; i < 3; i++){
        programLogInfoIfPossible("resolveName to get pdm.");
        corba::Client & client = Program::getProgram().getCorbaClient();
        pdm = client.resolveName<ProjectDatabaseManager>(
          "carma.projectDatabaseManager.projectDatabaseManagerControl");
        if(!(CORBA::is_nil(pdm))){
            // jump out of the for loop if we have a connection
            break;
        } else if(i == 2) {
            // if we have tried 3 times then quit with an error
            const string err("System reference not found for ProjectDatabaseManager." );
            programLogErrorIfPossible( err );
            CARMA_CPTRACE(carma::util::Trace::TRACE4, err );
            return;
        } else {
            sleep(3);
        }
    }

    programLogInfoIfPossible("Have pdm");
    CARMA_CPTRACE(carma::util::Trace::TRACE4,"HAVE PDM");
    carma::observertools::ItemValueSequence ivs;
// get pointing offsets
    ivs.length(4);
    ivs[0].name = "project";
    ivs[0].value = project.c_str();
    ivs[1].name = "obsblock";
    ivs[1].value = obsblock.c_str();
    ivs[2].name = "subObsblock";
    ivs[2].value = subObsblock.c_str();
    ivs[3].name = "trial";
    ivs[3].value = trial.c_str();
    CARMA_CPTRACE(carma::util::Trace::TRACE4,"HERE" << project << " " << obsblock << " " << subObsblock << " " << trial);
    ProjectSequence* ps;
    try{
      ps = pdm->projectQuery(ivs);
    }
    catch(...){
      CARMA_CPTRACE(carma::util::Trace::TRACE4,"EXCEPTION");
      return;
    }
    CARMA_CPTRACE(carma::util::Trace::TRACE4,"LEN "<< (*ps).length());
    if((*ps).length() != 1){
    CARMA_CPTRACE(carma::util::Trace::TRACE4,"HERE1");
      return;
    }
    else if((*ps)[0].obsblock.length() != 1){
    CARMA_CPTRACE(carma::util::Trace::TRACE4,"HERE2");
      return;
    }
    else if((*ps)[0].obsblock[0].subObsblock.length() != 1){
    CARMA_CPTRACE(carma::util::Trace::TRACE4,"HERE3");
      return;
    }
    else if((*ps)[0].obsblock[0].subObsblock[0].trial.length() != 1){
     CARMA_CPTRACE(carma::util::Trace::TRACE4,"HERE4");
     return;
    }
    CARMA_CPTRACE(carma::util::Trace::TRACE4,"HERE5");

    std::string script( (*ps)[0].obsblock[0].subObsblock[0].trial[
			(*ps)[0].obsblock[0]
		.subObsblock[0].trial.length() - 1].script.in());
    if(strcmp(script.c_str(),"NODE_NOT_FOUND") == 0 || strcmp(script.c_str(),"") == 0){
      CARMA_CPTRACE(carma::util::Trace::TRACE4,"NO SCRIPT");
      return;
    }
    CARMA_CPTRACE(carma::util::Trace::TRACE4,"Writing " << miriadFile << "/observingScript.obs");
    ofstream outFile;
    std::string fileName = miriadFile + "/observingScript.obs";
    outFile.open(fileName.c_str());
    outFile << script;
    outFile.close();
    CARMA_CPTRACE(carma::util::Trace::TRACE4,"WRITTEN");
  }
};
};
