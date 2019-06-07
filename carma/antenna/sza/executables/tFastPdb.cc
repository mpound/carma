#include <iostream>
#include <iomanip>
#include <fstream>
#include <map>

#include <time.h>

#include "carma/szautil/Program.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/String.h"
#include "carma/szautil/Sort.h"

using namespace std;
using namespace carma::util;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "file",      "",  "s", USAGE "File to source for the database"},
  { "src",       "",  "s", USAGE "Source to match"},
  { "list",      "t", "b", USAGE "True to list projects"},
  { "matchsrc",  "f", "b", USAGE "True to match source"},
  { "matchfreq", "f", "b", USAGE "True to match frequency"},
  { "freq",      "35","d", USAGE "Frequency to match"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

enum EmlSrcType {
  SRC_UNKNOWN,
  SRC_SRC,
  SRC_CAL
};

struct EmlFreq {
  double minFreq_;
  double maxFreq_;

  EmlFreq() {}

  EmlFreq(const EmlFreq& proj) {
    *this = proj;
  }

  EmlFreq(EmlFreq& proj) {
    *this = proj;
  }

  void operator=(const EmlFreq& proj) {
    *this = (EmlFreq&)proj;
  }

  void operator=(EmlFreq& proj) {
    minFreq_ = proj.minFreq_;
    maxFreq_ = proj.maxFreq_;
  };
};

struct EmlSrc {
  std::string name_;
  std::vector<EmlSrcType> types_;
  EmlSrcType type_;

  static string typeString(EmlSrcType type) {
    switch (type) {
    case SRC_SRC:
      return "S";
      break;
    case SRC_CAL:
      return "C";
      break;
    default:
      return "U";
      break;
    }
  }

  EmlSrc() {
    types_.resize(0);
  }

  EmlSrc(const EmlSrc& proj) {
    *this = proj;
  }

  EmlSrc(EmlSrc& proj) {
    *this = proj;
  }

  //------------------------------------------------------------
  // Add the type if it doesn't already exist
  //------------------------------------------------------------

  void addType(EmlSrcType type) 
  {
    for(unsigned i=0; i < types_.size(); i++) {
      if(type == types_[i])
	return;
    }

    types_.push_back(type);
  }

  void operator=(const EmlSrc& proj) {
    *this = (EmlSrc&)proj;
  }

  void operator=(EmlSrc& proj) {
    name_    = proj.name_;
    types_   = proj.types_;
    type_    = proj.type_;
  };
};

struct EmlTrial {
  unsigned trialNo_;
  std::string startDate_;
  std::string stopDate_;
  std::vector<EmlSrc> sources_;
  std::vector<EmlFreq> freqs_;
  std::map<std::string, EmlSrc*> srcMap_;

  void initialize() {
    sources_.resize(0);
    freqs_.resize(0);
    srcMap_.clear();
  }

  EmlTrial() 
  {
    sources_.resize(0);
    freqs_.resize(0);
  }

  EmlTrial(const EmlTrial& proj) {
    *this = proj;
  }

  EmlTrial(EmlTrial& proj) {
    *this = proj;
  }

  //------------------------------------------------------------
  // Add a source to this trial
  //------------------------------------------------------------

  void addSource(EmlSrc& src) {

    // First see if this source already exists

    if(srcMap_.find(src.name_) == srcMap_.end()) {
      sources_.push_back(src);

      EmlSrc* srcPtr = &sources_[sources_.size()-1];
      srcMap_[src.name_] = srcPtr;
      srcPtr->addType(src.type_);
      srcMap_.clear();

      for(unsigned iSrc=0; iSrc < sources_.size(); iSrc++) {
	EmlSrc* src = &sources_[iSrc];
	srcMap_[src->name_] = src;
      }
    } else {
      EmlSrc* srcPtr = srcMap_[src.name_];
      srcPtr->addType(src.type_);
    }
  }

  void addFreq(EmlFreq& freq) {

    // First see if this freq already exists

    for(unsigned iFreq=0; iFreq < freqs_.size(); iFreq++) {
      EmlFreq& currFreq = freqs_[iFreq];
      if(fabs(freq.minFreq_ - currFreq.minFreq_) < 1e-12 && 
	 fabs(freq.maxFreq_ - currFreq.maxFreq_) < 1e-12) {
	return;
      }
    }

    freqs_.push_back(freq);
  }

  void operator=(const EmlTrial& proj) {
    *this = (EmlTrial&)proj;
  }

  void operator=(EmlTrial& proj) {
    trialNo_   = proj.trialNo_;
    startDate_ = proj.startDate_;
    stopDate_  = proj.stopDate_;
    sources_   = proj.sources_;
    freqs_     = proj.freqs_;
  };
};

struct EmlSubObsblock {
  std::string name_;
  std::vector<EmlTrial> trials_;

  EmlSubObsblock() 
  {
    trials_.resize(0);
  }

  EmlSubObsblock(const EmlSubObsblock& proj) {
    *this = proj;
  }

  EmlSubObsblock(EmlSubObsblock& proj) {
    *this = proj;
  }

  void operator=(const EmlSubObsblock& proj) {
    *this = (EmlSubObsblock&)proj;
  }

  void operator=(EmlSubObsblock& proj) {
    name_   = proj.name_;
    trials_ = proj.trials_;
  };
};

struct EmlObsblock {
  std::string name_;
  std::vector<EmlSubObsblock> subObsblocks_;
  std::map<std::string, EmlSubObsblock*> subObsblockMap_;

  EmlObsblock() 
  {
    subObsblocks_.resize(0);
  }

  EmlObsblock(const EmlObsblock& proj) {
    *this = proj;
  }

  EmlObsblock(EmlObsblock& proj) {
    *this = proj;
  }

  void operator=(const EmlObsblock& proj) {
    *this = (EmlObsblock&)proj;
  }

  void operator=(EmlObsblock& proj) {
    name_         = proj.name_;
    subObsblocks_ = proj.subObsblocks_;
  };
};

struct EmlProject {
  std::string name_;
  std::vector<EmlObsblock> obsblocks_;
  std::map<std::string, EmlObsblock*> obsblockMap_;

  EmlProject() {}

  EmlProject(const EmlProject& proj) {
    *this = proj;
  }

  EmlProject(EmlProject& proj) {
    *this = proj;
  }

  void operator=(const EmlProject& proj) {
    *this = (EmlProject&)proj;
  }

  void operator=(EmlProject& proj) {
    name_        = proj.name_;
    obsblocks_   = proj.obsblocks_;
  };
};

std::vector<EmlProject> createDatabase(std::string file)
{
  std::ifstream fin;
  fin.open(file.c_str(), ios::in);

  if(!fin) {
    ThrowError("Unable to open file: " << file);
  }

  String line;

  std::vector<EmlProject> projects;
  std::map<std::string, EmlProject*> projectMap;

  try {
    //------------------------------------------------------------                                     
    // Iterate through the file                                                                        
    //------------------------------------------------------------                                     

    EmlProject tmpProj;
    EmlProject* proj = 0;

    // Pointer to the current obsblock and sub obsblock

    EmlObsblock* obsblock = 0;
    EmlSubObsblock* subblock = 0;
    
    // Pointer to the current trial
    
    EmlTrial tmpTrial;

    while(!fin.eof()) {
      line.initialize();
      getline(fin, line.str());

      line.advanceToNextNonWhitespaceChar();

      if(line.contains("PROJ")) {
	EmlProject tmpProj;
	String projTrialName = line.findNextInstanceOf("PROJ ", true, " ", true);
	projTrialName.strip(" ");

	line.advanceToNextNonWhitespaceChar();
	String startDate = line.findNextString();

	line.advanceToNextNonWhitespaceChar();
	String stopDate = line.findNextString();

	String projName     = projTrialName.findNextInstanceOf("", false, ".", true, true);
	String obsblockName = projTrialName.findNextInstanceOf("", false, ".", true, true);

	String subObsblockName("");
	if(projTrialName.remainder().contains(".")) {
	  subObsblockName = projTrialName.findNextInstanceOf("", false, ".", true, true);
	}

	unsigned trialNo    = projTrialName.remainder().toInt();

	tmpProj.name_ = projName.str();

	//------------------------------------------------------------
	// If we are encountering a new trial, add the current trial
	// to the last subobsblock.  If subblock == 0, this means that
	// this is the first trial that we've encountered, and we will
	// add it to the current subblock when we encounter the next
	// one
	//------------------------------------------------------------

	if(subblock != 0) {
	  subblock->trials_.push_back(tmpTrial);
	}

	//------------------------------------------------------------
	// Now check if this project has already been encountered.  If
	// not, create a new project, else return the ref to the
	// existing project
	//------------------------------------------------------------

	if(projectMap.find(projName.str()) == projectMap.end()) {

	  // Push back the new project

	  projects.push_back(tmpProj);

	  // And rebuild the map, since our pointers could now be
	  // invalid
	  
	  projectMap.clear();
	  for(unsigned iProj=0; iProj < projects.size(); iProj++) {
	    EmlProject* pr = &projects[iProj];
	    projectMap[projName.str()] = pr;
	  }
	}

	// Finally, look up the pointer to the current project

	proj = projectMap[projName.str()];
	
	//------------------------------------------------------------
	// Check if this obsblock has been encountered
	//------------------------------------------------------------

	EmlObsblock tmpObsblock;
	tmpObsblock.name_ = obsblockName.str();

	///------------------------------------------------------------
	// Now check if this obsblock has already been created
	///------------------------------------------------------------

	if(proj->obsblockMap_.find(obsblockName.str()) == proj->obsblockMap_.end()) {

	  // Push back the new obsblock

	  proj->obsblocks_.push_back(tmpObsblock);

	  // Now rebuild the map of obsblocks, since our pointer may
	  // have been invalidated

	  proj->obsblockMap_.clear();
	  for(unsigned iObs=0; iObs < proj->obsblocks_.size(); iObs++) {
	    EmlObsblock* block = &proj->obsblocks_[iObs];
	    proj->obsblockMap_[block->name_] = block;
	  }
	}

	// Finally, look up the pointer to the current obsblock

	obsblock = proj->obsblockMap_[obsblockName.str()];

	//------------------------------------------------------------
	// Now check if this subobsblock has already been created
	//------------------------------------------------------------

	EmlSubObsblock tmpSubObsblock;
	tmpSubObsblock.name_ = subObsblockName.str();

	if(obsblock->subObsblockMap_.find(subObsblockName.str()) == obsblock->subObsblockMap_.end()) {

	  // Push back the new subobsblock

	  obsblock->subObsblocks_.push_back(tmpSubObsblock);

	  // Now rebuild the map of subObsblocks, since our pointer may
	  // have been invalidated

	  obsblock->subObsblockMap_.clear();
	  for(unsigned iSub=0; iSub < obsblock->subObsblocks_.size(); iSub++) {
	    EmlSubObsblock* sblock = &obsblock->subObsblocks_[iSub];
	    obsblock->subObsblockMap_[sblock->name_] = sblock;
	  }
	}

	// Finally, look up the pointer to the current obsblock

	subblock = obsblock->subObsblockMap_[subObsblockName.str()];

	tmpTrial.initialize();
	tmpTrial.trialNo_   = trialNo;
	tmpTrial.startDate_ = startDate.str();
	tmpTrial.stopDate_  = stopDate.str();

      } else if(line.contains("SRC")) {

	EmlSrc src;
	EmlFreq freq;

	String srcCode = line.findNextInstanceOf("SRC ", true, " ", true);
	srcCode.strip(" ");

	if(srcCode == "C") {
	  src.type_ = SRC_CAL;
	} else if(srcCode == "S") {
	  src.type_ = SRC_SRC;
	} else {
	  src.type_ = SRC_UNKNOWN;
	}

	line.advanceToNextNonWhitespaceChar();
	String srcName = line.findNextString();
	srcName.strip(" ");

	src.name_ = srcName.str();

	line.advanceToNextNonWhitespaceChar();
	String minFreq = line.findNextInstanceOf("", false, "-", true, true);

	line.advanceToNextNonWhitespaceChar();
	String maxFreq = line.findNextString();

	tmpTrial.addSource(src);

	if(!(minFreq.contains("??") || maxFreq.contains("??"))) {
	  freq.minFreq_ = minFreq.toDouble();
	  freq.maxFreq_ = maxFreq.toDouble();
	  tmpTrial.addFreq(freq);
	}
      }
    }
  } catch(...) {
    COUT("Caught an error");
    return projects;
  }

  return projects;
}

void listDatabase(std::vector<EmlProject>& projects)
{
  std::ostringstream os;

  for(unsigned iProj=0; iProj < projects.size(); iProj++) {
    EmlProject& proj = projects[iProj];
    COUTCOLOR(proj.name_, "green");

    for(unsigned iObs=0; iObs < proj.obsblocks_.size(); iObs++) {
      EmlObsblock& obsblock = proj.obsblocks_[iObs];

      for(unsigned iSub=0; iSub < obsblock.subObsblocks_.size(); iSub++) {
	EmlSubObsblock& subblock = obsblock.subObsblocks_[iSub];

	for(unsigned iTrial=0; iTrial < subblock.trials_.size(); iTrial++) {
	  EmlTrial& trial = subblock.trials_[iTrial];

	  os.str("");

	  if(subblock.name_.size() == 0) {
	    os << proj.name_ << "." << obsblock.name_ << "." << trial.trialNo_;
	  } else {
	    os << proj.name_ << "." << obsblock.name_ << "." << subblock.name_ << "." << trial.trialNo_;
	  }
	  
	  std::ostringstream osFreq;
	  if(trial.freqs_.size() > 0) {
	    for(unsigned iFreq=0; iFreq < trial.freqs_.size(); iFreq++) {
	      EmlFreq& freq = trial.freqs_[iFreq];
	      if(iFreq > 0)
		osFreq << ", ";
	      osFreq << setw(6) << setprecision(2) << std::fixed << std::right << freq.minFreq_ << " - "
		     << setw(6) << setprecision(2) << std::fixed << std::right << freq.maxFreq_;
	    }
	  } else {
	    osFreq << setw(6) << setprecision(2) << std::fixed << std::right << "??" << " - "
		   << setw(6) << setprecision(2) << std::fixed << std::right << "??";
	  }
	  
	  COUTCOLORNNL("     "  << std::setw(30) << std::left << os.str() 
		       << setw(19) << std::left << trial.startDate_ << " - " 
		       << setw(19) << std::left << trial.stopDate_  << " ", "cyan");
	  COUTCOLOR(osFreq.str(), "magenta");
	  
	  for(unsigned iSrc=0; iSrc < trial.sources_.size(); iSrc++) {
	    EmlSrc& src = trial.sources_[iSrc];
	    os.str("");
	    os << setw(18) << std::left << src.name_;
	    for(unsigned iType=0; iType < src.types_.size(); iType++) {
	      os << " " << EmlSrc::typeString(src.types_[iType]);
	    }
	    COUTCOLOR("          " << os.str(), "yellow");
	  }
	}
      }
    }
  }
}

void listTrialsMatchingSource(std::vector<EmlProject>& projects, std::string source, bool doFreq=false, double freq=0.0);

void listTrialsMatchingSource(std::vector<EmlProject>& projects, std::string source, bool doFreq, double freq)
{
  std::vector<std::string> trials;
  std::vector<std::string> sortDates;
  std::vector<std::string> startDates;
  std::vector<std::string> stopDates;
  std::ostringstream osTrial;

  for(unsigned iProj=0; iProj < projects.size(); iProj++) {
    EmlProject& proj = projects[iProj];
    
    for(unsigned iObs=0; iObs < proj.obsblocks_.size(); iObs++) {
      EmlObsblock& obsblock = proj.obsblocks_[iObs];

      for(unsigned iSub=0; iSub < obsblock.subObsblocks_.size(); iSub++) {
	EmlSubObsblock& subblock = obsblock.subObsblocks_[iSub];

	for(unsigned iTrial=0; iTrial < subblock.trials_.size(); iTrial++) {
	  EmlTrial& trial = subblock.trials_[iTrial];
	  osTrial.str("");

	  if(subblock.name_.size() == 0) {
	    osTrial << proj.name_ << "." << obsblock.name_ << "." << trial.trialNo_;
	  } else {
	    osTrial << proj.name_ << "." << obsblock.name_ << "." << subblock.name_ << "." << trial.trialNo_;
	  }
	  
	  //------------------------------------------------------------
	  // If matching frequencies, do it now
	  //------------------------------------------------------------

	  bool freqMatch = true;
	  if(doFreq) {
	    freqMatch = false;
	    for(unsigned iFreq=0; iFreq < trial.freqs_.size(); iFreq++) {
	      EmlFreq& freqRef = trial.freqs_[iFreq];
	      if(freqRef.minFreq_ < freq && freqRef.maxFreq_ > freq) {
		freqMatch = true;
		break;
	      }
	    }
	  }

	  //------------------------------------------------------------
	  // Finally, match sources
	  //------------------------------------------------------------

	  if(freqMatch) {
	    for(unsigned iSrc=0; iSrc < trial.sources_.size(); iSrc++) {
	      EmlSrc& src = trial.sources_[iSrc];
	      String sourceStr(src.name_);

	      if(sourceStr.contains(source)) {

		trials.push_back(osTrial.str());

		String trialStr(osTrial.str());
		std::ostringstream osStart;

		osStart << trial.startDate_;
		if(trialStr.contains(".SL.")) {
		  osStart << "S";
		} else if(trialStr.contains(".WB.")) {
		  osStart << "W";
		}

		sortDates.push_back(osStart.str());
		startDates.push_back(trial.startDate_);
		stopDates.push_back(trial.stopDate_);
	      }
	    }
	  }

	}
      }
    }
  }

  std::map<std::string, unsigned> trialMap;
  for(unsigned i=0; i < trials.size(); i++) {
    trialMap[sortDates[i]] = i;
  }
  
  // Now sort the list by dates

  std::vector<std::string> sortedDates = Sort::sort(sortDates);
  
  for(unsigned i=0; i < trials.size(); i++) {
    unsigned index = trialMap[sortedDates[i]];
    COUT(setw(30) << std::left << trials[index] << " " << startDates[index] << " - " << stopDates[index]);
  }
}

int Program::main(void)
{
  std::string file = Program::getParameter("file");
  std::vector<EmlProject> projects = createDatabase(file);

  if(Program::getbParameter("list")) {
    if(Program::getbParameter("matchsrc")) {
      listTrialsMatchingSource(projects, Program::getStringParameter("src"), Program::getBoolParameter("matchfreq"), Program::getDoubleParameter("freq"));
    } else {
      listDatabase(projects);
    }
  }

  return 0;
}
