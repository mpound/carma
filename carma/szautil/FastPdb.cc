#include "carma/szautil/Exception.h"
#include "carma/szautil/FastPdb.h"
#include "carma/szautil/Sort.h"
#include "carma/szautil/String.h"

#include <math.h>

#include <fstream>
#include <iomanip>

using namespace std;

using namespace sza::util;

//============================================================
// Methods of EmlFreq
//============================================================

FastPdb::EmlFreq::EmlFreq() {}

FastPdb::EmlFreq::EmlFreq(const EmlFreq& proj) {
  *this = proj;
}

FastPdb::EmlFreq::EmlFreq(EmlFreq& proj) {
  *this = proj;
}

void FastPdb::EmlFreq::operator=(const EmlFreq& proj) {
  *this = (EmlFreq&)proj;
}

void FastPdb::EmlFreq::operator=(EmlFreq& proj) {
  minFreq_ = proj.minFreq_;
  maxFreq_ = proj.maxFreq_;
};

//============================================================
// Methods of EmlSrc
//============================================================

std::string FastPdb::EmlSrc::typeString(EmlSrcType type) 
{
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

FastPdb::EmlSrc::EmlSrc() {
  types_.resize(0);
}

FastPdb::EmlSrc::EmlSrc(const EmlSrc& proj) {
  *this = proj;
}

FastPdb::EmlSrc::EmlSrc(EmlSrc& proj) {
  *this = proj;
}

//------------------------------------------------------------
// Add the type if it doesn't already exist
//------------------------------------------------------------

void FastPdb::EmlSrc::addType(EmlSrcType type) 
{
  for(unsigned i=0; i < types_.size(); i++) {
    if(type == types_[i])
      return;
  }

  types_.push_back(type);
}

void FastPdb::EmlSrc::operator=(const EmlSrc& proj) {
  *this = (EmlSrc&)proj;
}

void FastPdb::EmlSrc::operator=(EmlSrc& proj) {
  name_    = proj.name_;
  types_   = proj.types_;
  type_    = proj.type_;
};

//============================================================
// Methods of EmlTrial
//============================================================

void FastPdb::EmlTrial::initialize() {
  sources_.resize(0);
  freqs_.resize(0);
  srcMap_.clear();
}

FastPdb::EmlTrial::EmlTrial() 
{
  sources_.resize(0);
  freqs_.resize(0);
}

FastPdb::EmlTrial::EmlTrial(const FastPdb::EmlTrial& proj) {
  *this = proj;
}

FastPdb::EmlTrial::EmlTrial(FastPdb::EmlTrial& proj) {
  *this = proj;
}

//------------------------------------------------------------
// Add a source to this trial
//------------------------------------------------------------

void FastPdb::EmlTrial::addSource(EmlSrc& src) {

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

void FastPdb::EmlTrial::addFreq(EmlFreq& freq) {

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

void FastPdb::EmlTrial::operator=(const FastPdb::EmlTrial& proj) {
  *this = (FastPdb::EmlTrial&)proj;
}

void FastPdb::EmlTrial::operator=(FastPdb::EmlTrial& proj) {
  trialNo_   = proj.trialNo_;
  startDate_ = proj.startDate_;
  stopDate_  = proj.stopDate_;
  sources_   = proj.sources_;
  freqs_     = proj.freqs_;
};

//============================================================
// Methods of subobsblock
//============================================================

FastPdb::EmlSubObsblock::EmlSubObsblock() 
{
  trials_.resize(0);
}

FastPdb::EmlSubObsblock::EmlSubObsblock(const EmlSubObsblock& proj) {
  *this = proj;
}

FastPdb::EmlSubObsblock::EmlSubObsblock(EmlSubObsblock& proj) {
  *this = proj;
}

void FastPdb::EmlSubObsblock::operator=(const EmlSubObsblock& proj) {
  *this = (EmlSubObsblock&)proj;
}

void FastPdb::EmlSubObsblock::operator=(EmlSubObsblock& proj) {
  name_   = proj.name_;
  trials_ = proj.trials_;
};

//============================================================
// Methods of obsblock
//============================================================

FastPdb::EmlObsblock::EmlObsblock() 
{
  subObsblocks_.resize(0);
}

FastPdb::EmlObsblock::EmlObsblock(const EmlObsblock& proj) {
  *this = proj;
}

FastPdb::EmlObsblock::EmlObsblock(EmlObsblock& proj) {
  *this = proj;
}

void FastPdb::EmlObsblock::operator=(const EmlObsblock& proj) {
  *this = (EmlObsblock&)proj;
}

void FastPdb::EmlObsblock::operator=(EmlObsblock& proj) {
  name_         = proj.name_;
  subObsblocks_ = proj.subObsblocks_;
};

//============================================================
// Methods of project
//============================================================

FastPdb::EmlProject::EmlProject() {}

FastPdb::EmlProject::EmlProject(const EmlProject& proj) {
  *this = proj;
}

FastPdb::EmlProject::EmlProject(EmlProject& proj) {
  *this = proj;
}

void FastPdb::EmlProject::operator=(const EmlProject& proj) {
  *this = (EmlProject&)proj;
}

void FastPdb::EmlProject::operator=(EmlProject& proj) {
  name_        = proj.name_;
  obsblocks_   = proj.obsblocks_;
};

//------------------------------------------------------------
// Methods of FastPdb
//------------------------------------------------------------

FastPdb::FastPdb()
{
  html_ = true;
}

FastPdb::~FastPdb()
{
}

void FastPdb::createDatabase(std::string file)
{
  std::ifstream fin;
  fin.open(file.c_str(), ios::in);

  if(!fin) {
    ThrowError("Unable to open file: " << file);
  }

  String line;

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
    
    FastPdb::EmlTrial tmpTrial;

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

	String subObsblockName(" ");
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

	  projects_.push_back(tmpProj);

	  // And rebuild the map, since our pointers could now be
	  // invalid
	  
	  projectMap.clear();
	  for(unsigned iProj=0; iProj < projects_.size(); iProj++) {
	    EmlProject* pr = &projects_[iProj];
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
  }
}

void FastPdb::listDatabase()
{
  std::ostringstream os;

  for(unsigned iProj=0; iProj < projects_.size(); iProj++) {
    EmlProject& proj = projects_[iProj];
    COUTCOLOR(proj.name_, "green");

    for(unsigned iObs=0; iObs < proj.obsblocks_.size(); iObs++) {
      EmlObsblock& obsblock = proj.obsblocks_[iObs];

      for(unsigned iSub=0; iSub < obsblock.subObsblocks_.size(); iSub++) {
	EmlSubObsblock& subblock = obsblock.subObsblocks_[iSub];

	for(unsigned iTrial=0; iTrial < subblock.trials_.size(); iTrial++) {
	  FastPdb::EmlTrial& trial = subblock.trials_[iTrial];

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
	      os << " " << FastPdb::EmlSrc::typeString(src.types_[iType]);
	    }
	    COUTCOLOR("          " << os.str(), "yellow");
	  }
	}
      }
    }
  }
}

std::string FastPdb::listTrialsMatchingSource(std::string source, bool doFreq, double freq)
{
  std::ostringstream os;

  std::vector<std::string> trials;
  std::vector<std::string> srcs;
  std::vector<std::string> sortDates;
  std::vector<std::string> startDates;
  std::vector<std::string> stopDates;
  std::ostringstream osTrial;

  for(unsigned iProj=0; iProj < projects_.size(); iProj++) {
    EmlProject& proj = projects_[iProj];
    
    for(unsigned iObs=0; iObs < proj.obsblocks_.size(); iObs++) {
      EmlObsblock& obsblock = proj.obsblocks_[iObs];

      for(unsigned iSub=0; iSub < obsblock.subObsblocks_.size(); iSub++) {
	EmlSubObsblock& subblock = obsblock.subObsblocks_[iSub];

	for(unsigned iTrial=0; iTrial < subblock.trials_.size(); iTrial++) {
	  FastPdb::EmlTrial& trial = subblock.trials_[iTrial];
	  osTrial.str("");

	  if(subblock.name_.size() == 0 || subblock.name_ == " ") {
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
		srcs.push_back(sourceStr.str());

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

  unsigned lenMax=0;
  for(unsigned i=0; i < trials.size(); i++) {
    unsigned index = trialMap[sortedDates[i]];
    std::string trial = trials[index];
    unsigned len = trial.length();
    lenMax = len > lenMax ? len : lenMax;
  }
  
  if(trials.size() > 0) {

    if(html_) {
      os << "Found " << trials.size() << " matches:<br>";
      os << "<table>";
      os << "<tr><hr></tr><br>";
      os << "<tr><b><td>Obsblock Name</td><td>Dates Observed</td><td>Source Name</td></b></tr><br>";
      os << "<tr><hr></tr><br>";
    }
    
    for(unsigned i=0; i < trials.size(); i++) {
      unsigned index = trialMap[sortedDates[i]];
      if(html_) {
	os << "<tr>";
	os << "<td>" << setw(lenMax+5) << std::left << trials[index] << "</td>";
	os << "<td>" << std::left << startDates[index] << " - " << stopDates[index] << "</td>";
	os << "<td>" << "&nbsp;" << "</td>";
	os << "<td>" << srcs[index] << "</td>";
	os << "</tr>";
      } else {
	os << setw(lenMax+5) << std::left << trials[index] << " " << std::left << startDates[index] << " - " << stopDates[index] << " " << srcs[index] << std::endl;
      }
    }
    
    if(html_) {
      os << "</table>";
    }

  } else {
    os << "No matches found in the database" << std::endl;
  }

  return os.str();
}

std::string FastPdb::listTrialsMatchingProject(std::string project)
{
  std::ostringstream os;

  std::vector<std::string> trials;
  std::vector<std::string> sortDates;
  std::vector<std::string> startDates;
  std::vector<std::string> stopDates;
  std::ostringstream osTrial;

  for(unsigned iProj=0; iProj < projects_.size(); iProj++) {
    EmlProject& proj = projects_[iProj];
    
    if(proj.name_ == project) {
      for(unsigned iObs=0; iObs < proj.obsblocks_.size(); iObs++) {
	EmlObsblock& obsblock = proj.obsblocks_[iObs];
	
	for(unsigned iSub=0; iSub < obsblock.subObsblocks_.size(); iSub++) {
	  EmlSubObsblock& subblock = obsblock.subObsblocks_[iSub];
	  
	  for(unsigned iTrial=0; iTrial < subblock.trials_.size(); iTrial++) {
	    FastPdb::EmlTrial& trial = subblock.trials_[iTrial];
	    osTrial.str("");
	    
	    if(subblock.name_.size() == 0 || subblock.name_ == " ") {
	      osTrial << proj.name_ << "." << obsblock.name_ << "." << trial.trialNo_;
	    } else {
	      osTrial << proj.name_ << "." << obsblock.name_ << "." << subblock.name_ << "." << trial.trialNo_;
	    }
	  
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

  std::map<std::string, unsigned> trialMap;
  for(unsigned i=0; i < trials.size(); i++) {
    trialMap[sortDates[i]] = i;
  }
  
  // Now sort the list by dates

  std::vector<std::string> sortedDates = Sort::sort(sortDates);
  
  for(unsigned i=0; i < trials.size(); i++) {
    unsigned index = trialMap[sortedDates[i]];
    os << setw(30) << std::left << trials[index] << " " << startDates[index] << " - " << stopDates[index] << std::endl;
  }

  if(trials.size() == 0) {
    os << "No match for project: " << project << " found in the database" << std::endl;
  }

  return os.str();
}
