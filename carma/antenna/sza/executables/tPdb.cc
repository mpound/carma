#include <iostream>
#include <iomanip>

#include <time.h>

#include "carma/corba/Client.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/String.h"
#include "carma/szautil/Sort.h"

#include "carma/util/Time.h"
#include "carma/observertools/ProjectDatabaseManager_skel.h"

using namespace std;
using namespace sza::util;
using namespace carma::util;

PROGRAM_KEYWORDS = {
  { "nameserver",  "corba.carma.pvt:9001",  "s", USAGE "Name Server host:port"},
  { "eventserver", "corba.carma.pvt:8001",  "s", USAGE "Event Server host:port"},
  { "notifyserver","corba.carma.pvt:10001", "s", USAGE "Notification Server host:port"},
  { "project",     "c0481",                 "s", USAGE "Project to list"},
  { "obsblock",    "1SL_95NGC133",          "s", USAGE "Obsblock to modify"},
  { "addobsblock", "f",                     "b", USAGE "True to add an obsblock"},
  { "addobsblocktime", "f",                 "b", USAGE "True to add an obsblock time"},
  { "addtrial",    "f",                     "b", USAGE "True to add a trial"},
  { "remtrial",    "f",                     "b", USAGE "True to remove a trial"},
  { "edittrial",   "f",                     "b", USAGE "True to edit a trial"},
  { "hours",       "10.0",                  "d", USAGE "min allocated time"},
  { "startlst",    "01:00:00",              "s", USAGE "Start LST of the latest trial"},
  { "stoplst",     "03:00:00",              "s", USAGE "Stop LST of the latest trial"},
  { "comment",     "",                      "s", USAGE "Comment"},
  { "list",        "f",                     "b", USAGE "True to list project, otherwise modify the latest trial"},
  { "qsrc",        "t",                     "b", USAGE "True to list projects matching srcname"},
  { "sci2only",    "t",                     "b", USAGE "True to only list projects for sci2"},
  { "source",      "MARS",                  "s", USAGE "True to list projects matching srcname"},
  { "startdate",   "2013-01-23",            "s", USAGE "Start date for source search"},
  { "stopdate",    "2013-01-24",            "s", USAGE "Start date for source search"},
  { "ntrial",      "f",                     "b", USAGE "True to list number of trials in a project"},
  { "itrial",      "0",                     "i", USAGE "trial to modify, otherwise most recent"},
  { "grade",       "95",                    "d", USAGE "grade to assign to the trial"},

  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace carma::observertools;

std::ostream& operator<<(std::ostream& os, carma::observertools::Trial& trial);
std::ostream& operator<<(std::ostream& os, carma::observertools::SubObsblock& subBlock);
std::ostream& operator<<(std::ostream& os, carma::observertools::Obsblock& block);
std::ostream& operator<<(std::ostream& os, carma::observertools::Project& project);
std::ostream& operator<<(std::ostream& os, carma::observertools::Investigator& inv);

bool appendAllMatchingTrials(std::vector<string>& ids, 
			     std::vector<string>& startDates, 
			     std::vector<string>& stopDates, 
			     std::vector<string>& bands,
			     std::vector<string>& comments,
			     carma::observertools::Project& project, 
			     std::string srcTest);

bool appendMatchingSci1Trials(std::ostringstream& osRet,
			      carma::observertools::Project& project, 
			      std::string srcTest);

bool appendMatchingSci2Trials(std::vector<string>& ids, 
			      std::vector<string>& startDates, 
			      std::vector<string>& stopDates, 
			      std::vector<string>& bands,
			      std::vector<string>& comments,
			      carma::observertools::Project& project, 
			      std::string srcTest);

bool projectContainsSource(carma::observertools::Project& project, std::string sourceName);
bool trialContainsSource(carma::observertools::Trial& trial, std::string sourceName);

void listProject(carma::observertools::ProjectDatabaseManager_var pdb, std::string projectName, std::string obsblockName, bool useObsblock);
void listProjectsByDate(carma::observertools::ProjectDatabaseManager_var pdb, std::string dateRange);
void listProjectsByLst(carma::observertools::ProjectDatabaseManager_var pdb, std::string dateRange);
void listProjectsByDateAndSrc(carma::observertools::ProjectDatabaseManager_var pdb, 
			      std::string sourceName,
			      std::string dateRange);
void listSci2ProjectsByDateAndSrc(carma::observertools::ProjectDatabaseManager_var pdb, 
				  std::string sourceName,
				  std::string dateRange);

void printQuery(carma::observertools::ItemValueSequence& query);

void listSource(carma::observertools::ProjectDatabaseManager_var pdb, std::string sourceName, std::string dateRange);

bool addObsblockTime(carma::observertools::ProjectDatabaseManager_var pdb, std::string project, std::string obsblock,
		     float hours);

Project& findProject(carma::observertools::ProjectDatabaseManager_var pdb, std::string projectName);
Obsblock& findObsblock(carma::observertools::ProjectDatabaseManager_var pdb, std::string projectName, std::string obsblockName);
Obsblock& findObsblock(Project& project, std::string obsblockName);

unsigned getNTrial(carma::observertools::ProjectDatabaseManager_var pdb, std::string projectName, std::string obsblockName);
unsigned getFirstIncompleteTrial(carma::observertools::ProjectDatabaseManager_var pdb, 
				 std::string projectName, std::string obsblockName);

unsigned getFirstTrialWithZeroTime(carma::observertools::ProjectDatabaseManager_var pdb, 
				   std::string projectName, std::string obsblockName);

bool remTrial(carma::observertools::ProjectDatabaseManager_var pdb, std::string project, std::string obsblock, short iTrial);
bool addTrial(carma::observertools::ProjectDatabaseManager_var pdb, std::string project, std::string obsblock);
bool addObsblock(carma::observertools::ProjectDatabaseManager_var pdb, std::string project, std::string obsblock);

void editLastTrial(carma::observertools::ProjectDatabaseManager_var pdb, 
		   std::string project, std::string obsblock, 
		   std::string startLstStr, std::string stopLstStr, 
		   std::string comment, float grade, std::string source);

void editTrial(carma::observertools::ProjectDatabaseManager_var pdb, 
	       std::string project, std::string obsblock, short iTrial,
	       std::string startLstStr, std::string stopLstStr, 
	       std::string comment, float grade, std::string source);

void editTrial(carma::observertools::ProjectDatabaseManager_var pdb, std::string project, std::string obsblock, short iTrial,
	       ProjectStatus status);

std::string getFITSdateString(double mjd);
  
int Program::main(void)
{
  try {
    
    //------------------------------------------------------------
    // Now do something with it
    //------------------------------------------------------------

    //    COUT("Attempting to resolve: ");

    carma::observertools::ProjectDatabaseManager_var pdb = 0;
    pdb = getCorbaClient().resolveName<ProjectDatabaseManager>("carma.projectDatabaseManager.projectDatabaseManagerControl");

    //    COUT("Attempting to resolve: done");

    std::string project  = Program::getParameter("project");
    std::string obsblock = Program::getParameter("obsblock");
    std::string startlst = Program::getParameter("startlst");
    std::string stoplst  = Program::getParameter("stoplst");
    std::string comment  = Program::getParameter("comment");
    //    double grade         = Program::getDoubleParameter("grade");
    //    double hours         = Program::getDoubleParameter("hours");
    //    unsigned iTrial      = Program::getIntParameter("itrial");
    std::string source   = Program::getParameter("source");

    if(Program::getBoolParameter("list")) {
      if(Program::getBoolParameter("ntrial")) {
	COUT("ntrial = " << getNTrial(pdb, project, obsblock));
      } else {
	listProject(pdb, project, obsblock, Program::parameterWasSpecified("obsblock"));
      }
    }

    if(Program::getBoolParameter("qsrc")) {
      ostringstream os;
      os << Program::getParameter("startdate") << "," << Program::getParameter("stopdate");

      if(Program::getBoolParameter("sci2only")) {
	listSci2ProjectsByDateAndSrc(pdb, source, os.str());
      } else {
	listProjectsByDateAndSrc(pdb, source, os.str());
      }
    } else {
      ostringstream os;
      os << Program::getParameter("startdate") << "," << Program::getParameter("stopdate");
      listProjectsByDate(pdb, os.str());
    }

#if 0
    if(Program::getBoolParameter("addtrial")) {
      addTrial(pdb, project, obsblock);
    }

    if(Program::getBoolParameter("remtrial")) {
      remTrial(pdb, project, obsblock, iTrial);
    }

    if(Program::getBoolParameter("addobsblock")) {
      addObsblock(pdb, project, obsblock);
    }

    if(Program::getBoolParameter("addobsblocktime")) {
      addObsblockTime(pdb, project, obsblock, hours);
    }

    if(Program::getBoolParameter("edittrial")) {
      if(iTrial==0)
	editLastTrial(pdb, project, obsblock, startlst, stoplst, comment, grade, source);
      else {
	editTrial(pdb, project, obsblock, iTrial, startlst, stoplst, comment, grade, source);
      }
    }      

    if(Program::getBoolParameter("list")) {
      if(Program::getBoolParameter("ntrial")) {
	COUT("ntrial = " << getNTrial(pdb, project, obsblock));
      } else {
	listProject(pdb, project, obsblock, Program::parameterWasSpecified("obsblock"));
      }

    }

    //    COUT(getNTrial(pdb, project, obsblock));
    //    remTrial(pdb, project, obsblock, 3);
    //    addTrial(pdb, project, obsblock);
    //    listProject(pdb, project);
#endif
  } catch(carma::util::ErrorException& err) {
    COUT("Caught an error: " << err.what());
  } catch(carma::util::UserException& err) {
    COUT("Caught an error: " << err.errorMsg);
  } catch(carma::observertools::ProjectDatabaseException& err) {
    COUT("Caught an error: " << err.errorMsg);
  } catch(CORBA::SystemException& err) {
    COUT("Caught a CORBA error: ");
  } catch(...) {
    COUT("Caught an unknown exception");
  }

  return 0;
}

std::ostream& operator<<(std::ostream& os, carma::observertools::Trial& trial)
{
  os << "      Trial:          " << trial.trialID                   << std::endl;
  os << "      Status:         " << trial.status                    << std::endl;
  os << "      Total obs time: " << trial.trialObservationLength    << std::endl;

  os << "      NTarget:        " << trial.target.length()                    << std::endl;

#if 0
  if(trial.target.length() > 0) {
    for(unsigned i=0; i < trial.target.length(); i++) {
      os << "  target " << i << " = " << (*trial.target)[i] << std::endl;
    }
  }
#endif

  os << "      NSource:        " << trial.source.length()                    << std::endl;

  for(unsigned i=0; i < trial.source.length(); i++)
    os << "  src[" << i << "] = " << trial.source[i].sourceName << std::endl;

  os << "      NCalibrator:        " << trial.calibrator.length()                    << std::endl;

  for(unsigned i=0; i < trial.calibrator.length(); i++)
    os << "  src[" << i << "] = " << trial.calibrator[i].calibratorName << std::endl;

  for(unsigned i=0; i < trial.correlator.length(); i++) {
    os << "  corr[" << i << "].setup = " << trial.correlator[i].setupNumber << std::endl;
    os << "  corr[" << i << "].nWin  = " << trial.correlator[i].numberOfWindows << std::endl;
  }

  os << "      Date started:   " << trial.trialObservationDateStart << std::endl;
  os << "      Date ended:     " << trial.trialObservationDateEnd   << std::endl;
  os << "      LST started:    " << trial.observedLSTstart          << std::endl;
  os << "      LST ended:      " << trial.observedLSTend            << std::endl;
  os << "      DQA grade:      " << trial.dqaOverallGrade           << std::endl;
  os << "      OBS grade:      " << trial.obsGrade                  << std::endl;
  os << "      OBS comments:   " << trial.obsComments               << std::endl;

  return os;
}

std::ostream& operator<<(std::ostream& os, carma::observertools::SubObsblock& subBlock)
{
  os << "    Sub Obsblock:   " << subBlock.subObsblockID << std::endl;
  os << "    Status:         " << subBlock.status     << std::endl;
  os << "    Total obs time: " << subBlock.subObsblockObservationTime << std::endl << std::endl;

  for(unsigned iTrial=0; iTrial < subBlock.trial.length(); iTrial++) {
    Trial& trial = subBlock.trial[iTrial];
    os << trial << std::endl;
  }

  return os;
}

std::ostream& operator<<(std::ostream& os, carma::observertools::Obsblock& block)
{
  os << "  Obsblock:           " << block.obsblockID       << std::endl;
  os << "  Status:             " << block.status           << std::endl;
  os << "  Min allocated time: " << block.minAllocatedTime << std::endl;
  os << "  Total obs time:     " << block.totalObsTime     << std::endl;
  os << "  Remaining time:     " << block.remainingTime    << std::endl << std::endl;

  for(unsigned iSubBlock=0; iSubBlock < block.subObsblock.length(); iSubBlock++) {
    SubObsblock& subBlock = block.subObsblock[iSubBlock];
    os << subBlock << std::endl;
  }

  return os;
}

std::ostream& operator<<(std::ostream& os, carma::observertools::Investigator& inv)
{
  os << "Investigator:   " << inv.name << " " << inv.email << " " << inv.affiliation << std::endl;
  return os;
}

std::ostream& operator<<(std::ostream& os, carma::observertools::Project& project)
{
  os << "Project:        " << project.projectID << std::endl;
  os << "Status:         " << project.status << std::endl;
  os << "Total obs time: " << project.totalTime << std::endl << std::endl;

  for(unsigned iInv=0; iInv < project.coInvestigator.length(); iInv++) {
    Investigator& inv = project.coInvestigator[iInv];
    os << inv << std::endl;
  }
  
  for(unsigned iBlock=0; iBlock < project.obsblock.length(); iBlock++) {
    Obsblock& block = project.obsblock[iBlock];
    os << block << std::endl;
  }

  return os;
}

bool appendMatchingSci1Trials(std::ostringstream& osRet,
			      carma::observertools::Project& project, 
			      std::string srcTest)
{
  for(unsigned iBlock=0; iBlock < project.obsblock.length(); iBlock++) {
    Obsblock& block = project.obsblock[iBlock];
    std::ostringstream os;
    os << block.obsblockID;
    String obsblockIDStr(os.str());

    if(!(obsblockIDStr.contains("SH_") || obsblockIDStr.contains("SL_") || obsblockIDStr.contains("sci2"))) {

      for(unsigned iSubBlock=0; iSubBlock < block.subObsblock.length(); iSubBlock++) {
	SubObsblock& subBlock = block.subObsblock[iSubBlock];
	
	for(unsigned iTrial=0; iTrial < subBlock.trial.length(); iTrial++) {
	  Trial& trial = subBlock.trial[iTrial];

	  if(trialContainsSource(trial, srcTest)) {
	    osRet << project.projectID << "." << block.obsblockID << "." << trial.trialID << std::endl;
	  }

	}

      }
    }
  }

  return false;
}

bool appendMatchingSci2Trials(std::vector<string>& ids, 
			      std::vector<string>& startDates, 
			      std::vector<string>& stopDates, 
			      std::vector<string>& bands, 
			      std::vector<string>& comments, 
			      carma::observertools::Project& project, 
			      std::string srcTest)
{
  for(unsigned iBlock=0; iBlock < project.obsblock.length(); iBlock++) {
    Obsblock& block = project.obsblock[iBlock];
    std::ostringstream os;
    os << block.obsblockID;
    String obsblockIDStr(os.str());

    os.str("");
    os << block.receiverBand;
    String rxBandStr(os.str());

    //    COUT("Obsblock is now " << obsblockIDStr);

    if(rxBandStr.contains("1CM") || obsblockIDStr.contains("SH_") || obsblockIDStr.contains("SL_") || 
       obsblockIDStr.contains("sci2")) {

      //      COUT("Obsblock contains SH or SL");
      for(unsigned iSubBlock=0; iSubBlock < block.subObsblock.length(); iSubBlock++) {
	SubObsblock& subBlock = block.subObsblock[iSubBlock];
	
	for(unsigned iTrial=0; iTrial < subBlock.trial.length(); iTrial++) {
	  Trial& trial = subBlock.trial[iTrial];

	  if(trialContainsSource(trial, srcTest)) {
	    os.str("");
	    os << project.projectID << "." << block.obsblockID << "." << trial.trialID;
	    ids.push_back(os.str());

	    os.str("");
	    os << trial.trialObservationDateStart;
	    startDates.push_back(os.str());

	    os.str("");
	    os << trial.trialObservationDateEnd;
	    stopDates.push_back(os.str());

	    os.str("");

	    //------------------------------------------------------------
	    // Print diagnostics about the frequency content of this database entry
	    //------------------------------------------------------------


	    for(unsigned i=0; i < trial.correlator.length(); i++) {
	      Correlator& corr = trial.correlator[i];
	      Window& winMin = corr.window[0];
	      Window& winMax = corr.window[corr.window.length()-1];

	      os << " Freq = " << std::fixed << std::setprecision(2) << winMin.minFrequency << " - " << winMax.maxFrequency;
	    }

	    bands.push_back(os.str());

	    os.str("");
	    os << "Obs Grade: " << trial.obsGrade << " Comments: " << trial.obsComments;

	    comments.push_back(os.str());
	  }

	}

      }
    }
  }

  return false;
}

bool appendAllMatchingTrials(std::vector<string>& ids, 
			     std::vector<string>& startDates, 
			     std::vector<string>& stopDates, 
			     std::vector<string>& bands,
			     std::vector<string>& comments,
			     carma::observertools::Project& project, 
			     std::string srcTest)
{
  for(unsigned iBlock=0; iBlock < project.obsblock.length(); iBlock++) {
    Obsblock& block = project.obsblock[iBlock];
    std::ostringstream os;
    os << block.obsblockID;
    String obsblockIDStr(os.str());

    for(unsigned iSubBlock=0; iSubBlock < block.subObsblock.length(); iSubBlock++) {
      SubObsblock& subBlock = block.subObsblock[iSubBlock];
      
      for(unsigned iTrial=0; iTrial < subBlock.trial.length(); iTrial++) {
	Trial& trial = subBlock.trial[iTrial];
	
	if(trialContainsSource(trial, srcTest)) {
	  os.str("");
	  os << project.projectID << "." << block.obsblockID << "." << trial.trialID << "." << block.receiverBand;
	  ids.push_back(os.str());
	  
	  os.str("");
	  os << trial.trialObservationDateStart;
	  startDates.push_back(os.str());
	  
	  os.str("");
	  os << trial.trialObservationDateEnd;
	  stopDates.push_back(os.str());

	  os.str("");

	  //------------------------------------------------------------
	  // Print diagnostics about the frequency content of this database entry
	  //------------------------------------------------------------

	  for(unsigned i=0; i < trial.correlator.length(); i++) {
	    Correlator& corr = trial.correlator[i];
	    Window& winMin = corr.window[0];
	    Window& winMax = corr.window[corr.window.length()-1];
	    
	    os << " Freq = " << std::fixed << std::setprecision(2) << winMin.minFrequency << " - " << winMax.maxFrequency;
	  }
	  
	  bands.push_back(os.str());
	  
	  os.str("");
	  os << "Obs Grade: " << trial.obsGrade << " Comments: " << trial.obsComments;
	  
	  comments.push_back(os.str());
	}
	
      }
      
    }
  }

  return false;
}

bool projectContainsSource(carma::observertools::Project& project, std::string sourceName)
{
  for(unsigned iBlock=0; iBlock < project.obsblock.length(); iBlock++) {
    Obsblock& block = project.obsblock[iBlock];

    for(unsigned iSubBlock=0; iSubBlock < block.subObsblock.length(); iSubBlock++) {
      SubObsblock& subBlock = block.subObsblock[iSubBlock];

      for(unsigned iTrial=0; iTrial < subBlock.trial.length(); iTrial++) {
	Trial& trial = subBlock.trial[iTrial];
	if(trialContainsSource(trial, sourceName)) 
	  return true;
      }
    }
  }

  return false;
}

bool trialContainsSource(carma::observertools::Trial& trial, std::string sourceName)
{
  for(unsigned i=0; i < trial.source.length(); i++) {
    std::ostringstream os;
    os << trial.source[i].sourceName;
    String str(os.str());
    if(str.contains(sourceName.c_str())) {
      return true;
    }
  }
  
  for(unsigned i=0; i < trial.calibrator.length(); i++) {
    std::ostringstream os;
    os << trial.calibrator[i].calibratorName;
    String str(os.str());
    if(str.contains(sourceName.c_str())) {
      return true;
    }
  }

  return false;
}

Project& findProject(carma::observertools::ProjectDatabaseManager_var pdb, std::string projectName)
{
  carma::observertools::ProjectSequence* projects=0;
  carma::observertools::ItemValueSequence query;

  query.length(1);
  query[0].name  = "project";
  query[0].value = projectName.c_str();
  
  projects = pdb->projectQuery(query);
  
  if((*projects).length() > 0) {
    Project& project = (*projects)[0];
    return project;
  } else {
    ThrowError("Project " << projectName << " not found");
    Project& project = (*projects)[0];
    return project;
  }
}

Obsblock& findObsblock(Project& project, std::string obsblockName)
{
  for(unsigned iBlock=0; iBlock < project.obsblock.length(); iBlock++) {
    Obsblock& block = project.obsblock[iBlock];

    std::ostringstream obsblockId;
    obsblockId << block.obsblockID;

    if(obsblockId.str() == obsblockName)
      return block;
  }

  ThrowError("Obsblock " << obsblockName << " not found");
  Obsblock& block = project.obsblock[0];
  return block;
}

Obsblock& findObsblock(carma::observertools::ProjectDatabaseManager_var pdb, std::string projectName, std::string obsblockName)
{
  Project& project = findProject(pdb, projectName);
  return findObsblock(project, obsblockName);
}

unsigned getNTrial(carma::observertools::ProjectDatabaseManager_var pdb, std::string projectName, std::string obsblockName)
{
  Project& project = findProject(pdb, projectName);
  Obsblock& block  = findObsblock(project, obsblockName);

  if(block.subObsblock.length() > 0) {
    return block.subObsblock[0].trial.length();
  }

  ThrowError("Project " << projectName << ", bbsblock " << obsblockName << " has no subobsblocks");
  return 0;
}

unsigned getFirstIncompleteTrial(carma::observertools::ProjectDatabaseManager_var pdb, 
				 std::string projectName, std::string obsblockName)
{
  Project& project = findProject(pdb, projectName);
  Obsblock& block  = findObsblock(project, obsblockName);

  if(block.subObsblock.length() > 0) {
    SubObsblock& subObsblock = block.subObsblock[0];

    for(unsigned iTrial=0; iTrial < subObsblock.trial.length(); iTrial++) {
      Trial& trial = subObsblock.trial[iTrial];
      
      if(trial.status != PSTATUS_COMPLETE)
	return iTrial+1;
    }
  }

  ThrowError("No incomplete trial found for project " << projectName << ", bbsblock " << obsblockName);
  return 0;
}

unsigned getFirstTrialWithZeroTime(carma::observertools::ProjectDatabaseManager_var pdb, 
				   std::string projectName, std::string obsblockName)
{
  Project& project = findProject(pdb, projectName);
  Obsblock& block  = findObsblock(project, obsblockName);

  if(block.subObsblock.length() > 0) {
    SubObsblock& subObsblock = block.subObsblock[0];

    for(unsigned iTrial=0; iTrial < subObsblock.trial.length(); iTrial++) {
      Trial& trial = subObsblock.trial[iTrial];
      
      if(!(trial.trialObservationLength > 0))
	return iTrial+1;
    }
  }

  ThrowError("No incomplete trial found for project " << projectName << ", bbsblock " << obsblockName);
  return 0;
}

void listProject(carma::observertools::ProjectDatabaseManager_var pdb, std::string projectName, std::string obsblockName, bool useObsblock)
{
  carma::observertools::ProjectSequence* projects=0;
  carma::observertools::ItemValueSequence query;

  if(useObsblock) {
    query.length(2);
    query[0].name  = "project";
    query[0].value = projectName.c_str();
    query[1].name  = "obsblock";
    query[1].value = obsblockName.c_str();
  } else {
    query.length(1);
    query[0].name  = "project";
    query[0].value = projectName.c_str();
  }
  
  projects = pdb->projectQuery(query);
  
  if((*projects).length() > 0) {

    Project& project = (*projects)[0];
    COUT(project);

  } else {
    COUT("Project " << projectName << " not found");
  }
}

void listSource(carma::observertools::ProjectDatabaseManager_var pdb, std::string sourceName, std::string dateRange)
{
  carma::observertools::ProjectSequence* projects=0;
  carma::observertools::ItemValueSequence query;

  query.length(2);
  query[0].name  = "sourceName";
  query[0].value = sourceName.c_str();
  query[1].name  = "trialObservationDate";
  query[1].value = dateRange.c_str();

  projects = pdb->projectQuery(query);
  
  if((*projects).length() > 0) {

    Project& project = (*projects)[0];
    COUT(project);

  } else {
    COUT("Source " << sourceName << " not found");
  }
}

void listProjectsByDateAndSrc(carma::observertools::ProjectDatabaseManager_var pdb, 
			      std::string sourceName,
			      std::string dateRange)
{
  carma::observertools::ProjectSequence* projects=0;
  carma::observertools::ItemValueSequence query;

  query.length(2);

  query[0].name  = "trialObservationDate";
  query[0].value = dateRange.c_str();

  query[1].name  = "notProject";
  query[1].value = "commissioning";

  projects = pdb->projectQuery(query);
  
  //------------------------------------------------------------
  // Now list projects containing the source name
  //------------------------------------------------------------

  if((*projects).length() > 0) {

    unsigned len = (*projects).length();
    
    std::ostringstream os;
    std::vector<std::string> ids;
    std::vector<std::string> startDates;
    std::vector<std::string> stopDates;
    std::vector<std::string> bands;
    std::vector<std::string> comments;

    for(unsigned i=0; i < len; i++) {
      Project& project = (*projects)[i];
      appendAllMatchingTrials(ids, startDates, stopDates, bands, comments, project, sourceName);
    }

    unsigned nTrial = ids.size();
    COUT("nTrial = " << nTrial);

    if(nTrial > 0) {

      std::map<std::string, unsigned> startDateMap;
      for(unsigned i=0; i < nTrial; i++) {
	startDateMap[startDates[i]] = i;
      }

      // Now sort on the start dates

      std::vector<string> sortedStartDates;
      sortedStartDates = Sort::sort(startDates);
      
      for(unsigned i=0; i < nTrial; i++) {
	unsigned index = startDateMap[sortedStartDates[i]];
	COUT(std::setw(30) << std::left << ids[index] << " "  
	     << startDates[index] << " " << stopDates[index] << bands[index]);
      }

    }

  } else {
    COUT("No projects found");
  }
}

void listProjectsByDate(carma::observertools::ProjectDatabaseManager_var pdb, 
			std::string dateRange)
{
  carma::observertools::ProjectSequence* projects=0;
  carma::observertools::ItemValueSequence query;

  query.length(2);

  query[0].name  = "trialObservationDate";
  query[0].value = dateRange.c_str();

  query[1].name  = "notProject";
  query[1].value = "commissioning";

  projects = pdb->projectQuery(query);
  
  //------------------------------------------------------------
  // Now list all projects
  //------------------------------------------------------------

  unsigned len = (*projects).length();

  COUT("Query returned " << len << " projects");

  for(unsigned iProj=0; iProj < len; iProj++) {
    Project& project = (*projects)[iProj];
    
    for(unsigned iBlock=0; iBlock < project.obsblock.length(); iBlock++) {
      Obsblock& block = project.obsblock[iBlock];
      std::ostringstream os;
      
      for(unsigned iSubBlock=0; iSubBlock < block.subObsblock.length(); iSubBlock++) {
	SubObsblock& subBlock = block.subObsblock[iSubBlock];
	
	for(unsigned iTrial=0; iTrial < subBlock.trial.length(); iTrial++) {
	  Trial& trial = subBlock.trial[iTrial];

	  std::ostringstream sBlock;
	  sBlock << subBlock.subObsblockID;

	  os.str("");
	  if(sBlock.str().size() == 0) {
	    os << project.projectID << "." << block.obsblockID << "." << trial.trialID;
	  } else {
	    os << project.projectID << "." << block.obsblockID << "." << subBlock.subObsblockID << "." << trial.trialID;
	  }

	  String trialIDStr(os.str());
	  
	  os.str("");
	  os << trial.trialObservationDateStart;
	  std::string startDate = os.str();

	  os.str("");
	  os << trial.trialObservationDateEnd;
	  std::string stopDate = os.str();

	  COUT("PROJ " << std::setw(30) << std::left << trialIDStr.str() << " "  
	       << startDate << " " << stopDate);

	  if(trial.correlator.length() == 0) {

	    os.str("");
	    os << std::setw(6) << std::right << std::fixed << std::setprecision(2) << "??"  << " - "
	       << std::setw(6) << std::right << std::fixed << std::setprecision(2) << "??"  << " GHz";
	    std::string freq = os.str();

	    for(unsigned i=0; i < trial.source.length(); i++) {
	      std::ostringstream os;
	      os.str("");
	      os << trial.source[i].sourceName;
	      String str(os.str());
	      COUT("          " << "SRC S " << std::setw(30) << std::left << str << " " << std::setw(30) << freq);
	    }
	    
	    for(unsigned i=0; i < trial.calibrator.length(); i++) {
	      os.str("");
	      os << trial.calibrator[i].calibratorName;
	      String str(os.str());
	      COUT("          " << "SRC C " << std::setw(30) << std::left << str << " " << std::setw(30) << freq);
	    }
	  } else {
	    for(unsigned i=0; i < trial.correlator.length(); i++) {
	      Correlator& corr = trial.correlator[i];
	      Window& winMin = corr.window[0];
	      Window& winMax = corr.window[corr.window.length()-1];
	      
	      os.str("");
#if 0
	      if(winMin.minFrequency < 40) {
		os << "1CM";
	      } else if(winMin.minFrequency < 200) {
		os << "3MM";
	      } else {
		os << "1MM";
	      }
#else
	      os << std::setw(6) << std::right << std::fixed << std::setprecision(2) << winMin.minFrequency  << " - "
		 << std::setw(6) << std::right << std::fixed << std::setprecision(2) << winMax.maxFrequency  << " GHz";
#endif
	      std::string freq = os.str();
	      
	      for(unsigned i=0; i < trial.source.length(); i++) {
		std::ostringstream os;
		os.str("");
		os << trial.source[i].sourceName;
		String str(os.str());
		COUT("          " << "SRC S " << std::setw(30) << std::left << str << " " << std::setw(30) << std::left << freq);
	      }
	      
	      for(unsigned i=0; i < trial.calibrator.length(); i++) {
		os.str("");
		os << trial.calibrator[i].calibratorName;
		String str(os.str());
		COUT("          " << "SRC C " << std::setw(30) << std::left << str << " " << std::setw(30) << std::left << freq);
	      }
	    }
	  }

	}

      }
    }
  }
}
  
void listSci2ProjectsByDateAndSrc(carma::observertools::ProjectDatabaseManager_var pdb, 
				  std::string sourceName,
				  std::string dateRange)
{
  carma::observertools::ProjectSequence* projects=0;
  carma::observertools::ItemValueSequence query;

  query.length(2);

  query[0].name  = "trialObservationDate";
  query[0].value = dateRange.c_str();

  query[1].name  = "notProject";
  query[1].value = "commissioning";

  projects = pdb->projectQuery(query);
  
  //  COUT("Query returned...");

  //------------------------------------------------------------
  // Now list projects containing the source name
  //------------------------------------------------------------

  if((*projects).length() > 0) {

    unsigned len = (*projects).length();
    
    std::ostringstream os;
    std::vector<std::string> ids;
    std::vector<std::string> startDates;
    std::vector<std::string> stopDates;
    std::vector<std::string> bands;
    std::vector<std::string> comments;

    for(unsigned i=0; i < len; i++) {
      Project& project = (*projects)[i];
      appendMatchingSci2Trials(ids, startDates, stopDates, bands, comments, project, sourceName);
    }

    unsigned nTrial = ids.size();
    //    COUT("nTrial = " << nTrial);

    if(nTrial > 0) {

      std::map<std::string, unsigned> startDateMap;
      for(unsigned i=0; i < nTrial; i++) {
	startDateMap[startDates[i]] = i;
      }

      // Now sort on the start dates

      std::vector<string> sortedStartDates;
      sortedStartDates = Sort::sort(startDates);

      for(unsigned i=0; i < nTrial; i++) {
	unsigned index = startDateMap[sortedStartDates[i]];
	COUT(std::setw(30) << std::left << ids[index] << " " 
	     << startDates[index] << " " << stopDates[index] << bands[index] << " ");
	String commentStr(comments[index]);
	commentStr.wrapTo(100, 51);
	COUT(commentStr.str());
      }

    }

  } else {
    COUT("No projects found");
  }
}

void listProjectsByLst(carma::observertools::ProjectDatabaseManager_var pdb, std::string dateRange)
{
  carma::observertools::ProjectSequence* projects=0;
  carma::observertools::ItemValueSequence query;

  query.length(9);

  query[0].name  = "obsblockStatus";
  query[0].value = "INCOMPLETE";

  query[1].name  = "remainingTime";
  query[1].value = "1.0";

  query[2].name  = "arrayConfiguration";
  query[2].value = "E";

  query[3].name  = "notProject";
  query[3].value = "opnt";

  query[4].name  = "notProject";
  query[4].value = "rpnt";

  query[5].name  = "notProject";
  query[5].value = "tilt";

  query[6].name  = "notProject";
  query[6].value = "fringe";

  query[7].name  = "notProject";
  query[7].value = "test";

  query[8].name  = "priority";
  query[8].value = "0.1,100000.0";

  printQuery(query);

  projects = pdb->projectQuery(query);
  
  if((*projects).length() > 0) {

    unsigned len = (*projects).length();
    
    for(unsigned i=0; i < len; i++) {
      Project& project = (*projects)[i];
      COUT(project);
    }

  } else {
    COUT("No projects found");
  }
}

bool addObsblockTime(carma::observertools::ProjectDatabaseManager_var pdb, std::string project, std::string obsblock,
		     float hours)
{
  ItemValueSequence ivs;
  ivs.length(1);

  ivs[0].name  = "minAllocationTime";

  std::ostringstream os;
  os << hours;
  ivs[0].value = os.str().c_str();

  COUT("Editing project: " << project << "  obsblock = " << obsblock << " with hours = " << hours);
  return pdb->projectEdit(project.c_str(), obsblock.c_str(), "", 1, ivs, ESTATUS_EDIT);
}

bool addObsblock(carma::observertools::ProjectDatabaseManager_var pdb, std::string project, std::string obsblock)
{
  ItemValueSequence ivs;
  ivs.length(1);
  ivs[0].name  = "newObsblock";
  ivs[0].value = obsblock.c_str();

  return pdb->projectEdit(project.c_str(), "", "", 1, ivs, ESTATUS_ADD);
}

#if 0
bool addTrial(carma::observertools::ProjectDatabaseManager_var pdb, std::string project, std::string obsblock)
{
  short nTrial = getNTrial(pdb, project, obsblock);

  ItemValueSequence ivs;
  ivs.length(1);
  ivs[0].name  = "trial";

  ostringstream os;
  os << "nTrial+1";

  ivs[0].value = os.str().c_str();

  return pdb->projectEdit(project.c_str(), obsblock.c_str(), "", nTrial+1, ivs, ESTATUS_ADD);
}
#else
bool addTrial(carma::observertools::ProjectDatabaseManager_var pdb, std::string project, std::string obsblock)
{
  short nTrial = getNTrial(pdb, project, obsblock);

  ItemValueSequence ivs;
  ivs.length(1);
  ivs[0].name  = "projectStatus";
  ivs[0].value = "INCOMPLETE";

  COUT("Here addTrial");
  return pdb->projectEdit(project.c_str(), obsblock.c_str(), "", nTrial, ivs, ESTATUS_EDIT);
}
#endif

bool remTrial(carma::observertools::ProjectDatabaseManager_var pdb, std::string project, std::string obsblock, short iTrial)
{
  short nTrial = getNTrial(pdb, project, obsblock);

  if(nTrial == 0) {
    ThrowError("No trial: " << iTrial << " exists");
  }

  ItemValueSequence ivs;
  ivs.length(1);
  ivs[0].name  = "trial";

  return pdb->projectEdit(project.c_str(), obsblock.c_str(), "", iTrial, ivs, ESTATUS_DELETE);
}

void editLastTrial(carma::observertools::ProjectDatabaseManager_var pdb, 
		   std::string project, std::string obsblock, 
		   std::string startLstStr, std::string stopLstStr, 
		   std::string comment, float grade, std::string source)
{
  short iTrial = getNTrial(pdb, project, obsblock);
  editTrial(pdb, project, obsblock, iTrial, startLstStr, stopLstStr, comment, grade, source);
}

void editTrial(carma::observertools::ProjectDatabaseManager_var pdb, 
	       std::string project, std::string obsblock, short iTrial,
	       std::string startLstStr, std::string stopLstStr, 
	       std::string comment, float grade, std::string source)
{
  std::ostringstream os;

  HourAngle startLst;
  startLst.setHours(startLstStr);

  HourAngle stopLst;
  stopLst.setHours(stopLstStr);

  HourAngle diff;
  diff = stopLst - startLst;

  float hours = (diff.seconds() / HourAngle::arcSecPerSec_) / 3600;

  //------------------------------------------------------------
  // Start modifying items here
  //------------------------------------------------------------

  ItemValueSequence ivs;
  ivs.length(5);

  //  ivs[0].name  = "trialStatus";
  //  ivs[0].value = "COMPLETE";

  os.str("");
  os << hours;
  ivs[0].name  = "trialObservationLength";
  ivs[0].value = os.str().c_str();

  os.str("");
  os << startLst.hours() << "," << stopLst.hours();
  ivs[1].name  = "trialObservedLST";
  ivs[1].value = os.str().c_str();

  os.str("");
  os << grade;
  ivs[2].name  = "obsGrade";
  ivs[2].value = os.str().c_str();

  ivs[3].name  = "comments";
  ivs[3].value = comment.c_str();

  TimeVal stopTime;
  stopTime.setToCurrentTime();
  os.str("");
  TimeVal startTime = stopTime;
  startTime.incrementSeconds(-hours*3600);
  
  os << getFITSdateString(startTime.getMjd())  
     << ","
     << getFITSdateString(stopTime.getMjd());

  COUT(os.str());

  ivs[4].name  = "trialObservationDate";
  ivs[4].value = os.str().c_str();

  if(!pdb->projectEdit(project.c_str(), obsblock.c_str(), "", iTrial, ivs, ESTATUS_EDIT)) {
    ThrowError("Failure in projectEdit");
  }
}

std::string getFITSdateString(double mjd)  
{
  // Get unix style time in seconds
  time_t secs = static_cast< time_t >(86400 * (mjd - 40587.0));
  struct tm* t;
  t = gmtime(&secs);
    
  const int  bufflen = 20;
  char buff[bufflen];

  // System call to convert time to string, encoded format in quotes
  strftime(buff, bufflen, "%Y-%m-%dT%H:%M:%S", t);
    
  return buff;
}

void printQuery(carma::observertools::ItemValueSequence& query)
{
  COUT("Calling projectQuery() with: ");
  for(unsigned i=0; i < query.length(); i++) {
    COUT("query[" << i << "].name = " << query[i].name << ", query[" << i << "].value = " << query[i].value);
  }
}
