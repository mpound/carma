/** @file
 * binary to input quality grades to the project database manager
 * @usage qualityGrade project=pid.obs.sub.trial opacity=tauGrade phase=rmsGrade grade=grade
 *
 * @key project "" s the totally qualified obsblock name
 * @key opacity 0.0 d opacity grade
 * @key phase 0.0 d phase rms grade
 * @key grade -1.0 d overall grade
 * @key obslen -1.0 d length of observations in hours
 *
 * @logger DEFAULT_FACILITY carma.obervertools.projectDatabaseManager
 *
 * @author Douglas N. Friedel
 */

#include <sstream>
#include <iostream>
#include <iomanip>

//CARMA includes
#include "carma/corba/corba.h"
#include "carma/corba/Client.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/observertools/ProjectDatabaseManagerImpl.h"

using namespace std;
using namespace carma::observertools;
using namespace carma::util;
using namespace carma;

ProjectDatabaseManager_var getPDM(){
    corba::Client & client = Program::getProgram().getCorbaClient();
    ProjectDatabaseManager_var pdm =
    client.resolveName<ProjectDatabaseManager>(
        "carma.projectDatabaseManager.projectDatabaseManagerControl");
    if(CORBA::is_nil(pdm)){
        cerr << "System reference not found for ProjectDatabaseManager." 
             << endl;
        exit(EXIT_FAILURE);
    }
    return pdm;
}

int carma::util::Program::main(){
    try{
	const std::string pid = getStringParameter("project");
	const double opacity = getDoubleParameter("opacity");
	const double phase = getDoubleParameter("phase");
	const double grade = getDoubleParameter("grade");
	const double obslen = getDoubleParameter("obslen");
	if(grade < 0){
	    cerr << "Invalid grade value given" << endl;
	    return EXIT_FAILURE;
	}
	if(obslen < 0){
	    cerr << "Invalid obslen value given" << endl;
	    return EXIT_FAILURE;
	}
	ProjectDatabaseManager_var pdm = getPDM();
// parse the project into project, obsblock, subObsblock,trial
	std::string project;
	std::string obsblock;
	std::string subObsblock;
	CORBA::Short trial;
	std::string::size_type start = 0;
	std::string::size_type end = 0;
	std::string::size_type last = 0;
	bool valid = true;
	last = pid.find_last_of(".");
	end = pid.find(".",end);
	if((end = pid.find(".",end)) != std::string::npos){
	    project = pid.substr(0,end);
	    start = end + 1;
	    if((end = pid.find(".",start)) != std::string::npos){
		obsblock = pid.substr(start,end-start);
		start = end + 1;
// we have no subObsblock name
		if(start >= last){
		    subObsblock = "";
		}
// we do have a subobsblock name
		else{
		    subObsblock = pid.substr(start,last-start);
		}
		trial = static_cast<short>
		    (atoi(pid.substr(last+1).c_str()));
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
	    cerr << "Invalid project format" << endl;
	    return EXIT_FAILURE;
	}
	ItemValueSequence ivs;
	ivs.length(4);
	ostringstream tempStream;
	tempStream << setiosflags(ios_base::dec | ios_base::showpoint);
	tempStream << setprecision(6);
	tempStream << opacity;
	ivs[0].name = "averageOpacity";
	ivs[0].value = tempStream.str().c_str();
	tempStream.str("");
	tempStream << setiosflags(ios_base::dec | ios_base::showpoint);
	tempStream << setprecision(6);
	tempStream << phase;
	ivs[1].name = "averagePhase";
	ivs[1].value = tempStream.str().c_str();
	tempStream.str("");
	tempStream << setiosflags(ios_base::dec | ios_base::showpoint);
	tempStream << setprecision(4);
	tempStream << grade;
	ivs[2].name = "DQAOverallGrade";
	ivs[2].value = tempStream.str().c_str();
	ivs[3].name = "trialObservationLength";
	tempStream.str("");
	tempStream << obslen;
	ivs[3].value = tempStream.str().c_str();

	bool success;
    pdm->projectEditInOut(project.c_str(),obsblock.c_str(),
                          subObsblock.c_str(),trial,ivs,
                          ESTATUS_EDIT,success);
	if(!success){
	    //this should never happen
	    return EXIT_FAILURE;
	}
    }
    catch (const carma::util::ErrorException &e) {
        cerr << "qualityGrade - Util exception " << e << endl;
	return EXIT_FAILURE;
    }
    catch (const carma::observertools::ProjectDatabaseException &e){
	cerr << "qualityGrade - PDM exception " << e << endl;
	cerr << e.errorMsg << endl;
	return EXIT_FAILURE;
    }
    catch(const CORBA::SystemException &e){
	cerr << "qualityGrade - CORBA Exception: " << e << endl;
	return EXIT_FAILURE;
    }
    catch(const carma::util::UserException &e){
	cerr << "qualityGrade - Util Exception: " << e << endl;
    }
    catch (...) {
        cerr << "Unknown exception caught in qualityGrade::Program::main()"
	     << endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
