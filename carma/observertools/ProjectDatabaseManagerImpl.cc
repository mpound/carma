#include <carma/observertools/ProjectDatabaseManagerImpl.h>
#include <carma/observertools/PDB_Validator.h>
#include <carma/observertools/PDB_MongoDB.h>
#include <carma/observertools/PDB_Script.h>
#include <carma/observertools/PDB_Grade.h>
#include <carma/observertools/PDB_Query.h>
#include <carma/observertools/PDB_Edit.h>
#include <carma/observertools/PDB_Util.h>
#include <carma/observertools/PDB_Run.h>
#include <carma/observertools/PDB_Add.h>

#include <carma/util/ScopedPthreadMutexLock.h>
#include <carma/util/corbaSequenceUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
#include <carma/util/TimedBenchmark.h>
#include <carma/util/ScopedLogNdc.h>

#include <boost/foreach.hpp>

#include <sstream>
#include <memory>
#include <string>
#include <vector>

using namespace carma::observertools;
using namespace carma::util;

/* -------------------------------------------------------------------------- */
/* Helper Functions                                                           */
/* -------------------------------------------------------------------------- */

/* Convert individual request parameters into the standard ID tuple form */
static std::string getIDTuple(
		const std::string &projectID,
		const std::string &obsblockID,
		const std::string &subObsblockID)
{
	std::ostringstream oss;
	oss << projectID << "." << obsblockID;

	if (!subObsblockID.empty()) {
		oss << "." << subObsblockID;
	}

	return oss.str();
}

static std::string getIDTuple(
		const std::string &projectID,
		const std::string &obsblockID,
		const std::string &subObsblockID,
		const short trialID)
{
	std::ostringstream oss;
	oss << projectID << "." << obsblockID;

	if (!subObsblockID.empty()) {
		oss << "." << subObsblockID;
	}

	oss << "." << trialID;
	return oss.str();
}

static std::string floatToString(const float f)
{
	std::ostringstream oss;
	oss << f;
	return oss.str();
}

/* -------------------------------------------------------------------------- */
/* Private isComissioning Implementation                                      */
/* -------------------------------------------------------------------------- */

/*
 * Check if a projectID is a commissioning project.
 *
 * TODO FIXME:
 *
 * This information is now contained in the Project IDL object in the
 * attribute "isCommissioning". We should query the database and grab the
 * Project object and check it directly, rather than hardcode a list of
 * strings here.
 */
static bool doIsCommissioning(const std::string &projectID)
{
	std::vector<std::string> commissioningProjects;
	commissioningProjects.push_back("ct");
	commissioningProjects.push_back("blank");
	commissioningProjects.push_back("test");
	commissioningProjects.push_back("fringe");
	commissioningProjects.push_back("base");
	commissioningProjects.push_back("vlbi");
	commissioningProjects.push_back("flux");
	commissioningProjects.push_back("opnt");
	commissioningProjects.push_back("rpnt");
	commissioningProjects.push_back("tilt");
	commissioningProjects.push_back("bandpass");

	BOOST_FOREACH(const std::string &commissioningProject, commissioningProjects) {
		if (projectID.find(commissioningProject) == 0) {
			return true;
		}
	}

	return false;
}

/* -------------------------------------------------------------------------- */
/* ProjectDatabaseManagerImpl Implementation                                  */
/* -------------------------------------------------------------------------- */

ProjectDatabaseManagerImpl::ProjectDatabaseManagerImpl(const struct PDBMArgs &args)
	: args_(args)
{
	// initialize the MongoDB client driver
	initializeDatabaseClientDriver();

	// add indexes to all collections
	const PDB_DB_Params db(args);
	addDatabaseIndexes(db);
}

void ProjectDatabaseManagerImpl::checkDatabase() const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::checkDatabase");
	TimedBenchmark timer;

	try {
		programLogInfoIfPossible("start check");
		timer.start();

		const PDB_DB_Params db(args_);

		// lock to prevent concurrent database access
		const ScopedPthreadMutexLock lock(mutex_);

		// now that we have acquired exclusive database access, notify
		// the monitor thread that we have started
		const PDB_Scoped_Monitor scopedMonitor(args_.monitor, "checkDatabase");

		// check the database
		const unsigned int failCount = checkEntireDatabase(db);
		if (failCount > 0) {
			std::ostringstream oss;
			oss << "Database validation failed: " << failCount
				<< " records failed to validate. See logs for details.";
			throw CARMA_ERROR(oss.str());
		}

		timer.stop();
		programLogInfoIfPossible("finish check: " + timer.print());
	} catch (...) {
		timer.stop();
		logCaughtAsErrorAndRethrowAsUser("caught exception after "
				+ timer.print() + ": ");
	}
}

ProjectSequence *ProjectDatabaseManagerImpl::projectQuery(
		const ItemValueSequence &theQuery) const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::projectQuery("
			+ itemValueSequenceToString(theQuery)
			+ ")");
	TimedBenchmark timer;

	try {
		programLogInfoIfPossible("start query");
		timer.start();

		const PDB_DB_Params db(args_);
		const PDB_Query query(db, theQuery);

		// lock to prevent concurrent database access
		const ScopedPthreadMutexLock lock(mutex_);

		// now that we have acquired exclusive database access, notify
		// the monitor thread that we have started
		const PDB_Scoped_Monitor scopedMonitor(args_.monitor, "projectQuery");

		// query the database
		const std::vector<Project> results = query.run();

		ProjectSequence_var projectSequence(new ProjectSequence(
			convertVectorToSequence<ProjectSequence>(results)));

		timer.stop();
		programLogInfoIfPossible("finish query: " + timer.print());

		// the _retn() function releases the reference to the parent scope
		return projectSequence._retn();
	} catch (...) {
		timer.stop();
		logCaughtAsErrorAndRethrowAsUser("caught exception after "
				+ timer.print() + ": ");
		return NULL; // stifle compiler warning
	}
}

void ProjectDatabaseManagerImpl::projectQueryInOut(
		const ItemValueSequence &theQuery,
		ProjectSequence_out pSeq) const
{
	// No locking needed, the locking is accomplished in projectQuery()
	pSeq = projectQuery(theQuery);
}

StringSequence *ProjectDatabaseManagerImpl::projectOscriptFind(
		const char *projectID,
		const char *obsblock,
		const char *subObsblock) const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::projectOscriptFind("
			+ getIDTuple(projectID, obsblock, subObsblock)
			+ ")");
	TimedBenchmark timer;

	try {
		programLogInfoIfPossible("start find script");
		timer.start();

		PDB_Script_Params params;
		params.projectID = projectID;
		params.obsblockID = obsblock;
		params.subObsblockID = subObsblock;

		const PDB_DB_Params db(args_);
		const PDB_Script script(db, params);

		// lock to prevent concurrent database access
		const ScopedPthreadMutexLock lock(mutex_);

		// now that we have acquired exclusive database access, notify
		// the monitor thread that we have started
		const PDB_Scoped_Monitor scopedMonitor(args_.monitor, "projectOscriptFind");

		// find the script in the database
		const PDB_Script_Return result = script.get();

		// we return in the order [script, catalog]
		StringSequence_var seq(new StringSequence());
		seq->length(2);
		(*seq)[0] = result.script.c_str();
		(*seq)[1] = result.catalog.c_str();

		timer.stop();
		programLogInfoIfPossible("finish find script: " + timer.print());

		// the _retn() function releases the reference to the parent scope
		return seq._retn();
	} catch (...) {
		timer.stop();
		logCaughtAsErrorAndRethrowAsUser("caught exception after "
				+ timer.print() + ": ");
		return NULL; // stifle compiler warning
	}
}

void ProjectDatabaseManagerImpl::projectOscriptAdd(
		const char *projectID,
		const char *obsblock,
		const char *subObsblock,
		const char *scriptFile,
		const char *catalogFile) const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::projectOscriptAdd("
			+ getIDTuple(projectID, obsblock, subObsblock)
			+ ")");
	TimedBenchmark timer;

	try {
		programLogInfoIfPossible("start add script");
		timer.start();

		PDB_Script_Params params;
		params.projectID = projectID;
		params.obsblockID = obsblock;
		params.subObsblockID = subObsblock;

		const PDB_DB_Params db(args_);
		const PDB_Script script(db, params);

		// lock to prevent concurrent database access
		const ScopedPthreadMutexLock lock(mutex_);

		// now that we have acquired exclusive database access, notify
		// the monitor thread that we have started
		const PDB_Scoped_Monitor scopedMonitor(args_.monitor, "projectOscriptAdd");

		// add the script to the database
		const short trialID = script.findLastTrialID();
		script.put(scriptFile, catalogFile, trialID);

		timer.stop();
		programLogInfoIfPossible("finish add script: " + timer.print());
	} catch (...) {
		timer.stop();
		logCaughtAsErrorAndRethrowAsUser("caught exception after "
				+ timer.print() + ": ");
	}
}

CORBA::Boolean ProjectDatabaseManagerImpl::projectEdit(
		const char *projectID,
		const char *obsblock,
		const char *subObsblock,
		const CORBA::Short trial,
		const ItemValueSequence &editItems,
		const EditStatus action) const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::projectEdit("
			+ getIDTuple(projectID, obsblock, subObsblock, trial)
			+ ","
			+ itemValueSequenceToString(editItems)
			+ ","
			+ editStatusToString(action)
			+ ")");
	TimedBenchmark timer;

	try {
		programLogInfoIfPossible("start project edit");
		timer.start();

		PDB_Edit_Params params;
		params.projectID = projectID;
		params.obsblockID = obsblock;
		params.subObsblockID = subObsblock;
		params.trialID = trial;
		assignSequenceToVector(editItems, params.editItems);
		params.action = action;

		const PDB_DB_Params db(args_);
		const PDB_Edit edit(db, params);

		// lock to prevent concurrent database access
		const ScopedPthreadMutexLock lock(mutex_);

		// now that we have acquired exclusive database access, notify
		// the monitor thread that we have started
		const PDB_Scoped_Monitor scopedMonitor(args_.monitor, "projectEdit");

		// edit the database
		const bool result = edit.run();

		timer.stop();
		programLogInfoIfPossible("finish project edit: " + timer.print());

		return result;
	} catch (...) {
		timer.stop();
		logCaughtAsErrorAndRethrowAsUser("caught exception after "
				+ timer.print() + ": ");
		return false; // stifle compiler warning
	}
}

void ProjectDatabaseManagerImpl::projectEditInOut(
		const char *projectID,
		const char *obsblock,
		const char *subObsblock,
		const CORBA::Short trial,
		const ItemValueSequence &editItems,
		const EditStatus action,
		const CORBA::Boolean_out success) const
{
	// No locking needed, the locking is accomplished in projectEdit()
	success = projectEdit(projectID, obsblock, subObsblock, trial, editItems, action);
}

CORBA::Short ProjectDatabaseManagerImpl::runProject(
		const char *projectID,
		const char *obsblock,
		const char *subObsblock,
		const bool isCommissioning,
		const bool isDualCorr,
		const char *arrayConfig1,
		const char *arrayConfig2,
		const char *scriptFile,
		const char *catalogFile) const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::runProject("
			+ getIDTuple(projectID, obsblock, subObsblock)
			+ ")");
	TimedBenchmark timer;

	try {
		programLogInfoIfPossible("start run project");
		timer.start();

		PDB_Run_Params params;
		params.projectID = projectID;
		params.obsblockID = obsblock;
		params.subObsblockID = subObsblock;
		params.isCommissioning = isCommissioning;
		params.isDualCorr = isDualCorr;
		params.arrayConfig1 = arrayConfig1;
		params.arrayConfig2 = arrayConfig2;
		params.scriptFile = scriptFile;
		params.catalogFile = catalogFile;

		const PDB_DB_Params db(args_);
		const PDB_Run runner(db, params);

		// lock to prevent concurrent database access
		const ScopedPthreadMutexLock lock(mutex_);

		// now that we have acquired exclusive database access, notify
		// the monitor thread that we have started
		const PDB_Scoped_Monitor scopedMonitor(args_.monitor, "runProject");

		// get the trial number from the database
		const short trialID = runner.run();

		// log return value
		{
			std::ostringstream oss;
			oss << "return value: " << trialID;
			programLogInfoIfPossible(oss.str());
		}

		timer.stop();
		programLogInfoIfPossible("finish run project: " + timer.print());

		return trialID;
	} catch (...) {
		timer.stop();
		logCaughtAsErrorAndRethrowAsUser("caught exception after "
				+ timer.print() + ": ");
		return -1; // stifle compiler warning
	}
}

void ProjectDatabaseManagerImpl::runProjectInOut(
		const char *projectID,
		const char *obsblock,
		const char *subObsblock,
		const bool isCommissioning,
		const bool isDualCorr,
		const char *arrayConfig1,
		const char *arrayConfig2,
		const char *scriptFile,
		const char *catalogFile,
		CORBA::Short &trialID) const
{
	// No locking needed, the locking is accomplished in runProject()
	trialID = runProject(projectID, obsblock, subObsblock,
			isCommissioning, isDualCorr,
			arrayConfig1, arrayConfig2,
			scriptFile, catalogFile);
}

CORBA::Short ProjectDatabaseManagerImpl::isUp() const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::isUp");
	try {
		programLogInfoIfPossible("We're alive");
		return 1;
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser("caught exception: ");
		return 0; // stifle compiler warning
	}
}

CORBA::Boolean ProjectDatabaseManagerImpl::isCommissioning(
		const char *pid) const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::isCommissioning("
			+ std::string(pid)
			+ ")");

	try {
		programLogInfoIfPossible("start");

		// No locking needed, this code does not use the database
		const CORBA::Boolean result = doIsCommissioning(pid);

		{
			std::ostringstream oss;
			oss << "finish: result=" << result;
			programLogInfoIfPossible(oss.str());
		}

		return result;
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser("caught exception: ");
		return false; // stifle compiler warning
	}
}

void ProjectDatabaseManagerImpl::projectAdd(const char *fileName) const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::projectAdd");
	TimedBenchmark timer;

	try {
		programLogInfoIfPossible("start add: " + std::string(fileName));
		timer.start();

		// No locking needed, the locking is accomplished in projectAddAsString()
		const std::string xmlString = readFile(fileName);
		projectAddAsString(xmlString.c_str());

		timer.stop();
		programLogInfoIfPossible("finish add: " + timer.print());
	} catch (...) {
		timer.stop();
		logCaughtAsErrorAndRethrowAsUser("caught exception after "
				+ timer.print() + ": ");
	}
}

bool ProjectDatabaseManagerImpl::projectAddAsString(const char *xmlString) const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::projectAddAsString");
	TimedBenchmark timer;

	try {
		programLogInfoIfPossible("start add as string");
		timer.start();

		const PDB_DB_Params db(args_);
		const PDB_Add adder(db, xmlString);

		// lock to prevent concurrent database access
		const ScopedPthreadMutexLock lock(mutex_);

		// now that we have acquired exclusive database access, notify
		// the monitor thread that we have started
		const PDB_Scoped_Monitor scopedMonitor(args_.monitor, "projectAddAsString");

		// add the project(s) to the database
		const bool result = adder.run();

		timer.stop();
		programLogInfoIfPossible("finish add as string: " + timer.print());

		return result;
	} catch (...) {
		timer.stop();
		logCaughtAsErrorAndRethrowAsUser("caught exception after "
				+ timer.print() + ": ");

		return false; // stifle compiler warning
	}
}

char *ProjectDatabaseManagerImpl::gradeToLetter(float grade) const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::gradeToLetter("
			+ floatToString(grade)
			+ ")");

	// Disable logging by default, as this function is called quite often by
	// client code, and spams the logs with too many messages. This is pure
	// C++, and does not use the database at all.
	const bool enableLogging = false;

	try {
		if (enableLogging) {
			programLogInfoIfPossible("start");
		}

		// No locking needed, this code does not use the database
		const std::string letter = convertNumericGradeToLetter(grade);

		if (enableLogging) {
			programLogInfoIfPossible("finish: result=" + letter);
		}

		return CORBA::string_dup(letter.c_str());
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser("caught exception: ");
		return CORBA::string_dup(""); // stifle compiler warning
	}
}

float ProjectDatabaseManagerImpl::letterToGrade(const char *letter) const
{
	const ScopedLogNdc ndc("ProjectDatabaseManagerImpl::letterToGrade("
			+ std::string(letter)
			+ ")");

	// Disable logging by default, as this function is called quite often by
	// client code, and spams the logs with too many messages. This is pure
	// C++, and does not use the database at all.
	const bool enableLogging = false;

	try {
		if (enableLogging) {
			programLogInfoIfPossible("start");
		}

		// No locking needed, this code does not use the database
		const float grade = convertLetterGradeToNumeric(letter);

		if (enableLogging) {
			programLogInfoIfPossible("finish: result=" + floatToString(grade));
		}

		return grade;
	} catch (...) {
		logCaughtAsErrorAndRethrowAsUser("caught exception: ");
		return 0.0; // stifle compiler warning
	}
}

/* vim: set ts=4 sts=4 sw=4 noet: */
