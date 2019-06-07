/**
 * @version $Revision: 1.1 $
 * @usage @autogen
 *
 * @description
 *  Project Database Unit Tests. This utility will run against a real database
 *  and check that the various supported commands work as advertised.
 *
 *  By default, this program generates a test database with the name based on
 *  the UID and PID of this process. This is to allow multiple instances to run
 *  against the same database at the same time. This behavior can be overridden
 *  with command line options.
 *
 * @key hostname sdp.carma.pvt string
 * MongoDB Hostname
 *
 * @key port 27017 int
 * MongoDB Port
 *
 * @key database automatic string
 * MongoDB Database. Use "automatic" for automatic name generation, otherwise
 * specify the database you wish to use.
 *
 * WARNING: Any data in this database will be dropped! DO NOT RUN THIS AGAINST
 * A DATABASE WHERE YOU HAVE USEFUL DATA!
 *
 * @logger DEFAULT_FACILITY carma.observertools.Test.tPdbUnit
 */

#include <carma/observertools/PDB_Query.h>
#include <carma/observertools/PDB_Edit.h>
#include <carma/observertools/PDB_Util.h>
#include <carma/observertools/PDB_Add.h>
#include <carma/observertools/PDB_Run.h>

#include <carma/observertools/QueryTags.h>
#include <carma/observertools/EditTags.h>

#include <carma/util/corbaSequenceUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
#include <carma/util/Program.h>

#include <boost/algorithm/string/join.hpp>
#include <boost/foreach.hpp>

#include <iostream>
#include <string>
#include <vector>

#include <sys/types.h>
#include <unistd.h>

using namespace carma::observertools;
using namespace carma::util;

/* -------------------------------------------------------------------------- */
/* Helper Functions                                                           */
/* -------------------------------------------------------------------------- */

static std::string getDatabaseName(const std::string &name)
{
	if (name != "automatic") {
		return name;
	}

	const pid_t pid = getpid();
	const uid_t uid = geteuid();
	const struct passwd *pw = getpwuid(uid);

	std::ostringstream oss;
	oss << "test" << "_";

	if (pw) {
		oss << pw->pw_name;
	} else {
		oss << uid;
	}

	oss << "_" << pid;
	return oss.str();
}

/* -------------------------------------------------------------------------- */
/* CORBA Object Comparisons                                                   */
/* -------------------------------------------------------------------------- */

static std::string getName(const std::string &parent, const std::string &child)
{
	return parent + "." + child;
}

#if 0
static std::string getName(const std::string &parent, const std::string &child, const CORBA::ULong index)
{
	std::ostringstream oss;
	oss << parent << "." << child << "[" << index << "]";
	return oss.str();
}
#endif

template <typename T>
static void comparePrimitive(const T &lhs, const T &rhs, const std::string &name)
{
	if (lhs != rhs) {
		std::ostringstream oss;
		oss << "Objects are not equal: " << name
			<< " lhs=" << lhs
			<< " rhs=" << rhs;
		throw CARMA_ERROR(oss.str());
	}
}

// TAO String comparisons don't work as expected (they compare pointers to strings, not the string contents)
static void comparePrimitive(const TAO::String_Manager_T<char> &lhs, const TAO::String_Manager_T<char> &rhs, const std::string &name)
{
	const std::string mylhs(lhs);
	const std::string myrhs(rhs);
	comparePrimitive(mylhs, myrhs, name);
}

static void comparePrimitive(const double &lhs, const double &rhs, const std::string &name)
{
	const double epsilon = 0.0001;
	if (!(fabs(lhs - rhs) < epsilon)) {
		std::ostringstream oss;
		oss << "Doubles are not equal (within epsilon=" << epsilon << "): " << name
			<< " lhs=" << lhs
			<< " rhs=" << rhs;
		throw CARMA_ERROR(oss.str());
	}
}

// Simple macro helper to avoid lots of error-prone typing.
//
// Source: MYCOMPARE(sourceName);
// Expansion: comparePrimitive(lhs.sourceName, rhs.sourceName, name + "." + "sourceName");
//
// The "do while(0)" thing allows you to put more code inside the macro (if necessary
// in the future) without making any code changes where the macro is used.
#define MYCOMPARE(NAME) do {												\
	comparePrimitive(lhs.NAME, rhs.NAME, name + "." #NAME);					\
} while (0)

template <typename SEQTYPE, typename OBJTYPE>
static void compareSequence(
		const SEQTYPE &lhs,
		const SEQTYPE &rhs,
		const std::string &name,
		void (*ObjectCompareFn)(const OBJTYPE &lhs, const OBJTYPE &rhs, const std::string &name))
{
	if (lhs.length() != rhs.length()) {
		std::ostringstream oss;
		oss << "Sequences do not have the same length: " << name
			<< " lhs.length()=" << lhs.length()
			<< " rhs.length()=" << rhs.length();
		throw CARMA_ERROR(oss.str());
	}

	for (CORBA::ULong i = 0; i < lhs.length(); i++) {
		std::ostringstream oss;
		oss << name << "[" << i << "]";
		ObjectCompareFn(lhs[i], rhs[i], oss.str());
	}
}

static void compareShort(const short &lhs, const short &rhs, const std::string &name)
{
	return comparePrimitive(lhs, rhs, name);
}

static void compareDouble(const double &lhs, const double &rhs, const std::string &name)
{
	return comparePrimitive(lhs, rhs, name);
}

static void compareSource(const Source &lhs, const Source &rhs, const std::string &name)
{
	MYCOMPARE(sourceName);
	MYCOMPARE(ephemeris);
	MYCOMPARE(ra);
	MYCOMPARE(dec);
	MYCOMPARE(dataFile);
	MYCOMPARE(velocity);
	MYCOMPARE(veltype);
	MYCOMPARE(isSelfcalibratable);
	MYCOMPARE(observationLength);

	{
		const std::string seqName = getName(name, "correlatorSetup");
		compareSequence(lhs.correlatorSetup, rhs.correlatorSetup, seqName, compareShort);
	}
}

static void compareCalibrator(const Calibrator &lhs, const Calibrator &rhs, const std::string &name)
{
	MYCOMPARE(calibratorName);
	MYCOMPARE(calType);
	MYCOMPARE(ra);
	MYCOMPARE(dec);
	MYCOMPARE(dataFile);
	MYCOMPARE(observationLength);

	{
		const std::string seqName = getName(name, "correlatorSetup");
		compareSequence(lhs.correlatorSetup, rhs.correlatorSetup, seqName, compareShort);
	}
}

static void compareInvestigator(const Investigator &lhs, const Investigator &rhs, const std::string &name)
{
	MYCOMPARE(name);
	MYCOMPARE(email);
	MYCOMPARE(affiliation);
	MYCOMPARE(isUsAffil);
}

static void compareWindow(const Window &lhs, const Window &rhs, const std::string &name)
{
	MYCOMPARE(windowNumber);
	MYCOMPARE(bandwidth);
	MYCOMPARE(frequencyResolution);
	MYCOMPARE(numberOfChannels);
	MYCOMPARE(minFrequency);
	MYCOMPARE(maxFrequency);
}

static void compareCorrelator(const Correlator &lhs, const Correlator &rhs, const std::string &name)
{
	MYCOMPARE(setupNumber);
	MYCOMPARE(numberOfWindows);

	{
		const std::string seqName = getName(name, "window");
		compareSequence(lhs.window, rhs.window, seqName, compareWindow);
	}
}

static void compareTarget(const Target &lhs, const Target &rhs, const std::string &name)
{
	MYCOMPARE(molecule);
	MYCOMPARE(transition);
}

static void compareTrial(const Trial &lhs, const Trial &rhs, const std::string &name)
{
	MYCOMPARE(parentProject);
	MYCOMPARE(parentObsblock);
	MYCOMPARE(parentSubObsblock);
	MYCOMPARE(documentName);
	MYCOMPARE(trialID);
	MYCOMPARE(status);
	MYCOMPARE(trialObservationLength);
	MYCOMPARE(trialObservationDateStart);
	MYCOMPARE(trialObservationDateEnd);
	MYCOMPARE(observedLSTstart);
	MYCOMPARE(observedLSTend);
	MYCOMPARE(fastSwitch);
	MYCOMPARE(averagePhase);
	MYCOMPARE(averageOpacity);
	MYCOMPARE(dqaOverallGrade);
	MYCOMPARE(obsGrade);
	MYCOMPARE(numberOfPointings);

	{
		const std::string seqName = getName(name, "offsets");
		compareSequence(lhs.offsets, rhs.offsets, seqName, compareDouble);
	}

	MYCOMPARE(numberOfAntennas);

	{
		const std::string seqName = getName(name, "target");
		compareSequence(lhs.target, rhs.target, seqName, compareTarget);
	}

	{
		const std::string seqName = getName(name, "source");
		compareSequence(lhs.source, rhs.source, seqName, compareSource);
	}

	{
		const std::string seqName = getName(name, "calibrator");
		compareSequence(lhs.calibrator, rhs.calibrator, seqName, compareCalibrator);
	}

	{
		const std::string seqName = getName(name, "correlator");
		compareSequence(lhs.correlator, rhs.correlator, seqName, compareCorrelator);
	}

	MYCOMPARE(imgVsSnr);
	MYCOMPARE(maxGaincalTime);
	MYCOMPARE(maxGaincalRms);
	MYCOMPARE(maxTsys);
	MYCOMPARE(minNumberOfAntennas);
	MYCOMPARE(maxOpacity);
	MYCOMPARE(maxRmsPathLength);
	MYCOMPARE(maxDecorrelationRatio);
	MYCOMPARE(requiredSourceRms);
	MYCOMPARE(script);
	MYCOMPARE(catalog);
	MYCOMPARE(systemScripts);
	MYCOMPARE(scriptParameterization);
}

static void compareSubObsblock(const SubObsblock &lhs, const SubObsblock &rhs, const std::string &name)
{
	MYCOMPARE(parentProject);
	MYCOMPARE(parentObsblock);
	MYCOMPARE(documentName);
	MYCOMPARE(subObsblockID);
	MYCOMPARE(status);
	MYCOMPARE(subObsblockObservationTime);
	MYCOMPARE(lastTrial);

	{
		const std::string seqName = getName(name, "trial");
		compareSequence(lhs.trial, rhs.trial, seqName, compareTrial);
	}
}

static void compareObsblock(const Obsblock &lhs, const Obsblock &rhs, const std::string &name)
{
	MYCOMPARE(parentProject);
	MYCOMPARE(documentName);
	MYCOMPARE(obsblockID);
	MYCOMPARE(status);
	MYCOMPARE(exceedTAC);
	MYCOMPARE(minAllocatedTime);
	MYCOMPARE(maxAllocatedTime);
	MYCOMPARE(priority);
	MYCOMPARE(likelihood);
	MYCOMPARE(totalObsTime);
	MYCOMPARE(remainingTime);
	MYCOMPARE(reqLowHourAngleCoverage);
	MYCOMPARE(reqHiHourAngleCoverage);
	MYCOMPARE(lowRa);
	MYCOMPARE(highRa);
	MYCOMPARE(actualHourAngleCoverage);
	MYCOMPARE(observationType);
	MYCOMPARE(receiverBand);
	MYCOMPARE(restFrequency);
	MYCOMPARE(arrayConfiguration);
	MYCOMPARE(isFlex);

	{
		const std::string seqName = getName(name, "subObsblock");
		compareSequence(lhs.subObsblock, rhs.subObsblock, seqName, compareSubObsblock);
	}
}

static void compareProject(const Project &lhs, const Project &rhs, const std::string &name)
{
	MYCOMPARE(projectID);
	MYCOMPARE(status);
	MYCOMPARE(proposalTerm);
	MYCOMPARE(totalTime);
	MYCOMPARE(title);
	MYCOMPARE(numberOfInvestigators);
	compareInvestigator(lhs.primaryInvestigator, rhs.primaryInvestigator, getName(name, "primaryInvestigator"));

	{
		const std::string seqName = getName(name, "coInvestigator");
		compareSequence(lhs.coInvestigator, rhs.coInvestigator, seqName, compareInvestigator);
	}

	MYCOMPARE(isTargetOfOpportunity);
	MYCOMPARE(isKeyProject);
	MYCOMPARE(isFastTrack);
	MYCOMPARE(isCommissioning);
	MYCOMPARE(category);
	MYCOMPARE(projectAbstract);

	{
		const std::string seqName = getName(name, "obsblock");
		compareSequence(lhs.obsblock, rhs.obsblock, seqName, compareObsblock);
	}
}

/* -------------------------------------------------------------------------- */
/* Helper Methods                                                             */
/* -------------------------------------------------------------------------- */

static void checkDatabaseIsMostlyEmpty(const PDB_DB_Params &db)
{
	static const unsigned long long MAX_OBJECTS = 20;
	const DBClientConnectionPtr conn = db.conn;
	unsigned long long count = 0;

	count += conn->count(db.PROJECTS);
	count += conn->count(db.OBSBLOCKS);
	count += conn->count(db.SUBOBSBLOCKS);
	count += conn->count(db.TRIALS);
	count += conn->count(db.SCRIPTS);

	if (count > MAX_OBJECTS) {
		std::ostringstream oss;
		oss << "Found too many objects in database:"
			<< " " << db.DATABASE_NAME
			<< " max=" << MAX_OBJECTS
			<< " actual=" << count;
		throw CARMA_ERROR(oss.str());
	}
}

static void dropTestDatabase(const PDB_DB_Params &db)
{
	db.conn->dropDatabase(db.DATABASE_NAME);
}

static void createTestDatabase(const PDB_DB_Params &db)
{
	const DBClientConnectionPtr conn = db.conn;

	// ensure the database is mostly empty (so we don't drop the production data)
	checkDatabaseIsMostlyEmpty(db);

	// drop the database
	dropTestDatabase(db);

	// create the collections
	conn->createCollection(db.PROJECTS);
	conn->createCollection(db.OBSBLOCKS);
	conn->createCollection(db.SUBOBSBLOCKS);
	conn->createCollection(db.TRIALS);
	conn->createCollection(db.SCRIPTS);

	// add proper indexes
	addDatabaseIndexes(db);

	// load in the test data
	const std::string filename = Program::getConfFile("observertools/c1185.xml");
	const std::string xmlContents = readFile(filename);
	PDB_Add adder(db, xmlContents);
	if (!adder.run()) {
		std::ostringstream oss;
		oss << "Unable to add test database data from file: " << filename;
		throw CARMA_ERROR(oss.str());
	}
}

static Project getProject(const PDB_DB_Params &db)
{
	std::vector<ItemValue> values;
	values.push_back(makeItemValue(QueryTags::PROJECT, "c1185"));
	const PDB_Query query(db, values);
	const std::vector<Project> results = query.run();

	if (results.size() != 1) {
		std::ostringstream oss;
		oss << "Unexpected vector length returned from query: " << results.size();
		throw CARMA_ERROR(oss.str());
	}

	return results.at(0);
}

static void checkChangedProject(const PDB_DB_Params &db, const Project &reference, const std::string &testname)
{
	std::cout << "Test: " << testname << " -> " << std::flush;

	try {
		const Project edited = getProject(db);
		compareProject(reference, edited, "Project");
	} catch (...) {
		std::cout << "FAIL" << std::endl;
		throw;
	}

	std::cout << "PASS" << std::endl;
}

/* -------------------------------------------------------------------------- */
/* Main Program Code                                                          */
/* -------------------------------------------------------------------------- */

static void runEdit(
		const PDB_DB_Params &db,
		PDB_Edit_Params &params,
		const std::vector<ItemValue> &values,
		const Project &reference)
{
	params.editItems = values;

	const PDB_Edit edit(db, params);
	const bool result = edit.run();

	if (!result) {
		std::ostringstream oss;
		oss << "Edit failed to run: " << itemValueVectorToString(values);
		throw CARMA_ERROR(oss.str());
	}

	checkChangedProject(db, reference, "projectEdit " + itemValueVectorToString(values));
}

static void runEdit(
		const PDB_DB_Params &db,
		PDB_Edit_Params &params,
		const ItemValue &iv,
		const Project &reference)
{
	std::vector<ItemValue> values;
	values.push_back(iv);

	runEdit(db, params, values, reference);
}

static void runEditTests(const PDB_DB_Params &db)
try {
	// create the test database
	createTestDatabase(db);

	// Grab the reference project. We will modify it with what we expect ourselves
	// as we go along, and use the equality checker to verify that the edits worked.
	Project reference = getProject(db);

	// Setup the edit parameters for testing simple project-only edits
	PDB_Edit_Params params;
	params.projectID = "c1185";
	params.obsblockID = "none";
	params.subObsblockID = "none";
	params.trialID = 0;
	params.action = ESTATUS_EDIT;

	// check simple project-only edits
	reference.proposalTerm = "2014b";
	runEdit(db, params, makeItemValue(EditTags::PROPOSALTERM, "2014b"), reference);

	reference.primaryInvestigator.affiliation = "bogus university";
	runEdit(db, params, makeItemValue(EditTags::PIINSTITUTION, "bogus university"), reference);

	reference.primaryInvestigator.email = "bogus@example.com";
	runEdit(db, params, makeItemValue(EditTags::PIEMAIL, "bogus@example.com"), reference);

	reference.isTargetOfOpportunity = true;
	runEdit(db, params, makeItemValue(EditTags::TOO, "true"), reference);

	reference.isKeyProject = true;
	runEdit(db, params, makeItemValue(EditTags::KEYPROJECT, "true"), reference);

	// check simple obsblock-only edits
	params.obsblockID = "1B_95G343";

	reference.obsblock[0].likelihood = LIKELIHOOD_B;
	runEdit(db, params, makeItemValue(EditTags::LIKELIHOOD, "B"), reference);

	reference.obsblock[0].observationType = TYPE_PACS_DUALPOL;
	runEdit(db, params, makeItemValue(EditTags::OBSERVATIONTYPE, "PACS_DUALPOL"), reference);

	reference.obsblock[0].receiverBand = "1CM";
	runEdit(db, params, makeItemValue(EditTags::RECEIVERBAND, "1CM"), reference);

	reference.obsblock[0].restFrequency = 123.456;
	runEdit(db, params, makeItemValue(EditTags::RESTFREQUENCY, "123.456"), reference);

	reference.obsblock[0].arrayConfiguration = "A";
	runEdit(db, params, makeItemValue(EditTags::ARRAYCONFIGURATION, "A"), reference);

	reference.obsblock[0].isFlex = false;
	runEdit(db, params, makeItemValue(EditTags::ISFLEX, "false"), reference);

	// check simple trial-only edits
	params.subObsblockID = "";
	params.trialID = 1;

	reference.obsblock[0].subObsblock[0].trial[0].trialObservationLength = 123.456;
	runEdit(db, params, makeItemValue(EditTags::TRIALOBSERVATIONLENGTH, "123.456"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].observedLSTstart = 10.10;
	reference.obsblock[0].subObsblock[0].trial[0].observedLSTend = 12.12;
	runEdit(db, params, makeItemValue(EditTags::TRIALOBSERVEDLST, "10.10,12.12"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].trialObservationDateStart = "2014-09-02T01:02:03";
	reference.obsblock[0].subObsblock[0].trial[0].trialObservationDateEnd = "2014-09-02T04:05:06";
	runEdit(db, params, makeItemValue(EditTags::TRIALOBSERVATIONDATE, "2014-09-02T01:02:03,2014-09-02T04:05:06"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].numberOfPointings = 3;
	runEdit(db, params, makeItemValue(EditTags::NUMBEROFPOINTINGS, "3"), reference);

	{
		std::vector<double> offsets;
		offsets.push_back(3);
		offsets.push_back(2);
		offsets.push_back(1);

		assignVectorToSequence(offsets, reference.obsblock[0].subObsblock[0].trial[0].offsets);
		runEdit(db, params, makeItemValue(EditTags::POINTINGOFFSETS, "3,2,1"), reference);
	}

	reference.obsblock[0].subObsblock[0].trial[0].numberOfAntennas = 15;
	runEdit(db, params, makeItemValue(EditTags::NUMBEROFANTENNAS, "15"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].fastSwitch = true;
	runEdit(db, params, makeItemValue(EditTags::FASTSWITCH, "true"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].averagePhase = 11.11;
	runEdit(db, params, makeItemValue(EditTags::AVERAGEPHASE, "11.11"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].averageOpacity = 13.13;
	runEdit(db, params, makeItemValue(EditTags::AVERAGEOPACITY, "13.13"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].dqaOverallGrade = 95.59;
	runEdit(db, params, makeItemValue(EditTags::DQAOVERALLGRADE, "95.59"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].imgVsSnr = "IMG";
	runEdit(db, params, makeItemValue(EditTags::IMGVSSNR, "IMG"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].maxGaincalTime = 12.34;
	runEdit(db, params, makeItemValue(EditTags::GAINCALMAXTIME, "12.34"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].maxGaincalRms = 56.78;
	runEdit(db, params, makeItemValue(EditTags::GAINCALMAXRMS, "56.78"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].maxTsys = 99;
	runEdit(db, params, makeItemValue(EditTags::MAXSYSTEMTEMP, "99"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].minNumberOfAntennas = 18;
	runEdit(db, params, makeItemValue(EditTags::MINNUMBEROFANTENNAS, "18"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].maxOpacity = 90.12;
	runEdit(db, params, makeItemValue(EditTags::MAXOPACITY, "90.12"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].maxRmsPathLength = 34.56;
	runEdit(db, params, makeItemValue(EditTags::MAXRMSPATHLENGTH, "34.56"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].maxDecorrelationRatio = 0.67;
	runEdit(db, params, makeItemValue(EditTags::MAXDECORRELATIONRATIO, "0.67"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].requiredSourceRms = 89.01;
	runEdit(db, params, makeItemValue(EditTags::REQUIREDSOURCERMS, "89.01"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].systemScripts = "system scripts";
	runEdit(db, params, makeItemValue(EditTags::SYSTEMSCRIPTS, "system scripts"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].scriptParameterization = "script parameterization";
	runEdit(db, params, makeItemValue(EditTags::SCRIPTPARAMETERIZATION, "script parameterization"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].obsGrade = 100.0;
	runEdit(db, params, makeItemValue(EditTags::OBSGRADE, "A+"), reference);

	reference.obsblock[0].subObsblock[0].trial[0].obsComments = "I love comments";
	runEdit(db, params, makeItemValue(EditTags::COMMENTS, "I love comments"), reference);

	{
		reference.obsblock[0].subObsblock[0].trial[0].source.length(1);
		Source &source = reference.obsblock[0].subObsblock[0].trial[0].source[0];
		source.sourceName = "UnitTest";
		source.ephemeris = true;
		source.ra = 2.34;
		source.dec = 1.23;
		source.dataFile = "UnitTestDataFile";
		source.velocity = 44.44;
		source.veltype = "UnitTestVelType";
		source.isSelfcalibratable = true;
		source.observationLength = 55.55;
		source.correlatorSetup.length(0);

		// the hour angle coverage is automatically updated when changing the ra/dec
		reference.obsblock[0].reqLowHourAngleCoverage = -4.0;
		reference.obsblock[0].reqHiHourAngleCoverage = 4.0;

		// the obsblock RA/DEC is automatically updated when changing the ra/dec
		reference.obsblock[0].lowRa = 1.2928;
		reference.obsblock[0].highRa = 3.3872;

		// switch to REPLACE mode
		params.action = ESTATUS_REPLACE;

		std::vector<ItemValue> values;
		values.push_back(makeItemValue(EditTags::SOURCE, "UnitTest"));
		values.push_back(makeItemValue(EditTags::EPHEMERIS, "true"));
		values.push_back(makeItemValue(EditTags::SRCRA, "2.34"));
		values.push_back(makeItemValue(EditTags::SRCDEC, "1.23"));
		values.push_back(makeItemValue(EditTags::SRCFILE, "UnitTestDataFile"));
		values.push_back(makeItemValue(EditTags::VELOCITY, "44.44"));
		values.push_back(makeItemValue(EditTags::VELTYPE, "UnitTestVelType"));
		values.push_back(makeItemValue(EditTags::SELFCALIBRATABLE, "true"));
		values.push_back(makeItemValue(EditTags::SRCOBSERVATIONLENGTH, "55.55"));

		runEdit(db, params, values, reference);
	}

	// drop the database on success
	dropTestDatabase(db);

} catch (...) {
	// drop the database on failure
	dropTestDatabase(db);

	// rethrow the exception
	throw;
}

static std::string getID(const PDB_Run_Params &params)
{
	std::ostringstream oss;
	oss << params.projectID << "." << params.obsblockID;
	if (!params.subObsblockID.empty()) {
		oss << "." << params.subObsblockID;
	}

	std::vector<std::string> options;
	if (params.isDualCorr) {
		options.push_back("dualcorr");
	}

	if (params.isCommissioning) {
		options.push_back("commissioning");
	}

	if (!options.empty()) {
		oss << " (" << boost::algorithm::join(options, ",") << ")";
	}

	return oss.str();
}

static void checkRunResult(const PDB_DB_Params &db, const PDB_Run_Params &params, const short expectedTID)
{
	std::cout << "Test: runProject " << getID(params)
		<< " (expect tid=" << expectedTID << ") -> " << std::flush;

	try {
		const PDB_Run runner(db, params);
		const short resultTID = runner.run();

		if (resultTID == expectedTID) {
			std::cout << "PASS" << std::endl;
		} else {
			std::ostringstream oss;
			oss << "expected tid=" << expectedTID << " actual tid=" << resultTID;
			throw CARMA_ERROR(oss.str());
		}
	} catch (...) {
		std::cout << "FAIL" << std::endl;
		throw;
	}
}

static void runRunTests(const PDB_DB_Params &db)
try {
	// create the test database
	createTestDatabase(db);

	// Setup the edit parameters for testing simple project-only edits
	PDB_Run_Params params;
	params.projectID = "c1185";
	params.obsblockID = "1B_95G343";
	params.subObsblockID = "";
	params.isCommissioning = false;
	params.isDualCorr = false;
	params.arrayConfig1 = "B";
	params.arrayConfig2 = "B";
	params.scriptFile = "";
	params.catalogFile = "";

	// we should get a few sequential trial numbers here
	checkRunResult(db, params, 4);
	checkRunResult(db, params, 5);
	checkRunResult(db, params, 6);

	// In dual corr mode, we should get trial 1. No SL or WB obsblocks
	// are present in the test dataset.
	params.isDualCorr = true;
	checkRunResult(db, params, 1);
	checkRunResult(db, params, 2);

	// Now we switch to just the SL obsblock and get a few trials. The
	// WB obsblock trial number will not be incremented.
	params.isDualCorr = false;
	params.subObsblockID = "SL";
	checkRunResult(db, params, 3);
	checkRunResult(db, params, 4);
	checkRunResult(db, params, 5);
	checkRunResult(db, params, 6);

	// Now we switch to just the WB obsblock and get a trial. The
	// WB obsblock should get trial 3 at this point.
	params.subObsblockID = "WB";
	checkRunResult(db, params, 3);
	checkRunResult(db, params, 4);

	// Now we have a "hole" in the trial numbers. The SL lastTrial == 6
	// while the WB lastTrial == 4. In dual corr mode, the SL + WB obsblocks
	// should return an identical highest non-used trial number, which should
	// be trial 7 in this case.
	params.isDualCorr = true;
	params.subObsblockID = "";
	checkRunResult(db, params, 7);
	checkRunResult(db, params, 8);

	// Now we can go back and make sure that the PDB does not try to "fill in"
	// the hole we created in the WB obsblocks. (WB trial 5 and trial 6 are unused).
	params.isDualCorr = false;
	params.subObsblockID = "WB";
	checkRunResult(db, params, 9);

	// drop the database on success
	dropTestDatabase(db);

} catch (...) {
	// drop the database on failure
	dropTestDatabase(db);

	// rethrow the exception
	throw;
}

int carma::util::Program::main()
try {
	struct PDBMArgs args;
	args.hostname = getStringParameter("hostname");
	args.port = getIntParameter("port");
	args.database = getDatabaseName(getStringParameter("database"));

	std::cerr << "Using Database:"
		<< " " << args.hostname << ":" << args.port
		<< " " << args.database
		<< std::endl;

	args.monitor = PDB_Monitor_Ptr(new PDB_Monitor());

	// initialize the mongodb database client driver
	initializeDatabaseClientDriver();

	// run the test code
	const PDB_DB_Params db(args);
	runEditTests(db);
	runRunTests(db);

	return EXIT_SUCCESS;
} catch (...) {
	std::ostringstream oss;
	oss << "FAIL: caught exception: " << getStringForCaught();
	std::cerr << oss.str() << std::endl;
	programLogErrorIfPossible(oss.str());
	return EXIT_FAILURE;
}

/* vim: set ts=4 sts=4 sw=4 noet: */
