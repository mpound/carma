/*
 * CARMA Project Database BSON <-> CORBA Conversion
 */

#include <carma/observertools/PDB_BSON_Convert.h>
#include <carma/observertools/PDB_Validator.h>
#include <carma/observertools/PDB_Enum.h>

#include <carma/util/corbaSequenceUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

#include <mongo/util/time_support.h>

#include <boost/foreach.hpp>

#include <algorithm>
#include <sstream>
#include <string>
#include <vector>

using namespace carma::observertools;
using namespace carma::util;

/* -------------------------------------------------------------------------- */
/* Helper Functions                                                           */
/* -------------------------------------------------------------------------- */

// Assign a std::string to a CORBA string member
static void corbaAssign(TAO::String_Manager_T<char> &corba, const mongo::BSONElement &elem)
{
	if (elem.type() != mongo::String) {
		std::ostringstream oss;
		oss << "expected BSON string, but got: " << elem.type()
			<< " fieldName: " << elem.fieldName();
		throw CARMA_ERROR(oss.str());
	}

	corba = elem.str().c_str();
}

// Sort CORBA Trial using trialID integer
static bool trialid_compare(const Trial &t1, const Trial &t2)
{
	return t1.trialID < t2.trialID;
}

/* -------------------------------------------------------------------------- */
/* Convert Objects from BSON into CORBA                                       */
/* -------------------------------------------------------------------------- */

// Convert BSON project status string into CORBA enumeration
static ProjectStatus convertBSONProjectStatus(const std::string &value)
{
	const StringEnumMap m = getProjectStatusMap();
	const StringEnumMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		std::ostringstream oss;
		oss << "unable to convert ProjectStatus: " << value;
		throw CARMA_ERROR(oss.str());
	}

	return static_cast<ProjectStatus>(it->second);
}

// Convert BSON ObsCategory string into CORBA enumeration
static ObsCategory convertBSONObsCategory(const std::string &value)
{
	const StringEnumMap m = getObsCategoryMap();
	const StringEnumMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		std::ostringstream oss;
		oss << "unable to convert ObsCategory: " << value;
		throw CARMA_ERROR(oss.str());
	}

	return static_cast<ObsCategory>(it->second);
}

// Convert a mongo::BSONObj investigator object into a CORBA Investigator
static Investigator convertBSONInvestigator(const mongo::BSONObj &obj)
{
	Investigator investigator;

	corbaAssign(investigator.name, obj["name"]);
	corbaAssign(investigator.email, obj["email"]);
	corbaAssign(investigator.affiliation, obj["affiliation"]);
	investigator.isUsAffil = obj["US"].boolean();

	return investigator;
}

// Convert the BSON CoI array into CORBA
static std::vector<Investigator> convertBSONCoInvestigators(const std::vector<mongo::BSONElement> &elements)
{
	std::vector<Investigator> coi;

	BOOST_FOREACH(const mongo::BSONElement &elem, elements) {
		coi.push_back(convertBSONInvestigator(elem.embeddedObject()));
	}

	return coi;
}

// Convert BSON ObsLikelihood String into CORBA ObsLikelihood Enum
static ObsLikelihood convertBSONObsLikelihood(const std::string &value)
{
	const StringEnumMap m = getObsLikelihoodMap();
	const StringEnumMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		std::ostringstream oss;
		oss << "unable to convert ObsLikelihood: " << value;
		throw CARMA_ERROR(oss.str());
	}

	return static_cast<ObsLikelihood>(it->second);
}

// Convert BSON ObsType String into CORBA ObsType Enum
static ObsType convertBSONObsType(const std::string &value)
{
	const StringEnumMap m = getObsTypeMap();
	const StringEnumMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		std::ostringstream oss;
		oss << "unable to convert ObsType: " << value;
		throw CARMA_ERROR(oss.str());
	}

	return static_cast<ObsType>(it->second);
}

// Convert BSON Date Object into CORBA Date String
static std::string convertBSONDate(const mongo::BSONElement &elem)
{
	if (elem.type() != mongo::Date) {
		std::ostringstream oss;
		oss << "expected BSON Date, but got: " << elem.type()
			<< " fieldName: " << elem.fieldName();
		throw CARMA_ERROR(oss.str());
	}

	const mongo::Date_t mongodate(elem.date());
	const time_t tt = (mongodate.asInt64() / 1000);

	struct tm date;
	memset(&date, 0, sizeof(date));
	if (gmtime_r(&tt, &date) == NULL) {
		std::ostringstream oss;
		oss << "gmtime_r failed: " << strerror(errno);
		throw CARMA_ERROR(oss.str());
	}

	char buf[1024];
	const size_t ret = strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%S", &date);
	if (ret == 0) {
		std::ostringstream oss;
		oss << "unable to format date: " << mongodate.asInt64();
		throw CARMA_ERROR(oss.str());
	}

	return std::string(buf);
}

// Convert BSON Targets Array into CORBA Targets Array
static std::vector<Target> convertBSONTargets(const std::vector<mongo::BSONElement> &elements)
{
	std::vector<Target> targets;

	BOOST_FOREACH(const mongo::BSONElement &elem, elements) {
		Target target;
		corbaAssign(target.molecule, elem["molecule"]);
		corbaAssign(target.transition, elem["transition"]);
		targets.push_back(target);
	}

	return targets;
}

// Convert BSON Integer Array into CORBA Integer Array
static std::vector<int> convertBSONIntSequence(const std::vector<mongo::BSONElement> &elements)
{
	std::vector<int> results;

	BOOST_FOREACH(const mongo::BSONElement &elem, elements) {
		results.push_back(elem.numberInt());
	}

	return results;
}

// Convert BSON Double Array into CORBA Double Array
static std::vector<double> convertBSONDoubleSequence(const std::vector<mongo::BSONElement> &elements)
{
	std::vector<double> results;

	BOOST_FOREACH(const mongo::BSONElement &elem, elements) {
		results.push_back(elem.numberDouble());
	}

	return results;
}

// Convert BSON Source Objects into CORBA Source Objects
static std::vector<Source> convertBSONSources(const std::vector<mongo::BSONElement> &elements)
{
	std::vector<Source> sources;

	BOOST_FOREACH(const mongo::BSONElement &elem, elements) {
		Source source;
		corbaAssign(source.sourceName, elem["sourceName"]);
		source.ephemeris = elem["ephemeris"].boolean();
		source.ra = elem["RA"].numberDouble();
		source.dec = elem["DEC"].numberDouble();
		corbaAssign(source.dataFile, elem["file"]);
		source.velocity = elem["velocity"].numberDouble();
		corbaAssign(source.veltype, elem["veltype"]);
		source.isSelfcalibratable = elem["selfcalibratable"].boolean();
		source.observationLength = elem["observationLength"].numberDouble();

		{
			const std::vector<int> correlatorSetup = convertBSONIntSequence(elem["correlatorSetup"].Array());
			assignVectorToSequence(correlatorSetup, source.correlatorSetup);
		}

		sources.push_back(source);
	}

	return sources;
}

// Convert BSON Calibrator Objects into CORBA Calibrator Objects
static std::vector<Calibrator> convertBSONCalibrators(const std::vector<mongo::BSONElement> &elements)
{
	std::vector<Calibrator> calibrators;

	BOOST_FOREACH(const mongo::BSONElement &elem, elements) {
		Calibrator calibrator;
		corbaAssign(calibrator.calibratorName, elem["calibratorName"]);
		corbaAssign(calibrator.calType, elem["type"]);
		calibrator.ra = elem["RA"].numberDouble();
		calibrator.dec = elem["DEC"].numberDouble();
		corbaAssign(calibrator.dataFile, elem["file"]);
		calibrator.observationLength = elem["observationLength"].numberDouble();

		{
			const std::vector<int> correlatorSetup = convertBSONIntSequence(elem["correlatorSetup"].Array());
			assignVectorToSequence(correlatorSetup, calibrator.correlatorSetup);
		}

		calibrators.push_back(calibrator);
	}

	return calibrators;
}

// Convert BSON Window Objects into CORBA Window Objects
static std::vector<Window> convertBSONWindows(const std::vector<mongo::BSONElement> &elements)
{
	std::vector<Window> windows;

	BOOST_FOREACH(const mongo::BSONElement &elem, elements) {
		Window window;

		window.windowNumber = elem["windowNumber"].numberInt();
		window.bandwidth = elem["bandwidth"].numberDouble();
		window.frequencyResolution = elem["resolution"].numberDouble();
		window.numberOfChannels = elem["numberOfChannels"].numberInt();
		window.minFrequency = elem["minfreq"].numberDouble();
		window.maxFrequency = elem["maxfreq"].numberDouble();

		windows.push_back(window);
	}

	return windows;
}

// Convert BSON Correlator Objects into CORBA Correlator Objects
static std::vector<Correlator> convertBSONCorrelators(const std::vector<mongo::BSONElement> &elements)
{
	std::vector<Correlator> correlators;

	BOOST_FOREACH(const mongo::BSONElement &elem, elements) {
		Correlator correlator;
		correlator.setupNumber = elem["setupNumber"].numberInt();
		correlator.numberOfWindows = elem["numberOfWindows"].numberInt();

		{
			const std::vector<Window> windows = convertBSONWindows(elem["window"].Array());
			assignVectorToSequence(windows, correlator.window);
		}

		correlators.push_back(correlator);
	}

	return correlators;
}

// Convert a single BSON Project into a CORBA Project
static Project convertBSONProject(const mongo::BSONObj &obj)
{
	Project proj;

	corbaAssign(proj.projectID, obj["projectID"]);
	proj.status = convertBSONProjectStatus(obj["projectStatus"].str());
	corbaAssign(proj.proposalTerm, obj["callForProposals"]["term"]);
	proj.totalTime = obj["totalTime"].numberDouble();
	corbaAssign(proj.title, obj["title"]);

	{
		const mongo::BSONElement &inv = obj["investigators"];
		proj.numberOfInvestigators = inv["numberOfInvestigators"].numberInt();
		proj.primaryInvestigator = convertBSONInvestigator(inv["PI"].embeddedObject());

		const std::vector<Investigator> coi = convertBSONCoInvestigators(inv["CoI"].Array());
		assignVectorToSequence(coi, proj.coInvestigator);
	}

	proj.isTargetOfOpportunity = obj["targetOfOpportunity"].boolean();
	proj.isKeyProject = obj["keyProject"].boolean();
	proj.isFastTrack = obj["fastTrack"].boolean();
	proj.isCommissioning = obj["commissioning"].boolean();
	proj.category = convertBSONObsCategory(obj["category"].str());
	corbaAssign(proj.projectAbstract, obj["abstract"]);

	// the proj.obsblock sequence will be filled in during merge

	return proj;
}

// Convert a single BSON Obsblock to a CORBA Obsblock
static Obsblock convertBSONObsblock(const mongo::BSONObj &obj)
{
	Obsblock obs;

	corbaAssign(obs.parentProject, obj["projectID"]);
	corbaAssign(obs.obsblockID, obj["obsblockID"]);
	corbaAssign(obs.documentName, obj["completeObsblockID"]);
	obs.status = convertBSONProjectStatus(obj["obsblockStatus"].str());
	obs.exceedTAC = obj["exceedTAC"].boolean();
	obs.minAllocatedTime = obj["allocatedTime"]["min"].numberDouble();
	obs.maxAllocatedTime = obj["allocatedTime"]["max"].numberDouble();
	obs.priority = obj["priority"].numberDouble();
	obs.likelihood = convertBSONObsLikelihood(obj["obsLikelihood"].str());
	obs.totalObsTime = obj["totalObservedTime"].numberDouble();
	obs.remainingTime = obj["remainingTime"].numberDouble();
	obs.reqLowHourAngleCoverage = obj["requestedHaCoverage"]["low"].numberDouble();
	obs.reqHiHourAngleCoverage = obj["requestedHaCoverage"]["high"].numberDouble();
	obs.lowRa = obj["requestedRaCoverage"]["low"].numberDouble();
	obs.highRa = obj["requestedRaCoverage"]["high"].numberDouble();
	corbaAssign(obs.actualHourAngleCoverage, obj["actualHaCoverage"]);
	obs.observationType = convertBSONObsType(obj["observationType"].str());
	corbaAssign(obs.receiverBand, obj["receiverBand"]);
	obs.restFrequency = obj["restFrequency"].numberDouble();
	corbaAssign(obs.arrayConfiguration, obj["arrayConfiguration"]);
	obs.isFlex = obj["isFlex"].boolean();

	// the obs.subObsblock sequence will be filled in during merge

	return obs;
}

// Convert a single BSON SubObsblock into a CORBA SubObsblock
static SubObsblock convertBSONSubObsblock(const mongo::BSONObj &obj)
{
	SubObsblock sub;

	corbaAssign(sub.parentProject, obj["projectID"]);
	corbaAssign(sub.parentObsblock, obj["obsblockID"]);
	corbaAssign(sub.subObsblockID, obj["subObsblockID"]);
	corbaAssign(sub.documentName, obj["completeSubObsblockID"]);
	sub.status = convertBSONProjectStatus(obj["subObsblockStatus"].str());
	sub.subObsblockObservationTime = obj["subObsblockObservedTime"].numberDouble();
	sub.lastTrial = obj["lastTrial"].numberInt();

	// the obs.trial sequence will be filled in during merge

	return sub;
}

// Convert a single BSON Trial into a CORBA Trial
static Trial convertBSONTrial(const mongo::BSONObj &obj)
{
	Trial trial;

	corbaAssign(trial.parentProject, obj["projectID"]);
	corbaAssign(trial.parentObsblock, obj["obsblockID"]);
	corbaAssign(trial.parentSubObsblock, obj["subObsblockID"]);
	corbaAssign(trial.documentName, obj["completeTrialID"]);
	trial.trialID = obj["trialID"].numberInt();

	trial.status = convertBSONProjectStatus(obj["trialStatus"].str());
	trial.trialObservationLength = obj["trialObservationLength"].numberDouble();

	{
		const mongo::BSONElement &tod = obj["trialObservationDate"];
		trial.trialObservationDateStart = convertBSONDate(tod["start"]).c_str();
		trial.trialObservationDateEnd = convertBSONDate(tod["end"]).c_str();
	}

	{
		const mongo::BSONElement &tol = obj["trialObservedLST"];
		trial.observedLSTstart = tol["lstStart"].numberDouble();
		trial.observedLSTend = tol["lstEnd"].numberDouble();
	}

	trial.fastSwitch = obj["fastSwitch"].boolean();

	{
		const mongo::BSONElement &grade = obj["grade"];

		trial.averagePhase = grade["averagePhase"].numberDouble();
		trial.averageOpacity = grade["averageOpacity"].numberDouble();
		trial.dqaOverallGrade = grade["DQAOverallGrade"].numberDouble();
		trial.obsGrade = grade["obsGrade"].numberDouble();
		corbaAssign(trial.obsComments, grade["comments"]);
	}

	trial.numberOfPointings = obj["numberOfPointings"].numberInt();

	{
		const std::vector<double> offsets = convertBSONDoubleSequence(obj["pointingOffsets"].Array());
		assignVectorToSequence(offsets, trial.offsets);
	}

	trial.numberOfAntennas = obj["numberOfAntennas"].numberInt();

	{
		const std::vector<Target> targets = convertBSONTargets(obj["target"].Array());
		assignVectorToSequence(targets, trial.target);
	}

	{
		const mongo::BSONElement &objects = obj["objects"];
		const std::vector<Source> sources = convertBSONSources(objects["source"].Array());
		assignVectorToSequence(sources, trial.source);

		const std::vector<Calibrator> cals = convertBSONCalibrators(objects["calibrator"].Array());
		assignVectorToSequence(cals, trial.calibrator);
	}

	{
		const std::vector<Correlator> correlators = convertBSONCorrelators(obj["correlator"].Array());
		assignVectorToSequence(correlators, trial.correlator);
	}

	{
		const mongo::BSONElement &constraints = obj["constraints"];
		corbaAssign(trial.imgVsSnr, constraints["imgVsSnr"]);

		{
			const mongo::BSONElement &gainCalibrator = constraints["gainCalibrator"];
			trial.maxGaincalTime = gainCalibrator["maxTime"].numberDouble();
			trial.maxGaincalRms = gainCalibrator["maxRms"].numberDouble();
		}

		trial.maxTsys = constraints["maxSystemTemperature"].numberInt();
		trial.minNumberOfAntennas = constraints["minNumberOfAntennas"].numberInt();
		trial.maxOpacity = constraints["maxOpacity"].numberDouble();
		trial.maxRmsPathLength = constraints["maxRmsPathLength"].numberDouble();
		trial.maxDecorrelationRatio = constraints["maxDecorrelationRatio"].numberDouble();
		trial.requiredSourceRms = constraints["requiredSourceRms"].numberDouble();
	}

	// the script and catalog are stored in a different object in the database
	// to reduce space using by the real time system
	trial.script = "";
	trial.catalog = "";

	// these are unused, but we support them anyway
	corbaAssign(trial.systemScripts, obj["systemScripts"]);
	corbaAssign(trial.scriptParameterization, obj["scriptParameterization"]);

	return trial;
}

/* -------------------------------------------------------------------------- */
/* Convert Objects from CORBA into BSON                                       */
/* -------------------------------------------------------------------------- */

// Convert CORBA ProjectStatus Enum to std::string
static std::string convertCORBAProjectStatus(const ProjectStatus value)
{
	const EnumStringMap m = getReverseMap(getProjectStatusMap());
	const EnumStringMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		std::ostringstream oss;
		oss << "unable to convert ProjectStatus: " << value;
		throw CARMA_ERROR(oss.str());
	}

	return it->second;
}

// Convert CORBA ObsCategory Enum to std::string
static std::string convertCORBAObsCategory(const ObsCategory value)
{
	const EnumStringMap m = getReverseMap(getObsCategoryMap());
	const EnumStringMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		std::ostringstream oss;
		oss << "unable to convert ObsCategory: " << value;
		throw CARMA_ERROR(oss.str());
	}

	return it->second;
}

// Convert a CORBA Investigator into a BSON Investigator
static mongo::BSONObj convertCORBAInvestigator(const Investigator &investigator)
{
	mongo::BSONObjBuilder builder;

	builder.append("name", std::string(investigator.name));
	builder.append("email", std::string(investigator.email));
	builder.append("affiliation", std::string(investigator.affiliation));
	builder.append("US", investigator.isUsAffil);

	return builder.obj();
}

// Convert a CORBA CoI Array into a BSON Array
static mongo::BSONArray convertCORBACoInvestigators(const InvestigatorSequence &seq)
{
	mongo::BSONArrayBuilder builder;
	std::vector<Investigator> coi;

	assignSequenceToVector(seq, coi);

	BOOST_FOREACH(const Investigator &investigator, coi) {
		builder.append(convertCORBAInvestigator(investigator));
	}

	return builder.arr();
}

// Convert a CORBA ObsLikelihood Enum to a BSON String
static std::string convertCORBAObsLikelihood(const ObsLikelihood value)
{
	const EnumStringMap m = getReverseMap(getObsLikelihoodMap());
	const EnumStringMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		std::ostringstream oss;
		oss << "unable to convert ObsLikelihood: " << value;
		throw CARMA_ERROR(oss.str());
	}

	return it->second;
}

// Convert CORBA ObsType Enum to BSON String
static std::string convertCORBAObsType(const ObsType value)
{
	const EnumStringMap m = getReverseMap(getObsTypeMap());
	const EnumStringMap::const_iterator it = m.find(value);
	if (it == m.end()) {
		std::ostringstream oss;
		oss << "unable to convert ObsType: " << value;
		throw CARMA_ERROR(oss.str());
	}

	return it->second;
}

// Convert CORBA String Date into BSON Date Object
static struct mongo::Date_t convertCORBADate(const TAO::String_Manager_T<char> &corba)
{
	const std::string value(corba);
	char *ret = NULL;
	struct tm date;

	// clear out the date object
	memset(&date, 0, sizeof(date));

	// full datetime format
	ret = strptime(value.c_str(), "%Y-%m-%dT%H:%M:%S", &date);
	if (ret != NULL && *ret == '\0') {
		const time_t tt = mktime(&date);
		if (tt == -1) {
			std::ostringstream oss;
			oss << "unable to convert to time_t (long format): " << value;
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}

		const int64_t ms = static_cast<int64_t>(tt) * 1000;
		const struct mongo::Date_t mongodate(ms);
		return mongodate;
	}

	// clear out the date object
	memset(&date, 0, sizeof(date));

	// date only format
	ret = strptime(value.c_str(), "%Y-%m-%d", &date);
	if (ret != NULL && *ret == '\0') {
		const time_t tt = mktime(&date);
		if (tt == -1) {
			std::ostringstream oss;
			oss << "unable to convert to time_t (short format): " << value;
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}

		const int64_t ms = static_cast<int64_t>(tt) * 1000;
		const struct mongo::Date_t mongodate(ms);
		return mongodate;
	}

	// error
	{
		std::ostringstream oss;
		oss << "unable to parse ISO 8601 datetime from: " << value;
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

// Convert CORBA Targets Sequence into BSON Targets Array
static mongo::BSONArray convertCORBATargets(const TargetSequence &seq)
{
	mongo::BSONArrayBuilder builder;
	std::vector<Target> targets;

	assignSequenceToVector(seq, targets);

	BOOST_FOREACH(const Target &target, targets) {
		mongo::BSONObjBuilder bob;
		bob.append("molecule", std::string(target.molecule));
		bob.append("transition", std::string(target.transition));

		builder.append(bob.obj());
	}

	return builder.arr();
}

// Convert CORBA Integer Sequence into BSON Integer Array
static mongo::BSONArray convertCORBAIntSequence(const shortSequence &seq)
{
	mongo::BSONArrayBuilder builder;
	std::vector<int> vec;

	assignSequenceToVector(seq, vec);

	BOOST_FOREACH(const int elem, vec) {
		builder.append(elem);
	}

	return builder.arr();
}

// Convert CORBA Double Sequence into BSON Double Array
static mongo::BSONArray convertCORBADoubleSequence(const doubleSequence &seq)
{
	mongo::BSONArrayBuilder builder;
	std::vector<double> vec;

	assignSequenceToVector(seq, vec);

	BOOST_FOREACH(const double elem, vec) {
		builder.append(elem);
	}

	return builder.arr();
}

// Convert CORBA Source Sequence into BSON Source Array
static mongo::BSONArray convertCORBASources(const SourceSequence &seq)
{
	mongo::BSONArrayBuilder builder;
	std::vector<Source> sources;

	assignSequenceToVector(seq, sources);

	BOOST_FOREACH(const Source &source, sources) {
		mongo::BSONObjBuilder bob;

		bob.append("sourceName", std::string(source.sourceName));
		bob.append("ephemeris", source.ephemeris);
		bob.append("RA", source.ra);
		bob.append("DEC", source.dec);
		bob.append("file", std::string(source.dataFile));
		bob.append("velocity", source.velocity);
		bob.append("veltype", std::string(source.veltype));
		bob.append("selfcalibratable", source.isSelfcalibratable);
		bob.append("observationLength", source.observationLength);
		bob.append("correlatorSetup", convertCORBAIntSequence(source.correlatorSetup));

		builder.append(bob.obj());
	}

	return builder.arr();
}

// Convert CORBA Calibrator Sequence into BSON Calibrator Array
static mongo::BSONArray convertCORBACalibrators(const CalibratorSequence &seq)
{
	mongo::BSONArrayBuilder builder;
	std::vector<Calibrator> calibrators;

	assignSequenceToVector(seq, calibrators);

	BOOST_FOREACH(const Calibrator &calibrator, calibrators) {
		mongo::BSONObjBuilder bob;

		bob.append("calibratorName", std::string(calibrator.calibratorName));
		bob.append("type", calibrator.calType);
		bob.append("RA", calibrator.ra);
		bob.append("DEC", calibrator.dec);
		bob.append("file", std::string(calibrator.dataFile));
		bob.append("observationLength", calibrator.observationLength);
		bob.append("correlatorSetup", convertCORBAIntSequence(calibrator.correlatorSetup));

		builder.append(bob.obj());
	}

	return builder.arr();
}

// Convert CORBA Window Sequence to BSON Window Array
static mongo::BSONArray convertCORBAWindows(const WindowSequence &seq)
{
	mongo::BSONArrayBuilder builder;
	std::vector<Window> windows;

	assignSequenceToVector(seq, windows);

	BOOST_FOREACH(const Window &window, windows) {
		mongo::BSONObjBuilder bob;

		bob.append("windowNumber", window.windowNumber);
		bob.append("bandwidth", window.bandwidth);
		bob.append("resolution", window.frequencyResolution);
		bob.append("numberOfChannels", window.numberOfChannels);
		bob.append("minfreq", window.minFrequency);
		bob.append("maxfreq", window.maxFrequency);

		builder.append(bob.obj());
	}

	return builder.arr();
}

// Convert CORBA Correlator Sequence to BSON Correlator Array
static mongo::BSONArray convertCORBACorrelators(const CorrelatorSequence &seq)
{
	mongo::BSONArrayBuilder builder;
	std::vector<Correlator> correlators;

	assignSequenceToVector(seq, correlators);

	BOOST_FOREACH(const Correlator &correlator, correlators) {
		mongo::BSONObjBuilder bob;

		bob.append("setupNumber", correlator.setupNumber);
		bob.append("numberOfWindows", correlator.numberOfWindows);
		bob.append("window", convertCORBAWindows(correlator.window));

		builder.append(bob.obj());
	}

	return builder.arr();
}

// Convert CORBA Project into BSON Project
static mongo::BSONObj convertCORBAProject(const Project &proj)
{
	mongo::BSONObjBuilder builder;

	builder.append("projectID", std::string(proj.projectID));
	builder.append("projectStatus", convertCORBAProjectStatus(proj.status));
	builder.append("callForProposals", BSON("term" << std::string(proj.proposalTerm)));
	builder.append("totalTime", proj.totalTime);
	builder.append("title", std::string(proj.title));

	// investigators sub-object
	{
		mongo::BSONObjBuilder subBuilder;

		subBuilder.append("numberOfInvestigators", proj.numberOfInvestigators);
		subBuilder.append("PI", convertCORBAInvestigator(proj.primaryInvestigator));
		subBuilder.append("CoI", convertCORBACoInvestigators(proj.coInvestigator));

		builder.append("investigators", subBuilder.obj());
	}

	builder.append("targetOfOpportunity", proj.isTargetOfOpportunity);
	builder.append("keyProject", proj.isKeyProject);
	builder.append("fastTrack", proj.isFastTrack);
	builder.append("commissioning", proj.isCommissioning);
	builder.append("category", convertCORBAObsCategory(proj.category));
	builder.append("abstract", std::string(proj.projectAbstract));

	// synthesized attributes
	{
		std::ostringstream oss;

		oss << proj.projectID;
		builder.append("completeProjectID", oss.str());
	}

	// check and return BSON object
	const mongo::BSONObj obj = builder.obj();
	checkProjectObject(obj);
	return obj;
}

// Convert CORBA Obsblock Object into BSON Obsblock Object
static mongo::BSONObj convertCORBAObsblock(const Obsblock &obs)
{
	mongo::BSONObjBuilder builder;

	builder.append("projectID", std::string(obs.parentProject));
	builder.append("obsblockID", std::string(obs.obsblockID));
	builder.append("obsblockStatus", convertCORBAProjectStatus(obs.status));
	builder.append("exceedTAC", obs.exceedTAC);

	// allocatedTime sub-object
	{
		mongo::BSONObjBuilder subBuilder;
		subBuilder.append("min", obs.minAllocatedTime);
		subBuilder.append("max", obs.maxAllocatedTime);

		builder.append("allocatedTime", subBuilder.obj());
	}

	builder.append("priority", obs.priority);
	builder.append("obsLikelihood", convertCORBAObsLikelihood(obs.likelihood));
	builder.append("totalObservedTime", obs.totalObsTime);
	builder.append("remainingTime", obs.remainingTime);

	// requestedHaCoverage sub-object
	{
		mongo::BSONObjBuilder subBuilder;
		subBuilder.append("low", obs.reqLowHourAngleCoverage);
		subBuilder.append("high", obs.reqHiHourAngleCoverage);

		builder.append("requestedHaCoverage", subBuilder.obj());
	}

	// requestedRaCoverage sub-object
	{
		mongo::BSONObjBuilder subBuilder;
		subBuilder.append("low", obs.lowRa);
		subBuilder.append("high", obs.highRa);

		builder.append("requestedRaCoverage", subBuilder.obj());
	}

	builder.append("actualHaCoverage", std::string(obs.actualHourAngleCoverage));
	builder.append("observationType", convertCORBAObsType(obs.observationType));
	builder.append("receiverBand", std::string(obs.receiverBand));
	builder.append("restFrequency", obs.restFrequency);
	builder.append("arrayConfiguration", std::string(obs.arrayConfiguration));
	builder.append("isFlex", obs.isFlex);

	// synthesized attributes
	{
		std::ostringstream oss;

		oss << obs.parentProject;
		builder.append("completeProjectID", oss.str());

		oss << "." << obs.obsblockID;
		builder.append("completeObsblockID", oss.str());
	}

	// check and return BSON object
	const mongo::BSONObj obj = builder.obj();
	checkObsblockObject(obj);
	return obj;
}

// Convert CORBA SubObsblock Object into BSON SubObsblock Object
static mongo::BSONObj convertCORBASubObsblock(const SubObsblock &sub)
{
	mongo::BSONObjBuilder builder;

	builder.append("projectID", std::string(sub.parentProject));
	builder.append("obsblockID", std::string(sub.parentObsblock));
	builder.append("subObsblockID", std::string(sub.subObsblockID));
	builder.append("subObsblockStatus", convertCORBAProjectStatus(sub.status));
	builder.append("subObsblockObservedTime", sub.subObsblockObservationTime);
	builder.append("lastTrial", sub.lastTrial);

	// synthesized attributes
	{
		std::ostringstream oss;

		oss << sub.parentProject;
		builder.append("completeProjectID", oss.str());

		oss << "." << sub.parentObsblock;
		builder.append("completeObsblockID", oss.str());

		const std::string subObsblockID(sub.subObsblockID);
		if (!subObsblockID.empty()) {
			oss << "." << subObsblockID;
		}

		builder.append("completeSubObsblockID", oss.str());
	}

	// check and return BSON object
	const mongo::BSONObj obj = builder.obj();
	checkSubobsblockObject(obj);
	return obj;
}

// Convert CORBA Trial Object into BSON Trial Object
static mongo::BSONObj convertCORBATrial(const Trial &trial)
{
	mongo::BSONObjBuilder builder;

	builder.append("projectID", std::string(trial.parentProject));
	builder.append("obsblockID", std::string(trial.parentObsblock));
	builder.append("subObsblockID", std::string(trial.parentSubObsblock));
	builder.append("trialID", trial.trialID);
	builder.append("trialStatus", convertCORBAProjectStatus(trial.status));
	builder.append("trialObservationLength", trial.trialObservationLength);

	// trialObservationDate sub-object
	{
		mongo::BSONObjBuilder subBuilder;
		subBuilder.append("start", convertCORBADate(trial.trialObservationDateStart));
		subBuilder.append("end", convertCORBADate(trial.trialObservationDateEnd));

		builder.append("trialObservationDate", subBuilder.obj());
	}

	// trialObservedLST sub-object
	{
		mongo::BSONObjBuilder subBuilder;
		subBuilder.append("lstStart", trial.observedLSTstart);
		subBuilder.append("lstEnd", trial.observedLSTend);

		builder.append("trialObservedLST", subBuilder.obj());
	}

	builder.append("fastSwitch", trial.fastSwitch);

	// grade sub-object
	{
		mongo::BSONObjBuilder subBuilder;
		subBuilder.append("averagePhase", trial.averagePhase);
		subBuilder.append("averageOpacity", trial.averageOpacity);
		subBuilder.append("DQAOverallGrade", trial.dqaOverallGrade);
		subBuilder.append("obsGrade", trial.obsGrade);
		subBuilder.append("comments", std::string(trial.obsComments));

		builder.append("grade", subBuilder.obj());
	}

	builder.append("numberOfPointings", trial.numberOfPointings);
	builder.append("pointingOffsets", convertCORBADoubleSequence(trial.offsets));
	builder.append("numberOfAntennas", trial.numberOfAntennas);
	builder.append("target", convertCORBATargets(trial.target));

	// objects sub-object
	{
		mongo::BSONObjBuilder subBuilder;
		subBuilder.append("source", convertCORBASources(trial.source));
		subBuilder.append("calibrator", convertCORBACalibrators(trial.calibrator));

		builder.append("objects", subBuilder.obj());
	}

	builder.append("correlator", convertCORBACorrelators(trial.correlator));

	// constraints sub-object
	{
		mongo::BSONObjBuilder subBuilder;
		subBuilder.append("imgVsSnr", std::string(trial.imgVsSnr));

		// gainCalibrator sub-object
		{
			mongo::BSONObjBuilder gcBuilder;
			gcBuilder.append("maxTime", trial.maxGaincalTime);
			gcBuilder.append("maxRms", trial.maxGaincalRms);

			subBuilder.append("gainCalibrator", gcBuilder.obj());
		}

		subBuilder.append("maxSystemTemperature", trial.maxTsys);
		subBuilder.append("minNumberOfAntennas", trial.minNumberOfAntennas);
		subBuilder.append("maxOpacity", trial.maxOpacity);
		subBuilder.append("maxRmsPathLength", trial.maxRmsPathLength);
		subBuilder.append("maxDecorrelationRatio", trial.maxDecorrelationRatio);
		subBuilder.append("requiredSourceRms", trial.requiredSourceRms);

		builder.append("constraints", subBuilder.obj());
	}

	// these attributes are basically unused
	builder.append("systemScripts", std::string(trial.systemScripts));
	builder.append("scriptParameterization", std::string(trial.scriptParameterization));

	// synthesized attributes
	{
		std::ostringstream oss;

		oss << trial.parentProject;
		builder.append("completeProjectID", oss.str());

		oss << "." << trial.parentObsblock;
		builder.append("completeObsblockID", oss.str());

		const std::string subObsblockID(trial.parentSubObsblock);
		if (!subObsblockID.empty()) {
			oss << "." << subObsblockID;
		}

		builder.append("completeSubObsblockID", oss.str());

		oss << "." << trial.trialID;
		builder.append("completeTrialID", oss.str());
	}

	// check and return BSON object
	const mongo::BSONObj obj = builder.obj();
	checkTrialObject(obj);
	return obj;
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

// Convert mongo::BSONObj to CORBA Project objects
std::vector<Project> convertBSONProjects(const std::vector<mongo::BSONObj> &objects)
{
	std::vector<Project> results;

	BOOST_FOREACH(const mongo::BSONObj &obj, objects) {
		const Project proj = convertBSONProject(obj);
		results.push_back(proj);
	}

	return results;
}

// Convert mongo::BSONObj to CORBA Obsblock objects
std::vector<Obsblock> convertBSONObsblocks(const std::vector<mongo::BSONObj> &objects)
{
	std::vector<Obsblock> results;

	BOOST_FOREACH(const mongo::BSONObj &obj, objects) {
		const Obsblock obs = convertBSONObsblock(obj);
		results.push_back(obs);
	}

	return results;
}

// Convert mongo::BSONObj to CORBA SubObsblock objects
std::vector<SubObsblock> convertBSONSubObsblocks(const std::vector<mongo::BSONObj> &objects)
{
	std::vector<SubObsblock> results;

	BOOST_FOREACH(const mongo::BSONObj &obj, objects) {
		const SubObsblock sub = convertBSONSubObsblock(obj);
		results.push_back(sub);
	}

	return results;
}

// Convert mongo::BSONObj to CORBA Trial objects
std::vector<Trial> convertBSONTrials(const std::vector<mongo::BSONObj> &objects)
{
	std::vector<Trial> results;

	BOOST_FOREACH(const mongo::BSONObj &obj, objects) {
		const Trial trial = convertBSONTrial(obj);
		results.push_back(trial);
	}

	return results;
}

// Convert CORBA Project to mongo::BSONObj
std::vector<mongo::BSONObj> convertCORBAProjects(const std::vector<Project> &objects)
{
	std::vector<mongo::BSONObj> results;

	BOOST_FOREACH(const Project &proj, objects) {
		const mongo::BSONObj obj = convertCORBAProject(proj);
		results.push_back(obj);
	}

	return results;
}

// Convert CORBA Obsblock to mongo::BSONObj
std::vector<mongo::BSONObj> convertCORBAObsblocks(const std::vector<Obsblock> &objects)
{
	std::vector<mongo::BSONObj> results;

	BOOST_FOREACH(const Obsblock &obs, objects) {
		const mongo::BSONObj obj = convertCORBAObsblock(obs);
		results.push_back(obj);
	}

	return results;
}

// Convert CORBA SubObsblock to mongo::BSONObj
std::vector<mongo::BSONObj> convertCORBASubObsblocks(const std::vector<SubObsblock> &objects)
{
	std::vector<mongo::BSONObj> results;

	BOOST_FOREACH(const SubObsblock &sub, objects) {
		const mongo::BSONObj obj = convertCORBASubObsblock(sub);
		results.push_back(obj);
	}

	return results;
}

// Convert CORBA Trial to mongo::BSONObj
std::vector<mongo::BSONObj> convertCORBATrials(const std::vector<Trial> &objects)
{
	std::vector<mongo::BSONObj> results;

	BOOST_FOREACH(const Trial &trial, objects) {
		const mongo::BSONObj obj = convertCORBATrial(trial);
		results.push_back(obj);
	}

	return results;
}

/*
 * Merge together the results of the four separate queries into a single
 * CORBA ProjectSequence object.
 */
std::vector<Project> mergeCORBAObjects(const CORBA_Object_Merge &merge)
{

	/*
	 * TODO FIXME:
	 *
	 * You might think about adding a "parentDocument" object to the IDL
	 * for obsblock/subobsblock/trial objects. This would refer to the
	 * first parent upwards.
	 *
	 * Obsblock.parentDocument:		projectID
	 * SubObsblock.parentDocument:	projectID.obsblockID
	 * Trial.parentDocument:		projectID.obsblockID.subObsblockID
	 *
	 * This makes the merge step much easier!
	 */

	// Maps the "projectID" string to a vector of Obsblock objects
	typedef std::vector<Obsblock> ObsblockVector;
	typedef std::map<std::string, ObsblockVector> ObsblockParentMap;
	ObsblockParentMap obsblocks;
	BOOST_FOREACH(const Obsblock &obs, merge.obsblocks) {
		std::ostringstream oss;
		oss << obs.parentProject;
		obsblocks[oss.str()].push_back(obs);
	}

	// Maps the "projectID.obsblockID" string to a vector of SubObsblock objects
	typedef std::vector<SubObsblock> SubObsblockVector;
	typedef std::map<std::string, SubObsblockVector> SubObsblockParentMap;
	SubObsblockParentMap subobsblocks;
	BOOST_FOREACH(const SubObsblock &sub, merge.subobsblocks) {
		std::ostringstream oss;
		oss << sub.parentProject << "." << sub.parentObsblock;
		subobsblocks[oss.str()].push_back(sub);
	}

	// Maps the "projectID.obsblockID.subObsblockID" string to a vector of Trial objects
	typedef std::vector<Trial> TrialVector;
	typedef std::map<std::string, TrialVector> TrialParentMap;
	TrialParentMap trials;
	BOOST_FOREACH(const Trial &trial, merge.trials) {
		const std::string subObsblockID(trial.parentSubObsblock);

		std::ostringstream oss;
		oss << trial.parentProject << "." << trial.parentObsblock;
		if (!subObsblockID.empty()) {
			oss << "." << subObsblockID;
		}

		trials[oss.str()].push_back(trial);
	}

	// Copy the project vector
	std::vector<Project> projects = merge.projects;

	// merge trials into subobsblocks
	BOOST_FOREACH(SubObsblockParentMap::value_type &elem, subobsblocks) {
		const std::string completeObsblockID(elem.first);
		SubObsblockVector &subVec = elem.second;

		BOOST_FOREACH(SubObsblock &sub, subVec) {
			const std::string completeSubObsblockID(sub.documentName);

			// the trials need to be sorted by trialID number
			std::vector<Trial> &tvec = trials[completeSubObsblockID];
			std::sort(tvec.begin(), tvec.end(), trialid_compare);

			assignVectorToSequence(tvec, sub.trial);
		}
	}

	// merge subobsblocks into obsblocks
	BOOST_FOREACH(ObsblockParentMap::value_type &elem, obsblocks) {
		const std::string completeProjectID(elem.first);
		ObsblockVector &obsVec = elem.second;

		BOOST_FOREACH(Obsblock &obs, obsVec) {
			const std::string completeObsblockID(obs.documentName);
			assignVectorToSequence(subobsblocks[completeObsblockID], obs.subObsblock);
		}
	}

	// merge obsblocks into projects
	BOOST_FOREACH(Project &proj, projects) {
		const std::string completeProjectID(proj.projectID);
		assignVectorToSequence(obsblocks[completeProjectID], proj.obsblock);
	}

	return projects;
}

// Split apart a vector of CORBA Projects into vectors of each of their types
CORBA_Object_Merge splitCORBAObjects(const std::vector<Project> &projects)
{
	CORBA_Object_Merge merge;

	for (size_t p = 0; p < projects.size(); p++) {
		const Project &proj = projects.at(p);
		merge.projects.push_back(proj);

		for (size_t o = 0; o < proj.obsblock.length(); o++) {
			const Obsblock &obs = proj.obsblock[o];
			merge.obsblocks.push_back(obs);

			for (size_t s = 0; s < obs.subObsblock.length(); s++) {
				const SubObsblock &sub = obs.subObsblock[s];
				merge.subobsblocks.push_back(sub);

				for (size_t t = 0; t < sub.trial.length(); t++) {
					const Trial &trial = sub.trial[t];
					merge.trials.push_back(trial);
				}
			}
		}
	}

	// remove sub-sequences out of the split-apart objects
	BOOST_FOREACH(Project &proj, merge.projects) {
		assignVectorToSequence(std::vector<Obsblock>(), proj.obsblock);
	}

	BOOST_FOREACH(Obsblock &obs, merge.obsblocks) {
		assignVectorToSequence(std::vector<SubObsblock>(), obs.subObsblock);
	}

	BOOST_FOREACH(SubObsblock &sub, merge.subobsblocks) {
		assignVectorToSequence(std::vector<Trial>(), sub.trial);
	}

	return merge;
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
