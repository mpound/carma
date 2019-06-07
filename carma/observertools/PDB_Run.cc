/*
 * CARMA Project Database runProject() Implementation
 */

#include <carma/observertools/PDB_Run.h>
#include <carma/observertools/PDB_Util.h>
#include <carma/observertools/PDB_Script.h>
#include <carma/observertools/PDB_Validator.h>
#include <carma/observertools/PDB_BSON_Convert.h>

#include <carma/util/corbaSequenceUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

#include <boost/shared_ptr.hpp>
#include <boost/foreach.hpp>

#include <sstream>
#include <string>
#include <set>

using namespace carma::observertools;
using namespace carma::util;

typedef boost::shared_ptr<Obsblock> ObsblockPtr;
typedef boost::shared_ptr<SubObsblock> SubObsblockPtr;
typedef boost::shared_ptr<Trial> TrialPtr;

struct RunMetadata {
	// constructor
	RunMetadata(const PDB_DB_Params &db, const PDB_Run_Params &params);

	const PDB_DB_Params &db;
	const PDB_Run_Params &params;

	// objects to save back to database
	std::set<ObsblockPtr> obsblocks;
	std::set<SubObsblockPtr> subobsblocks;
	std::set<TrialPtr> trials;
	std::set<mongo::BSONObj> scripts;

private:
	// no copying
	RunMetadata();
	RunMetadata(const RunMetadata &rhs);
	RunMetadata& operator=(const RunMetadata &rhs);
};

RunMetadata::RunMetadata(const PDB_DB_Params &db, const PDB_Run_Params &params)
	: db(db)
	, params(params)
	, obsblocks()
	, subobsblocks()
	, trials()
{
	// intentionally left empty
}

// true if s1 contains s2, otherwise false
static bool strContains(const std::string &s1, const std::string &s2)
{
	return s1.find(s2) != std::string::npos;
}

static std::string getID(const std::string &pid, const std::string &oid)
{
	return pid + "." + oid;
}

static std::string getID(const std::string &pid, const std::string &oid, const std::string &sid)
{
	std::ostringstream oss;
	oss << pid << "." << oid;
	if (!sid.empty()) {
		oss << "." << sid;
	}

	return oss.str();
}

static std::string getID(const std::string &pid, const std::string &oid, const std::string &sid, const int trialID)
{
	std::ostringstream oss;
	oss << getID(pid, oid, sid) << "." << trialID;
	return oss.str();
}

// Get the single requested Obsblock from the database.
// You must ensure that it already exists before calling this method.
static ObsblockPtr fetchObsblock(const PDB_DB_Params &db, const mongo::BSONObj &query)
{
	std::vector<mongo::BSONObj> bsonVec;

	const std::auto_ptr<mongo::DBClientCursor> cursor = db.conn->query(db.OBSBLOCKS, query);
	while (cursor->more()) {
		const mongo::BSONObj obj = cursor->next();
		bsonVec.push_back(obj.copy());
	}

	const std::vector<Obsblock> corbaVec = convertBSONObsblocks(bsonVec);

	if (corbaVec.empty()) {
		throw CARMA_ERROR("Obsblock not found");
	}

	if (corbaVec.size() > 1) {
		throw CARMA_ERROR("Too many obsblocks found");
	}

	const ObsblockPtr p(new Obsblock(corbaVec.at(0)));
	return p;
}

// Get the single requested Obsblock. This method takes into account the need to
// clone the "projectID.default" Obsblock if the requested Obsblock does not
// exist and this is a commissioning project.
static ObsblockPtr fetchRequiredObsblock(RunMetadata &metadata)
{
	const PDB_Run_Params &params = metadata.params;
	const PDB_DB_Params &db = metadata.db;
	const std::string collection = db.OBSBLOCKS;

	// look for the requested obsblock first
	{
		mongo::BSONObjBuilder bob;
		bob.append("projectID", params.projectID);
		bob.append("obsblockID", params.obsblockID);
		const mongo::BSONObj query = bob.obj();
		if (documentExists(db.conn, collection, query)) {
			return fetchObsblock(db, query);
		}
	}

	// science projects are not allowed to run if the obsblock does not exist
	if (!params.isCommissioning) {
		std::ostringstream oss;
		oss << "Obsblock not found: "
			<< getID(params.projectID, params.obsblockID);
		throw CARMA_ERROR(oss.str());
	}

	// commissioning projects can duplicate the "projectID.default" obsblock automatically
	{
		mongo::BSONObjBuilder bob;
		bob.append("projectID", params.projectID);
		bob.append("obsblockID", "default");
		const mongo::BSONObj query = bob.obj();
		if (!documentExists(db.conn, collection, query)) {
			std::ostringstream oss;
			oss << "Default Obsblock not found: "
				<< getID(params.projectID, "default");
			throw CARMA_ERROR(oss.str());
		}

		// copy the Obsblock
		const ObsblockPtr obs = fetchObsblock(db, query);

		obs->obsblockID = params.obsblockID.c_str();
		obs->documentName = getID(params.projectID, params.obsblockID).c_str();
		obs->status = PSTATUS_INCOMPLETE;
		obs->totalObsTime = 0.0;
		obs->priority = 0.0;
		obs->actualHourAngleCoverage = "";
		obs->remainingTime = obs->minAllocatedTime;

		// save the copied Obsblock back to the database
		metadata.obsblocks.insert(obs);
		return obs;
	}
}

// If this is a single correlator project, then just return the subObsblockID
// If this is a dual correlator project, then:
//     If the subObsblockID is empty, return the subObsblockID "SL" or "WB"
//     Else return the subObsblockID + "-SL" or "-WB"
//
// NOTE: if this is a single correlator project, then the corrType is not used!
static std::string getSubObsblockName(const PDB_Run_Params &params, const std::string &corrType)
{
	if (!params.isDualCorr) {
		return params.subObsblockID;
	}

	if (params.subObsblockID.empty()) {
		return corrType;
	}

	return params.subObsblockID + "-" + corrType;
}

// Get the single requested SubObsblock from the database.
// You must ensure that it already exists before calling this method.
static SubObsblockPtr fetchSubObsblock(const PDB_DB_Params &db, const mongo::BSONObj &query)
{
	std::vector<mongo::BSONObj> bsonVec;

	const std::auto_ptr<mongo::DBClientCursor> cursor = db.conn->query(db.SUBOBSBLOCKS, query);
	while (cursor->more()) {
		const mongo::BSONObj obj = cursor->next();
		bsonVec.push_back(obj.copy());
	}

	const std::vector<SubObsblock> corbaVec = convertBSONSubObsblocks(bsonVec);

	if (corbaVec.empty()) {
		throw CARMA_ERROR("SubObsblock not found");
	}

	if (corbaVec.size() > 1) {
		throw CARMA_ERROR("Too many SubObsblocks found");
	}

	const SubObsblockPtr p(new SubObsblock(corbaVec.at(0)));
	return p;
}

// Clean out any data from a SubObsblock when duplicating it
static void sanitizeSubObsblock(const SubObsblockPtr sub, const PDB_Run_Params &params, const std::string &subObsblockID)
{
	sub->parentObsblock = params.obsblockID.c_str();
	sub->subObsblockID = subObsblockID.c_str();
	sub->documentName = getID(params.projectID, params.obsblockID, subObsblockID).c_str();
	sub->status = PSTATUS_INCOMPLETE;
	sub->subObsblockObservationTime = 0.0;
	sub->lastTrial = 1;
}

// Get the single requested SubObsblock.
//
// If the SubObsblock does not exist, the "projectID.obsblockID" SubObsblock can
// be cloned for all types of projects.
//
// If the NULL SubObsblock does not exit, then the "projectID.default" SubObsblock
// can be cloned for commissioning projects only.
static SubObsblockPtr fetchRequiredSubObsblock(RunMetadata &metadata, const std::string &subObsblockID)
{
	const PDB_Run_Params &params = metadata.params;
	const PDB_DB_Params &db = metadata.db;
	const std::string collection = db.SUBOBSBLOCKS;

	// look for the requested subobsblock first
	{
		mongo::BSONObjBuilder bob;
		bob.append("projectID", params.projectID);
		bob.append("obsblockID", params.obsblockID);
		bob.append("subObsblockID", subObsblockID);
		const mongo::BSONObj query = bob.obj();
		if (documentExists(db.conn, collection, query)) {
			// not modified, do not save it to the database
			return fetchSubObsblock(db, query);
		}
	}

	// look for a NULL subobsblock next
	{
		mongo::BSONObjBuilder bob;
		bob.append("projectID", params.projectID);
		bob.append("obsblockID", params.obsblockID);
		bob.append("subObsblockID", "");
		const mongo::BSONObj query = bob.obj();
		if (documentExists(db.conn, collection, query)) {
			// copy the SubObsblock and save to the database
			const SubObsblockPtr sub = fetchSubObsblock(db, query);
			sanitizeSubObsblock(sub, params, subObsblockID);

			// This fixes the case where we are duplicating the NULL SubObsblock.
			// In that case, the next trial should be trial 1. I couldn't figure
			// out a cleaner way to make this happen.
			sub->lastTrial = 0;
			metadata.subobsblocks.insert(sub);
			return sub;
		}
	}

	// science projects are not allowed to run if the NULL subobsblock does not exist
	if (!params.isCommissioning) {
		std::ostringstream oss;
		oss << "SubObsblock not found: "
			<< getID(params.projectID, params.obsblockID, subObsblockID);
		throw CARMA_ERROR(oss.str());
	}

	// look for a default obsblock + subobsblock last
	{
		mongo::BSONObjBuilder bob;
		bob.append("projectID", params.projectID);
		bob.append("obsblockID", "default");
		bob.append("subObsblockID", "");
		const mongo::BSONObj query = bob.obj();
		if (documentExists(db.conn, collection, query)) {
			// copy the SubObsblock and save to the database
			const SubObsblockPtr sub = fetchSubObsblock(db, query);
			sanitizeSubObsblock(sub, params, subObsblockID);
			metadata.subobsblocks.insert(sub);
			return sub;
		}
	}

	// no acceptable object found
	{
		std::ostringstream oss;
		oss << "No SubObsblock found: "
			<< getID(params.projectID, params.obsblockID, subObsblockID);
		throw CARMA_ERROR(oss.str());
	}
}

// Fetch the required SubObsblocks for all required correlators
static std::vector<SubObsblockPtr> fetchRequiredSubObsblocks(RunMetadata &metadata)
{
	// in single-correlator mode this set has one id only:
	// the one specified in PDB_Run_Params
	std::set<std::string> ids;
	ids.insert(getSubObsblockName(metadata.params, "SL"));
	ids.insert(getSubObsblockName(metadata.params, "WB"));

	std::vector<SubObsblockPtr> subVec;
	BOOST_FOREACH(const std::string &id, ids) {
		const SubObsblockPtr sub = fetchRequiredSubObsblock(metadata, id);
		subVec.push_back(sub);
	}

	return subVec;
}

// Find the maximum trial number actually present in the database,
// even if the sub.lastTrial member has a lower number than the actual
// last trial taken. The sub.lastTrial member is updated appropriately.
static void fixupLastTrial(RunMetadata &metadata, const SubObsblockPtr sub)
{
	const PDB_DB_Params &db = metadata.db;

	const std::string pid(sub->parentProject);
	const std::string oid(sub->parentObsblock);
	const std::string sid(sub->subObsblockID);

	int misses = 0;
	while (misses < 2) {
		const int trialID = sub->lastTrial + misses + 1;

		mongo::BSONObjBuilder bob;
		bob.append("projectID", pid);
		bob.append("obsblockID", oid);
		bob.append("subObsblockID", sid);
		bob.append("trialID", sub->lastTrial + 1);
		const mongo::BSONObj query = bob.obj();
		if (!documentExists(db.conn, db.TRIALS, query)) {
			misses++;
			continue;
		}

		misses = 0;

		if (sub->lastTrial != trialID) {
			sub->lastTrial = trialID;
			metadata.subobsblocks.insert(sub);
		}
	}
}

// Get the next trialID available. Each SubObsblock checked vs. the actual trials
// present in the database, and the sub.lastTrial member is updated if necessary.
static short getNextTrialID(RunMetadata &metadata, const std::vector<SubObsblockPtr> &subVec)
{
	short currentMaxTrialID = -1;

	BOOST_FOREACH(const SubObsblockPtr sub, subVec) {
		// ensure that the sub.lastTrial member actually matches the trials
		// in the database
		fixupLastTrial(metadata, sub);

		// find the maximum sub.lastTrial present in all of our SubObsblocks
		currentMaxTrialID = std::max(currentMaxTrialID, sub->lastTrial);
	}

	// I don't think we can actually ever hit this, as we automatically set the
	// sub.lastTrial == 1 when replicating the default SubObsblock. But it is
	// a good check to have just in case.
	if (currentMaxTrialID == -1) {
		std::ostringstream oss;
		oss << "SubObsblock(s) do not contain valid trial numbers: ";
		BOOST_FOREACH(const SubObsblockPtr sub, subVec) {
			oss << sub->documentName << " ";
		}
		throw CARMA_ERROR(oss.str());
	}

	const short nextTrialID = currentMaxTrialID + 1;
	if (nextTrialID < 1) {
		std::ostringstream oss;
		oss << "Invalid next trial ID chosen: " << nextTrialID;
		throw CARMA_ERROR(oss.str());
	}

	return nextTrialID;
}

// Get the single requested Trial from the database.
// You must ensure that it already exists before calling this method.
static TrialPtr fetchTrial(const PDB_DB_Params &db, const mongo::BSONObj &query)
{
	std::vector<mongo::BSONObj> bsonVec;

	const std::auto_ptr<mongo::DBClientCursor> cursor = db.conn->query(db.TRIALS, query);
	while (cursor->more()) {
		const mongo::BSONObj obj = cursor->next();
		bsonVec.push_back(obj.copy());
	}

	const std::vector<Trial> corbaVec = convertBSONTrials(bsonVec);

	if (corbaVec.empty()) {
		throw CARMA_ERROR("Trial not found");
	}

	if (corbaVec.size() > 1) {
		throw CARMA_ERROR("Too many Trials found");
	}

	const TrialPtr p(new Trial(corbaVec.at(0)));
	return p;
}

// Clean out any data from a Trial when duplicating it
static void sanitizeTrial(const SubObsblockPtr sub, const TrialPtr trial, const int trialID)
{
	const std::string pid(sub->parentProject);
	const std::string oid(sub->parentObsblock);
	const std::string sid(sub->subObsblockID);

	trial->parentProject = pid.c_str();
	trial->parentObsblock = oid.c_str();
	trial->parentSubObsblock = sid.c_str();
	trial->trialID = trialID;
	trial->documentName = getID(pid, oid, sid, trial->trialID).c_str();

	trial->status = PSTATUS_RUNNING;
	trial->trialObservationLength = 0.0;
	trial->trialObservationDateStart = "1970-01-01T00:00:00";
	trial->trialObservationDateEnd = "1970-01-01T00:00:00";
	trial->observedLSTstart = 0.0;
	trial->observedLSTend = 0.0;
	trial->averagePhase = 0.0;
	trial->averageOpacity = 0.0;
	trial->dqaOverallGrade = 0.0;
	trial->obsGrade = 0.0;
	trial->obsComments = "";
	trial->numberOfPointings = 0;

	{
		const std::vector<double> vec;
		assignVectorToSequence(vec, trial->offsets);
	}

	trial->numberOfAntennas = 0;

	if (trial->source.length() > 0) {
		Source source = trial->source[0];

		source.dataFile = "";
		source.observationLength = 0.0;
		source.correlatorSetup.length(0);

		trial->source.length(1);
		trial->source[0] = source;
	}

	trial->calibrator.length(0);
	trial->correlator.length(0);
	trial->script = "";
	trial->catalog = "";
	trial->systemScripts = "";
	trial->scriptParameterization = "";
}

// Get the single requested Trial.
//
// If the Trial does not exist, the "projectID.obsblockID.1" Trial can
// be cloned for all types of projects.
//
// If the Trial does not exit, then the "projectID.default.1" Trial
// can be cloned for commissioning projects only.
static TrialPtr fetchRequiredTrial(RunMetadata &metadata, const SubObsblockPtr sub, const short trialID)
{
	const PDB_Run_Params &params = metadata.params;
	const PDB_DB_Params &db = metadata.db;

	const std::string collection = db.TRIALS;
	const std::string pid(sub->parentProject);
	const std::string oid(sub->parentObsblock);
	const std::string sid(sub->subObsblockID);

	// This handles the case where the SubObsblock already exists in the database.
	// There may be a template trial (number 1) or any number of previous trials.
	{
		mongo::BSONObjBuilder bob;
		bob.append("projectID", pid);
		bob.append("obsblockID", oid);
		bob.append("subObsblockID", sid);
		bob.append("trialID", sub->lastTrial);
		const mongo::BSONObj query = bob.obj();
		if (documentExists(db.conn, collection, query)) {
			// copy the trial
			const TrialPtr trial = fetchTrial(db, query);

			// If this trial is INCOMPLETE, then it is the starting template for the rest
			// of the trials. Normal trials are either RUNNING or COMPLETE.
			if (trial->trialID == 1 && trial->status == PSTATUS_INCOMPLETE) {
				trial->status = PSTATUS_RUNNING;
				metadata.trials.insert(trial);
				return trial;
			}

			// Not the template trial, we need to sanitize it appropriately and update
			// the trial number before returning it.
			sanitizeTrial(sub, trial, trialID);

			metadata.trials.insert(trial);
			return trial;
		}
	}

	// This handles the case where the SubObsblock does not exist in the database.
	// In this case, we copy the first trial from the NULL SubObsblock and give
	// it the correct trial number.
	{
		mongo::BSONObjBuilder bob;
		bob.append("projectID", pid);
		bob.append("obsblockID", oid);
		bob.append("subObsblockID", "");
		bob.append("trialID", 1);
		const mongo::BSONObj query = bob.obj();
		if (documentExists(db.conn, collection, query)) {
			// copy the trial
			const TrialPtr trial = fetchTrial(db, query);

			// Not the template trial, we need to sanitize it appropriately and update
			// the trial number before returning it.
			sanitizeTrial(sub, trial, trialID);

			metadata.trials.insert(trial);
			return trial;
		}
	}

	// science projects are not allowed to run if the null subobsblock does not exist
	if (!params.isCommissioning) {
		std::ostringstream oss;
		oss << "Trial not found: "
			<< getID(pid, oid, sid, sub->lastTrial);
		throw CARMA_ERROR(oss.str());
	}

	// Look for a default obsblock + null subobsblock and copy trial 1 from it.
	// This is a template provided for commissioning projects.
	{
		mongo::BSONObjBuilder bob;
		bob.append("projectID", pid);
		bob.append("obsblockID", "default");
		bob.append("subObsblockID", "");
		bob.append("trialID", 1);
		const mongo::BSONObj query = bob.obj();
		if (documentExists(db.conn, collection, query)) {
			const TrialPtr trial = fetchTrial(db, query);

			// Not the template trial, we need to sanitize it appropriately and update
			// the trial number before returning it.
			sanitizeTrial(sub, trial, trialID);

			metadata.trials.insert(trial);
			return trial;
		}
	}

	// no acceptable object found
	{
		std::ostringstream oss;
		oss << "No Trial found: "
			<< getID(pid, oid, sid, sub->lastTrial);
		throw CARMA_ERROR(oss.str());
	}
}

// Fetch the required Trial objects for each type of correlator
static std::vector<TrialPtr> fetchRequiredTrials(RunMetadata &metadata, const std::vector<SubObsblockPtr> &subVec, const short trialID)
{
	std::vector<TrialPtr> trialVec;

	BOOST_FOREACH(const SubObsblockPtr sub, subVec) {
		const TrialPtr trial = fetchRequiredTrial(metadata, sub, trialID);
		trialVec.push_back(trial);
	}

	return trialVec;
}

// Generate each required BSON script object to be saved into the database
static void generateScriptObjects(RunMetadata &metadata, const std::vector<TrialPtr> &trialVec)
{
	const std::string &script = metadata.params.scriptFile;
	const std::string &catalog = metadata.params.catalogFile;

	// nothing to do, neither file was specified
	if (identifierIsNoneOrEmpty(script) && identifierIsNoneOrEmpty(catalog)) {
		return;
	}

	BOOST_FOREACH(const TrialPtr trial, trialVec) {
		PDB_Script_Params params;
		params.projectID = std::string(trial->parentProject);
		params.obsblockID = std::string(trial->parentObsblock);
		params.subObsblockID = std::string(trial->parentSubObsblock);

		const PDB_Script pdbScript(metadata.db, params);
		const mongo::BSONObj obj = pdbScript.getScriptObject(script, catalog, trial->trialID);

		metadata.scripts.insert(obj);
	}
}

// Convert a std::set of boost:shared_ptr into a vector of copies of the objects
template <typename T>
static std::vector<T> convertSetToVector(const std::set< boost::shared_ptr<T> > &input)
{
	std::vector<T> results;
	BOOST_FOREACH(const boost::shared_ptr<T> elem, input) {
		results.push_back(*elem.get());
	}

	return results;
}

// Save all objects present in the RunMetadata back to the database
static void saveObjects(RunMetadata &metadata)
{
	const PDB_DB_Params &db = metadata.db;

	// save all obsblocks
	{
		const std::vector<Obsblock> corbaVec = convertSetToVector(metadata.obsblocks);
		const std::vector<mongo::BSONObj> bsonVec = convertCORBAObsblocks(corbaVec);
		BOOST_FOREACH(const mongo::BSONObj &obj, bsonVec) {
			programLogInfoIfPossible("SAVE OBSBLOCK: " + getObjectName(obj));
			const mongo::BSONObj query = generateQueryFromBSONObject(obj);
			writeToDatabase(db.conn, db.OBSBLOCKS, query, obj);
		}
	}

	// save all subobsblocks
	{
		const std::vector<SubObsblock> corbaVec = convertSetToVector(metadata.subobsblocks);
		const std::vector<mongo::BSONObj> bsonVec = convertCORBASubObsblocks(corbaVec);
		BOOST_FOREACH(const mongo::BSONObj &obj, bsonVec) {
			programLogInfoIfPossible("SAVE SUBOBSBLOCK: " + getObjectName(obj));
			const mongo::BSONObj query = generateQueryFromBSONObject(obj);
			writeToDatabase(db.conn, db.SUBOBSBLOCKS, query, obj);
		}
	}

	// save all trials
	{
		const std::vector<Trial> corbaVec = convertSetToVector(metadata.trials);
		const std::vector<mongo::BSONObj> bsonVec = convertCORBATrials(corbaVec);
		BOOST_FOREACH(const mongo::BSONObj &obj, bsonVec) {
			programLogInfoIfPossible("SAVE TRIAL: " + getObjectName(obj));
			const mongo::BSONObj query = generateQueryFromBSONObject(obj);
			writeToDatabase(db.conn, db.TRIALS, query, obj);
		}
	}

	// save all scripts
	{
		BOOST_FOREACH(const mongo::BSONObj &obj, metadata.scripts) {
			programLogInfoIfPossible("SAVE SCRIPT: " + getObjectName(obj));
			const mongo::BSONObj query = generateQueryFromBSONObject(obj);
			writeToDatabase(db.conn, db.SCRIPTS, query, obj);
		}
	}
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

PDB_Run::PDB_Run(const PDB_DB_Params &db, const PDB_Run_Params &params)
	: db_(db)
	, params_(params)
{
	// intentionally left empty
}

short PDB_Run::run() const
{
	const PDB_Run_Params &params = this->params_;
	const PDB_DB_Params &db = this->db_;

	RunMetadata metadata(db, params);

	// Check and see if the project document exists. This is a simple sanity check.
	{
		mongo::BSONObjBuilder bob;
		bob.append("projectID", params.projectID);
		const mongo::BSONObj query = bob.obj();
		if (!documentExists(db.conn, db.PROJECTS, query)) {
			std::ostringstream oss;
			oss << "Project not found: " << params.projectID;
			throw CARMA_ERROR(oss.str());
		}
	}

	// Grab the Obsblock from the database if it exists. If it does not exist
	// and this is a commissioning project, then we can duplicate the
	// "projectID.default" Obsblock automatically (if it exists).
	const ObsblockPtr obs = fetchRequiredObsblock(metadata);

	// various checks to make sure that the project is runnable
	{
		const std::string ac(obs->arrayConfiguration);
		if (!strContains(params.arrayConfig1, ac) && !strContains(params.arrayConfig2, ac) && !strContains(ac, "X")) {
			std::ostringstream oss;
			oss << "This Obsblock is not for the current array configuration: "
				<< getID(params.projectID, params.obsblockID);
			throw CARMA_ERROR(oss.str());
		}

		if ((obs->remainingTime <= 0.0 || obs->status == PSTATUS_COMPLETE) && !obs->exceedTAC) {
			std::ostringstream oss;
			oss << "This Obsblock has no more time allocated: "
				<< getID(params.projectID, params.obsblockID);
			throw CARMA_ERROR(oss.str());
		}
	}

	// fetch SubObsblock(s) and ensure that each object has the correct lastTrial field
	const std::vector<SubObsblockPtr> subVec = fetchRequiredSubObsblocks(metadata);

	// find the next trial number
	const short nextTrialID = getNextTrialID(metadata, subVec);

	// fetch the correct trials
	const std::vector<TrialPtr> trialVec = fetchRequiredTrials(metadata, subVec, nextTrialID);

	// update all SubObsblock(s) to ensure they have the correct trial number too
	BOOST_FOREACH(const SubObsblockPtr sub, subVec) {
		if (sub->lastTrial != nextTrialID) {
			sub->lastTrial = nextTrialID;
			metadata.subobsblocks.insert(sub);
		}
	}

	// sanity check
	if (trialVec.empty()) {
		std::ostringstream oss;
		oss << "No trials returned, unable to run project: "
			<< getID(params.projectID, params.obsblockID, params.subObsblockID);
		throw CARMA_ERROR(oss.str());
	}

	// generate the script objects to be saved to the database
	generateScriptObjects(metadata, trialVec);

	// save all modified objects back to the database
	saveObjects(metadata);

	// return the correct trialID
	{
		const TrialPtr trial = trialVec.at(0);
		return trial->trialID;
	}
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
