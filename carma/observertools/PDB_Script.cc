/*
 * CARMA Project Database Script/Catalog Functionality
 */

#include <carma/observertools/PDB_Script.h>
#include <carma/observertools/PDB_Query.h>
#include <carma/observertools/PDB_Util.h>
#include <carma/observertools/QueryTags.h>

#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>
#include <carma/util/FileUtils.h>

#include <boost/foreach.hpp>

#include <fstream>
#include <sstream>
#include <string>

using namespace carma::observertools;
using namespace carma::util;

/* -------------------------------------------------------------------------- */
/* Helper Methods                                                             */
/* -------------------------------------------------------------------------- */

static std::string readFileIfExists(const std::string &filename)
{
	// not specified, return empty string
	if (identifierIsNoneOrEmpty(filename))
		return "";

	if (!FileUtils::exists(filename)) {
		std::ostringstream oss;
		oss << "File does not exist: " << filename;
		throw CARMA_ERROR(oss.str());
	}

	// read the entire file
	{
		std::ifstream fin(filename.c_str(), std::ios::binary);
		std::ostringstream oss;
		oss << fin.rdbuf();

		return oss.str();
	}
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

PDB_Script::PDB_Script(const PDB_DB_Params &db, const PDB_Script_Params &params)
	: db_(db)
	, params_(params)
{
	// intentionally left empty
}

PDB_Script_Return PDB_Script::get() const
{
	const PDB_Script_Params &params = this->params_;
	const PDB_DB_Params &db = this->db_;
	mongo::BSONObjBuilder builder;

	builder.append("projectID", params.projectID);
	builder.append("obsblockID", params.obsblockID);
	builder.append("subObsblockID", params.subObsblockID);

	const mongo::BSONObj query = builder.obj();

	const std::auto_ptr<mongo::DBClientCursor> cursor = db.conn->query(db.SCRIPTS, query);

	mongo::BSONObj resultObj;
	while (cursor->more()) {
		const mongo::BSONObj obj = cursor->next();

		// handle first object correctly
		if (!resultObj.hasField("trialID")) {
			resultObj = obj.copy();
		}

			// current object has a higher trialID, save it instead
		if (obj.getIntField("trialID") > resultObj.getIntField("trialID")) {
			resultObj = obj.copy();
		}
	}

	// check to make sure we found at least one result
	if (!resultObj.hasField("trialID")) {
		programLogInfoIfPossible("no script or catalog found");

		// return an empty result
		PDB_Script_Return result;
		return result;
	}

	// log what we found
	{
		std::ostringstream oss;
		oss << "the highest numbered trial found was: " << resultObj.getIntField("trialID");
		programLogInfoIfPossible(oss.str());
	}

	// convert it to a convenient return format
	PDB_Script_Return result;
	result.script = resultObj.getStringField("script");
	result.catalog = resultObj.getStringField("catalog");
	return result;
}

void PDB_Script::put(const std::string &scriptFile, const std::string &catalogFile, const short trialID) const
{
	const PDB_DB_Params &db = this->db_;

	// nothing to do, don't save a record for no input
	if (identifierIsNoneOrEmpty(scriptFile) && identifierIsNoneOrEmpty(catalogFile)) {
		return;
	}

	// write the object to the database
	{
		const mongo::BSONObj obj = getScriptObject(scriptFile, catalogFile, trialID);
		const mongo::Query writeQuery = generateQueryFromBSONObject(obj);
		writeToDatabase(db.conn, db.SCRIPTS, writeQuery, obj);
	}
}

mongo::BSONObj PDB_Script::getScriptObject(const std::string &scriptFile, const std::string &catalogFile, const short trialID) const
{
	const PDB_Script_Params &params = this->params_;
	const std::string scriptContents = readFileIfExists(scriptFile);
	const std::string catalogContents = readFileIfExists(catalogFile);

	mongo::BSONObjBuilder builder;
	builder.append("projectID", params.projectID);
	builder.append("obsblockID", params.obsblockID);
	builder.append("subObsblockID", params.subObsblockID);
	builder.append("trialID", trialID);
	builder.append("script", scriptContents);
	builder.append("catalog", catalogContents);

	return builder.obj().copy();
}

short PDB_Script::findLastTrialID() const
{
	const PDB_Script_Params &params = this->params_;
	const PDB_DB_Params &db = this->db_;

	std::vector<ItemValue> queryItems;
	queryItems.push_back(makeItemValue(QueryTags::PROJECT, params.projectID));
	queryItems.push_back(makeItemValue(QueryTags::OBSBLOCK, params.obsblockID));
	queryItems.push_back(makeItemValue(QueryTags::SUBOBSBLOCK, params.subObsblockID));

	const PDB_Query query(db, queryItems);
	const std::vector<Project> projects = query.run();

	short maxTrialID = -1;
	BOOST_FOREACH(const Project &proj, projects) {
		for (CORBA::ULong oIdx = 0; oIdx < proj.obsblock.length(); oIdx++) {
			const Obsblock &obs = proj.obsblock[oIdx];
			for (CORBA::ULong sIdx = 0; sIdx < obs.subObsblock.length(); sIdx++) {
				const SubObsblock &sub = obs.subObsblock[sIdx];
				for (CORBA::ULong tIdx = 0; tIdx < sub.trial.length(); tIdx++) {
					const Trial &trial = sub.trial[tIdx];
					maxTrialID = std::max(trial.trialID, maxTrialID);
				}
			}
		}
	}

	if (maxTrialID == -1) {
		std::ostringstream oss;
		oss << "Unable to add script/catalog: no trials found for SubObsblock "
			<< params.projectID << "." << params.obsblockID;
		if (!params.subObsblockID.empty()) {
			oss << "." << params.subObsblockID;
		}

		throw CARMA_ERROR(oss.str());
	}

	return maxTrialID;
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
