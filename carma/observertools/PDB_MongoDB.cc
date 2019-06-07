/*
 * CARMA Project Database MongoDB Utilities
 */

#include <carma/observertools/PDB_MongoDB.h>

#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

#include <boost/foreach.hpp>

#include <sstream>
#include <vector>

using namespace carma::observertools;
using namespace carma::util;

/* -------------------------------------------------------------------------- */
/* Private Helper Methods                                                     */
/* -------------------------------------------------------------------------- */

static DBClientConnectionPtr getDBConnection(const struct PDBMArgs &args)
{
	const bool autoReconnect = true;
	DBClientConnectionPtr conn(new mongo::DBClientConnection(autoReconnect));

	{
		std::ostringstream oss;
		oss << args.hostname << ":" << args.port;
		const std::string hostAndPort = oss.str();

		std::string errMsg;
		if (conn->connect(hostAndPort, errMsg) == false) {
			programLogErrorIfPossible("database connection failed: " + errMsg);
			throw CARMA_ERROR("database connection failed: " + errMsg);
		}
	}

	return conn;
}

static std::string getDBCollection(const PDBMArgs &args, const std::string &collection)
{
	std::ostringstream oss;
	oss << args.database << "." << collection;
	return oss.str();
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

PDB_DB_Params::PDB_DB_Params(const PDBMArgs &args)
	: conn(getDBConnection(args))
	, DATABASE_NAME(args.database)
	, PROJECTS(getDBCollection(args, "projects"))
	, OBSBLOCKS(getDBCollection(args, "obsblocks"))
	, SUBOBSBLOCKS(getDBCollection(args, "subobsblocks"))
	, TRIALS(getDBCollection(args, "trials"))
	, SCRIPTS(getDBCollection(args, "scripts"))
{
	// intentionally left empty
}

std::string PDB_DB_Params::getCollection(const std::string &name) const
{
	if (name == "projects") {
		return this->PROJECTS;
	} else if (name == "obsblocks") {
		return this->OBSBLOCKS;
	} else if (name == "subobsblocks") {
		return this->SUBOBSBLOCKS;
	} else if (name == "trials") {
		return this->TRIALS;
	} else if (name == "scripts") {
		return this->SCRIPTS;
	} else {
		std::ostringstream oss;
		oss << "Unknown Database Collection: " << name;
		throw CARMA_ERROR(oss.str());
	}
}

void initializeDatabaseClientDriver()
{
	const mongo::Status status = mongo::client::initialize();
	if (!status.isOK()) {
		std::ostringstream oss;
		oss << "Failed to initialize MongoDB client driver: " << status.toString();
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

void addDatabaseIndexes(const PDB_DB_Params &db)
{
	// projects
	db.conn->ensureIndex(db.PROJECTS, BSON("completeProjectID" << 1));
	db.conn->ensureIndex(db.PROJECTS, BSON("projectID" << 1));

	// obsblocks
	db.conn->ensureIndex(db.OBSBLOCKS, BSON("completeProjectID" << 1));
	db.conn->ensureIndex(db.OBSBLOCKS, BSON("completeObsblockID" << 1));
	db.conn->ensureIndex(db.OBSBLOCKS, BSON("projectID" << 1));
	db.conn->ensureIndex(db.OBSBLOCKS, BSON("obsblockID" << 1));

	// subobsblocks
	db.conn->ensureIndex(db.SUBOBSBLOCKS, BSON("completeProjectID" << 1));
	db.conn->ensureIndex(db.SUBOBSBLOCKS, BSON("completeObsblockID" << 1));
	db.conn->ensureIndex(db.SUBOBSBLOCKS, BSON("completeSubObsblockID" << 1));
	db.conn->ensureIndex(db.SUBOBSBLOCKS, BSON("projectID" << 1));
	db.conn->ensureIndex(db.SUBOBSBLOCKS, BSON("obsblockID" << 1));
	db.conn->ensureIndex(db.SUBOBSBLOCKS, BSON("subObsblockID" << 1));

	// trials
	db.conn->ensureIndex(db.TRIALS, BSON("completeProjectID" << 1));
	db.conn->ensureIndex(db.TRIALS, BSON("completeObsblockID" << 1));
	db.conn->ensureIndex(db.TRIALS, BSON("completeSubObsblockID" << 1));
	db.conn->ensureIndex(db.TRIALS, BSON("completeTrialID" << 1));
	db.conn->ensureIndex(db.TRIALS, BSON("projectID" << 1));
	db.conn->ensureIndex(db.TRIALS, BSON("obsblockID" << 1));
	db.conn->ensureIndex(db.TRIALS, BSON("subObsblockID" << 1));
	db.conn->ensureIndex(db.TRIALS, BSON("trialID" << 1));

	// scripts
	db.conn->ensureIndex(db.SCRIPTS, BSON("projectID" << 1));
	db.conn->ensureIndex(db.SCRIPTS, BSON("obsblockID" << 1));
	db.conn->ensureIndex(db.SCRIPTS, BSON("subObsblockID" << 1));
	db.conn->ensureIndex(db.SCRIPTS, BSON("trialID" << 1));
}

mongo::BSONObj generateQueryFromBSONObject(const mongo::BSONObj &obj)
{
	mongo::BSONObjBuilder bob;

	std::vector<std::string> keys;
	keys.push_back("projectID");
	keys.push_back("obsblockID");
	keys.push_back("subObsblockID");

	BOOST_FOREACH(const std::string &key, keys) {
		if (obj.hasField(key)) {
			bob.append(key, obj[key].str());
		}
	}

	// trialID is an integer, not a string
	if (obj.hasField("trialID")) {
		bob.append("trialID", obj["trialID"].numberInt());
	}

	return bob.obj();
}

void removeFromDatabase(
		const DBClientConnectionPtr conn,
		const std::string &collection,
		const mongo::Query &query)
{
	// remove the object
	const bool justOne = true;
	conn->remove(collection, query, justOne);

	// check for errors
	const bool fsync = false;
	const bool journal = true;
	const std::string error = conn->getLastError(collection, fsync, journal);
	if (!error.empty()) {
		std::ostringstream oss;
		oss << "ERROR: database remove failed: " << error;
		throw CARMA_ERROR(oss.str());
	}
}

void writeToDatabase(
		const DBClientConnectionPtr conn,
		const std::string &collection,
		const mongo::Query &query,
		const mongo::BSONObj &obj)
{
	// replace the object
	const bool upsert = true;
	const bool multi = false;
	conn->update(collection, query, obj, upsert, multi);

	// check for errors
	const bool fsync = false;
	const bool journal = true;
	const std::string error = conn->getLastError(collection, fsync, journal);
	if (!error.empty()) {
		std::ostringstream oss;
		oss << "ERROR: database update failed: " << error;
		throw CARMA_ERROR(oss.str());
	}
}

bool documentExists(
		const DBClientConnectionPtr conn,
		const std::string &collection,
		const mongo::BSONObj &query)
{
	const unsigned long long count = conn->count(collection, query);
	return count > 0;
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
