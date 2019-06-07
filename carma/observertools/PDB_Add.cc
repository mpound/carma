/*
 * CARMA Project Database projectAdd() Implementation
 */

#include <carma/observertools/PDB_Add.h>
#include <carma/observertools/PDB_Util.h>
#include <carma/observertools/PDB_Validator.h>
#include <carma/observertools/PDB_XML_Convert.h>

#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>

#include <boost/foreach.hpp>

#include <sstream>
#include <string>
#include <vector>

using namespace carma::observertools;
using namespace carma::util;

static std::vector<mongo::BSONObj> convertJsonToBson(const std::vector<std::string> &jsonVec)
{
	std::vector<mongo::BSONObj> bsonVec;

	BOOST_FOREACH(const std::string &json, jsonVec) {
		const mongo::BSONObj obj = mongo::fromjson(json);
		bsonVec.push_back(obj);
	}

	return bsonVec;
}

// Check many BSON objects for validity
typedef void (*ObjectCheckFn)(const mongo::BSONObj &obj);
static void checkObjectsValidity(const std::vector<mongo::BSONObj> &objectVec, ObjectCheckFn check)
{
	BOOST_FOREACH(const mongo::BSONObj &obj, objectVec) {
		check(obj);
	}
}

// Check to see that none of the objects to be added exist in the database
static void checkObjectsExist(
		const DBClientConnectionPtr conn,
		const std::string &collection,
		const std::vector<mongo::BSONObj> &objectVec)
{
	BOOST_FOREACH(const mongo::BSONObj &obj, objectVec) {
		const mongo::BSONObj query = generateQueryFromBSONObject(obj);
		if (documentExists(conn, collection, query)) {
			std::ostringstream oss;
			oss << "Object already exists in database: " << getObjectName(obj);
			throw CARMA_ERROR(oss.str());
		}
	}
}

// Write many objects to the database
static void writeObjectsToDatabase(
		const DBClientConnectionPtr conn,
		const std::string &collection,
		const std::vector<mongo::BSONObj> &objectVec)
{
	BOOST_FOREACH(const mongo::BSONObj &obj, objectVec) {
		const mongo::BSONObj query = generateQueryFromBSONObject(obj);
		writeToDatabase(conn, collection, query, obj);
	}
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

PDB_Add::PDB_Add(const PDB_DB_Params &db, const std::string &xmlString)
	: db_(db)
	, xmlString_(xmlString)
{
	// intentionally left empty
}

bool PDB_Add::run() const
{
	typedef std::vector<mongo::BSONObj> BSONObjVec;
	typedef std::map<std::string, BSONObjVec> BSONObjMap;

	const PDB_DB_Params &db = this->db_;
	const std::string &xmlString = this->xmlString_;

	PDB_JSON_Results results;
	const bool pretty = false;

	convertXmlToJson(xmlString, pretty, results);

	// convert all JSON strings into BSON objects
	BSONObjMap objMap;
	objMap["projects"] = convertJsonToBson(results.projects);
	objMap["obsblocks"] = convertJsonToBson(results.obsblocks);
	objMap["subobsblocks"] = convertJsonToBson(results.subobsblocks);
	objMap["trials"] = convertJsonToBson(results.trials);
	objMap["scripts"] = convertJsonToBson(results.scripts);

	// check each BSON object to ensure that it has all fields,
	// and that each field has the correct type
	checkObjectsValidity(objMap["projects"], checkProjectObject);
	checkObjectsValidity(objMap["obsblocks"], checkObsblockObject);
	checkObjectsValidity(objMap["subobsblocks"], checkSubobsblockObject);
	checkObjectsValidity(objMap["trials"], checkTrialObject);
	checkObjectsValidity(objMap["scripts"], checkScriptObject);

	// check to see that none of the objects to be added exist already
	BOOST_FOREACH(const BSONObjMap::value_type &elem, objMap) {
		const std::string collection = db.getCollection(elem.first);
		const BSONObjVec &objs = elem.second;

		checkObjectsExist(db.conn, collection, objs);
	}

	// write everything to the database
	BOOST_FOREACH(const BSONObjMap::value_type &elem, objMap) {
		const std::string collection = db.getCollection(elem.first);
		const BSONObjVec &objs = elem.second;

		writeObjectsToDatabase(db.conn, collection, objs);
	}

	return true;
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
