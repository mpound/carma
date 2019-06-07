/*
 * CARMA Project Database MongoDB Utilities
 */

#ifndef CARMA_OBSERVERTOOLS_PDB_MONGODB_H
#define CARMA_OBSERVERTOOLS_PDB_MONGODB_H

#include <carma/observertools/ProjectDatabaseManager.h>
#include <carma/observertools/ProjectDatabaseManagerImpl.h>

#include <mongo/client/dbclient.h>
#include <mongo/bson/bsonobj.h>

#include <boost/shared_ptr.hpp>

#include <string>

typedef boost::shared_ptr<mongo::DBClientConnection> DBClientConnectionPtr;

namespace carma {
namespace observertools {

struct PDB_DB_Params {
	const DBClientConnectionPtr conn;

	const std::string DATABASE_NAME;

	const std::string PROJECTS;
	const std::string OBSBLOCKS;
	const std::string SUBOBSBLOCKS;
	const std::string TRIALS;
	const std::string SCRIPTS;

	// Constructor
	PDB_DB_Params(const PDBMArgs &args);

	// Helper
	std::string getCollection(const std::string &name) const;
};

// Initialize the MongoDB Database Client Driver
// Only needs to be run once per application startup
void initializeDatabaseClientDriver();

// Add all of the necessary database indexes
void addDatabaseIndexes(const PDB_DB_Params &db);

// Generate a MongoDB database query from a BSON Object
mongo::BSONObj generateQueryFromBSONObject(const mongo::BSONObj &obj);

// Remove a single BSON Object from the database
void removeFromDatabase(const DBClientConnectionPtr conn, const std::string &collection, const mongo::Query &query);

// Write a single BSON Object to the database
void writeToDatabase(const DBClientConnectionPtr conn, const std::string &collection, const mongo::Query &query, const mongo::BSONObj &obj);

// Check to see if a document exists in the database
bool documentExists(const DBClientConnectionPtr conn, const std::string &collection, const mongo::BSONObj &query);

} // namespace carma::observertools
} // namespace carma

#endif /* CARMA_OBSERVERTOOLS_PDB_MONGODB_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
