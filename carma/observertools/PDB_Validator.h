/*
 * Project Database MongoDB Validator
 */

#ifndef PDB_VALIDATOR_H
#define PDB_VALIDATOR_H

#include <mongo/client/dbclient.h>
#include <mongo/bson/bsonobj.h>
#include <string>

namespace carma {
namespace observertools {

// forward declaration to avoid bringing in an extra header
class PDB_DB_Params;

std::string getObjectName(const mongo::BSONObj &obj);

void checkProjectObject(const mongo::BSONObj &obj);
void checkObsblockObject(const mongo::BSONObj &obj);
void checkSubobsblockObject(const mongo::BSONObj &obj);
void checkTrialObject(const mongo::BSONObj &obj);
void checkScriptObject(const mongo::BSONObj &obj);

// check the entire database, returning the number of failures
unsigned int checkEntireDatabase(const PDB_DB_Params &db);

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_VALIDATOR_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
