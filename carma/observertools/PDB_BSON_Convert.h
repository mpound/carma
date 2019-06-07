/*
 * CARMA Project Database BSON <-> CORBA Conversion
 */

#ifndef PDB_BSON_CONVERT_H
#define PDB_BSON_CONVERT_H

#include <carma/observertools/ProjectDatabaseManager.h>

#include <mongo/client/dbclient.h>
#include <mongo/bson/bsonobj.h>

#include <string>
#include <vector>

namespace carma {
namespace observertools {

// Convert BSON object types to CORBA object types
std::vector<Project> convertBSONProjects(const std::vector<mongo::BSONObj> &objects);
std::vector<Obsblock> convertBSONObsblocks(const std::vector<mongo::BSONObj> &objects);
std::vector<SubObsblock> convertBSONSubObsblocks(const std::vector<mongo::BSONObj> &objects);
std::vector<Trial> convertBSONTrials(const std::vector<mongo::BSONObj> &objects);

// Convert CORBA object types to BSON object types
std::vector<mongo::BSONObj> convertCORBAProjects(const std::vector<Project> &objects);
std::vector<mongo::BSONObj> convertCORBAObsblocks(const std::vector<Obsblock> &objects);
std::vector<mongo::BSONObj> convertCORBASubObsblocks(const std::vector<SubObsblock> &objects);
std::vector<mongo::BSONObj> convertCORBATrials(const std::vector<Trial> &objects);

// Structure to help with merge
struct CORBA_Object_Merge {
	std::vector<Project> projects;
	std::vector<Obsblock> obsblocks;
	std::vector<SubObsblock> subobsblocks;
	std::vector<Trial> trials;
};

// Merge a group of many CORBA Objects into a vector of many Projects
std::vector<Project> mergeCORBAObjects(const CORBA_Object_Merge &merge);

// Split a vector of many Projects into a group of many CORBA Objects
CORBA_Object_Merge splitCORBAObjects(const std::vector<Project> &projects);

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_BSON_CONVERT_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
