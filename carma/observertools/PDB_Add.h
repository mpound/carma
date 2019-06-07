/*
 * CARMA Project Database projectAdd() Implementation
 */

#ifndef PDB_ADD_H
#define PDB_ADD_H

#include <carma/observertools/ProjectDatabaseManagerImpl.h>
#include <carma/observertools/ProjectDatabaseManager.h>
#include <carma/observertools/PDB_MongoDB.h>

#include <string>

namespace carma {
namespace observertools {

class PDB_Add {
public:
	PDB_Add(const PDB_DB_Params &db, const std::string &xmlString);
	bool run() const;

private:
	const PDB_DB_Params db_;
	const std::string xmlString_;
};

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_ADD_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
