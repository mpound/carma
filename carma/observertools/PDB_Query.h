/*
 * CARMA Project Database Query Functionality
 */

#ifndef PDB_QUERY_H
#define PDB_QUERY_H

#include <carma/observertools/ProjectDatabaseManagerImpl.h>
#include <carma/observertools/ProjectDatabaseManager.h>
#include <carma/observertools/PDB_MongoDB.h>

#include <vector>

namespace carma {
namespace observertools {

class PDB_Query {
public:
	PDB_Query(const PDB_DB_Params &db, const ItemValueSequence &theQuery);
	PDB_Query(const PDB_DB_Params &db, const std::vector<ItemValue> &theQuery);
	std::vector<Project> run() const;

private:
	const PDB_DB_Params db_;
	const std::vector<ItemValue> queryParams_;
};

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_QUERY_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
