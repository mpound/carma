/*
 * CARMA Project Database Edit Functionality
 */

#ifndef PDB_EDIT_H
#define PDB_EDIT_H

#include <carma/observertools/ProjectDatabaseManagerImpl.h>
#include <carma/observertools/ProjectDatabaseManager.h>
#include <carma/observertools/PDB_MongoDB.h>

#include <string>
#include <vector>

namespace carma {
namespace observertools {

std::string editStatusToString(const EditStatus action);

struct PDB_Edit_Params {
	std::string projectID;
	std::string obsblockID;
	std::string subObsblockID;
	int trialID;
	std::vector<ItemValue> editItems;
	EditStatus action;
};

class PDB_Edit {
public:
	PDB_Edit(const PDB_DB_Params &db, const PDB_Edit_Params &params);
	bool run() const;

private:
	const PDB_DB_Params db_;
	const PDB_Edit_Params params_;
};

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_EDIT_H */
