/*
 * CARMA Project Database runProject() Implementation
 */

#ifndef PDB_RUN_H
#define PDB_RUN_H

#include <carma/observertools/ProjectDatabaseManagerImpl.h>
#include <carma/observertools/ProjectDatabaseManager.h>
#include <carma/observertools/PDB_MongoDB.h>

#include <string>

namespace carma {
namespace observertools {

struct PDB_Run_Params {
	std::string projectID;
	std::string obsblockID;
	std::string subObsblockID;

	bool isCommissioning;
	bool isDualCorr;

	std::string arrayConfig1;
	std::string arrayConfig2;

	std::string scriptFile;
	std::string catalogFile;
};

class PDB_Run {
public:
	PDB_Run(const PDB_DB_Params &db, const PDB_Run_Params &params);
	short run() const;

private:
	const PDB_DB_Params db_;
	const PDB_Run_Params params_;
};

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_RUN_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
