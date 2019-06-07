/*
 * CARMA Project Database Script/Catalog Functionality
 */

#ifndef PDB_SCRIPT_H
#define PDB_SCRIPT_H

#include <carma/observertools/ProjectDatabaseManagerImpl.h>
#include <carma/observertools/ProjectDatabaseManager.h>
#include <carma/observertools/PDB_MongoDB.h>

namespace carma {
namespace observertools {

struct PDB_Script_Params {
	std::string projectID;
	std::string obsblockID;
	std::string subObsblockID;
};

struct PDB_Script_Return {
	std::string script;
	std::string catalog;
};

class PDB_Script {
public:
	PDB_Script(const PDB_DB_Params &db, const PDB_Script_Params &params);
	void put(const std::string &scriptFile, const std::string &catalogFile, const short trialID) const;
	PDB_Script_Return get() const;

	mongo::BSONObj getScriptObject(const std::string &scriptFile, const std::string &catalogFile, const short trialID) const;
	short findLastTrialID() const;

private:
	const PDB_DB_Params db_;
	const PDB_Script_Params params_;
};

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_SCRIPT_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
