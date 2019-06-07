/**
 * @version $Revision: 1.1 $
 * @usage @autogen
 *
 * @description
 * Project Database MongoDB Database Check Utility (standalone). This allows you
 * to run a database check at any time.
 *
 * Note that running this check against the production database may impact the
 * performance of the real time system. Use with cation.
 *
 * @key hostname sdp.carma.pvt string
 * MongoDB Hostname
 *
 * @key port 27017 int
 * MongoDB Port
 *
 * @key database test string
 * MongoDB Database
 *
 * @logger DEFAULT_FACILITY carma.observertools.pdbDbCheck
 */

#include <carma/observertools/ProjectDatabaseManagerImpl.h>
#include <carma/observertools/PDB_Validator.h>
#include <carma/observertools/PDB_MongoDB.h>

#include <carma/util/Program.h>

#include <iostream>
#include <string>
#include <vector>

using namespace carma::observertools;

int carma::util::Program::main()
{
	struct PDBMArgs args;
	args.hostname = getStringParameter("hostname");
	args.port = getIntParameter("port");
	args.database = getStringParameter("database");

	std::cout << "Checking database: " << args.hostname << ":" << args.port
		<< " " << args.database << std::endl;

	const PDB_DB_Params db(args);
	const unsigned int failCount = checkEntireDatabase(db);

	if (failCount <= 0) {
		std::cout << "No failures detected!" << std::endl;
		return EXIT_SUCCESS;
	} else {
		std::cout << failCount << " failures detected, check logs for details" << std::endl;
		return EXIT_FAILURE;
	}
}

/* vim: set ts=4 sts=4 sw=4 noet: */
