/*
 * CARMA Project Database Monitor System
 *
 * This class contains the shortest lived locking possible, so that it can be
 * updated without causing any stalls anywhere else in the application.
 *
 * The scoped version automatically resets the method to "NONE" when it goes
 * out of scope. This makes the code easier to understand. RAII is great.
 */

#ifndef PDB_MONITOR_H
#define PDB_MONITOR_H

#include <carma/util/PthreadMutex.h>

#include <boost/shared_ptr.hpp>

#include <string>
#include <vector>

// forward declaration
namespace carma {
namespace monitor {
class ProjectDatabaseManagerSubsystem;
} // namespace carma::monitor
} // namespace carma

namespace carma {
namespace observertools {

// forward declaration
class PDB_DB_Params;

class PDB_Monitor {
public:
	PDB_Monitor();

	void updateDBInfo(const PDB_DB_Params &db);
	void setCurrentMethod(const std::string &method);
	void writeToMonitorSystem(carma::monitor::ProjectDatabaseManagerSubsystem &mon) const;

private:
	struct PDB_Server_Info {
		double timestamp;
		std::string name;
		int health;
		std::string state;
		int uptime;
		int ping;
	};

	std::vector<PDB_Server_Info> serverInfo_;

	std::string currentMethod_;
	double currentMethodStart_;

	mutable carma::util::PthreadMutex mutex_;
};

typedef boost::shared_ptr<PDB_Monitor> PDB_Monitor_Ptr;

class PDB_Scoped_Monitor {
public:
	PDB_Scoped_Monitor(const PDB_Monitor_Ptr mon, const std::string &method);
	~PDB_Scoped_Monitor();
private:
	const PDB_Monitor_Ptr mon_;
};

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_MONITOR_H */

/* vim: set ts=8 sts=8 sw=8 noet tw=92: */
