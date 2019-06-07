/*
 * CARMA Project Database Monitor System
 */

#include <carma/observertools/PDB_Monitor.h>
#include <carma/observertools/PDB_MongoDB.h>

#include <carma/monitor/ProjectDatabaseManagerSubsystem.h>

#include <carma/util/ScopedPthreadMutexLock.h>
#include <carma/util/programLogging.h>
#include <carma/util/Time.h>

using namespace carma::observertools;
using namespace carma::monitor;
using namespace carma::util;

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

PDB_Monitor::PDB_Monitor()
	: serverInfo_(ProjectDatabaseManagerSubsystem::serverCount())
	, currentMethod_("NONE")
	, currentMethodStart_(Time::MJD())
	, mutex_()
{
	// intentionally left empty
}

void PDB_Monitor::updateDBInfo(const PDB_DB_Params &db)
{
	mongo::BSONObj obj;
	const bool result = db.conn->runCommand("admin", BSON("replSetGetStatus" << 1), obj);
	if (!result) {
		std::ostringstream oss;
		oss << "Unable to retrieve MongoDB replica set status: " << db.conn->getLastError();
		programLogErrorIfPossible(oss.str());
		return;
	}

	if (!obj.hasField("members")) {
		programLogErrorIfPossible("MongoDB database info object does not have members array");
		return;
	}

	std::vector<PDB_Server_Info> serverInfo(ProjectDatabaseManagerSubsystem::serverCount());
	const std::vector<mongo::BSONElement> members = obj["members"].Array();
	for (size_t i = 0; i < serverInfo.size(); i++) {
		PDB_Server_Info &info = serverInfo.at(i);

		// set default values
		info.timestamp = Time::MJD();
		info.name = "NONE";
		info.health = 0;
		info.state = "NONE";
		info.uptime = 0;
		info.ping = 0;

		// no more info returned from server, leave default values alone
		if (i >= members.size()) {
			continue;
		}

		// store values from server
		const mongo::BSONObj &member = members.at(i).embeddedObject();
		info.name = member["name"].valuestrsafe();
		info.health = member["health"].numberInt();
		info.state = member["stateStr"].valuestrsafe();
		info.uptime = member["uptime"].numberInt();
		if (member.hasField("pingMs")) {
			info.ping = member["pingMs"].numberInt();
		}
	}

	// success, insert new data under lock
	const ScopedPthreadMutexLock lock(mutex_);
	serverInfo_ = serverInfo;
}

void PDB_Monitor::setCurrentMethod(const std::string &method)
{
	const ScopedPthreadMutexLock lock(mutex_);
	currentMethod_ = method;
	currentMethodStart_ = Time::MJD();
}

void PDB_Monitor::writeToMonitorSystem(ProjectDatabaseManagerSubsystem &mon) const
{
	const ScopedPthreadMutexLock lock(mutex_);
	mon.currentMethod().setValue(currentMethod_);
	mon.currentMethodStart().setValue(currentMethodStart_);

	for (int i = 0; i < ProjectDatabaseManagerSubsystem::serverCount(); i++) {
		const PDB_Server_Info &info = serverInfo_.at(i);
		ProjectDatabaseManagerSubsystem::Server &server = mon.server(i);

		server.timestamp().setValue(info.timestamp);
		server.name().setValue(info.name);
		server.health().setValue(info.health);
		server.state().setValue(info.state);
		server.uptime().setValue(info.uptime);
		server.ping().setValue(info.ping);
	}
}

PDB_Scoped_Monitor::PDB_Scoped_Monitor(const PDB_Monitor_Ptr mon, const std::string &method)
	: mon_(mon)
{
	mon_->setCurrentMethod(method);
}

PDB_Scoped_Monitor::~PDB_Scoped_Monitor()
{
	mon_->setCurrentMethod("NONE");
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
