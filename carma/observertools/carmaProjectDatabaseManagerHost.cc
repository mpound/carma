/** $Id: carmaProjectDatabaseManagerHost.cc,v 1.20 2014/11/03 19:12:28 iws Exp $
 *
 * @usage @autogen
 *
 * @description
 * Server for the Project Database Manager system.
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
 * @key rt false bool
 * Running on the CARMA Real Time System.
 *
 * @key awdelay 0.200 double
 * Monitor system autowrite delay in seconds
 *
 * @logger DEFAULT_FACILITY carma.observertools.projectDatabaseManager
 *
 * @author Douglas N. Friedel
 * @author Ira W. Snyder
 */

#include <carma/corba/Server.h>

#include <carma/util/Time.h>
#include <carma/util/Program.h>
#include <carma/util/ThreadQuit.h>
#include <carma/util/StartPthread.h>
#include <carma/util/programLogging.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/FrameAlignedTimer.h>
#include <carma/util/AutoPthreadQuitAndJoinGroup.h>

#include <carma/observertools/ProjectDatabaseManagerImpl.h>
#include <carma/observertools/ProjectDatabaseManager_skel.h>
#include <carma/observertools/ProjectDatabaseManager_skel_tie.h>
#include <carma/observertools/PDB_MongoDB.h>
#include <carma/observertools/PDB_Monitor.h>

#include <carma/monitor/ProjectDatabaseManagerSubsystem.h>

#include <iostream>

using namespace carma::observertools;
using namespace carma::util;

// Monitor Thread Entry Point -- publishes data to the FSP every frame
static void monitorThreadEP(PDBMArgs &args)
{
    carma::monitor::ProjectDatabaseManagerSubsystem pdbmon;
    FrameAlignedTimer framer;

    pdbmon.startAutoWriter(args.awdelay);
    framer.ResetNextFireTime();

    // create database connection
    const PDB_DB_Params db(args);

    // register for thread quit notifications
    const ScopedThreadQuitRegisterSelf quitRegister;

    while (true) {
        try {
            ThreadQuitTestSelf();
            framer.WaitForNextFireTime();
            ThreadQuitTestSelf();

            // internal points
            pdbmon.timeStamp().setValue(Time::MJD());
            pdbmon.online().setValue(true);

            // update database info only every 20 seconds
            if ((Time::computeClosestFrame() % 40) == 0) {
                args.monitor->updateDBInfo(db);
            }

            // points from the database itself
            args.monitor->writeToMonitorSystem(pdbmon);

        } catch (...) {
            if (CaughtExceptionIsThreadQuitRequestedError()) {
                programLogInfoIfPossible("monitor thread exit requested");
                pdbmon.stopAutoWriter();
                return;
            }

            std::ostringstream oss;
            oss << "monitor thread exception: " << getStringForCaught();
            programLogErrorIfPossible(oss.str());
        }
    }
}

int Program::main()
try {
    struct PDBMArgs args;
    args.hostname = getStringParameter("hostname");
    args.port = getIntParameter("port");
    args.database = getStringParameter("database");
    args.rt = getBoolParameter("rt");
    args.monitor = PDB_Monitor_Ptr(new PDB_Monitor());
    args.awdelay = getDoubleParameter("awdelay");

    // create servant
    ProjectDatabaseManagerImpl servant(args);

    // create corba server
    corba::Server & server = Program::getCorbaServer();
    server.addServant<POA_carma::observertools::ProjectDatabaseManager_tie>
        ( servant, observertools::PROJECT_DATABASE_MANAGER_NAME );

    // start monitor thread
    AutoPthreadQuitAndJoinGroup group;
    group.insert(StartPthreadWithRef(monitorThreadEP, args));

    // block in this thread until termination is requested
    programLogInfoIfPossible("Blocking on ORB forever - type ^C to terminate");
    server.run(false);
    programLogInfoIfPossible("ORB stopped blocking");

    return EXIT_SUCCESS;

} catch(...) {
    std::ostringstream oss;
    oss << "Exception caught: " << getStringForCaught();
    programLogCriticalIfPossible(oss.str());
    std::cerr << oss.str() << std::endl;
    return EXIT_FAILURE;
}

/* vim: set ts=4 sts=4 sw=4 et: */
