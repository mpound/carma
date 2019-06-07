/**
 * @author Original Dave Mehringer
 *
 * @usage monitorConfigurationLoader conffile=<configuration file>
 * @description
 * monitorConfigurationLoader loads monitor point descriptions from an
 * mpml file into the database.  IMPORTANT it does not handle <Common> elements
 * so if an mpml file contains <Common> elements, mpmlgen -L should be run
 * to produce a file which has the <Common> descriptions integrated into
 * the output file. monitorConfigurationLoader can then be run successfully
 * on this output file
 *
 * @key mpml     none           string  name of mpml file to load
 * @key conffile dbms/dbms.conf string  file from which to get database config info; the conffile location interpreted by Program::getConfFile()
 *
 * @key changefile staticMonitorPointChanges string name of file to write list of attempted changes to static monitor points
 * @key noinsertdenied true	bool	If true, don't throw insert denied. Generate a change file instead.
 * @logger DEFAULT_FACILITY carma.dbms.monitorConfigurationLoader
 */

#include "mysql/mysql_version.h"
#include <stdlib.h> // for debuginng getenv() call
#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/dbms/MPMLException.h"
#include "carma/dbms/SaxHandler.h"
#include "carma/util/Program.h"
#include "carma/util/RuntimeDirs.h"
#include "carma/util/Trace.h"
#include "carma/util/Time.h"
#include "carma/util/StopWatch.h"
#include <iostream>
#include <fstream>
#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/TransService.hpp>

using namespace std;
using namespace carma::dbms;
using carma::util::StopWatch;

static StopWatch runTimeSW, parserSW, mpDescSetTime, mpPopulateSW, mpInsertSW;
static StopWatch commitSW;
static unsigned loadCount = 0;

// Print value of a stopwatch.
static void spit(StopWatch &sw, const char *name, ostringstream &msg)
{
  if(sw.isRunning())
    sw.stop();
   double et = sw.getCumulativeElapsedTime(true);
   msg << name << "=" << et << " seconds; ";
}

static void timeOut(const string &mpmlName)
{ ostringstream msg;

    msg << "Timings for " << mpmlName << " " << loadCount << " entries. ";
    spit(runTimeSW, "runTime", msg);
    spit(parserSW, "parser", msg);
    spit(mpDescSetTime, "mpDescSetTime", msg);
    spit(mpPopulateSW, "mpPopulateSW", msg);
    spit(mpInsertSW, "mpInsertSW", msg);
    spit(commitSW, "commitSW", msg);
    CPTRACE(carma::util::Trace::TRACE5, msg.str());
}

// Append a line to the change file.
static bool appendToInsertErrorLog(const string &changefile,
				   unsigned tagID, const string &insrtMsg)
{string fname = changefile;
 std::fstream file;
 bool status;

 file.open(fname.c_str(), std::ios_base::out|std::ios_base::app);
 status = !file.fail();
 if(! status)
   return false;
 // <Space for instigator's login name> <MD message> < tagID>
 file << "                " << " disposition=\"\""
      << insrtMsg << " tagID=\"" << tagID << "\""
      << endl;
 status = ! file.fail();
 file.close();
 return status;
}

int carma::util::Program::main() {
    runTimeSW.start();
    const ::std::string mpmlFileName = 
      carma::util::Program::getStringParameter("mpml");
    const string changefile =
      carma::util::Program::getStringParameter("changefile");
    const string conffile = getConfFile(getStringParameter("conffile"));
    bool noInsertDenied = getBoolParameter("noinsertdenied");
    int exitStatus = EXIT_SUCCESS;

    CPTRACE(carma::util::Trace::TRACE6, "getStringParameter(\"conffile\") " 
            << getStringParameter("conffile"));
    CPTRACE(carma::util::Trace::TRACE6, "getArg0() " << getArg0());
    CPTRACE(carma::util::Trace::TRACE6, "conffile " << conffile);
    CPTRACE(carma::util::Trace::TRACE6, "changefile " << changefile);
    CPTRACE(carma::util::Trace::TRACE6, "noinsertdenied " << noInsertDenied);
    CPTRACE(carma::util::Trace::TRACE6, "RuntimeDirs::getConfDir() " << RuntimeDirs::getConfDir(getArg0()));
    CPTRACE(carma::util::Trace::TRACE6, "RuntimeDirs::getRootDir() " << RuntimeDirs::getRootDir(getArg0()));
    CPTRACE(carma::util::Trace::TRACE6, "$CARMA " << getenv("CARMA"));

    carma::util::frameType now = carma::util::Time::computeCurrentFrame();
    try {
        XMLPlatformUtils::Initialize();
    } catch (const XMLException &ex) {
        char *message = XMLString::transcode(ex.getMessage());
	ostringstream oss;
        oss << "Error during initialization: \n"
                  << message
                  << std::endl;
	CPTRACE(carma::util::Trace::TRACE3,oss.str().c_str());
        XMLString::release(&message);
        return EXIT_FAILURE;
    }
    {
        auto_ptr<SAXParser> parser(new SAXParser());
        parser->setValidationScheme(SAXParser::Val_Auto);
        parser->setDoNamespaces(true);
        parser->setDoSchema(true);
        parser->setValidationSchemaFullChecking(true);

        auto_ptr<SaxHandler> handler(new SaxHandler());
        parser->setDocumentHandler(handler.get());
        parser->setErrorHandler(handler.get());
        try {
	parserSW.start();
            parser->parse(mpmlFileName.c_str());
	parserSW.stop();
            string aggSubsysName = handler->getAggregateSubsystemName();
            unsigned count = handler->getAggregateSubsystemCount();
            unsigned maxsamples = handler->getAggregateSubsystemMaxSamples();
            unsigned maxpoints = handler->getAggregateSubsystemMaxPoints();
            vector<MonitorDescription> monitorDescriptions 
                = handler->getMonitorDescriptions();
	    mpDescSetTime.start();
            for(unsigned i=0; i < monitorDescriptions.size(); i++) {
                monitorDescriptions[i].setTime(now);            
            }
	    mpDescSetTime.stop();
            CPTRACE(carma::util::Trace::TRACE6, mpmlFileName 
                    << ": total number of points " 
                    << monitorDescriptions.size());
            auto_ptr<DBConfigurator> dbconf(new DBConfigurator(conffile));
            if(DBConnection::isUp(dbconf.get())) {
                CPTRACE(carma::util::Trace::TRACEALL, "database is up, "
                        << "trying to create a connection...");
                auto_ptr<DBConnection> dbc;
                try {
                    auto_ptr<DBConnection> temp
                        (DBConnectionFactory::createConnection(dbconf.get()));
                    dbc = temp;
                    dbc->beginTransaction();
                    CPTRACE(carma::util::Trace::TRACEALL, 
                            "database connection successfully opened");
                    MonitorConfigurationDatabase mcdb(dbc.get());
		    mpPopulateSW.start();
                    if(mcdb.getValiditiesTable().rowCount() == 0) {
                        mcdb.populateValiditiesTable();
                    }
                    if(mcdb.getBlankingFlagsTable().rowCount() == 0) {
                        mcdb.populateBlankingFlagsTable();
                    }
                    if(mcdb.getMonitorPointDataTypesTable().rowCount() == 0) {
                        mcdb.populateMonitorPointDataTypesTable();
                    }
                    if(mcdb.getMonitorPointTypesTable().rowCount() == 0) {
                        mcdb.populateMonitorPointTypesTable();
                    }
                    if(mcdb.getSubsystemsTable().rowCount() == 0) {
                        mcdb.populateSubsystemsTable();
                    }
                    // the correct thing will happen even if these tables already
                    // contain some entries
                    mcdb.populateLocationsTable();
                    mcdb.populateDevicesTable();
                    mcdb.insertAggregateSubsystemsRecord
                        (aggSubsysName, count, maxsamples, maxpoints); 
		    mpPopulateSW.stop();
                    
                    unsigned i, MPsLoaded=0;
		    string insrtMsg;
		    mpInsertSW.start();
                    for(i = 0; i < monitorDescriptions.size(); i++) {
		      CPTRACE(carma::util::Trace::TRACE7, "Processing "
			      << monitorDescriptions[i].getLongName()
			      << "(" 
			      << monitorDescriptions[i].getShortName()
			      << ")");
#if 0
                        mcdb.insertMonitorConfiguration
                            (monitorDescriptions[i]);
#else
                        unsigned tagID = mcdb.insertMonitorConfiguration(
					monitorDescriptions[i],
					insrtMsg, true, noInsertDenied);
			if(!noInsertDenied) // Probably not needed
			  continue;  // since there'd be an exception.
			if(insrtMsg == "")
			  MPsLoaded++;  // MP was inserted..
			else if(insrtMsg != "NI") // NI -> Generic Not Inserted.
			{ // Not inserted because a static field changed.
			  bool ok = appendToInsertErrorLog(changefile,
							   tagID, insrtMsg);
			  CPTRACE(carma::util::Trace::TRACE6,
				  "\"" << monitorDescriptions[i].getName()
				  << "\""
				  << " was not inserted.");
			  if(!ok) // Throw an exception??
			    CPTRACE(carma::util::Trace::TRACE3,
				    "Error appending "
				    << monitorDescriptions[i].getName()
				    << " to error log file: " << changefile);
			  insrtMsg = "";
			  // If we found an incompatible MP, we need to
			  // exit with a failure.
			  exitStatus = EXIT_FAILURE;
			  continue;
			}
#endif
                    }
		    mpInsertSW.stop();
		    commitSW.start();
                    dbc->commitTransaction();
		    commitSW.stop();

                    CPTRACE(carma::util::Trace::TRACE6, MPsLoaded
			    << " of " << monitorDescriptions.size()
			    << " descriptions "
                            << "successfully loaded into the database");
		    loadCount = MPsLoaded;

                } catch (const DBConnectionException &exc) {
		    ostringstream oss;
                    oss << "DBConnectionException caught while loading data "
			<< "from " << mpmlFileName << "  " << exc.what()<< endl;
		    CPTRACE(carma::util::Trace::TRACE3,oss.str().c_str());
                    exc.report();
                    dbc->rollBackTransaction();
		    timeOut(mpmlFileName);
                    return EXIT_FAILURE;
                } catch (const InsertDeniedException &exc) {
                    ostringstream oss;
                    oss << "InsertDeniedException caught while loading data "
                         << "from " << mpmlFileName << "  " << exc.what()<< endl;
                    CPTRACE(carma::util::Trace::TRACE3,oss.str().c_str());
                    exc.report();
                    dbc->rollBackTransaction();
		    timeOut(mpmlFileName);
                    return EXIT_FAILURE;
                } catch (const SQLException &exc) {
                    ostringstream oss;
                    oss << "SQLException caught while loading data from "
                         << mpmlFileName << "  " << exc.what()<< endl;
                    CPTRACE(carma::util::Trace::TRACE3,oss.str().c_str());
                    exc.report();
                    dbc->rollBackTransaction();
		    timeOut(mpmlFileName);
                    return EXIT_FAILURE;
                }
            } else {
                CPTRACE(carma::util::Trace::TRACE4, "Database cannot be "
                        << "contacted so descriptions cannot be loaded into "
                        << "it");
            }
        } catch (const XMLException &ex) {
            char *message = XMLString::transcode(ex.getMessage());
	    ostringstream oss;
            oss << "An error occurred: "
                      << message
                      << std::endl;
            CPTRACE(carma::util::Trace::TRACE3,oss.str().c_str());
	    XMLString::release(&message);
            XMLPlatformUtils::Terminate();
	    timeOut(mpmlFileName);
            return EXIT_FAILURE;
        } catch (const MPMLException &exc) {
	    ostringstream oss;
            oss << "MPMLException caught while parsing " << mpmlFileName 
                 << "  " << exc.what()<< endl;
            CPTRACE(carma::util::Trace::TRACE3,oss.str().c_str());
	    exc.report();
	    timeOut(mpmlFileName);
            return EXIT_FAILURE;
        } 
    }
    XMLPlatformUtils::Terminate();
    timeOut(mpmlFileName);
    return exitStatus;
}
