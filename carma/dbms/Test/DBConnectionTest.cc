/** 
 * @file carma/environment/Test/DBConnectionTest.cc
 *
 * CppUnit test fixture for carma::dbms::DBConnection
 *
 * @author Dave Mehringer
 * @version $Id: DBConnectionTest.cc,v 1.42 2011/08/20 17:37:41 scott Exp $
 */
#include <cmath>
#include <iostream>
#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/TransService.hpp>
#include "DBConnectionTest.h"
#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/dbms/MPMLException.h"
#include "carma/dbms/ResultsCache.h"
#include "carma/dbms/SaxHandler.h"
#include "carma/dbms/Table.h"
#include "carma/dbms/TableNames.h"
#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/SubsystemFrame.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"

using namespace carma::dbms;
//using namespace carma::dbms::Test;
using namespace CppUnit;
using namespace std;

string DBConnectionTest::conffile = "";
string DBConnectionTest::mpmlFileName = "";

void DBConnectionTest::setUp() {
    //string dbname = DBConnection::TEST_MONITOR_DB;
    //rdbms = "PostgreSQL";
    //string conffile="conf/dbms/dbms.test.conf";
    // FIXME I haven't figured out a better way of being able to set parameters
    // passed in from the launching program for test suites
    //    char *cf = getenv("CARMA_DBMSTESTCONF");
    //if(cf != NULL) {
    //       conffile = string(cf);
    //}
    if(conffile == "") {
        conffile="conf/dbms/dbms.test.conf";
    }    
    //cout << "DBConnectionTest constructor: using " << conffile << " as the "
    //<< "config file" << endl;
    dbconf_ = new DBConfigurator(conffile);
    dbc_ = DBConnectionFactory::createConnection(dbconf_);
}

void DBConnectionTest::tearDown() {   
    delete dbc_;
    delete dbconf_;
}

void DBConnectionTest::upTests() {
    //cout << endl;
    //cout << "Testing DBConnection::isUp()" << endl;
    CPPUNIT_ASSERT( DBConnection::isUp(dbconf_)); 
    //cout << "Testing dbc_->isDBUp()" << endl;
    CPPUNIT_ASSERT( dbc_->isDBUp()); 
}

void DBConnectionTest::memberTests() {
    //cout << endl;
    //cout << "Testing get data member functions" << endl;
    //cout << dbc_->rdbmsName() << endl;
    //cout << dbconf_->getRDBMS() << endl;
    CPPUNIT_ASSERT( carma::util::StringUtils::lowASCIIAlphaNumericToLower(
                                                           dbc_->rdbmsName())
                    == carma::util::StringUtils::lowASCIIAlphaNumericToLower(
                                                       dbconf_->getRDBMS())); 
}

// the tDBConnectionTest.rt script has already created these tables
void DBConnectionTest::loadMonitorSubsystemTableTest() {
    //cout << endl;
    //cout << "Testing populateSubsystemTable" << endl;
    // the only way this will fail is if a DBConnectionException is thrown
    // populate the various auxiliary config tables
    MonitorConfigurationDatabase mcdb(dbc_);
    mcdb.populateSubsystemsTable();
    mcdb.populateLocationsTable();
    mcdb.populateDevicesTable();
}

void DBConnectionTest::monitorConfigInsertTests() {
    //cout << endl;
    //cout << "Testing monitor configuration inserts" << endl;
    //cout << "MPTYPE_SOFT " << MPTYPE_SOFT << endl;
    //cout << "DATATYPE_DOUBLE " << DATATYPE_DOUBLE << endl;
    MonitorDescription md("Bima6.my.monitor.point",MPTYPE_SOFT,
                          DATATYPE_DOUBLE,true,1,false);
    md.setShortName("mp5");
    md.setLongName("this is a test point for testing");
    md.setUnits("m/s");
    md.setDescription("this is a test description for a monitor point");
    md.setTime(carma::util::Time::computeCurrentFrame());
    MonitorConfigurationDatabase mcdb(dbc_);
    int tagID1 = mcdb.insertMonitorConfiguration(md);

    // insert a maximum tagID for the Bima7 subsystem so that we can check that
    // an exception is correctly thrown if an additional point is inserted
    ushort subsysID = mcdb.getSubsystemID("Bima7");
    ushort monPointID = 65535;
    uint maxTagID = carma::dbms::TagIDAuthority
        ::composeTagID(subsysID,monPointID);
    // insert a point with the maximum tagID for subsystems 
    //cout << "maximum allowable tagID for subsystemID " << subsysID << " is "
    //<< maxTagID << endl;
    ostringstream ss;
    ss << "INSERT INTO " << getTableName(STATIC_MONITOR_CONFIG_TABLE) 
       << " (tagID, subsysID, name, units, dataTypeID, mpTypeID, "
       << "isPersistent, isSpectrum) VALUES (" << maxTagID 
       << ", 7, 'my.favorite.monitor.point', 'm/s', " 
       << mpDataType2DB(DATATYPE_DOUBLE) << ", " << mpType2DB(MPTYPE_SOFT)
       << ", 1,0)";
    dbc_->directSQLExec_(ss.str());
    CPPUNIT_ASSERT(mcdb.getMaxTagID(subsysID) == maxTagID);
    //cout << "dbc_->getMaxTagID(subsysID) == maxTagID passed" << endl;
    try {

        MonitorDescription md("Bima7.my.monitor.point2",MPTYPE_SOFT,
                              DATATYPE_DOUBLE,true,1,false);
        md.setShortName("mp6");
        md.setLongName("another point for testing");
        md.setUnits("V");
        md.setDescription("I'm a little monitor point, short and stout");
        md.setTime(carma::util::Time::computeCurrentFrame());
        //int tagID2 = dbc_->insertMonitorDescription(md);
        //int tagID2 = 
	mcdb.insertMonitorConfiguration(md);
        //cout << "shouldn't have gotten here, tagID2 is " << tagID2 << endl;
        CPPUNIT_ASSERT( false );
    } catch (const out_of_range & exc) {
        // this is where we want to be, this exception should be thrown
        // since the calling script inserts a row which inserts a row with
        // the max tag id for subsysID=6
        //cout << "Exception thrown as expected" << endl;
        //cout << "Message is: " << exc.what() << endl;
    }
    string str = "SELECT tagID,name FROM " 
        + getTableName(STATIC_MONITOR_CONFIG_TABLE);
    Table t = dbc_->execSQLSelect(str);
    //cout << t.getStringColumn("name").toString() << endl;
    //cout << t.getIntColumn("tagID").toString() << endl;
    //cout << "tagID for Bima6.my.monitor.point is " 
    //<< mcdb.getTagID("Bima6.my.monitor.point") << endl;
    //cout << "tagID for Bima6.my.monitor.point2 is " 
    //<< mcdb.getTagID("Bima7.my.monitor.point2") << endl;
    CPPUNIT_ASSERT( mcdb.getTagID("Bima6.my.monitor.point") == tagID1);
    CPPUNIT_ASSERT( mcdb.getTagID("Bima7.my.monitor.point2") == -1);

    string badSelect = "INSERT INTO MonitorConfig (frameCount,tagID,name) ";
    badSelect += "VALUES(20,100000,not allowed)";
    try {
        t = dbc_->execSQLSelect(badSelect);
        CPPUNIT_ASSERT(false);
    } catch (const SQLException& exc) {
        cout << "SQLException successfully caught as expected" << endl;
        exc.report();
    }

    // threshold tests
    string cname = "Bima3.testpoint"; 
    MonitorDescription md1
        (cname, MPTYPE_CONTROL, DATATYPE_DOUBLE, true, 5, false);
    string desc1 = " a new point ";
    md1.setDescription(desc1);
    string shortName = "I'm a short name";
    md1.setShortName(shortName);
    string longName = "I'm a longer name";
    md1.setLongName(longName);
    md1.setLocation("BIMA3");
    string device = "WIDGET1";
    md1.setDevice(device);
    CPPUNIT_ASSERT(md1.isDefaultThresholdValue
                   (carma::monitor::THRESHOLD_LOW_ERROR_VALUE));
    CPPUNIT_ASSERT(md1.isDefaultThresholdValue
                   (carma::monitor::THRESHOLD_LOW_WARN_VALUE));
    CPPUNIT_ASSERT(md1.isDefaultThresholdValue
                   (carma::monitor::THRESHOLD_HIGH_WARN_VALUE));
    CPPUNIT_ASSERT(md1.isDefaultThresholdValue
                   (carma::monitor::THRESHOLD_HIGH_ERROR_VALUE));
    int tagID = mcdb.insertMonitorConfiguration(md1);
    //cout << "tagID for new descrption " << tagID << endl;
    try {
        MonitorDescription md1f = mcdb.getMonitorConfiguration(tagID);
        //cout << md1f.toString() << endl;
        CPPUNIT_ASSERT(md1f.getName() == cname);
        CPPUNIT_ASSERT(md1f.getShortName() == shortName);
        CPPUNIT_ASSERT(md1f.getLongName() == longName);
        CPPUNIT_ASSERT(md1f.getDevice() == device);
        CPPUNIT_ASSERT(md1f.getMonitorPointType() == MPTYPE_CONTROL);
        CPPUNIT_ASSERT(md1f.getDataType() == DATATYPE_DOUBLE);
        CPPUNIT_ASSERT(md1f.isPersistent() == true);
        CPPUNIT_ASSERT(md1f.isSpectrum() == false);
        CPPUNIT_ASSERT(md1f.getUpdateInterval() == 5);
        CPPUNIT_ASSERT(md1f.isDefaultThresholdValue
                       (carma::monitor::THRESHOLD_LOW_ERROR_VALUE));
        CPPUNIT_ASSERT(md1f.isDefaultThresholdValue
                       (carma::monitor::THRESHOLD_LOW_WARN_VALUE));
        CPPUNIT_ASSERT(md1f.isDefaultThresholdValue
                       (carma::monitor::THRESHOLD_HIGH_WARN_VALUE));
        CPPUNIT_ASSERT(md1f.isDefaultThresholdValue
                       (carma::monitor::THRESHOLD_HIGH_ERROR_VALUE));
        vector<int> v;
        v.push_back(tagID);
        ID2MDMap tagID2MD 
            = mcdb.getMonitorConfigurations(&v);
        MonitorDescription sameMD = tagID2MD.find(tagID)->second;
        CPPUNIT_ASSERT
            (md1f.isEqualExceptForTime(sameMD) 
             && md1f.getTime() == sameMD.getTime());
    } catch (const carma::util::NotFoundException & exc) {
        ostringstream emsg;
        //cout << "falure, couldn't find monitor configuration for tagID " 
        //<< tagID << endl;
        CPPUNIT_ASSERT(false);
    }
    double loErr = -90.23456929324293423;
    double loWarn = -5.63450045034532423425034;
    double hiWarn =  4.9435353599922342353243;
    double hiErr = 100.7234242545949234252534;
    md1.setThresholdValue(carma::monitor::THRESHOLD_LOW_ERROR_VALUE, loErr);
    md1.setThresholdValue(carma::monitor::THRESHOLD_LOW_WARN_VALUE, loWarn);
    md1.setThresholdValue(carma::monitor::THRESHOLD_HIGH_WARN_VALUE, hiWarn);
    md1.setThresholdValue(carma::monitor::THRESHOLD_HIGH_ERROR_VALUE, hiErr);
    carma::util::frameType newTime = md1.getTime()+1;
    md1.setTime(newTime);
    int ntagID = mcdb.insertMonitorConfiguration(md1);
    //cout << "tagID for updated descrption " << ntagID << endl;
    CPPUNIT_ASSERT(tagID == ntagID);
    MonitorDescription md1f = mcdb.getMonitorConfiguration(tagID,newTime);
    //cout << md1f.toString() << endl;
    CPPUNIT_ASSERT(!md1f.isDefaultThresholdValue
                   (carma::monitor::THRESHOLD_LOW_ERROR_VALUE));
    CPPUNIT_ASSERT(!md1f.isDefaultThresholdValue
                   (carma::monitor::THRESHOLD_LOW_WARN_VALUE));
    CPPUNIT_ASSERT(!md1f.isDefaultThresholdValue
                   (carma::monitor::THRESHOLD_HIGH_WARN_VALUE));
    CPPUNIT_ASSERT(!md1f.isDefaultThresholdValue
                   (carma::monitor::THRESHOLD_HIGH_ERROR_VALUE));
    double f = 2e-16;
    double fepsilon = f*abs(loErr);
    CPPUNIT_ASSERT
        (abs(1-(md1f.getThresholdValue(carma::monitor::THRESHOLD_LOW_ERROR_VALUE)/loErr)) < fepsilon);
    fepsilon = f*abs(loWarn);
    CPPUNIT_ASSERT
        (abs(1-(md1f.getThresholdValue(carma::monitor::THRESHOLD_LOW_WARN_VALUE)/loWarn)) < fepsilon);
    fepsilon = f*abs(hiWarn);
    CPPUNIT_ASSERT
        (abs(1-md1f.getThresholdValue(carma::monitor::THRESHOLD_HIGH_WARN_VALUE)/hiWarn) < fepsilon);
    fepsilon = f*abs(hiErr);
    CPPUNIT_ASSERT
        (abs(1-md1f.getThresholdValue(carma::monitor::THRESHOLD_HIGH_ERROR_VALUE)/hiErr) < fepsilon);
    ID2MDMap tid2md = mcdb.getMonitorConfigurations(NULL);
    //cout << "number of configurations currently in the db " << tid2md.size()
    //<< endl;
}

void DBConnectionTest::aggregateSubsystemsTableTests() {
    //cout << "Begin aggregateSubsystemsTableTests()" << endl;
    MonitorConfigurationDatabase mcdb(dbc_);
    mcdb.insertAggregateSubsystemsRecord("DBTest",4,1000,400);
    mcdb.insertAggregateSubsystemsRecord("DBTestJ",10,5000,800);
    Table t = mcdb.getAggregateSubsystemsTable();
    //cout << getTableName(AGGREGATE_SUBSYSTEMS_TABLE) << endl << t.toString() 
    //<< endl;
    int tms1 = mcdb.getTotalMaxSamples();
    int tms2 = t.getIntColumn("maxsamples").sum();
    //cout << "Total max samples method1: " << tms1 << " method 2: " 
    //<< tms2 << endl;
    CPPUNIT_ASSERT( tms1 == tms2 ); 
    int tmp1 = mcdb.getTotalMaxPoints();
    int tmp2 = t.getIntColumn("maxpoints").sum();
    //cout << "Total max points method1: " << tmp1 << " method 2: " 
    //<< tmp2 << endl;
    CPPUNIT_ASSERT( tmp1 == tmp2 );
    //cout << mcdb.getAggregateSubsystemsTable().toString() << endl;
    // this statement should result in nothing being updated which can be
    // verified by checking the trace log
    mcdb.insertAggregateSubsystemsRecord("DBTest",4,1000,400);
    //cout << getTableName(AGGREGATE_SUBSYSTEMS_TABLE) 
    //<< " after action which should have "
    //<< "left the table unaltered" << endl
    //<< mcdb.getAggregateSubsystemsTable().toString() << endl;
    // this should update the existing record
    mcdb.insertAggregateSubsystemsRecord("DBTest",5,3000,900);
    //cout << getTableName(AGGREGATE_SUBSYSTEMS_TABLE) 
    //<< " after action which should have "
    //<< "altered the table" << endl
    //<< mcdb.getAggregateSubsystemsTable().toString() << endl;
    unsigned int samples = tms2+2000;
    unsigned int points  = tmp2+500;
    CPPUNIT_ASSERT( mcdb.getTotalMaxSamples() == samples );
    CPPUNIT_ASSERT( mcdb.getTotalMaxPoints()  == points  );
}

/*
void DBConnectionTest::loggingTests() {
    dbc_->insertLogMessage("First test message");
    dbc_->insertLogMessage("Second test message",20);
    Table t = dbc_->getLogMessages(0);
    Column<int> times = t.getIntColumn(0);
    Column<string> messages = t.getStringColumn(1);
    cout << "Table 1" << endl;
    for(int i=0; i<t.rowCount(); i++) {
        cout << times[i] << " | " << messages[i] << endl;
    }
    t = dbc_->getLogMessages(40);
    times = t.getIntColumn(0);
    messages = t.getStringColumn(1);
    cout << "Table 2" << endl;
    for(int i=0; i<t.rowCount(); i++) {
        cout << times[i] << " | " << messages[i] << endl;
    }
    string str = "Second";
    t = dbc_->getLogMessages(0,carma::util::Time::computeCurrentFrame(),&str);
    times = t.getIntColumn(0);
    messages = t.getStringColumn(1);
    cout << "Table 3" << endl;
    for(int i=0; i<t.rowCount(); i++) {
        cout << times[i] << " | " << messages[i] << endl;
    }
    t = dbc_->getLogMessages(40,carma::util::Time::computeCurrentFrame(),&str);
    times = t.getIntColumn(0);
    messages = t.getStringColumn(1);
    cout << "Table 4" << endl;
    for(int i=0; i<t.rowCount(); i++) {
        cout << times[i] << " | " << messages[i] << endl;
    }

}
*/

void DBConnectionTest::transactionTests() {
    //cout << "transaction tests..." << endl;
    dbc_->directSQLExec_("CREATE TABLE t (i INT) ENGINE=InnoDB");
    dbc_->directSQLExec_("INSERT INTO t VALUES(0)");
    int c1 = dbc_->execSQLSelect("SELECT * FROM t").rowCount();
    //cout << "nrows in tt " << c1 << endl;
    CPPUNIT_ASSERT( !dbc_->inTransaction() );
    dbc_->beginTransaction();
    CPPUNIT_ASSERT( dbc_->inTransaction() );
    //cout << "\ninTransaction:" << dbc_->inTransaction();
    ostringstream stmt;
    int i=0;
    for( i = 1; i < 20; i++) {
        stmt.str("");
        stmt << "INSERT INTO t VALUES(" << i << ")";
        dbc_->directSQLExec_(stmt.str());
    }
    //cout << "\ninTransaction:" << dbc_->inTransaction();
    CPPUNIT_ASSERT(dbc_->inTransaction());
    dbc_->rollBackTransaction();
    //cout << "\ninTransaction:" << dbc_->inTransaction();
    CPPUNIT_ASSERT( !dbc_->inTransaction() );
    int c2 = dbc_->execSQLSelect("SELECT * FROM t").rowCount();
    //cout << "nrows in t " << c2 << endl;
    if (true && (c1 != c2)) {
        cout  << "\nFailed test of transaction rollback; c1 != c2";
        cout << " (" << c1 << " != " << c2 << ")" << endl;
    }
    CPPUNIT_ASSERT( c1 == c2 );
    dbc_->beginTransaction();
    CPPUNIT_ASSERT( dbc_->inTransaction() );
    for( i = 1; i < 20; i++) {
        stmt.str("");
        stmt << "INSERT INTO t VALUES(" << i << ")";
        dbc_->directSQLExec_(stmt.str());
    }
    dbc_->commitTransaction();
    CPPUNIT_ASSERT( !dbc_->inTransaction() );
    int c3 = dbc_->execSQLSelect("SELECT * FROM t").rowCount();
    if (true && (c3 != 20)) {
        cout  << "\nFailed test of transactions; c3 != 20";
        cout << " (c3 = " << c3 << ")" << endl;
    }
    //cout << "nrows in t " << c3 << endl;
    CPPUNIT_ASSERT( c3 == 20 );
}
    /*
    t = dbc_->getLogMessages(0);
    CPPUNIT_ASSERT( t.rowCount() == nRowsInit );
    dbc_->insertLogMessage("test again",30);
    dbc_->beginTransaction();
    dbc_->insertLogMessage("test again",30);
    dbc_->insertLogMessage("test again",30);
    dbc_->rollBackTransaction();
    t = dbc_->getLogMessages(0);
    CPPUNIT_ASSERT( t.rowCount() == (nRowsInit+1) );
    dbc_->beginTransaction();
    dbc_->insertLogMessage("test again",30);
    dbc_->insertLogMessage("test again",30);
    dbc_->insertLogMessage("test again",30);
    dbc_->insertLogMessage("test again",30);
    CPPUNIT_ASSERT( dbc_->inTransaction() );
    dbc_->commitTransaction();
    t = dbc_->getLogMessages(0);
    CPPUNIT_ASSERT( t.rowCount() == (nRowsInit+5) );
    CPPUNIT_ASSERT( !dbc_->inTransaction() );
    
}
*/


void DBConnectionTest::monitorConfigLoaderTests() {
    // get the monitor descriptions from TestSubsystem.mpml, these should
    // already have been loaded into the db, this test is to verify that
    // the descriptions in the db match those in the mpml
    //char *mpmlFileName = getenv("CARMA_MONITOR_TESTSUBSYSTEM_MPML");
    //cout << "mpml file " << mpmlFileName << endl;
    try {
        XMLPlatformUtils::Initialize();
    } catch (const XMLException & ex) {
        char *message = XMLString::transcode(ex.getMessage());
        std::cerr << "Error during initialization: \n"
                  << message
                  << std::endl;
        XMLString::release(&message);
        CPPUNIT_ASSERT(false);
    }

    SAXParser *parser = new SAXParser();
    parser->setValidationScheme(SAXParser::Val_Auto);
    parser->setDoNamespaces(true);
    parser->setDoSchema(true);
    parser->setValidationSchemaFullChecking(true);

    SaxHandler *handler = new SaxHandler();
    parser->setDocumentHandler(handler);
    parser->setErrorHandler(handler);
    vector<MonitorDescription> monitorDescriptions;
    try {
        parser->parse(mpmlFileName.c_str());
        string aggSubsysName = handler->getAggregateSubsystemName();
	/*
        uint count = handler->getAggregateSubsystemCount();
        uint maxsamples = handler->getAggregateSubsystemMaxSamples();
        uint maxpoints = handler->getAggregateSubsystemMaxPoints();
	*/
        monitorDescriptions = handler->getMonitorDescriptions();
    } catch (const XMLException & ex) {
        char *message = XMLString::transcode(ex.getMessage());
        std::cerr << "An error occurred: "
                  << message
                  << std::endl;
        XMLString::release(&message);
        XMLPlatformUtils::Terminate();
        CPPUNIT_ASSERT(false);
    } catch (const MPMLException & exc) {
        cerr << "MPMLException caught while parsing " << mpmlFileName << endl;
        exc.report();
        CPPUNIT_ASSERT(false);
    } 
    unsigned int i=0;
    MonitorConfigurationDatabase mcdb(dbc_);
    for( ; i<monitorDescriptions.size(); i++) {
        MonitorDescription mdDB 
            = mcdb.getMonitorConfiguration(monitorDescriptions[i].getName());
        /*
          lots of output!
        cout << "From mpml:" << endl;
        cout << monitorDescriptions[i].toString() << endl;
        cout << "From database:" << endl;
        cout << mdDB->toString() << endl << endl;;
        */
        if(!monitorDescriptions[i].isEqualExceptForTime(mdDB)) {
            if (true) {
                cout << "From mpml:" << endl;
                cout << monitorDescriptions[i].toString() << endl;
                cout << "From database:" << endl;
                cout << mdDB.toString() << endl << endl;
            }
            CPPUNIT_ASSERT(monitorDescriptions[i].isEqualExceptForTime(mdDB));
        }
    }
    cout << "All " << i << " descriptions in the database match the mpml "
    << "descriptions" << endl;
}


void DBConnectionTest::resultsCacheTests() {
    ResultsCache rc = ResultsCache::getCache(dbc_);
    const Table &t1 = rc.getFullMonitorConfigurationTable();
    const Table &t2 = rc.getFullMonitorConfigurationTable();
    //cout << t1.rowCount() << endl;
    //cout << t2.rowCount() << endl;
    CPPUNIT_ASSERT( &t1 == &t2 );
}

void DBConnectionTest::databaseInfoTests() {
    dbc_->directSQLExec_("CREATE TABLE scratch.x (i INT, j INT)");
    dbc_->directSQLExec_("INSERT INTO scratch.x VALUES(4,5)");
    Table t = dbc_->databaseInfo();
    //cout << "Database Info" << endl;
    //cout << t << endl;
    t = dbc_->execSQLSelect("SELECT * FROM scratch.x");
    //cout << t << endl;
    CPPUNIT_ASSERT( t.rowCount() == 1 );
    t = dbc_->databaseInfo("scratch");
    CPPUNIT_ASSERT( t.rowCount() == 1 );
    CPPUNIT_ASSERT( dbc_->tableExists("x","scratch"));
    dbc_->dropScratchTable("x");
    CPPUNIT_ASSERT( !dbc_->tableExists("x","scratch"));
    t = dbc_->databaseInfo("scratch");
    CPPUNIT_ASSERT( t.rowCount() == 0 );
    // doesn't exist, but check to make sure nothing bad happens
    dbc_->dropScratchTable("x"); 
}
