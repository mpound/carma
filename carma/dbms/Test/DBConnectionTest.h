/** @file 
 *
 * CppUnit test fixture for carma::dbms::DBConnection 
 *
 * @author Dave Mehringer
 * @version $Id: DBConnectionTest.h,v 1.16 2004/12/19 00:54:18 dmehring Exp $
 *
 */
#ifndef CARMA_DBMS_DBCONNECTION_TEST_H
#define CARMA_DBMS_DBCONNECTION_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>
#include "carma/util/Program.h"

namespace carma {
    namespace dbms {
    class DBConnection;
    class DBConfigurator;

/**
 * carma::dbms::DBConnection test class for CppUnit.
 */
class DBConnectionTest : public CppUnit::TestFixture {
public:	
    friend class carma::util::Program;

    static std::string rdbms;

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();


    CPPUNIT_TEST_SUITE(DBConnectionTest);
    CPPUNIT_TEST( upTests );
    //    CPPUNIT_TEST( loadMonitorSubsystemTableTest );
    CPPUNIT_TEST( memberTests );
    CPPUNIT_TEST( monitorConfigLoaderTests );
    CPPUNIT_TEST( monitorConfigInsertTests );
    CPPUNIT_TEST( aggregateSubsystemsTableTests );
    //CPPUNIT_TEST( loggingTests );
    CPPUNIT_TEST( transactionTests );
    CPPUNIT_TEST( resultsCacheTests );
    CPPUNIT_TEST( databaseInfoTests );
    
    CPPUNIT_TEST_SUITE_END();
	
    void upTests();
    void memberTests();
    void loadMonitorSubsystemTableTest();
    void monitorConfigLoaderTests();
    void monitorConfigInsertTests();
    void loggingTests();
    void transactionTests();
    void aggregateSubsystemsTableTests();
    void resultsCacheTests();
    void databaseInfoTests();
	
    static std::string conffile;
    static std::string mpmlFileName;

private:

    carma::dbms::DBConnection *dbc_;
    carma::dbms::DBConfigurator *dbconf_;
};

    }}
#endif //CARMA_DBMS_DBCONNECTION_TEST_H
