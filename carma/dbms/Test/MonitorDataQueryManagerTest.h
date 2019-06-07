/** @file 
 *
 * CppUnit test fixture for carma::dbms::MonitorDataQueryManager 
 *
 * Author: Dave Mehringer
 *
 */
#ifndef CARMA_DBMS_MONITORDATAQUERYMANAGERTEST_H
#define CARMA_DBMS_MONITORDATAQUERYMANAGERTEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>
#include "carma/util/Program.h"


namespace carma {
    namespace dbms {
    class DBConnection;
    class DBConfigurator;


/**
 * carma::dbms::MonitorDataQueryManager test class for CppUnit.
 */
class MonitorDataQueryManagerTest : public CppUnit::TestFixture {
public:	

    friend class carma::util::Program;

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();


    CPPUNIT_TEST_SUITE(MonitorDataQueryManagerTest);
    CPPUNIT_TEST( constructionTests );
    CPPUNIT_TEST_SUITE_END();
	
    void constructionTests();

    static void setConffile(const std::string& conffile);

private:

    carma::dbms::DBConnection *dbc_;
    static std::string conffile_;
	
};

    }}
#endif //CARMA_DBMS_MONITORDATAQUERYMANAGERTEST_H
