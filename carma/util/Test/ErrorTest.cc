
/** @file
 *
 * CppUnit test fixture implementation for carma::util exception classes.
 *
 * @author: Steve Scott
 *
 * $Id: ErrorTest.cc,v 1.6 2006/11/23 18:06:50 tcosta Exp $
 * 
 *
 */
#include <vector>

#include "ErrorTest.h"

using namespace ::std;
using namespace ::CppUnit;
using namespace carma;
using namespace carma::util;


// Uses char* constructor
void ErrorTest::setUp()
{
    lineNumber_ = 0;
} 


void ErrorTest::tearDown() 
{
    //delete msg_;
}


string
ErrorTest::fullstr( const string & msg )
{
    ostringstream oss;
    
    const char * f = __FILE__;
    
    oss << "File: " << f << ", "
        << "Line: " << lineNumber_ << ", "
        << "Message: " << msg;
    
    return oss.str();
} 

void ErrorTest::testConstructor() 
{
    ErrorException* err;
    string m("exception test msg");

    // string constructor
    err = new CARMA_ERROR(m);
    lineNumber_ = __LINE__ -1;

    // filename and line number
    CPPUNIT_ASSERT(err->getLineNumber() == lineNumber_);
    CPPUNIT_ASSERT(!strcmp(err->getSourceFile(), __FILE__));

    // string constructor test
    CPPUNIT_ASSERT((err->getErrorMessage() == fullstr(m)));
    CPPUNIT_ASSERT(string(err->what()) == fullstr(m));
    delete err;


    // ostringstream constructor
    ostringstream os;
    os << m;
    err = new CARMA_ERROR(os);
    lineNumber_ = __LINE__ -1;
    CPPUNIT_ASSERT((err->getErrorMessage() == fullstr(m)));
    delete err;
}

// Test both constructor types
void ErrorTest::testThrow() 
{
    string m = "testing 1,2,3";

    // string constructor
    try {
        string s(m);
        ErrorException error = CARMA_ERROR(s);
        lineNumber_ = __LINE__ -1;
        throw (error);
    }
    catch (exception& e) {
        CPPUNIT_ASSERT(string(e.what()) == fullstr(m));
    }

    // ostringstream constructor
    try {
        ostringstream s;
        s << m;
        ErrorException error = CARMA_ERROR(s);
        lineNumber_ = __LINE__ -1;
        throw (error);
    }
    catch (exception& e) {
        CPPUNIT_ASSERT(string(e.what()) == fullstr(m));
    }
}


// Test throwing, copying, polymorphic behavior
// You only get polymorphic behavior if you catch a reference to the
// exception, otherwise it is a 'sliced' cast to the requested type.
void ErrorTest::testPolymorphism() 
{
    string m("the quick brown fox...");

    // Polymorphic
    try {
        ErrorException err = CARMA_ERROR(m);
        lineNumber_ = __LINE__ -1;
        throw err ;
        // Shouldn't get to the next line...
        CPPUNIT_FAIL("Should never get here");
    }
    catch (exception& e) {
        CPPUNIT_ASSERT(string(e.what()) == fullstr(m));
    }

    try {
        ErrorException err = CARMA_ERROR(m);
        lineNumber_ = __LINE__ -1;
        throw err ;
        CPPUNIT_FAIL("Should never get here");
    }
    catch (BaseException& e) {
        CPPUNIT_ASSERT(string(e.what()) == fullstr(m));
    }

    try {
        ErrorException err = CARMA_ERROR(m);
        lineNumber_ = __LINE__ -1;
        throw err ;
        CPPUNIT_FAIL("Should never get here");
    }
    catch (ErrorException& e) {
        CPPUNIT_ASSERT(e.getErrorMessage() == fullstr(m));
    }
        
}


void ErrorTest::streamInsertion() 
{
    string m("testMessage");
    ErrorException err = CARMA_ERROR(m);
    lineNumber_ = __LINE__ -1;

    ostringstream os;
    os << err ;
    string str = os.str();
    CPPUNIT_ASSERT(str == fullstr(m));
}




