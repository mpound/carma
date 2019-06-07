/** @file 
 *
 * CppUnit test fixture for dbFFIO classes
 *
 * @author Harold Ravlin
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_DBMS_DBFFIOTEST_H
#define CARMA_DBMS_DBFFIOTEST_H

#if !defined(CPPUNIT_STD_NEED_ALLOCATOR)
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>
#include "carma/util/Program.h"
#include "carma/dbms/dbFFIO.h"

namespace carma {
  namespace dbms {

typedef struct {int siglen; long framecount; std::string sig;} HeaderInfo;

typedef struct {long framecount; long tagid; short blanking;
	short validity; short value; int isample;} ShortRecord;
typedef struct {long framecount; long tagid; short blanking;
	short validity; int value; int isample;} IntRecord;
typedef struct {long framecount; long tagid; short blanking;
	short validity; long value; int isample;} LongRecord;
typedef struct {long framecount; long tagid; short blanking;
	short validity; float value; int isample;} FloatRecord;
typedef struct {long framecount; long tagid; short blanking;
	short validity; double value; int isample;} DoubleRecord;
#if 0
// String files are different.
typedef struct {long framecount; long tagid; short blanking;
	short validity; std::string value;} StringRecord;
#endif
 
 typedef struct {
   dbFFIO::RECORD_TYPE type;
   int len;
   union{
     void *vrec;		// Here to allow initialization.
     ShortRecord *srec;
     IntRecord *irec;
     LongRecord *lrec;
     FloatRecord *frec;
     DoubleRecord *drec;
   };
 } Record;

/**
 * carma::dbms::DbFFIOTest test class for CppUnit.
 * (This gets instantiated once per test).
 */
class dbFFIOTest : public CppUnit::TestFixture {
public:	

  dbFFIOTest();
  ~dbFFIOTest();

    /** initialization required by CppUnit::TestFixture */
    void setUp();

    /** clean up required by CppUnit::TestFixture */
    void tearDown();
	 
    bool openTest(dbFFIO &, const std::string &filename);
    // Write the test data to a file.
    bool writeTest(dbFFIO &);
    // Call dbFFIOb's copy routine.
    bool copyTest(const std::string &ifilename, const std::string &ofilename);
    // Read the binary output file and compare to test data.
    bool readTestb(const std::string &binfilename);
    // Read the ASCII output file and compare to test data.
    bool readTesta(const std::string &asciifilename);
    // Close the file.
    bool closeTest(dbFFIO &);

    // All tests are run from this because the class gets instantiated
    // once (with a setup and teardown) for each "test" and we want
    // everything to stick around for all tests.
    void test1();

    CPPUNIT_TEST_SUITE(dbFFIOTest);
     CPPUNIT_TEST(test1);
    CPPUNIT_TEST_SUITE_END();
	
 private:
    std::string bfilename_;
    std::string afilename_;
    int	serial_;
    static int serialktr_;
};
  }
}

#endif //CARMA_DBMS_DBFFIOTEST_H
