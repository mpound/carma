/** 
 * @file
 * CppUnit test fixture for carma::dbms::dbFFIO
 * @author Harold Ravlin
 *
 * $CarmaCopyright$
 */

#include <iostream>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "dbFFIOTest.h"
#include "carma/util/FileUtils.h"

using namespace carma::dbms;
using namespace CppUnit;
using namespace std;

////////////////////////////////////////////////////////////////
// 			Test data
static HeaderInfo header = {
  59,	  436434360,	"c2c09c88459aa8850fe9ec2c91d940ab1bf32cb6"};

static ShortRecord shortdata[] = {
  {436434361,	    2294094,	 0,	32,	           1,	 0},
  {436434361,	    2294095,	 0,	32,	           2,	 0},
  {436434361,	    2294096,	 0,	32,	           3,	 0},
  {436434361,	    2294097,	 0,	32,	           4,	 0},
  {436434361,	    2294098,	 0,	32,	           5,	 0},
  {436434361,	    2294099,	 0,	32,	           6,	 0}
};
static const int NSHORTS = sizeof(shortdata)/sizeof(*shortdata);

static IntRecord intdata[] = {
  {436434361,	      65544,	 0,	32,	       14490,	 0},
  {436434361,	      65607,	 0,	32,	         721,	 0},
  {436434361,	      65631,	 0,	32,	           0,	 0},
  {436434361,	      65641,	 0,	32,	           0,	 0},
  {436434361,	      65713,	 0,	32,	       45796,	 0},
  {436434361,	      65714,	 0,	32,	          15,	 0},
  {436434361,	      65716,	 0,	32,	      -10689,	 0},
  {436434361,	      65717,	 0,	32,	        9103,	 0},
  {436434361,	      65718,	 0,	32,	        7006,	 0},
  {436434361,	      65719,	 0,	32,	           0,	 0},
  {436434361,	      65721,	 0,	32,	      -12554,	 0},
  {436434361,	      65722,	 0,	32,	       -8902,	 0}
};
static const int NINTS = sizeof(intdata)/sizeof(*intdata);

static FloatRecord floatdata[] = {
  {436434361,	      65541,	 0,	32,	 6.61472082e-01,	 0},
  {436434361,	      65547,	 0,	32,	 1.09929848e+00,	 0},
  {436434361,	      65548,	 0,	40,	 6.35401607e-01,	 0},
  {436434361,	      65549,	 0,	32,	 4.21746790e-01,	 0},
  {436434361,	      65552,	 0,	40,	-1.83875293e-01,	 0},
  {436434361,	      65553,	 0,	32,	-1.60168391e-02,	 0},
  {436434361,	      65557,	 0,	32,	 8.99625492e+00,	 0},
  {436434361,	      65562,	 0,	32,	 5.84771991e-01,	 0},
  {436434361,	      65564,	 0,	 0,	 0.00000000e+00,	 0},
  {436434361,	      65591,	 0,	32,	 2.19567896e+03,	 0},
  {436434361,	      65602,	 0,	32,	 2.72705566e+02,	 0},
  {436434361,	      65603,	 0,	32,	 1.46096821e+01,	 0},
  {436434361,	      65604,	 0,	 0,	 0.00000000e+00,	 0},
  {436434361,	      65609,	 0,	32,	-6.77667284e+00,	 0},
  {436434361,	     131077,	 0,	32,	 4.22405712e-02,	 0},
  {436434361,	    3998134,	 0,	32,	 1.00000000e+04,	 0},
  {436434361,	    3998137,	 0,	32,	 1.00000000e+04,	 0},
  {436434361,	    3998138,	 0,	32,	 3.43338196e+02,	 0},
  {436434361,	    3998139,	 0,	32,	 3.39768036e+02,	 0},
  {436434361,	    3998140,	 0,	32,	 3.65502899e+02,	 0},
  {436434361,	    3998141,	 0,	32,	 8.63634338e+02,	 0},
  {436434361,	    3998142,	 0,	32,	 1.00000000e+04,	 0},
  {436434361,	    3998143,	 0,	32,	 1.00000000e+04,	 0},
  {436434361,	    3998146,	 0,	32,	 2.31526901e+02,	 0},
  {436434361,	    3998147,	 0,	32,	 2.23985336e+02,	 0},
  {436434361,	    3998148,	 0,	32,	 2.24951675e+02,	 0},
  {436434361,	    3998149,	 0,	32,	 4.94381006e+03,	 0},
  {436434361,	    3998150,	 0,	32,	 1.00000000e+04,	 0},
  {436434361,	    3998153,	 0,	32,	 1.00000000e+04,	 0}
};
static int NFLOATS = sizeof(floatdata)/sizeof(*floatdata);

static DoubleRecord doubledata[] = {
  {436434361,  65539,	 0,	32,	 3.2701231338186445e+00,	 0},
  {436434361,  65540,	 0,	32,	 3.5157805023474750e-02,	 0},
  {436434361,  65545,	 0,	32,	 1.8367948655864112e+02,	 0},
  {436434361,  65546,	 0,	32,	 1.8368243171353143e+02,	 0},
  {436434361,  65550,	 0,	32,	 5.4689598799056782e+01,	 0},
  {436434361,  65551,	 0,	32,	 5.4689553438798690e+01,	 0},
  {436434361,  65561,	 0,	32,	 0.0000000000000000e+00,	 0},
  {436434361,  65572,	 0,	32,	 1.1268193200957330e+02,	 0},
  {436434361,  65574,	 0,	32,	 8.3770952770776681e+00,	 0},
  {436434361,  65575,	 0,	32,	 6.8021850100067144e-02,	 0},
  {436434361,  65578,	 0,	32,	 1.1268193200957330e+02,	 0},
  {436434361,  65585,	 0,	 0,	 0.0000000000000000e+00,	 0},
  {436434361,  65586,	 0,	 0,	 0.0000000000000000e+00,	 0},
  {436434361,  65590,	 0,	32,	 2.4185861709501586e+02,	 0},
  {436434361,  65595,	 0,	 0,	 0.0000000000000000e+00,	 0},
  {436434361, 2166241,	 0,	 0,	 0.0000000000000000e+00,	 0},
  {436434361, 2167018,   0,	32,	 5.4069582159951409e+04,	 0},
  {436434361, 2167021,	 0,	 0,	 0.0000000000000000e+00,	 0},
  {436434361, 2167022,	 0,	 0,	 0.0000000000000000e+00,	 0}
};
static int NDOUBLES = sizeof(doubledata)/sizeof(*doubledata);

static LongRecord longdata[] = {
  {436434361,	      65625,	 0,	32,	       12353937,	0},
  {436434361,	     131161,	 0,	32,	       12354068,	0},
  {436434361,	     196697,	 0,	32,	       12354061,	0},
  {436434361,	     262233,	 0,	32,	       54068,	 0},
  {436434361,	     327769,	 0,	32,	       54068,	 0},
  {436434361,	     393305,	 0,	32,	       54068,	 0},
  {436434361,	     458841,	 0,	32,	       54062,	 0},
  {436434361,	     524377,	 0,	32,	       54026,	 0},
  {436434361,	     589913,	 0,	32,	       54068,	 0}
};
static int NLONGS = sizeof(longdata)/sizeof(*longdata);

static Record records[] = {
  {dbFFIO::RECORD_SHORT, NSHORTS, shortdata},
  {dbFFIO::RECORD_INTEGER, NINTS, intdata},
  {dbFFIO::RECORD_FLOAT, NFLOATS, floatdata},
  {dbFFIO::RECORD_DOUBLE, NDOUBLES, doubledata},
  {dbFFIO::RECORD_LONG, NLONGS, longdata}
};

static int NRECORDS = sizeof(records)/sizeof(*records);

////////////////////////////////////////////////////////////////
int dbFFIOTest::serialktr_ = 0;

dbFFIOTest::dbFFIOTest()
{
  serial_ = serialktr_++;
  //  cout << "Creating dbFFIOTest: " << serial_ << endl;
}

dbFFIOTest::~dbFFIOTest()
{
  //  cout << "Deleting dbFFIOTest: " << serial_ << endl;
}

void dbFFIOTest::setUp()
{const char *fileprefix = "/tmp/tDBFFIO_";
 char buf[16];
 pid_t pid = getpid();

 sprintf(buf, "%s%04d.mpdat", fileprefix, pid);
 afilename_ = buf;
 bfilename_ = afilename_ + ".bin";
 // cout << "bfn = " << bfilename_ << " serial = " << serial_ << endl;
}

void dbFFIOTest::tearDown()
{
  //  cout << "Teardown: " << serial_ << " " << afilename_ << endl;
  unlink(afilename_.c_str());
  unlink(bfilename_.c_str());
}

// Open a file.
bool dbFFIOTest::openTest(dbFFIO &db, const string &filename)
{
  //  cout << "opentest " << serial_ << " " << afilename_ << endl;
  CPPUNIT_ASSERT(db.open(filename, true));
  CPPUNIT_ASSERT(db.isOpen());
  CPPUNIT_ASSERT(db.isWrite());
  CPPUNIT_ASSERT(!db.isRead());
  bool ok = db.isWrite();
  return ok;
}

// Write the test data to a file.
bool dbFFIOTest::writeTest(dbFFIO &db)
{ Record *recp = records;
  int num;
 
  //  cout << "writeTest " << serial_ << " " << afilename_ << endl;
  //  cout << "Writing header\n";
  db.writeFFHeader(header.framecount, header.sig);

#define DUMP(p) db.dumpInstAverage(p->framecount, p->tagid, p->blanking,\
			      p->validity, p->value, p->isample)

  for(int index=0; index < NRECORDS; index++, recp++)
  { int type = recp->type;
    num = recp->len;; 

    switch (type) {
    case dbFFIO::RECORD_SHORT:
    { ShortRecord *p = recp->srec;
       for(int i=0; i< num; i++, p++)
       {   DUMP(p);
       }
    }
    break;
    case dbFFIO::RECORD_INTEGER:
    { IntRecord *p = recp->irec;
       for(int i=0; i< num; i++, p++)
       {   DUMP(p);
       }
    }
    break;
    case dbFFIO::RECORD_LONG:
    { LongRecord *p = recp->lrec;
       for(int i=0; i< num; i++, p++)
       {   DUMP(p);
       }
    }
    break;
    case dbFFIO::RECORD_FLOAT:
    { FloatRecord *p = recp->frec;
       for(int i=0; i< num; i++, p++)
       {   DUMP(p);
       }
    }
    break;
    case dbFFIO::RECORD_DOUBLE:
    { DoubleRecord *p = recp->drec;
       for(int i=0; i< num; i++, p++)
       {   DUMP(p);
       }
    }
    break;
    default:
      cerr << "writeTest:: Unsupported record type at records[: "
	   << index << "]\n";
    }
  }

 return true;
}

bool dbFFIOTest::closeTest(dbFFIO &db)
{
  bool ok;
  //  db.flush();
  CPPUNIT_ASSERT((ok=db.close()));
  //  cout << "closeTest " << serial_ << " " << afilename_ << endl;
  return ok;
}

// Call dbFFIOb's copy routine.
bool dbFFIOTest::copyTest(const std::string &ifilename,
			  const std::string &ofilename)
{ bool ok;
  CPPUNIT_ASSERT((ok = dbFFIOb::copyToASCII(ifilename, ofilename, true)));
  //  cout << "copyTest " << serial_ << " " << afilename_ << endl;
  return ok;
}

// Read the ASCII output file and compare to test data.
bool dbFFIOTest::readTestb(const std::string &fileName)
{ dbFFIOb in;
  dbFFIO::RECORD_TYPE mvt;
  uint recordCount;
  long frameCount, tagID;
  short blanking, validity;
  int iSample;
  string sig;
  bool ok;

  // cout << "readTest " << serial_ << " " << fileName << endl;

#define OKCHK() {if(!ok){in.close();return false;}}
#define CMP(r,f,t,b,v,val,i) ((r.framecount==f)&&(r.tagid==t)&&\
(r.blanking==b)&&(r.validity==v)&&(r.value==val)&&(r.isample==i))

  CPPUNIT_ASSERT((ok=in.open(fileName, false)));
  if(!ok)
    return false;

  // Read flat file header.
  // False probably means non dbFFIOb.
  CPPUNIT_ASSERT((ok=in.readFFHeader(frameCount, sig)));
  OKCHK();

  // Compare
  CPPUNIT_ASSERT((ok=(frameCount == header.framecount)));
  OKCHK();

  CPPUNIT_ASSERT((ok=(sig == header.sig)));
  OKCHK();

  // For each record type, read the records and compare them to test data.
  CPPUNIT_ASSERT((ok=in.getRecordInfo(mvt, recordCount)));
  OKCHK();

  while(recordCount > 0)
  {
    switch(mvt) {
    case dbFFIO::RECORD_SHORT:
      {	for(uint i=0; i< recordCount; i++)
		{ short v;
		  CPPUNIT_ASSERT((ok=in.readInstAverage(frameCount, tagID,
							blanking, validity,
							v, iSample)));
		  OKCHK();

		  CPPUNIT_ASSERT((ok=(CMP(shortdata[i],
					  frameCount, tagID,
					  blanking, validity,
					  v, iSample))));
		  OKCHK();
		}
	}
	break;
     case dbFFIO::RECORD_INTEGER:
       {	for(uint i=0; i< recordCount; i++)
		{ int v;
		  CPPUNIT_ASSERT((ok=in.readInstAverage(frameCount, tagID,
							blanking, validity,
							v, iSample)));
		  OKCHK();

		  CPPUNIT_ASSERT((ok=(CMP(intdata[i],
					  frameCount, tagID,
					  blanking, validity,
					  v, iSample))));
		  OKCHK();
		}
	}
	break;
     case dbFFIO::RECORD_LONG:
       {	for(uint i=0; i< recordCount; i++)
		{ long v;
		  CPPUNIT_ASSERT((ok=in.readInstAverage(frameCount, tagID,
							blanking, validity,
							v, iSample)));
		  OKCHK();

		  CPPUNIT_ASSERT((ok=(CMP(longdata[i],
					  frameCount, tagID,
					  blanking, validity,
					  v, iSample))));
		  OKCHK();
		}
	}
	break;
     case dbFFIO::RECORD_FLOAT:
       {	for(uint i=0; i< recordCount; i++)
		{ float v;
		  CPPUNIT_ASSERT((ok=in.readInstAverage(frameCount, tagID,
							blanking, validity,
							v, iSample)));
		  OKCHK();

		  CPPUNIT_ASSERT((ok=(CMP(floatdata[i],
					  frameCount, tagID,
					  blanking, validity,
					  v, iSample))));
		  OKCHK();
		}
	}
	break;
     case dbFFIO::RECORD_DOUBLE:
       {	for(uint i=0; i< recordCount; i++)
		{ double v;
		  CPPUNIT_ASSERT((ok=in.readInstAverage(frameCount, tagID,
							blanking, validity,
							v, iSample)));
		  OKCHK();

		  CPPUNIT_ASSERT((ok=(CMP(doubledata[i],
					  frameCount, tagID,
					  blanking, validity,
					  v, iSample))));
		  OKCHK();
		}
	}
	break;
#if 0
     case dbFFIO::RECORD_COMPLEX:
       {	for(uint i=0; i< recordCount; i++)
		{ complex<float> v;
		  CPPUNIT_ASSERT((ok=in.readInstAverage(frameCount, tagID,
							blanking, validity,
							v, iSample)));
		  OKCHK();
		  CPPUNIT_ASSERT((ok=(CMP(complexdata[i],
					  frameCount, tagID,
					  blanking, validity,
					  v, iSample))));
		  OKCHK();
		}
	}
	break;
#endif
#if 0
     case dbFFIO::RECORD_STRING:
       {	for(uint i=0; i< recordCount; i++)
		{ string v;
			if(!in.readInstAverage(frameCount, tagID,
						   blanking, validity, v))
			  {
			    //			    cout << "Bad read?? " << i << endl;
			  return false;
			  }
		}
	}
	break;
#endif
     default:;
	cerr << "readTestb: Unsupported record type: "
	     << dbFFIO::valuetypeToString(mvt) << "(" << mvt << ")\n";
	return false;
    } //switch
    // Get record count for next record group.
    in.getRecordInfo(mvt, recordCount);
  }//while

  CPPUNIT_ASSERT((ok=in.close()));
  return ok;
}

// Read the ASCII data file and compare to test data.
bool dbFFIOTest::readTesta(const std::string &fileName)
{ Record *recp = records;
  int num;
  dbFFIOa in;
  long framecount, tagid;
  short blanking, validity;
  int isample;
  string sig;
  bool ok;
 
  //  cout << "readTesta " << serial_ << " " << afilename_ << endl;
#define OKCHK() {if(!ok){in.close();return false;}}

  CPPUNIT_ASSERT((ok=in.open(fileName, false)));
  OKCHK();

  // Read flat file header.
  CPPUNIT_ASSERT((ok=in.readFFHeader(framecount, sig)));
  OKCHK();

  // Compare
  CPPUNIT_ASSERT((ok=(framecount == header.framecount)));
  OKCHK();
  CPPUNIT_ASSERT((ok=(sig == header.sig)));
  OKCHK();

#define ACMP(r,f,t,b,v,val,i) ((r->framecount==f)&&(r->tagid==t)&&\
(r->blanking==b)&&(r->validity==v)&&(r->value==val)&&(r->isample==i))

#define READ(p) in.readInstAverage(framecount, tagid, blanking,\
			      validity, value, isample)

  // Loop through the test data and compare to data file.
  for(int index=0; index < NRECORDS; index++, recp++)
  { dbFFIO::RECORD_TYPE type = recp->type;
    num = recp->len;; 

    switch (type) {
    case dbFFIO::RECORD_SHORT:
    { ShortRecord *p = recp->srec;
      short value;
       for(int i=0; i< num; i++, p++)
       {   READ(p);

           CPPUNIT_ASSERT((ok=ACMP(p,framecount, tagid, blanking,
				  validity, value, isample)));
	   OKCHK();
       }
    }
    break;
    case dbFFIO::RECORD_INTEGER:
    { IntRecord *p = recp->irec;
      int value;
       for(int i=0; i< num; i++, p++)
       {   READ(p);
           CPPUNIT_ASSERT((ok=ACMP(p,framecount, tagid, blanking,
				  validity, value, isample)));
	   OKCHK();
       }
    }
    break;
    case dbFFIO::RECORD_LONG:
    { LongRecord *p = recp->lrec;
      long value;
       for(int i=0; i< num; i++, p++)
       {   READ(p);
           CPPUNIT_ASSERT((ok=ACMP(p,framecount, tagid, blanking,
				  validity, value, isample)));
	   OKCHK();
       }
    }
    break;
    case dbFFIO::RECORD_FLOAT:
    { FloatRecord *p = recp->frec;
      float value;
       for(int i=0; i< num; i++, p++)
       {   READ(p);
           CPPUNIT_ASSERT((ok=ACMP(p,framecount, tagid, blanking,
				  validity, value, isample)));
	   OKCHK();
       }
    }
    break;
    case dbFFIO::RECORD_DOUBLE:
    { DoubleRecord *p = recp->drec;
      double value;
       for(int i=0; i< num; i++, p++)
       {   READ(p);
           CPPUNIT_ASSERT((ok=ACMP(p,framecount, tagid, blanking,
				  validity, value, isample)));
	   OKCHK();
       }
    }
    break;
    default:
      ostringstream msg;
      msg << "readTesta:: Unsupported record type at records[: "
	  << index << "]";
      CPPUNIT_FAIL(msg.str());
    }
  }

 return true;
}

// Run all tests.
void dbFFIOTest::test1()
{
  string bfilename=bfilename_;
  string afilename=afilename_;
  dbFFIOb binary;
  bool ok;

  CPPUNIT_ASSERT((ok=openTest(binary, bfilename)));
  if(!ok)
    return;
  CPPUNIT_ASSERT((ok=writeTest(binary)));
  if(!ok)
    return;
  CPPUNIT_ASSERT((ok=closeTest(binary)));
  if(!ok)
    return;
  CPPUNIT_ASSERT((ok=copyTest(bfilename, afilename)));
  if(!ok)
    return;
  CPPUNIT_ASSERT((ok=readTestb(bfilename)));
  if(!ok)
    return;
  CPPUNIT_ASSERT((ok=readTesta(afilename)));
  if(!ok)
    return;
}
