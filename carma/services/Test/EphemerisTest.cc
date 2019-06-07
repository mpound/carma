/** 
 * @file 
 *
 * $Id: EphemerisTest.cc,v 1.3 2007/10/05 15:00:28 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Ephemeris
 *
 * Author: Peter Teuben
 * Version: $Revision: 1.3 $
 * $Date: 2007/10/05 15:00:28 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>
#include "EphemerisTest.h"


using namespace CppUnit;
using namespace std;
using namespace carma::services;
using namespace carma::util;

void EphemerisTest::setUp() 
{   
  this->tEph = new carma::services::Ephemeris();
}

void EphemerisTest::tearDown() 
{   
  delete tEph;
}

void EphemerisTest::testEph1()
{
  double eps = 5.0;         // 5 arcsec for now
  tEph->setSource("3c273");
  tEph->setMJD(54077.0);    // 7 dec 2006  
#if 1
  double dRa  = (tEph->getRa()  - 3.2701396141953878)*206264.81;
  double dDec = (tEph->getDec() - 0.0351495422390435)*206264.81;
  cout << "testEph1: " << dRa << " " << dDec << "\n";
  if (abs(dRa) > eps || abs(dDec) > eps) {
    cout << "bad Ra/Dec with no IERS" << endl;
    CPPUNIT_ASSERT( false );
  } else {
    CPPUNIT_ASSERT( true );
  }

  tEph->setDeltaT(0.0);    // force IERS dependant numbers to be 0
  dRa  = (tEph->getRa()  - 3.2701396141953878)*206264.81;
  dDec = (tEph->getDec() - 0.0351495422390435)*206264.81;
  cout << "testEph1: " << dRa << " " << dDec << "\n";
  if (abs(dRa) > eps || abs(dDec) > eps) {
    cout << "bad Ra/Dec after IERS" << endl;
    CPPUNIT_ASSERT( false );
  } else {
    CPPUNIT_ASSERT( true );
  }
  // on 7 dec with old IERS table:  0.000909378 2.79992e-05     arcsec
  //               new IERS table   0.000911102 2.8052e-05      
  //               zero             -3.55576e-11 -2.03436e-12
  // after 7 months IERS has diverged in this way:
  // old: 54076.00000  0.18743   -0.1148   0.3277   2006-Dec-07
  // new: 54076.00000  0.06591   -0.0454   0.3124   2006-Dec-07

#else
  double Ra  = tEph->getRa();
  double Dec = tEph->getDec();
  double MJD = tEph->getMJD();
  printf("testEph1: %20.6f Ra/Dec: %20.16f %20.16f\n",MJD,Ra,Dec);
#endif

}

void EphemerisTest::testIERS()
{
      IERSTable iers("conf/catalogs/IERS.tab");
      bool IERSTableIsOutOfDate = iers.isOutOfDate();
      if ( IERSTableIsOutOfDate ) {
	cout << "IERS table is overdue for an update. Yours is more than "
             << IERSTable::MAX_ALLOWABLE_DAYS_OUT_OF_DATE 
	     << " days old."
	     << endl;
      }

      CPPUNIT_ASSERT ( ! IERSTableIsOutOfDate );
}
