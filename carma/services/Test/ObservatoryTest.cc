/** 
 * @file 
 *
 * $Id: ObservatoryTest.cc,v 1.5 2006/10/03 21:16:30 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Observatory
 *
 * @author Marc Pound
 * @version $Revision: 1.5 $
 *
 */
#include <iostream>
#include "ObservatoryTest.h"
#include "carma/services/AstroTime.h"
#include "carma/services/padUtils.h"
#include "carma/services/Types.h"
#include "carma/util/NotFoundException.h"

using namespace CppUnit;
using namespace std;
using namespace carma::services;

void ObservatoryTest::setUp() 
{   

}

void ObservatoryTest::tearDown() 
{   

}

void ObservatoryTest::testObservatory()
{

    try {
	//Observatory obs("carma");
	Observatory obs; // dfault to carma
	cout << obs.toString() << endl;
	// get number of pad in FL configuration;
	// NB: this will throw NotFound if we eliminate FL from 
	// configuration.
	int numFLconfigs = obs.numPadsInConfig("FL");
	CPPUNIT_ASSERT( numFLconfigs == 15 );
	map<string,Pad> m = obs.getPadMap();
	for ( PadIterator pi = obs.mapBegin(); pi != obs.mapEnd(); ++pi) {
	    const Pad p = pi->second;
	    cout << p.toString() << endl;
	}
	cout << "LST = " 
	     << AstroTime(obs.getReference()).localSiderealTime() << endl;
	Pad pd = obs.getPad(defaultPadName(23));
	cout << pd.getName() << endl;
    } catch (carma::util::NotFoundException& nfex) {
	cout << " ObservatoryTest failed because a look-up failed: " 
	    << nfex.getMessage()
	    << endl;
	CPPUNIT_ASSERT(1 == 0);
    } catch (std::exception& stdex) {
	cout << " ObservatoryTest failed because: " 
	    << stdex.what()
	    << endl;
	CPPUNIT_ASSERT(1 == 0);
    }
}

