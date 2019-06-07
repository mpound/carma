
/**
 * Test data for IPQ writer and reader tests.
 *
 *@author: Steve Scott
 *
 * $Id: ipqWriterReaderData.h,v 1.4 2008/02/12 21:02:28 scott Exp $
 *
 */

#include <iostream>

#include "carma/util/Time.h"


namespace carma {
namespace util {
namespace test {


// Name of the shared memory
const std::string fname = "/writeReadTest.ipq";

// If the size of this class is more than 1.02+ MB then the
// reader program will crash. Don't know why, but it may
// be a stack space issue...

struct Element {
    int i;

#if 0
    double a[128][100];
#else
	::size_t a[2000];
#endif

    double mjd;
	
    void dump( ) const;
};


inline void
Element::dump( ) const {
	std::cout << "Time=" 
			  << Time::getDateTimeString( mjd, 2 ) 
			  << "  i="  << i
			  << std::endl;
}


}  // namespace carma::util::test
}  // namespace carma::util
}  // namespace carma
