/**
 *
 * Carma subarray.
 *
 * @author: Steve Scott
 *
 * $Id: Subarray.cc,v 1.2 2007/09/04 19:53:17 tcosta Exp $
 *
 * $CarmaCopyright$
 *
 */
 

#include <sstream>

 
#include "carma/control/Subarray.h"
                                                   
using namespace std;
using namespace carma::control;

 
Subarray::Subarray()
{
}
 
Subarray::~Subarray() 
try {
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}
 

string Subarray::makeName(const string& name, int number)
{
    ostringstream o;
    o << name << number;
    return o.str();
}



