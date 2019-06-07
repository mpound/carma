
/**
 *
 * Implementation of the Observer class.
 *
 * @author: Steve Scott
 *
 * $Id: Observer.cc,v 1.1 2005/01/06 00:03:33 scott Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <sstream>

#include "carma/util/Observer.h"
#include "carma/util/ObserverRegistry.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::util;


Observer::Observer()
{
    regID_ = ObserverRegistry::instance().registerObserver();
}


Observer::~Observer()
{
    if (regID_ != 0) {
        ObserverRegistry::instance().unregisterObserver(regID_);
    }
    regID_ = 0;
}

int Observer::regID() const
{
    return regID_;
}




