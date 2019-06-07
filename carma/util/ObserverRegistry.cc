
/**
 *
 * Implementation of the ObserverReistry class.
 *
 * @author: Steve Scott
 *
 * $Id: ObserverRegistry.cc,v 1.3 2011/12/21 22:57:06 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <sstream>
#include <iostream>
#include <algorithm>

#include "carma/util/ObserverRegistry.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::util;

class Observer;

ObserverRegistry::ObserverRegistry(): 
        nextRegistrationId_(1)
{
}


ObserverRegistry::~ObserverRegistry()
{
}

ObserverRegistry& ObserverRegistry::instance()
{
    static ObserverRegistry& registry = *new ObserverRegistry();
    return registry;
}

void ObserverRegistry::lock()
{
    registryGuard_.Lock();
}

void ObserverRegistry::unlock()
{
    registryGuard_.Unlock();
}

// Note: no locking - do inisde a locking block
bool ObserverRegistry::isRegistered(int regID)
{
    vector<int>::iterator reg;
    reg = find(registry_.begin(), registry_.end(), regID);
    return (reg != registry_.end());    
}

int ObserverRegistry::registerObserver()
{
    lock();
    registry_.push_back(nextRegistrationId_);
    unlock();

    CPTRACE(Trace::TRACE4, registryToString());
    
    return nextRegistrationId_++;
}

void ObserverRegistry::unregisterObserver(int regID)
{
    vector<int>::iterator reg;
    lock();
    reg = find(registry_.begin(), registry_.end(), regID);
    if (reg != registry_.end()) {
        registry_.erase(reg);
        if(false) cout << "Unreg(" <<regID << "): new size=" 
                      << registry_.size() << endl;
    }
    else {
        unlock();
        ostringstream o;
        o << "ObserverRegistry::unregister(" << regID 
          << "): no entry found for " << regID;
        throw CARMA_ERROR(o);
    }
    
    CPTRACE(Trace::TRACE4, registryToString());
    unlock();
}

int ObserverRegistry::getNumObservers()
{
    return registry_.size();
}

vector<int> ObserverRegistry::registryIds()
{
    vector<int> ids = registry_;    
    return ids;
}


string ObserverRegistry::registryToString()
{
    ostringstream s;
    ::size_t count = 0;
    vector<int> ids = registryIds();
    s << "ObserverRegistry(" << ids.size() << "):" ;
    for (::size_t i = 0; i < ids.size(); i++) {
        if ( count++ > 0) s << ",";
        s << " " << ids[i];
    }
    
    return s.str();
}


