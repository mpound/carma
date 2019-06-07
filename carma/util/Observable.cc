
/**
 *
 * Implementation of the Observable class.
 *
 * @author: Steve Scott
 *
 * $Id: Observable.cc,v 1.5 2005/08/29 16:35:05 tcosta Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <iostream>
//#include <sstream>

#include "carma/util/Observable.h"
#include "carma/util/ObserverRegistry.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ScopedPthreadMutexLockManager.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::util;


Observable::Observable()
{
}

Observable::~Observable()
{
    //cout << "Observable::destructor" << endl;
}

void Observable::registerObserver(Observer& observer)
{
    ScopedPthreadMutexLockManager m(registryGuard_);
    m.LockMutex();
    Registry::iterator r = registry_.find(observer.regID());
    if (r == registry_.end()) {
        // Didn't find it, put it in
        registry_[observer.regID()] = &observer;
        m.UnlockMutex();
    }
    else {
        m.UnlockMutex();
        ostringstream o;
        o << "Observable::register() - Observer with id="
          << observer.regID() << " is already registered." ;
        throw CARMA_ERROR(o);
    }

    CARMA_CPTRACE(Trace::TRACE4, registryToString());
}

void Observable::unregisterObserver(Observer& observer)
{
    ScopedPthreadMutexLockManager m(registryGuard_);
    m.LockMutex();
    Registry::iterator r = registry_.find(observer.regID());
    if (r != registry_.end()) {
        // Found it, now remove it
        registry_.erase(r);
        m.UnlockMutex();
    }
    else {
        m.UnlockMutex();
        ostringstream o;
        o << "Observable::unregister() - Observer with id="
          << observer.regID() << " is not registered." ;
        throw CARMA_ERROR(o);
    }

    CARMA_CPTRACE(Trace::TRACE4, registryToString());
}

/*
 * This goes through its list of registered observers, checks them
 * against the singleton registry of all observers to make sure
 * that the observers still exist, then calls their nofication method.
 * Both registries are locked throughout the whole process.
 */
void Observable::notifyObservers()
{
    bool debug = false; 
    // We lock our registry so no one can sneak in and register, mucking up
    // the register while we are using it
    ScopedPthreadMutexLockManager m(registryGuard_);
    m.LockMutex();
    
    if (registry_.empty()) {
        m.UnlockMutex();
        return; 
    }
  
    if (debug) cout << "**notifyObservers**" << endl;
    // Also check and make sure the observer still exists in ObsReg
    ObserverRegistry& obreg = ObserverRegistry::instance();
    obreg.lock();
    Registry::iterator reg;
    for (reg = registry_.begin(); reg != registry_.end(); ++reg) {
        if (obreg.isRegistered(reg->first)) {
            if (debug) cout << "  notifying:" << reg->first << endl;
            reg->second->observerUpdate(*this);
        }
        else { 
            // The observer is no longer registered, remove it
            if (debug) cout << "  removing:" << reg->first << endl;
            Registry::iterator temp = reg;
            // Back up so we don't point to spot we are going to erase
            if (reg != registry_.begin())--reg; 
            registry_.erase(temp);
        }
    }
    obreg.unlock();
    m.UnlockMutex();
}

int Observable::getNumObservers()
{
    return registry_.size();
}

vector<int> Observable::registryIds()
{
    vector<int> ids;
    Registry::iterator reg;
    for (reg = registry_.begin(); reg != registry_.end(); ++reg) {
       ids.push_back(reg->first);
    }
    
    return ids;
}

string Observable::registryToString()
{
    ostringstream s;
    ::size_t count = 0;
    vector<int> ids = registryIds();
    s << "Observerable.registry(" << ids.size() << "):" ;
    for (::size_t i = 0; i < ids.size(); i++) {
        if ( count++ > 0) s << ",";
        s << " " << ids[i];
    }
    
    return s.str();
}


