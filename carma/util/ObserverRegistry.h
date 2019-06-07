
#ifndef CARMA_UTIL_OBSERVERREGISTRY_H
#define CARMA_UTIL_OBSERVERREGISTRY_H


/**
 * @file
 *
 * This class keeps track of Observer instances and should only be used
 * internally by the Observer/Observable classes
 * @see Observer
 * @see Observable
 *
 * @author: Steve Scott
 *
 * $Id: ObserverRegistry.h,v 1.1 2005/01/06 00:03:34 scott Exp $
 *
 * $CarmaCopyright$
 *
 */


#include "carma/util/PthreadMutex.h"
#include <vector>

  
namespace carma {
    namespace util {
    
class Observer;  // forward declaration
    
/**
 * All instances of the Observer class are assigned an ID and registered
 * within this class. An Observable can then see if an Observer registered
 * with it still exists. The constructor/destructor of Observer will take
 * care of registering with this singleton class. 
 */
class ObserverRegistry {
public:

    /**
     * Return singleton instance
     */
    static ObserverRegistry& instance();
    
    /**
     * Destructor
     */
    virtual ~ObserverRegistry();
   
    /**
     * Register objects to be notified when event occurs.
     * @param observer Object to notify when event occurs
     * @return registerationId that can be used to unregister
     */
    int registerObserver();

    /**
     * Unregister objects to be notified when event occurs.
     * @param registrationId
     * @throws ErrorException if regID is invalid
     */
    void unregisterObserver(int registrationId);

    /**
     * Get the number of registered observers.
     * There is no test for uniqueness; if a single object is registered
     * multiple times it will be counted multiple times.
     */
    int getNumObservers();

    /**
     * Put all regIDs into a vector
     */
    std::vector<int> registryIds();

    /**
     * Put all regIDs into a single line string
     */
    std::string registryToString();

    /**
     * Lock the registry
     */
    void lock();

    /**
     * Unlock the registry
     */
    void unlock();

    /**
     * See if an id is registered
     * @param regID id to check
     */
    bool isRegistered(int regID);

private:
    /*
     * Constructor; private so can only be created by instance method
     */
    ObserverRegistry();
    
    std::vector<int> registry_;
    int nextRegistrationId_;
    PthreadMutex registryGuard_;

};

} }  // End namespace carma::util 



#endif  // CARMA_UTIL_OBSERVERREGISTRY_H









