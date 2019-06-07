
#ifndef CARMA_UTIL_OBSERVABLE_H
#define CARMA_UTIL_OBSERVABLE_H


/**
 * @file
 *
 * An Observable object executes a method on a registered
 * Observer when an observable event occurs.
 * @see Observer
 *
 * @author: Steve Scott
 *
 * $Id: Observable.h,v 1.1 2005/01/06 00:03:34 scott Exp $
 *
 * $CarmaCopyright$
 *
 */


#include "carma/util/PthreadMutex.h"
#include "carma/util/Observer.h"
#include <map>
#include <vector>

  
namespace carma {
    namespace util {
    
/**
 * The Observable class is paired with the Observer class, with 
 * the Observable class calls its observableEvent() method to
 * notify all registered Observers.
 * Observers may register once, and only once, with an unlimited number 
 * of Observable objects,
 * It is left to the clients to do any resource synchronization.
 * The simplest use of this class is to provide a programmable callback
 * mechanism. 
 */
class Observable {
    /**
     * @typedef Registry
     * Map of registryId and Observer
     */
    typedef std::map<int, Observer*> Registry; 
public:

    /**
     * Constructor
     */
    Observable();
    
    /**
     * Destructor
     */
    virtual ~Observable();
   
    /**
     * Register an Observer object to be notified when event occurs.
     * The Observers are notified by calling their observerUpdate() method
     * @throw ErrorException if the object is already registered
     * @param observer Object to notify when event occurs
     */
    void registerObserver(Observer& observer);

    /**
     * Unregister an Observer object.
     * @param observer Object to unregister
     * @throw ErrorException if the object is not already registered
     * @see registerObserver
     */
    void unregisterObserver(Observer& observer);


    /**
     * Notify all registered Observers, by calling Observer::observerUpdate()
     */
    void notifyObservers();

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

private:
    Registry registry_;
    PthreadMutex registryGuard_;

};

} }  // End namespace carma::util 



#endif  // CARMA_UTIL_OBSERVABLE_H









