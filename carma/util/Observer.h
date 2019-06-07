
#ifndef CARMA_UTIL_OBSERVER_H
#define CARMA_UTIL_OBSERVER_H


/**
 * @file
 *
 * An Observer executes a method when an observable event occurs.
 * The method must be defined and the Observer registered with an
 * Observable object.
 * @see Observable
 *
 * @author: Steve Scott
 *
 * $Id: Observer.h,v 1.1 2005/01/06 00:03:33 scott Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <map>
  
namespace carma {
    namespace util {

class Observable;
    
/**
 * An abstract base class for an implementation of the Observer design pattern.
 * The observerMethod of this class will be called when an
 * observable event occurs in an Observable object.
 */
class Observer {
 
public:


    /**
     * Constructor
     */
    Observer();
    
    /**
     * Destructor
     */
    virtual ~Observer();
   
    /**
     * Abstract method called by the Observable.
     * @param observable The Observable object calling this method
     * Can be used to determine who is calling back in case this object
     * is registered with more than one Observable.
     */
    virtual void observerUpdate(Observable& observable) = 0;

    /**
     * Get the registration ID
     * @return the registration ID
     */
    int regID() const ;

private:
    int regID_; // ID given by the ObserverRegistry

};

} }  // End namespace carma::util 



#endif  // CARMA_UTIL_OBSERVER_H









