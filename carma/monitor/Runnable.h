
#ifndef CARMA_MONITOR_RUNNABLE_H 
#define CARMA_MONITOR_RUNNABLE_H

/**
 * @file
 *
 * An interface for executing a method with no parameters
 *
 * @author: Steve Scott
 *
 * $Id: Runnable.h,v 1.2 2006/03/02 00:14:59 tcosta Exp $
 * $CarmaCopyright$
 *
 */
 



namespace carma {
    namespace monitor {


/**
 *
 * Abstract class that defines the interface for executing a method.
 * Inherit from this interface and define the execute method.
 *
 */
class Runnable {
    public:
    
        /**
         * Constructor
         */
        explicit Runnable( );
    
        virtual ~Runnable( );
        
        /**
         * Execute the method
         @return an integer to be defined by the overriding method
         */
        virtual int execute( ) const = 0;
};


} }  // End namespace carma::monitor  


inline
carma::monitor::Runnable::Runnable( )
{
}


inline
carma::monitor::Runnable::~Runnable( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


#endif  // CARMA_MONITOR_RUNNABLE_H
