
#ifndef CARMA_LOBEROTATOR_CHASSIS_H
#define CARMA_LOBEROTATOR_CHASSIS_H

/**
 * @file
 * This is the chassis that holds the loberotators.
 * It is a simple container where Loberotators can be accessed.
 *
 * @author Steve Scott
 *
 * $Id: Chassis.h,v 1.2 2005/07/19 23:45:42 scott Exp $
 * 
 * $CarmaCopyright$
 */



namespace carma {
namespace loberotator {
    
    // Forward declaration
    class Loberotator;

    /** 
     * Provides storage for Loberotator objects
     *  
     */
    class Chassis { 
    public:

        /** 
         * Constructor
         */ 
        Chassis();

        /** 
         * Destructor
         */ 
        virtual ~Chassis();

        /**
         * Insert a Loberotator into the system.
         * @param loberotator  
         */
        void insert(Loberotator*);

        /**
         * Get a loberotator
         * @param chan Channel index, starting at zero  
         */
        Loberotator& loberotator(int chan) const;

       /**
        * Maximum number of loberotator channels.
        * This number controls many loops and may affect the stability
        * of the system. For example, reducing it will place 
        * CAN messages to the same node closer together in time in the
        * phase and rate update.
        */
       static const int N_CHAN = 24;
                          
    protected:
        carma::loberotator::Loberotator* loberotator_[N_CHAN];

    private:        
        // Copying and assignment are not allowed
        Chassis(const Chassis&);
        Chassis &operator=(const Chassis&);
        
        int nextChan;
                                 
    }; // End Chassis class
    
}; // End loberotator namespace
}; // End carma namespace

#endif // CARMA_LOBEROTATOR_CHASSIS_H
