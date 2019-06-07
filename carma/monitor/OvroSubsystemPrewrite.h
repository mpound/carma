/**
 * @file
 * Definition of OvroSubsystemPrewrite class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.2 $
 * $Date: 2005/11/18 17:05:28 $
 * $Id: OvroSubsystemPrewrite.h,v 1.2 2005/11/18 17:05:28 abeard Exp $
 */
#ifndef CARMA_MONITOR_OVROSUBSYSTEMPREWRITE_H
#define CARMA_MONITOR_OVROSUBSYSTEMPREWRITE_H

// CARMA includes
#include "carma/monitor/Runnable.h"

namespace carma {
namespace monitor {

    class OvroSubsystem;

    /**
     * OvroSubsystemPrewrite class.  
     * Several AntennaCommon monitor devices have OvroSubsystem counterparts
     * which do not map to each other in a 1-to-1 fashion.  For example, the
     * OvroSubsystem contains several instances of a SisReceiver monitor
     * Device, one for the 1mm receiver, another for 3mm, etc.  However
     * AntennaCommon contains only a single receiver which then distinguishes 
     * between different receiver types via the 'currentRx' monitor point. 
     * This class exists as a sort of precollator to map OvroSubsystem
     * monitor points to AntennaCommon monitor points prior to writing them
     * to an FSP.
     */
    class OvroSubsystemPrewrite : public carma::monitor::Runnable {
    public:
        
        /**
         * Constructor
         */
        OvroSubsystemPrewrite( OvroSubsystem & subsys );

        /**
         * Destructor
         */
        virtual ~OvroSubsystemPrewrite();

        /**
         * Execute
         * Defines prewrite activities.
         */
        int execute() const;

    protected:
        
        // Nothing is protected.

    private:

        OvroSubsystem & mon_;

    };
}} // End namespace carma::monitor
#endif // CARMA_MONITOR_OVROSUBSYSTEMPREWRITE_H
