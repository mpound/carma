#ifndef CARMA_UI_RTD_MONITORDISPLAY_H
#define CARMA_UI_RTD_MONITORDISPLAY_H


/*
 * @file
 *
 * Base class for carma realtime monitoring windows.
 * This has a built in copy of the monitoring system, with all values
 * updated using the preInternalUpdate method.
 *
 * @author Steve Scott 
 * $id: $
 *
 * $CarmaCopyright$
 *
 */


#include "carma/monitor/MonitorSystem.h"
#include "carma/ui/rtd/common/CarmaDisplay.h"

namespace carma {
    namespace ui {
        namespace rtd { 



class MonitorDisplay: public carma::ui::rtd::CarmaDisplay {
public:
    /**
     * Constructor.
     * @param subtitle put on the title bar of the display; this is prefaced
     *                 by the system name
     * @param ut string for ut in time panel
     * @param lst string for lst in time panel
     * @param visibleTimePanel flag to make time panel visible
     */
    MonitorDisplay(
        const std::string& subtitle, 
        const char* ut, const char* lst, 
        bool visibleTimePanel = true);
        
    /**
     * Constructor.
     * @param subtitle put on the title bar of the display but is prefaced
     *                 by the system name
     * @param visibleTimePanel flag to make time panel visible
     */
    MonitorDisplay(const std::string& subtitle, 
        bool visibleTimePanel = true);
 
    /**
     * Destructor.
     */
    virtual ~MonitorDisplay() {};
    
    /**
     * Get a reference to the carma monitor system.
     */
    carma::monitor::CarmaMonitorSystem& cms() const; 
 
private:
    // Overrides RtDisplay
    void preInternalUpdate();
    carma::monitor::CarmaMonitorSystem& carma_;  
};

}}}

#endif  // CARMA_UI_RTD_MONITORDISPLAY_H 
