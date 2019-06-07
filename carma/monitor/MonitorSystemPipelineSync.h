/** @file
 * MonitorSystemPipelineSync class declaration.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.3 $
 * $Date: 2008/01/16 22:01:30 $
 * $Id: MonitorSystemPipelineSync.h,v 1.3 2008/01/16 22:01:30 abeard Exp $
 */

#ifndef CARMA_MONITOR_MONITORSYSTEMPIPELINESYNC_H
#define CARMA_MONITOR_MONITORSYSTEMPIPELINESYNC_H

#include "carma/monitor/MonitorSystemSelector.h"

namespace carma {
namespace monitor {

/**
 * Class to synchronize an input carma monitor system with appropriately 
 * time tagged pipeline monitor data.
 */
class MonitorSystemPipelineSync {
public:

    /**
     * Constructor
     * @param inputCMS Input Carma Monitor System selector.
     * @param outputCMS Output Carma Monitor System selector.
     */
    explicit MonitorSystemPipelineSync( CmsSelector inputCMS,
                                        CmsSelector outputCMS );

    /* virtual */ ~MonitorSystemPipelineSync( );
    
    void syncNextValidFrame( );

protected:

private:
    
    CmsAP inputCMS_;
    CmsAP bufferCMS_;
    CmsAP outputCMS_;

    ::std::string inputCmsName_;
    ::std::string bufferCmsName_;
    ::std::string outputCmsName_;

}; // class MonitorSystemPipelineSync

} } // namespace carma::monitor
#endif
