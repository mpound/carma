
#ifndef CARMA_MONITOR_MONITORSYSTEMCONTAINER_H 
#define CARMA_MONITOR_MONITORSYSTEMCONTAINER_H

/**
 * @file
 *
 * An abstract base class used to support the common functionality
 * of a MonitorSystem and a MonitorSubsystem..
 *
 * @author: Steve Scott
 *
 * $Id: MonitorSystemContainer.h,v 1.9 2008/02/11 16:45:16 scott Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/MonitorContainer.h" 
#include <string>


namespace carma {
    namespace monitor {

 
/**
 *
 * Monitor system container interface definition.
 *
 */
class MonitorSystemContainer : public MonitorContainer {
public:

    /**
     * Constructor base subsystem configuration
     * @param systemName     monitor system name
     */
    MonitorSystemContainer(const std::string& systemName);

    /**
     * Destructor
     */
    virtual ~MonitorSystemContainer() ;
        
    /**
     * Returns true if the IPQ contains data that is current.
     * A read could make the data current, and hence make isCurrent return true.
     * If a read does not make isCurrent true, then the IPQ 
     * is probably not being written into.
     * @return bool true if IPQ is current, false otherwise.
     */
    virtual bool isCurrent() const = 0;
    
    /**
     * Reads oldest unread data from the IPQ into the local
     * set of monitor points. This is a blocking read.
     * @return number of elements lost in reading the queue
     * (non-zero if the reads are so far apart in time that the queue
     * has wrapped around.
     */
    virtual unsigned int read() = 0;
    
    /**
     * Reads in the newest data from the IPQ into the local
     * set of monitor points. There is no blocking!!
     * @return false if no data was available in the queue
     */
    virtual bool readNewest() = 0;
    
    /**
     * If new data is available it is copied from the queue into
     * the monitor system. If no unread data is available then the
     * method returns. In either case there is no blocking.
     * @return false if no data was copied
     */
    virtual bool readNewestConditionalCopy() = 0;

    /**
     * Reads in data from the IPQ into the local
     * set of monitor points if data is stale, that is, !isCurrent().
     * There is no blocking!!
     * @return false if no data was available in the queue
     * @deprecated Replaced by readNewestConditionalCopy
     */
    virtual bool readNewestIfStale();
    
    /**
     * Write out the monitor point data values. 
     */
    virtual void write() = 0;

    /**
     * Get the frame count for the last read frame.. 
     */
    virtual int getFrameCount() const = 0;
    
    /**
     * Checks to see if this system is actively receiving data.
     * It reads the newest frame and checks to see if the frameCount
     * is within two seconds of the current time.
     * Note: this moves the read pointer to the top of the queue.
     * @return true if system has received data in the last 2 seconds.
     */
    bool isActive();

          
private:

};



} }  // End namespace carma::monitor  


#endif  // CARMA_MONITOR_MONITORSYSTEMCONTAINER_H









