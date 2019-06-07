#ifndef CARMA_MONITOR_SUBSYSTEMFRAME_BUFFER_H
#define CARMA_MONITOR_SUBSYSTEMFRAME_BUFFER_H

/**
 * @file
 *
 * Class for managing IPQ for monitor subsystem.
 *
 * @author: N. S. Amarnath
 *
 * $CarmaCopyright$
 *
 */

#include <memory>

#include "carma/monitor/SubsystemFrame.h"

namespace carma  {
namespace monitor  {

class MonitorPointSample;


typedef enum {  
    MONITOR_NOT_WRITTEN = -1, 
    MONITOR_WRITTEN_ONCE = 0, 
    MONITOR_WRITTEN_AGAIN = 1  
} WriteStatus;


/*!
 * @brief Frame containing monitor points for a monitor subsystem,
 *        and mapped to an IPQ - essentially a SubsystemFrameQueue.
 *
 * A monitor subsystem queue is an IPQ implementation for
 * a shared queue of SubsystemFrames. The depth of the queue
 * is determined by the constant "kDefaultQueueDepth".
 *
 * @ref read method.
 *
 * @see carma::monitor::SubsystemFrame
 * @see carma::util::IPQwriter
 * @see carma/monitor/monitorframe.idl
 * @see http://www.mmarray.org/project/WP/Monitoring/monitoringDesign.pdf
 */
class SubsystemFrameBuffer : public SubsystemFrame {
  public:

    static const int kDefaultQueueDepth;
    
    /**
     * Destructor.
     */
    ~SubsystemFrameBuffer() ;

    void write (bool force = false) ;  // update to shared memeory

    unsigned int read( );
    bool readNewest( );
    bool readNewestConditionalCopy();

    /**
     * Factory method for producing a SubsystemFrameBuffer object.
     * Memory is allocated on the heap, so caller is responsible for deleting
     * the returned object after use. All SubsystemFrameBuffer objects with 
     * the same subsystemID attach to the same piece of shred memory, and so
     * the last one to write wins.
     *
     * @param subsystemID long numerical ID of monitor subsystem 
     * @param maxMonitorPoints long maximum # of monitor points the system 
     *        has to accommodate.
     * @param maxSamples long maximum # of monitor samples the system 
     *        has to accommodate.
     * @return SubsystemFrameBuffer& reference to SubsystemFrameBuffer
     *        created on the heap. Caller has to delete after use.
     */
    static SubsystemFrameBuffer &
    getSubsystemFrameBuffer( long subsystemID,
                             long maxMonitorPoints,
                             long maxSamples );

    /**
     * Writes specified sample value 'value' into this subsystem frame
     * as sample with sample number 'iSample' for the monitor point 
     * specified by 'tagID'
     *
     * @param tagID const tagIDType identifier of monitor point
     * @param value const MonitorPointSample& value to be written
     * @return WriteStatus status of write
     */
    virtual WriteStatus writeSampleValue 
                (const tagIDType tagID, const MonitorPointSample& value);

    /**
     * Writes specified sample value 'value' into this subsystem frame
     * as sample with sample number 'iSample' for the monitor point at
     * index 'index' in the monitorHeaders array.
     *
     * @param index const int index of monitor point header in monitorHeader 
     *        array in this subsystem frame
     * @param value const MonitorPointSample& value to be written
     * @return WriteStatus status of write
     */
    virtual WriteStatus writeSampleValue 
                (const int index, const MonitorPointSample& value);

    /**
     * Returns true if local copy contains data that differs from data
     * in IPQ - in other words, data in local copy has been modifed, 
     * and a write to IPQ hasnt happened yet.
     *
     * @return bool true if write is required to synchronize with IPQ.
     */
    bool        isDirty () const  ;

    /**
     * Returns true if numCleanSamples_ == 0. This means there are no
     * samples with modified values that need to be reflected in the IPQ.
     *
     * @return bool true if no modified values in this frame need to go
     *         into shared memory.
     */
    bool        isComplete () const  ;

  private:
    // No copying
    SubsystemFrameBuffer( const SubsystemFrameBuffer & rhs );
    SubsystemFrameBuffer & operator=( const SubsystemFrameBuffer & rhs );

    /**
     * Resets buffer status to 'clean'. Sets numCleanSamples_ to zero,
     * sets isDirty_ to false, and calls SubsystemFrame's clearSamples 
     * method. 
     */
    void clearFrame( );

    /**
     * Sets boolean flag to indicate that a write() is required to synchronize
     * with IPQ.
     *
     * @param flag bool true if valuea have been modified locally and requires
     *        synchronization with IPQ.
     */
    void decrementCleanSamples( );

    /**
     * Constructor. Constructs SubsystemFrame using subsystemID, 
     * pointer to SubsystemHeader, sizing information such as 
     * maxMonitorPoints and maxSamples, and attaches it to an IPQ
     * defined by the SubsystemHeader, the file name for the IPQ,
     * the 'isCreator' flag and the number of elements in the queue.
     *
     * @param subsystemID long numerical ID of monitor subsystem 
     * @param fname std::string& reference to file name for shared memory
     * @param maxMonitorPoints long maximum # of monitor points the system 
     *        has to accommodate.
     * @param maxSamples long maximum # of monitor samples the system 
     *        has to accommodate.
     * @param isCreator bool true if IPQ must be created if it doesnt exist
     * @param queueDepth int& number of elements in the IPQ
     * @see ::carma::monitor::SubsystemFrame
     * @see ::carma::util::IPQbuffer
     */
    SubsystemFrameBuffer(
        ushort                subsystemID,
        const ::std::string & fname, 
        long                  maxMonitorPoints,
        long                  maxSamples,
        bool                  isCreator,
        int                   queueDepth );

    class InternalIpq;
    
    ::std::auto_ptr< InternalIpq > ipq_;
    bool                           isDirty_;
    int                            numCleanSamples_;
};


} // namespace carma::monitor
} // namespace carma


#endif
