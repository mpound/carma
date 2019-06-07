#ifndef CARMA_MONITOR_SYSTEMFRAME_BUFFER_H
#define CARMA_MONITOR_SYSTEMFRAME_BUFFER_H

/**
 * @file
 *
 * Class for managing monitor system IPQ.
 *
 * @author: N. S. Amarnath
 *
 * $CarmaCopyright$
 *
 */

#include <memory>

#include "carma/monitor/SystemFrame.h"
// To get the default queueDepth...
#include "carma/monitor/SubsystemFrameBuffer.h"

namespace carma  {
namespace monitor  {


class MonitorPointSet;


class SystemFrameBuffer : public SystemFrame {
  public:
    void  write () ;  // update to shared memeory

    /**
     * Read from the shared memory of the IPQ
     * @return number of elements lost in reading the queue
     * (non-zero if the reads are so far apart in time that the queue
     * has wrapped around.
     */
    unsigned int read( );
    bool readNewest( );
    bool readNewestIfStale( );
    bool readNewestConditionalCopy();
    
    /*!
     * @brief method to return instances of MonitorPointSet
     *        tied to a specific subsystem frame.
     *
     * This method returns a reference to an instance of a MonitorPointSet
     * object. If the subsystemID is invalid, this method throws a
     * BaseException. User is responsible for allocated memory -
     * free MonitorPointSet object using 'delete' operator after use.
     *
     * @return reference to (newly allocated) MonitorPointSet.
     */
    MonitorPointSet&        getMonitorPointSet (ushort subsystemID,
                                                int numMonitorPoints = 0,
                                                int numSamples = 0);

    /*!
     * @brief method to return instance of SystemFrameBuffer.
     *
     * This factory method returns a reference to an instance
     * of a SystemFrameBuffer object. The method returns a valid reference
     * within the ACC. Do not use this method within any other subsystem.
     *
     * @return reference to SystemFrameBuffer&.
     */
    static SystemFrameBuffer &
    getSystemFrameBuffer( 
            const ::std::string& name = "Carma",
            long        maxSubsystems = 0,
            long     maxMonitorPoints = 0,
            long           maxSamples = 0,
            int        queueDepth = SubsystemFrameBuffer::kDefaultQueueDepth);

    /**
     * Destructor.
     */
    virtual ~SystemFrameBuffer ();

    void setNoneAvailable();

  private:
    // No copying
    SystemFrameBuffer( const SystemFrameBuffer & rhs );
    SystemFrameBuffer & operator=( const SystemFrameBuffer & rhs );

    /**
     * Constructor that uses maxSubsystems, maxMonitorPoints, maxSamples
     * to construct the SystemFrame;
     * and the file name 'fname', the boolean 'isCreator' and the
     * number of elements in the queue (queueDepth) to create an IPQ
     * of SystemHeader+ structures.
     *
     * @param fname ::std::string& reference to file name for shared memory
     * @param maxSubsystems long maximum # of subsystems the system
     *        has to accommodate.
     * @param maxMonitorPoints long maximum # of monitor points the system
     *        has to accommodate.
     * @param maxSamples long maximum # of monitor samples the system
     *        has to accommodate.
     * @param isCreator bool true if IPQ must be created if it doesnt exist
     * @param queueDepth int& number of elements in the IPQ
     * @see ::carma::monitor::SystemFrame
     * @see ::carma::util::IPQbuffer
     */
    SystemFrameBuffer( const ::std::string & fname,
                       long                  maxSubsystems,
                       long                  maxMonitorPoints,
                       long                  maxSamples,
                       bool                  isCreator,
                       int                   queueDepth );

    class InternalIpq;
    
    ::std::auto_ptr< InternalIpq > ipq_;
};


} // namespace carma::monitor
} // namespace carma


#endif
