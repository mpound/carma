#ifndef CARMA_MONITOR_SYSTEMFRAME_H
#define CARMA_MONITOR_SYSTEMFRAME_H

/**
 * @file
 *
 * Class wrapper for monitor system frame structure that manages storage
 * for a monitor system frame.
 *
 * @author: N. S. Amarnath
 *
 * $CarmaCopyright$
 *
 */


#include <memory>
#include "carma/util/PthreadRWLock.h"

namespace carma  {
namespace monitor  {


struct TransportSubsystemFrame;
struct SubsystemHeader;
struct SubsystemDataPointers;
class SubsystemFrame;


/*!
 * @class SystemFrame SystemFrame.h "carma/monitor/SystemFrame.h"
 * @brief Frame containing monitor points for all monitor subsystems.
 *
 * A monitor subsystem is any collection of CARMA components that 
 * communicates with the Array Control Computer through a single 
 * computer. A monitor system is the collection of all monitor subsystems 
 * of interest. This collection of monitor points across all subsystems
 * is available in the Array Control Computer. The Carma Monitor System
 * storage is managed by this class. 
 *
 * A SystemFrameBuffer allows changes made to monitor point values in 
 * shared memory to be reflected in the local copy in this class instance.
 *
 * NOTE: The term "monitor stream" as used in this document refers to all 
 * monitor data collected at each monitor subsystem, flowing to the ACC
 * and merged there to form the totality of monitor information across all 
 * of CARMA. The term "client" as used in this document refers to clients of
 * monitor system processes - these can be processes that write monitor data 
 * to the monitor stream, and processes that read monitor data from the
 * monitor stream.
 *
 * @ref ::carma::monitor::SubsystemFrameBuffer::read 
 *
 * @see ::carma::monitor::MonitorSystem
 * @see ::carma::monitor::SubsystemFrameBuffer
 * @see carma/monitor/monitorframe.idl
 * @see http://www.mmarray.org/project/WP/Monitoring/monitoringDesign.pdf
 */
class SystemFrame {
  public:
    /**
     * Destructor.
     */
    virtual ~SystemFrame( );

    void synchronize( const SystemFrame & src );

   /**
    * Method returns true if frame's timestamp is current; false otherwise
    *
    * @return bool true if current monitor system frame has a frameCount 
    *         that corresponds to the current half-second tick.
    */
    bool isCurrentFrame( ) const;

    // callers responsibility to delete memory
    /**
     * Returns a SubsystemFrame object wrapped around an internally
     * stored SubsystemHeader structure containing monitor subsystem 
     * information for subsystem with ID 'subsystemID'. It is the caller's
     * responsibility to delete memory.
     *
     * @param subsystemID identifier of subsystem
     * @return SubsystemFrame& reference to newly created SubsystemFrame 
     *         object.
     * @see ::carma::monitor::SubsystemFrame
     */
    ::std::auto_ptr< SubsystemFrame >
    makeSubsystemFrame( unsigned short subsystemID ) const;

    /**
     * Returns newly created SubsystemFrame object given the index to the 
     * subsystem in the subsystemFrameOffset array.
     *
     * @param index int index in subsystemFrameOffset array in 
     *        SystemHeader+ managed by this SystemFrame.
     * @return SubsystemFrame& reference to newly created SubsystemFrame 
     *         object.
     */
    ::std::auto_ptr< SubsystemFrame >
    makeSubsystemFrameForIndex( int index ) const;

    /**
     * True if subsystem with subsystemID has 
     * frameCount == this->getFrameCount().
     *
     * @param subsystemID identifier of subsystem of interest.
     * @return bool true if getSubsystemFrame(subsystemID).getFrameCount() ==
     *                      getFrameCount().
     */
    bool        isCurrent (unsigned short subsystemID) const ;

    /**
     * Returns timestamp for SystemHeader as frameCount.
     *
     * @return long frameCount timestamp for this SystemFrame.
     */
    long        getFrameCount ()  const ;

    /**
     * Sets timestamp for SystemHeader as frameCount.
     *
     * @param frameCount const long frameCount timestamp for this SystemFrame.
     */
    long        setFrameCount (const long frameCount)  ;

    /**
     * True if getNumCleanFrames() == 0. Implies that data in local copy 
     * (buffer) reflects data in shared memory.
     *
     * @return bool true if data in local copy is same as data in 
     *         shared memory.
     */
    bool        isComplete ()  const ;

    /**
     * Writes transported sample values into subsystem frame given all
     * sample values assciated with the subsystem in the form of a 
     * TransportSubsystemFrame structure.
     *
     * @param subsystemFrame TransportSubsystemFrame& transported sample data
     *        containing all sample values with a subsystem
     */
    void writeSubsystemFrame (
        const TransportSubsystemFrame & subsystemFrame );

    /**
     * Size information - returns maximum number of subsystems this 
     * SystemFrame can accommodate.
     *
     * @return long maximum number of subsystems frames possible 
     *         in this SystemFrame.
     */
    long        getMaxNumSubsystemFrames ()  const ;

    /**
     * Size information - returns maximum cumulative number of monitor points 
     * across all subsystems that this SystemFrame can accommodate.
     *
     * @return long cumulative maximum number of monitor points,
     *         across all subsystem frames, possible 
     *         in this SystemFrame.
     */
    long        getMaxTotalMonitorPoints ()  const ;

    /**
     * Size information - returns cumulative maximum number of monitor
     * samples, across all subsystems, this SystemFrame can accommodate.
     *
     * @return long cumulative maximum number of monitor samples,
     *         across all subsystems, possible in this SystemFrame.
     */
    long        getMaxTotalSamples ()  const ;

    /**
     * Size information - returns number of allocated subsystems in this 
     * SystemFrame.
     *
     * @return long number of allocated subsystems frames 
     *         in this SystemFrame.
     */
    long        getNumSubsystemFrames () const ;

    /**
     * Size information - Sets number of allocated subsystems in this 
     * SystemFrame.
     *
     * @param numSubsystems maximum number of allocated subsystems 
     *        frames in this SystemFrame.
     */
    void        setNumSubsystemFrames (unsigned short numSubsystems) ;

    /**
     * Writes the value of the delay used by the writer part of the 
     * frameCollator * to write to the associated IPQ in the ACC. 
     * The delay is specified as a fraction of a second after the 
     * half-second tick - it is in the range [0.0, 500.0)
     *
     * @param writeDelay double delay specified as fractions of a second
     */
    void        setCollatorWriteDelay (double writeDelay);

    /**
     * Returns the delay used by the writer part of the frameCollator
     * to write to the associated IPQ in the ACC. 
     * The delay is specified as a fraction of a second after the 
     * half-second tick - it is in the range [0.0, 500.0)
     *
     * @return double delay specified as fractions of a second
     */
    double getCollatorWriteDelay () const;

    /**
     * Sets the time of the last collator write to the associated
     * system IPQ. The time is specified as an MJD.
     *
     * @param mjdTimestamp time of last system IPQ write specified as an MJD
     */
    void setCollatorWriteTime( double mjdTimestamp );
    //void setCollatorWriteTime( );

    /**
     * Gets the time of the last collator write to the associated
     * system IPQ. The time is specified as an MJD.
     *
     * @return double time of last system IPQ write specified as an MJD
     */
    double getCollatorWriteTime () const;

    /**
     * Sets the time of the last read from the associated
     * RawCarmaMonitorSystem. The time is specified as an MJD.
     *
     * @param mjdTimestamp time of last RawCarmaMonitorSystem IPQ read 
     *                     specified as an MJD
     */
    void setRawReadTime ( double mjdTimestamp );
    //void setRawReadTime( );

    /**
     * Gets the time of the last read from the associated
     * RawCarmaMonitorSystem. The time is specified as an MJD.
     *
     * @return double time of last RawCarmaMonitorSystem IPQ read 
     *        specified as an MJD
     */
    double getRawReadTime () const;

    /**
     * Sets the time of the last write to the associated
     * CarmaMonitorSystem. The time is specified as an MJD.
     *
     * @param mjdTimestamp time of last CarmaMonitorSystem IPQ write 
     *                     specified as an MJD
     */
    void setFinalWriteTime( double mjdTimestamp );
    //void setFinalWriteTime( );

    /**
     * Gets the time of the last write to the associated
     * CarmaMonitorSystem. The time is specified as an MJD.
     *
     * @return double time of last CarmaMonitorSystem IPQ write 
     *        specified as an MJD
     */
    double getFinalWriteTime () const;

    /**
     * Clears all times - collator time, raw read time and final write time.
     * Resets all times to 0.0D.
     */
    void        clearAllTimes();

    /**
     * Returns status flags for system frame. Used internally to determine
     * if frame was written to IPQ.
     *
     * @return set of 8 bit flags representing various status 
     *         information about this frame.
     */
    unsigned char getStatusFlags ()  const ;

    /**
     * Sets status flags for system frame. Used internally to determine
     * if frame was written to IPQ.
     *
     * @return set of 8 bit flags representing various status 
     *         information about this frame.
     */
    void        setStatusFlags (unsigned char flags)  ;

    /**
     * Returns true if subsystem frame corresponding to 'subsystemID'
     * was written to its IPQ even once. In other words, this is false 
     * iff the frameScriberPublisher for this subsystem was not started 
     * to date.
     *
     * @param subsystemID identifier of subsystem of interest
     * @return bool false iff FSP for subsystem has not been started
     */
    bool subsystemFrameIsWritten( unsigned short subsystemID ) const;

    /** 
     * @brief method to allocate space within the system frame 
     *        for a subsystem frame, its monitor points and its samples.
     *
     * This method returns the index for the allocated subsystem frame.
     * This index may be used with other methods of the SystemFrame class
     * to get the subsystem frame.
     *
     * @param subsystemID identifier for subsystem to be allocated
     * @param ssMaxMonitorPoints long maximum number of monitor points
     *        this subsystem must accommodate
     * @param ssMaxSamples long maximum number of monitor samples
     *        this subsystem must accommodate
     * @return index to allocated subsystem frame.
     * @exception throws an ErrorException if the frame does not have
     *            enough space to accomodate the subsystem frame.
     */
    int allocateSubsystemFrame( unsigned short subsystemID, 
                                long           ssMaxMonitorPoints,
                                long           ssMaxSamples );

    /**
     * Sets numCleanSubsystemnFrames_ to getNumSubsystemFrames().
     */
    void        resetNumCleanFrames() ;

    static size_t getSystemHeaderSizeInBytes( );
    
    size_t getSystemTotalSizeInBytes( ) const;

  protected:
    static const int SUBSYSTEM_FRAME_ABSENT;

    struct SystemHeader;

    /**
     * Constructs a SystemFrame instance
     */
    SystemFrame( long maxSubsystems, 
                 long maxMonitorPoints, 
                 long maxSamples );

    SystemHeader & getSystemFrameDataHoldingWriteLock( );
    const SystemHeader & getSystemFrameDataHoldingLock( ) const;

    ::std::auto_ptr< SubsystemFrame >
    makeSubsystemFrameForIndexHoldingLock( int index ) const;

    int
    allocateSubsystemHoldingWriteLock( unsigned short subsystemID, 
                                       long           ssMaxMonitorPoints,
                                       long           ssMaxSamples );

    bool checkIsCurrentFrameHoldingLock( double * mjdDelta ) const;

    /**
     * Given the subsystemID, returns the index of the subsystem in
     * the subsystemFrameOffset array. Uses a binary search on the 
     * sorted subsystemIndex array with the subsystemID as the key.
     *
     * @param subsystemID identifier of subsystem of interest
     * @return int index to entry for subsystem in subsystemFrameOffset
     *         array in this SystemFrame - is SUBSYSTEM_FRAME_ABSENT if
     *         subsystemID is not found.
     */
    int getSubsystemIndexHoldingLock( unsigned short subsystemID ) const;

    /**
     * Sets the pointers to the subsystem index array (subsystemIndex) 
     * and the subsystem offset array (subsystemFrameOffset). These pointers 
     * have to be set so there's enough space for each of these arrays. 
     * This method is also called after a read as the pointers will be 
     * pointing to addresses in the address space of the process that last 
     * wrote to the IPQ.
     */
    void syncSystemAndSubsystemsToNewFrameDataHoldingWriteLock( );

  private:
    // No copying
    SystemFrame( const SystemFrame & rhs );
    SystemFrame & operator=( const SystemFrame & rhs );

    class SsDataPointersManager;

    /**
     * Returns SystemHeader object given the index to the 
     * subsystem in the subsystemFrameOffset array.
     *
     * @param index int index in subsystemFrameOffset array in 
     *        SystemHeader managed by this SystemFrame.
     * @return SubsystemHeader& reference to SubsystemHeader 
     *         structure corresponding to subsystem at position 'index' in
     *         subsystemFrameOffset array.
     */
    SubsystemHeader &
    getSubsystemFrameStructHoldingLock( int index ) const;

    /**
     * Inserts an entry in the subsystemIndex array. The subsystemIndex 
     * array is an array of subsystem indices, sorted in ascending 
     * order of subsystemID's. The insertion of indices in this array 
     * is performed using a simple insertion sort. 
     *
     * @param subsystemID identifier for monitor point whose index has to
     *        be inserted
     * @param newIndex int index of subsystem in subsystemFrameOffset array.
     * @return bool true if inserted, false if already present.
     */
    void indexInsertKeyHoldingWriteLock( unsigned short subsystemID,
                                         int            newIndex );

    /**
     * Finds the offset of the beginning of free space in the SystemFrame
     * 'frame'. 
     *
     * @param frame SystemFrame in which free space is sought.
     * @return size_t offset from beginning of SystemHeader where free
     *         space is available. If offset is equal to total size of 
     *         SystemHeader, then available space is zero.
     */
    size_t getNextFreeSubsystemFrameHoldingLock( );

    static size_t getSystemFrameStorageSizeInBytes( long maxSubsystems,
                                                    long maxMonitorPoints,
                                                    long maxSamples );

  protected:
    const long   maxSubsystems_;
    const long   maxMonitorPoints_;
    const long   maxSamples_;
    const size_t frameStorageSizeInBytes_;

    mutable util::PthreadRWLock guard_;

    const int *    frameDataSsIndexArray_;
    const size_t * frameDataSsDataOffsetArray_;

  private:
    void * frameStorage_;

    int *    writableFrameDataSsIndexArray_;
    size_t * writableFrameDataSsDataOffsetArray_;

    SsDataPointersManager * ssDataPointersManager_;

    int    numCleanSubsystemFrames_;
    size_t nextFreeSubsystemFrame_;
};


}  // namespace carma::monitor
}  // namespace carma


#endif
