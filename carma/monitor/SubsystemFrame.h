#ifndef CARMA_MONITOR_SUBSYSTEMFRAME_H
#define CARMA_MONITOR_SUBSYSTEMFRAME_H

/**
 * @file
 *
 * Class wrapper for monitor subsystem frame structure that manages storage
 * for a monitor subsystem frame.
 *
 * @author: N. S. Amarnath
 *
 * $CarmaCopyright$
 *
 */

#include "carma/util/BaseException.h"
#include "carma/monitor/types.h"
#include "carma/monitor/MonitorPointSample.h"
#include "carma/monitor/MonitorPointHeader.h"
#include "carma/monitor/SubsystemFrameHeader.h"

#include <map>

namespace carma  {
namespace monitor  {


void SetFlag( unsigned short &       flags,
              const unsigned short & mask );
              
void UnsetFlag( unsigned short &       flags,
                const unsigned short & mask );
                
unsigned short GetFlag( const unsigned short & flags,
                        const unsigned short & mask );


/**
 * Structure used to manage memory for a subsystem frame. Contains
 * bookkeeping information and the monitor data for a monitor subsystem.
 */
struct SubsystemDataPointers;

class  MonitorPointSet;
struct TransportSubsystemFrame;


/*!
 * @brief Frame containing monitor points for a monitor subsystem
 *        frame.
 *
 * A monitor subsystem is any collection of CARMA components that 
 * communicates with the Array Control Computer through a single 
 * computer. In other words, a monitor subsystem is any collection 
 * of CARMA components that, from the viewpoint of the ACC, is
 * controlled by one computer. A computer may control/host more than 
 * one subsystem - for example, the line length subsystem, and the 
 * LO Reference both live on one computer. 
 *
 * A SubsystemFrame is a collection of status information from a 
 * monitor subsystem, in the form of monitor points. A SubsystemFrameBuffer 
 * allows changes made to monitor point values in shared memory to be 
 * reflected in the local copy in this class instance.
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
 * @see ::carma::monitor::SubsystemFrameBuffer
 * @see ::carma::monitor::MonitorSubsystem
 * @see carma/monitor/monitorframe.idl
 * @see http://www.mmarray.org/project/WP/Monitoring/monitoringDesign.pdf
 */
class SubsystemFrame {

 public:
    static const int MONITOR_POINT_ABSENT = -1;
    
    /**
     * Returns reference to internal structure. Used by derived classes
     * and other methods. Used for efficiency in internal monitor system
     * methods only. Not meant for external use.
     *
     * @return SubsystemHeader& reference to structure that stores
     *         actual monitor points.
     */
    const SubsystemHeader & getSubsystemHeader() const;

    const SubsystemDataPointers & getSubsystemDataPointers() const;

    /**
     * Gets the frame's timestamp. Timestamp is measured in number of 
     * half-seconds elapsed since the CARMA Epoch - 00:00:00.00 January 1, 2000.
     *
     * @return long timestamp for this frame.
     * @see    ::carma::util::Time
     */
    long        getFrameCount ()  const ;

    /**
     * Sets the frame's timestamp. Timestamp is measured in number of 
     * half-seconds elapsed since the CARMA Epoch.
     *
     * @return long timestamp for this frame.
     * @see    ::carma::util::Time
     */
    long        setFrameCount (const long frameCount)  ;

   /**
    * Method returns true if frame's timestamp is current; false otherwise
    *
    * @return bool true if frame's timestamp (frameCount) is current
    */
    bool        isCurrentFrame () const ;

    static bool mjdIsCurrentFrame( double mjd, double * mjdDelta );

    /**
     * Returns status flags for subsystem frame. Used internally to determine
     * if frame was written to IPQ, if frame has "holes" in it, if frame was
     * published, and if frame was received.
     *
     * @return uchar set of 8 bit flags representing various status 
     *         information about this frame.
     */
    uchar	getStatusFlags()  const ;

    /**
     * Sets status flags for subsystem frame. Used internally to determine
     * if frame was written to IPQ, if frame has "holes" in it, if frame was
     * published, and if frame was received.
     *
     * @param status uchar set of 8 bit flags representing various status 
     *         information about this frame.
     */
    void	setStatusFlags (uchar status)  ;

    /**
     * Returns maximum number of monitor points this subsystem frame 
     * can accommodate.
     *
     * @return long maximum number of monitor points this subsystem frame
     *         can accomodate.
     */
    long        getMaxNumMonitorPoints ()  const ;

    /**
     * Returns maximum number of monitor samples this subsystem frame 
     * can accommodate.
     *
     * @return long maximum number of monitor samples this subsystem frame
     *         can accomodate.
     */
    long        getMaxNumSamples ()  const ;

    /**
     * Returns MonitorPointHeader object corresponding to the monitor point
     * with the specified tagID.
     * 
     * @todo    add IllegalMonitorPoint exception
     * @return MonitorPointHeader maximum number of monitor samples this 
     *         subsystem frame can accomodate.
     */
    MonitorPointHeader        getHeaderByTagID (const tagIDType tagID) const;

    /**
     * Returns maximum number of monitor samples this subsystem frame 
     * can accommodate.
     *
     * @return long maximum number of monitor samples this subsystem frame
     *         can accomodate.
     */
    MonitorPointHeader        getHeaderByIndex (int index) const ;

    /**
     * Returns index of monitor point header corresponding to the 
     * specified tagID, in monitor header array in this subsystem frame.
     *
     * @todo    add IllegalMonitorPoint exception
     * @return int index of moinitor point header with specified tagID
     */
    int           getIndex (const tagIDType tagID) const;

    /**
     * Returns tagID of the monitor point with header at index \"index\"
     * in the monitorHeaders array in this subsystem frame.
     *
     * @return tagID associated with monitor point header at index 
     */
    tagIDType     getTagID (const int index) const ;

    /**
     * True if tagID is that of a valid monitor point within this 
     * subsystem frame.
     *
     * @return bool true if monitor point with specified tagID is 
     *         found in this subsystem frame.
     */
    bool        isValid (const tagIDType tagID) const ;

    /**
     * Returns subsystemID of this subsystem frame.
     *
     * @return ushort subsystemID for this subsystem frame.
     */
    ushort getSubsystemID( ) const;

    /**
     * Sets subsystemID of this subsystem frame.
     *
     * @param subsystemID ushort subsystemID for this subsystem frame.
     */
    void        setSubsystemID (ushort subsystemID)  ;

    /**
     * Returns the number of samples to allocate for a sampling
     * rate specified as number of samples per cycle (a cycle is 
     * every half-second). Class static method.
     *
     * @param  nSamplesPerCycle ushort sampling frequency, specified
     *         as number of samples every half-second.
     * @return # of samples to allocate - includes average if 
     *         nSamplesPerCycle > 1.
     */
    static ushort numSamplesToAllocate (const ushort nSamplesPerCycle) ;

    /**
     * Returns number of samples to allocate for the entire subsystem given
     * the maximum number of monitor points and maximum number of monitor
     * samples the subsystem frame has to accommodate.
     *
     * @param maxMonitorPoints const long maximum number of monitor points
     *        in the subsystem frame
     * @param maxSamples const long maximum number of monitor point samples
     *        in the subsystem frame
     * @return long number of samples to allocate - includes space for 
     *         averages and string expansion.
     */
    static long   maxNumSamplesIncludingAverages (const long maxMonitorPoints, 
                                                      const long maxSamples) ;

    /**
     * This method is used to compute the size of a subsystem frame when the 
     * the maximum number of monitor points and monitor samples it must 
     * accommodate are specified. Returns the size as a size_t in bytes.
     *
     * @param maxMonitorPoints long maximum number of monitor points
     *        in the subsystem frame
     * @param maxSamples long maximum number of monitor point samples
     *        in the subsystem frame
     * @return size_t size of subsystem frame to accommodate maxMonitorPoints 
     *         and maxsamples, in bytes.
     */
    static size_t       sizeFrame (long maxMonitorPoints, long maxSamples);

    /**
     * Sets size parameters in the subsystem frame. Size of the frame is 
     * determined by the maximum number of monitor points and maximum number 
     * of samples the subsystem frame must accommodate.
     *
     * @param frame SubsystemHeader& frame whose size parameters must be 
     *        set
     * @param maxMonitorPoints long maximum number of monitor points
     *        in the subsystem frame
     * @param maxSamples long maximum number of monitor point samples
     *        in the subsystem frame
     */
    static void
    setFrameSize( SubsystemHeader &       frame,
                  SubsystemDataPointers & dataPointers,
                  long                    maxMonitorPoints,
                  long                    maxSamples );

    /**
     * Sets the pointers to the monitor point index array (monitorPointIndex), 
     * monitor point header array (monitorHeaders), and the monitor sample 
     * array (monitorValues). These pointers have to be set so there's enough 
     * space for each of these arrays. This method is also called after a read
     * as the pointers will be pointing to addresses in the address space of 
     * the process that last wrote to the IPQ.
     */
    void syncSubsystemToNewFrameData( );

    static void syncSubsystemToNewFrameData(
        SubsystemHeader &       frame,
        SubsystemDataPointers & dataPointers );

    /**
     * Static method used to calculate frame count as
     * number of half-seconds since Januray 1, 2000.
     *
     * @see carma::util::Time.h
     */
    static long         computeCurrentFrameTime () ;

    /**
     * Returns allocated number of monitor points - in other words, the
     * actual number of monitor points present in this subsystem frame.
     *
     * @return long actual number of monitor points in this subsystem frame.
     */
    long        getNumMonitorPoints()  const ;

    /**
     * Returns allocated number of monitor points of type == 'type' - 
     * in other words, the actual number of monitor points of type 'type' 
     * present in this subsystem frame.
     *
     * @return long actual number of monitor points of specified type in 
     *         this subsystem frame.
     */
    long        getNumMonitorPoints (MonitorValueType type) const;

    /**
     * Returns allocated number of monitor samples of type == 'type' - 
     * in other words, the actual number of monitor samples of type 'type' 
     * present in this subsystem frame. Includes averages.
     *
     * @return long actual number of monitor samples of specified type in 
     *         this subsystem frame.
     */
    long        getNumMonitorSamples (MonitorValueType type) const;

    /**
     * Returns total allocated number of monitor samples in this subsystem 
     * frame. Includes averages.
     *
     * @return long actual, total  number of monitor samples in
     *         this subsystem frame.
     */
    long        getNumMonitorSamples () const ;

    /**
     * Get highwater number of monitor samples. 
     * Returns the maximum number of monitor samples we've seen
     * for a given sample for this subsystem.
     */
    unsigned int getHighwaterNumMonitorSamples ( MonitorValueType type );

    /**
     * Returns total allocated number of monitor points with one sample 
     * per cycle, a sampling rate of 2 Hz. Gets modified by each monitor 
     * point allocation.
     *
     * @return long actual number of monitor points with a single sample in
     *         this subsystem frame.
     */
    long        getNumSingleSamplePoints ()  const ;

    /**
     * Counts number of single sample monitor points in this subsystem frame.
     * Used to check allocation - if everything is right, then
     * (countNumSingleSamplePoints() == getNumSingleSamplePoints()).
     *
     * @return long counted number of monitor points with a single sample in
     *         this subsystem frame.
     */
    long        countNumSingleSamplePoints () const ;

    /**
     * Sets total allocated number of monitor points with one sample 
     * per cycle, a sampling rate of 2 Hz. Gets modified by each monitor 
     * point allocation.
     *
     * @param long actual number of monitor points with a single sample in
     *         this subsystem frame.
     */
    void        setNumSingleSamplePoints (const long numSingleSamples) ;

    /**
     * Returns number of samples per cycle (0.5*sampling rate) associated
     * with the monitor point with tagID == 'tagID'.
     *
     * @param tagID identifies monitor point of interest
     * @return ushort number of samples per cycle (a cycle is 500 ms)
     */
    ushort        getNumSamplesPerCycle (const tagIDType tagID) const ;

    /**
     * Returns number of samples per cycle (0.5*sampling rate) associated
     * with the monitor point at index == 'index' in the 
     * monitorHeaders array in this subsystem frame.
     *
     * @param index Index of monitor point of interest
     * @return ushort number of samples per cycle (a cycle is 500 ms)
     */
    ushort        getNumSamplesPerCycle (const int index) const ;

    /**
     * Sets number of samples per cycle (0.5*sampling rate) associated
     * with the monitor point at index == 'index' in the 
     * monitorHeaders array in this subsystem frame. Affects 
     * return values of getNumAllocatedSamples()
     * and getNumMonitorSamples().
     *
     * @param index int index of monitor point of interest
     * @param samples ushort number of samples per cycle (a cycle is 500 ms)
     */
    void        setNumSamplesPerCycle (int index, const ushort samples);

    /**
     * Returns requested monitor sample as a MonitorPointSample object.
     * Monitor point sample is specified using tagID (monitor point identifier)
     * and sample number (iSample) where iSample ranges from 0 to
     * getNumSamplesPerCycle (tagID) when getNumSamplesPerCycle (tagID) > 1,
     * 0 otherwise.
     *
     * @param tagID identifies monitor point of interest
     * @param iSample sample index in array of samples associated with 
     *        monitor point.
     * @return MonitorPointSample object representing the requested sample value
     */
    MonitorPointSample getSampleValue (const tagIDType tagID, 
	                               ushort iSample) const ;

    /**
     * Returns requested monitor sample as a MonitorPointSample object.
     * Monitor point sample is specified using index (monitor point index)
     * and sample number (iSample) where iSample ranges from 0 to
     * getNumSamplesPercycle (index) when getNumSamplesPerCycle (tagID) > 1,
     * 0 otherwise.
     *
     * @param index identifies monitor point of interest
     * @param iSample sample index in array of samples associated with 
     *        monitor point.
     * @return MonitorPointSample object representing the requested sample value
     */
    MonitorPointSample getSampleValue (const int index, ushort iSample) const ;

    /**
     * True if sample is valid - this means that the tagID of the monitor
     * point is valid in this subsystem frame, and the sample index is within
     * the range required by the monitor point's sampling rate.
     *
     * @param tagID Identifies the monitor point associated
     *        with the sample 'value'
     * @param value const MonitorPointSample& sample being tested for validity
     * @return bool true if sample is valid
     */
    bool validSampleValue (const tagIDType tagID, 
	                   const MonitorPointSample& value) const ;

    /**
     * Returns average of all samples in current cycle as a MonitorPointSample
     * object. This is equivalent to getSampleValue (tagID, 0).
     *
     * @param tagID Identifies the monitor point of interest
     * @return MonitorPointSample average of all samples in current cycle 
     *         for monitor point with tagID == 'tagID'.
     */
    const MonitorPointSample  getSampleAverage (const tagIDType tagID) const ;

    /**
     * Returns average of all samples in current cycle as a MonitorPointSample
     * object. This is equivalent to getSampleValue (index, 0).
     *
     * @param index const int index of monitor point of interest
     * @return MonitorPointSample average of all samples in current cycle 
     *         for monitor point at index == 'index'.
     */
    const MonitorPointSample  getSampleAverage (const int index) const ;

    /**
     * Returns type of sample value associated with the monitor point
     * specified by tagID. 
     *
     * @param tagID Identifies the monitor point of interest
     * @return MonitorValueType value type of samples from monitor point
     * @see ::carma::monitor::MonitorValueType
     */
    MonitorValueType        getValueType (const tagIDType tagID) const ;

    /**
     * Returns type of sample value associated with the monitor point
     * specified by index. 
     *
     * @param index const int identifies the monitor point of interest
     * @return MonitorValueType value type of samples from monitor point
     * @see ::carma::monitor::MonitorValueType
     */
    MonitorValueType        getValueType (const int index) const ;

    /**
     * Sets the delay used by the last client to write to the associated
     * frame scriber publisher process. There may be more than one client,
     * but only the last one gets to survive. the delay is specified 
     * as a fraction of a second after the half-second tick - it is
     * in the range [0.0, 500.0)
     *
     * @param time double delay specified as fractions of a second
     */
    void        setLastWriterDelay (double time);

    /**
     * Returns the delay used by the last client to write to the associated
     * frame scriber publisher process. There may be more than one client,
     * but only the last one gets to survive. the delay is specified 
     * as a fraction of a second after the half-second tick - it is
     * in the range [0.0, 500.0)
     *
     * @return double delay specified as fractions of a second
     */
    double      getLastWriterDelay () const ;

    /**
     * Sets the time of the last client write to the associated
     * frame scriber publisher process. The time is specified as
     * as an MJD.
     *
     * @param mjdTimestamp time of last client write specified as an MJD
     */
    void setLastWriteTime( double mjdTimestamp );
    void setLastWriteTime( );

    /**
     * Gets the time of the last client write to the associated
     * frame scriber publisher process. The time is specified as
     * as an MJD.
     *
     * @return double time of last client write specified as an MJD
     */
    double      getLastWriteTime () const ;

    /**
     * Sets the delay used by the scriber part of the frameScriberPublisher 
     * to write to the associated IPQ in the subsystem. 
     * The delay is specified as a fraction of a second after the 
     * half-second tick - it is in the range [0.0, 500.0)
     *
     * @param writeDelay double delay specified as fractions of a second
     */
    void        setScriberWriteDelay (double writeDelay);

    /**
     * Returns the delay used by the scriber part of the frameScriberPublisher 
     * to write to the associated IPQ in the subsystem. 
     * The delay is specified as a fraction of a second after the 
     * half-second tick - it is in the range [0.0, 500.0)
     *
     * @return double delay specified as fractions of a second
     */
    double      getScriberWriteDelay () const ;


    /**
     * Sets the time of the last scriber write to the associated
     * subsystem IPQ. The time is specified as an MJD.
     *
     * @param mjdTimestamp time of last subsystem IPQ write specified as an MJD
     */
    void setScriberWriteTime( double mjdTimestamp );
    void setScriberWriteTime( );

    /**
     * Gets the time of the last scriber write to the associated
     * subsystem IPQ. The time is specified as as an MJD.
     *
     * @return double time of last subsystem IPQ write specified as an MJD
     */
    double      getScriberWriteTime () const ;

    /**
     * Sets the time when this subsystem frame was published to the ACC.
     * The time is specified as as an MJD.
     *
     * @param mjdTimestamp time of publication of this frame specified as an MJD
     */
    void setPublishTime( double mjdTimestamp );
    void setPublishTime( );

    /**
     * Returns the time when this subsystem frame was published to the ACC.
     * The time is specified as as an MJD.
     *
     * @return double time of publication of this frame, specified as an MJD
     */
    double      getPublishTime () const ;

    /**
     * Sets the time when this subsystem frame was received at the ACC.
     * The time is specified as as an MJD.
     *
     * @param mjdTimestamp time of reception of this frame specified as an MJD
     */
    void setReceiveTime( double mjdTimestamp );
    void setReceiveTime( );

    /**
     * Returns the time when this subsystem frame was received at the ACC.
     * The time is specified as as an MJD.
     *
     * @return double time of reception of this frame, specified as an MJD
     */
    double      getReceiveTime() const ;

    /**
     * Resets lastWriteTime, scriberWriteTime, publishTime and receiveTime to
     * 0.
     */
    void        clearAllTimes();

    /**
     * True if this subsystem frame has been published.
     *
     * @return bool true if this subsystem frame has been published.
     */
    bool        isPublished () const ;

    /**
     * Sets status flag to indicate that this frame is published.
     */
    void        setPublished () ;

    /**
     * Clears status flag to indicate that this frame is not published.
     */
    void        clearPublished () ;

    /**
     * True if this subsystem frame has been received by the frame collator
     * (in the ACC).
     *
     * @return bool true if this subsystem frame has been published 
     *                   and received.
     */
    bool        received () const ;

    /**
     * Sets status flag to indicate that this frame was received.
     */
    void        setReceived () ;

    /**
     * Clears status flag to indicate that this frame has not been received.
     */
    void        clearReceived () ;

    /**
     * Convenience method that sets the publish time and sets the status flag
     * to indicate that the frame was published.
     */
    void        markFramePublished();

    /**
     * Convenience method that sets the recive time and sets the status flag
     * to indicate that the frame was received.
     */
    void        markFrameReceived();

    struct AllocMpInfo {
        tagIDType        tagID;
        MonitorValueType valueType;
        ushort           nSamplesPerCycle;
    };

    // Allocates space for new monitor point in the subsystem frame.
    // Returns the offset into the value array for the next new monitor point
    // throw ErrorException when no more space available for monitor points
    /** 
     * @brief method to allocate space within the subsystem frame 
     *        for a monitor point and its samples.
     *
     * @exception throws an ErrorException if the frame does not have
     *            enough space to accomodate either the monitor point header
     *            or the associated samples.
     */
    void allocateMonitorPoints( const AllocMpInfo * infos,
                                size_t              infosCount,
                                bool markMpsAsModified = true );

    /**
     * Constructor that constructs around a SubsystemHeader structure.
     * Used primarily in creating MonitorSubsystem objects from MonitorSystem.
     *
     * @param frame SubsystemHeader& reference to a monitor 
     *        subsystem frame.
     */
    SubsystemFrame( SubsystemHeader &       frame,
                    SubsystemDataPointers & dataPointers,
                    bool                    embeddedInAFullSystem );

    /**
     * Destructor. Cleans up allocated memory if this->myFrame_ is true.
     * Its virtual so that any classes that inherit from this will also
     * get their destructors called. Very important!
     */
    virtual ~SubsystemFrame( );

    /**
     * Consolidates storage in the array of monitor samples. "Holes"
     * may occur due to a change in the number of samples asscociated with
     * a monitor point. This method compacts samples, and makes the set
     * allocated samples occupy contiguous space in the sample array, starting
     * from index 0.
     */
    void consolidateSamples();

    /**
     * Returns a TransportSubsystemFrame filled with monitor sample 
     * values from this subsystem frame, ready for transport to the ACC 
     * for incorporation into the monitor system object in the ACC. 
     * All subsystem frame header information is transported, with a 
     * sequence of sample values, where each value is associated with a 
     * sample index and a pointID (part of the monitor point tagID that 
     * is unique within a subsystem). The pointID serves to identify the 
     * associated monitor point as the subsystemID is sent as part of 
     * the header. All sample values from this subsystem frame are 
     * transported, including averages.
     *
     * @param transFrame TransportSubsystemFrame& reference to empty subsystem
     *                   transport frame
     * @see carma/monitor/monitorframe.idl
     * @see ::carma::monitor::TransportSubsystemFrame
     * @see ::carma::monitor::SubsystemHeader
     */
    void writeToTransport( TransportSubsystemFrame & transFrame );

    /**
     * Takes the sample values from a transported subsystem frame
     * and fills them into this subsystem frame. These values replace
     * all values in this subsystem frame. It is assumed that all samples
     * are available in the transport subsystem frame.
     *
     * @param transportFrame const TransportSubsystemFrame& reference to 
     *        transported subsystem frame containing all sample values for 
     *        this subsystem
     * @see carma/monitor/monitorframe.idl
     * @see ::carma::monitor::TransportSubsystemFrame
     * @see ::carma::monitor::SubsystemHeader
     */
    void writeFromTransportFrame( const TransportSubsystemFrame & transFrame );

    /**
     * Takes the sample values from a transported sequence of sample
     * values and fills them into this subsystem frame. These values replace
     * corresponding values in this subsystem frame. It is assumed that only
     * modified sample values are sent in the sequence dataSeq. Sent by
     * ::carma::monitor::MonitorPointSet::write() .
     *
     * @param dataSeq const MonitorSamplevalueSeq& reference to 
     *        transported sequence containing modified sample values for 
     *        this subsystem
     * @see carma/monitor/monitorframe.idl
     * @see ::carma::monitor::MonitorSampleValueSeq
     * @see ::carma::monitor::MonitorPointSet
     */
    void writeFromPointSet( const MonitorSampleValues & samples );

    /**
     * Sets number of monitor points for this subsystem frame.
     *
     * @param numMonitorPoints unsigned short number of monitor points
     *        in this subsystem frame.
     */
    void        setNumMonitorPoints (ushort numMonitorPoints)  ;

    /**
     * Sets number of monitor samples for this subsystem frame.
     *
     * @param numMonitorSamples unsigned short number of monitor samples
     *        in this subsystem frame.
     */
    void        setNumMonitorSamples (ushort numSamples)  ;

    /**
     * Sets number of actual monitor samples for this subsystem frame.
     *
     * @param numMonitorSamples unsigned short number of monitor samples
     *        in this subsystem frame.
     */
    void        setNumActualSamples (ushort numSamples) ;

    /**
     * Returns number of monitor samples for this subsystem frame.
     *
     * @return int actual number of monitor samples in this subsystem frame.
     */
    int         getNumActualSamples () const ;

  protected:

    /**
     * Constructor that constructs a SubsystemHeader of the appropriate
     * size and then constructs and initializes the SubsystemFrame object. 
     *
     * @param subsystemID ushort ID of this subsystem frame
     * @param numMonitorPoints const ushort maxmimum number of monitor points 
     *        this subsystemframe is expected to accommodate.
     * @param numSamples const ushort maxmimum number of monitor samples 
     *        this subsystemframe is expected to accommodate.
     */
    SubsystemFrame( ushort         subsystemID,
                    unsigned short numMonitorPoints,
                    unsigned short numSamples,
                    bool           leakStorage );

    /**
     * Clears all samples in this subsystem frame. Resets the 
     * blanking/validity flags for every allocated sample.
     */
    void        clearSamples();

    /**
     * Returns the number of allocated samples. Includes "holes"
     * created by requests to setNumSamplesperCycle. Used to determine
     * place to allocate next set of samples.
     *
     * @return int number of allocated samples.
     */
    int         getNumAllocatedSamples () const ;

    // Allocates space for new monitor point values in the subsystem frame.
    /**
     * Allocates monitor samples in this subsystem frame for a monitor point
     * with header at 'headerOffset', sampleOffset = 'sampleOffset' and samples
     * per cycle == 'nSamplesPerCycle'. Since most monitor points have
     * a sampling rate of 2 Hz and the sampling cycle is 2 Hz, 
     * 'nSamplesPerCycle' has a default value of 1. Allocates the samples as
     * well.
     *
     * @param headerOffset int index in monitorHeader array where the 
     *        associated monitor header will live
     * @param sampleOffset ushort offset in the monitorValues array in this
     *        subsystem frame where the samples will be allocated.
     * @param nSamplesPercycle number of sampled values available per 
     *        sampling cycle
     * @return int sample offset (sampleOffset) in sample array 
     */
    ushort      allocateSamples (int headerOffset, ushort sampleOffset, 
                                ushort nSamplesPerCycle = 1);
    /**
     * Used when subsystem frame is generated from a MonitorPointSet object 
     * using the MonitorPointSet::getSubsystemFrame() method. Sets the 
     * monitor point set object which it comes from. Used to mark
     * monitor points as modified.
     *
     * @param set MonitorPointSet* pointer to MonitorPointSet which was
     *        this subsystem frame's progenitor.
     * @see ::carma::monitor::MonitorPointSet
     */
    void        setMonitorPointSet (MonitorPointSet* set) ;

    /**
     * Writes specified sample value 'value' into this subsystem frame
     * as sample with sample number 'iSample' for the monitor point at
     * index 'index' in the monitorHeaders array.
     *
     * @param index const int index of monitor point header in monitorHeader 
     *        array in this subsystem frame
     * @param value const MonitorSampleValue& value to be written
     * @param iSample ushort sample index in sample array associated with 
     *        monitor point.
     */
    void writeSampleValue (const int index, 
	                   const MonitorSampleValue& value, ushort iSample);

    /**
     * Returns the index to the monitor point header corresponding to
     * specified 'tagID'. Searches through the sorted index array using
     * binary search to find the index for the specified 'tagID' and returns
     * it, else returns MONITOR_POINT_ABSENT constant.
     *
     * @param tagID Identifier for monitor point
     * @return int index of monito point header
     */
    int         findMonitorPoint(const tagIDType tagID) const ;
    
    /**
     * Returns pointer to beginning of monitor sample array in this subsystem
     * frame.
     *
     * @return MonitorSampleValue* pointer to first element in monitor
     *         sample array in this subsystem frame.
     */
    MonitorSampleValue*           getSamples() const ;

    const MonitorHeader & getMonitorHeaderRefByIndex( int index ) const;
    MonitorHeader & getWritableMonitorHeaderRefByIndex( int index ) const;

    friend class MonitorPointSet;

  private:
    // No copying
    SubsystemFrame( const SubsystemFrame & rhs );
    SubsystemFrame & operator=( const SubsystemFrame & rhs );

  protected:
    SubsystemHeader &       frame_;
    SubsystemDataPointers & dataPointers_;

  private:
    const bool        shouldDeallocateStorage_;
    const bool        embeddedInAFullSystem_;
    MonitorPointSet * set_;
    ::std::map< MonitorValueType, unsigned int > highwaterMonitorSamples_;
};


/*!
 * @brief Exception class thrown when an invalid monitor point is detected.
 *
 * An exception class to inform users that an invalid monitor point has been
 * detected. A monitor point is invalid if the tagID is invalid for 
 * a given subsystem frame, index for a monitor points is invalid, 
 * value type is unrecognizable, or if the sample number is greater 
 * than the allowed number of samples for that monitor point.
 */
class IllegalMonitorPointExceptionObj 
  : public virtual carma::util::BaseException  {
    public:
        IllegalMonitorPointExceptionObj (const char* mesg, 
                                const char* fileName = __FILE__, 
                                const int lineNum = __LINE__)  ;
        IllegalMonitorPointExceptionObj (std::ostringstream& errStream, 
                                const char* fileName = __FILE__, 
                                const int lineNum = __LINE__) ;
        IllegalMonitorPointExceptionObj (const tagIDType tagID, 
		                const ushort subsystemID,
                                const char* fileName = __FILE__, 
                                const int lineNum = __LINE__) ;
};


inline void
SetFlag( unsigned short &       flags,
         const unsigned short & mask ) {
    flags |= mask;
}


inline void
UnsetFlag( unsigned short &       flags,
           const unsigned short & mask ) {
    flags &= (~mask);
}


inline unsigned short
GetFlag( const unsigned short & flags,
         const unsigned short & mask ) {
    return (flags & mask);
}


} // namespace carma::monitor
} // namespace carma


inline long
carma::monitor::SubsystemFrame::getNumMonitorPoints( ) const
{
    return frame_.numMonitorPoints;
}


inline unsigned short
carma::monitor::SubsystemFrame::getSubsystemID( ) const
{
    return frame_.subsystemID;
}


inline void
carma::monitor::SubsystemFrame::setMonitorPointSet( MonitorPointSet * const s )
{
    set_ = s;
}


inline const carma::monitor::SubsystemHeader &
carma::monitor::SubsystemFrame::getSubsystemHeader( ) const
{
    return frame_;
}


inline const carma::monitor::SubsystemDataPointers &
carma::monitor::SubsystemFrame::getSubsystemDataPointers( ) const
{
    return dataPointers_;
}


inline int
carma::monitor::SubsystemFrame::getNumAllocatedSamples( ) const
{
    return frame_.numSamples;
}


inline const carma::monitor::MonitorHeader &
carma::monitor::SubsystemFrame::getMonitorHeaderRefByIndex(
    const int index ) const
{
    return dataPointers_.monitorHeaders[ index ];
}


inline unsigned short
carma::monitor::SubsystemFrame::getNumSamplesPerCycle( const int index ) const
{
    return getMonitorHeaderRefByIndex( index ).getNumSamplesPerCycle();
}


#endif
