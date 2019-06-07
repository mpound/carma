/**
 * @file
 *
 * MonitorPointSet class which manages sets of monitor points
 * for a monitor subsystem.
 *
 * @author: N. S. Amarnath
 *
 * $CarmaCopyright$
 *
 */

#ifndef CARMA_MONITOR_MONITOR_POINT_SET_h
#define CARMA_MONITOR_MONITOR_POINT_SET_h

#include "carma/corba/corba.h"
#include "carma/monitor/monitorframe.h"
#include "carma/monitor/types.h"
#include "carma/util/PthreadMutex.h"

#include <boost/thread/thread.hpp>
#include <map>

namespace carma  {
  namespace monitor  {

class SubsystemFrame;
class SubsystemFrameBuffer;
class Runnable;


/**
 * A class that manages the sets of monitor points for a monitor
 * subsystem.
 */

class MonitorPointSet {
  public:
    /**
     * Causes a read from the IPQ associated with the internally stored 
     * subsystem frame. Takes effect only in within a subsystem - in the
     * ACC, the only IPQ available is the IPQ for the entire MonitorSystem.
     */
    virtual unsigned int read()  ;

    /**
     * Causes a read of the latest frame from the IPQ associated wit 
     * the internally stored subsystem frame. Takes effect only in within 
     * a subsystem - in the ACC, the only IPQ available is the IPQ for 
     * the entire MonitorSystem.
     */
    virtual bool        readNewest()  ;  
                                      
    /**
     * If new data is available it is copied from the queue into
     * the monitor system. If no unread data is available then the
     * method returns. In either case there is no blocking.
     * @return false if no data was copied
     */
    virtual bool readNewestConditionalCopy();

    /**
     * Returns subsystemID of subsystem managed by this MonitorPointSet.
     *
     * @return ushort subsystemID of subsystem managed by this MonitorPointSet.
     */
    virtual ushort        getSubsystemID() const;

    /**
     * Marks the monitor point specified by index in monitorHeader array
     * as modified.
     *
     * @param index int index of modified monitor point in monitorHeader 
     *        array in SubsystemFrame managed by this MonitorPointSet.
     */
    void                markMpAtIndexModified (int index);

    /**
     * Marks the monitor point specified by tagID as modified.
     *
     * @param tagID Identifier of modified monitor point 
     *        in SubsystemFrame managed by this MonitorPointSet.
     */
    void                markMpWithTagIdModified (const tagIDType tagID);

    /**
     * Method to modify number of samples associated with the monitor
     * point whose header is at position 'index' in the monitorHeader
     * array in the SubsystemFrame managed by this MonitorPointSet.
     *
     * @param index int index of modified monitor point in monitorHeader 
     *        array in SubsystemFrame managed by this MonitorPointSet.
     * @param samples const ushort number of samples to be associated with
     *        the monitor point specified by 'index'.
     */
    virtual void        setNumSamplesPerCycle (int index, const ushort samples);

    /**
     * Returns reference to SubsystemFrame managed by this MonitorPointSet.
     *
     * @return SubsystemFrame& reference to managed SubsystemFrame.
     */
    SubsystemFrame & getSubsystemFrame () const;

    /**
     * Calls SubsystemFrame::consolidateSamples() on the SubsystemFrame
     * managed by this MonitorPointSet.
     */
    void                   consolidateStorage();


    /*! 
     * @brief method to return a MonitorPointSet. 
     *
     * This factory method returns a reference to a MonitorPointSet
     * object.  When subsystemID > 0, it returns the MonitorPointSet 
     * object for the specified subsystem. It is expected that the case 
     * when subsystemID > 0 will occur only where a SystemFrame object 
     * is available, or when a MonitorPointSet object is being initialized 
     * within a subsystem.
     *
     * @return reference to MonitorPointSet.
     * @exception throws an ErrorException if subsystemID = 0 
     */
    static MonitorPointSet&        getMonitorPointSet(long subsystemID,
                                                 long maxMonitorPoints,
                                                 long maxSamples); 


    /*! 
     * @brief method to start the auto-write thread.
     *
     * This method starts the thread for automatic writing.
     * The thread does an initial write immediately, and then writes
     * all modified monitor points to the monitor stream with a 
     * period of a half-second. The modified points are written
     * at (half-second+delay) seconds. The halfsecond corresponds
     * to the half-second tick in UTC (corrected).
     *
     * @return none
     */
    void        startAutoWriter ();

    /*! 
     * @brief method to configure and start the auto-write thread.
     *
     * This method configures and starts the thread for automatic writing.
     * The parameter delay must be greater than zero.
     *
     * @parameter double delay in seconds. delay must be less than or equal to
     *            MAX_AUTO_WRITE_DELAY and greater than zero as the auto-writer 
     *            operates on a period of a half-second. So delay must 
     *            always be less than MAX_AUTO_WRITE_DELAY.
     *
     * @return none
     * @exception throws an ErrorException if delay > MAX_AUTO_WRITE_DELAY
     * @see MonitorPointSet::startAutoWriter
     */
    void        startAutoWriter (double delay);

    /*! 
     * @brief method to stop the auto-write thread.
     *
     * This method stops the thread for automatic writing.
     * The thread makes a clean exit, completing any pending write.
     *
     * @return none
     */
    void        stopAutoWriter ();

    /*! 
     * @brief method to test whether the auto-writer thread is alive.
     *
     * @return bool - true if auto-writer thread is alive, false
     *                otherwise.
     */
    bool        autoWriterIsAlive ();

    /*! 
     * @brief method to return the time at which the last write started.
     *
     * Bear in mind that this time may not be consistent with the
     * startScriberWriteTime and the EndWriteTime on those occasions
     * when a write intervenes between calls to get these values.
     *
     * @return double - returns the time as measured by Time class.
     *                Time when MonitorPointSet::write was last invoked.
     */
    double        getStartWriteTime () const  {  return this->startWriteTime_;  }

    /*! 
     * @brief method to return the time at which the last write 
     *        invoked the remote procedure on the ScriberPublisher.
     *
     * @return double - returns the time as measured by Time class.
     *              Time when MonitorPointUpdateServant::monitorPointSeqUpdate 
     *              was last invoked.
     */
    double        getStartScriberWriteTime () const  
        {  return this->inScriberTime_;  }

    /*! 
     * @brief method to return the time at which the last write ended.
     *
     * @return double - returns the time as measured by Time class.
     *                Time when last MonitorPointSet::write ended.
     */
    double        getEndWriteTime ()  const {  return this->endWriteTime_;  }

    /*! 
     * Install a method to be executed before the write.
     * 
     * Create a class that inherits from Runnable and define the
     * execute() method. This method will be called before the write is done.
     * Useful for marking sense points as INVALID_NO_HARDWARE if the data
     * has not shown up by the time it is ready to write.
     *
     * Take caution to only install the prewrite method on MPset instances that
     * have any necessary monitor points populated within that MPset. 
     * For example, using the same prewrite method for MPsets instances 
     * running in different processes is usually not what you want. 
     *
     */
    void installPrewriteMethod( const Runnable & prewriteMethod );

    /*! 
     * Remove a previously installed prewrite method.
     */
    void removePrewriteMethod( const Runnable & prewriteMethod );

   /**
    * Destructor.
    */
    virtual ~MonitorPointSet();
    
    SubsystemFrameBuffer & getBuffer( ) const;

    /**
     * Sends local monitor point values to FrameScriberPublisher 
     * Only sample values of modified monitor points are sent 
     * to the frame scriber. 
     * If 'autoWrite' is true, then the sending method is
     * called by the auto writer thread.
     *
     * @param autoWrite bool if true, then auto writer is calling this method.
     * @param autoWriteDelay The autowrite delay if autoWrite true, else ignore.
     */
    virtual void write( bool autoWrite, double autoWriteDelay = 0.0 );
    
    /** 
     * Get highwater marks for a particular MonitorValueType.
     * Highwater marks represent the maximum number of monitor samples
     * seen in a frame for a particular type for all frames thus 
     * far processed.  This is useful for preallocating transport 
     * sequence sizes to avoid excessive reallocation which can be 
     * a killer on some large subsystems.
     */
    unsigned int getHighWaterMark( MonitorValueType mvt );

  private:
    friend class SystemFrameBuffer;

    MonitorPointSet( const MonitorPointSet & rhs );
    MonitorPointSet & operator=( const MonitorPointSet & rhs );

    explicit MonitorPointSet( ::std::auto_ptr< SubsystemFrame > & frame );

    MonitorPointSet( long subsystemID,
                     long maxMonitorPoints,
                     long maxSamples );


    static MonitorPointUpdate_var  getScriberHandle (const std::string& name);

    static double
    writeToScriber( const SubsystemFrame &   ssFrame,
                    MonitorPointUpdate_var & monUpdater,
                    double                   delay,
                    bool &                   writeSucceeded,
                    const MonitorSampleValues & samples );

    void 
    fillMonSampleValuesHoldingMpModifiedLock( 
                    MonitorSampleValues & samples,
                    const SubsystemFrame & ssFrame,
                    const char * const mpModifiedClone );

    void remergeFailedWriteInfoHoldingWritingLock( );

    const Runnable * prewriteMethod_;

    SubsystemFrameBuffer *            frameBuffer_;
    ::std::auto_ptr< SubsystemFrame > frame_;

    const int                    mpModifiedAllocCount_;
    
    // various memebers which indicates which monitor points have been modified
    // since the last update
    util::PthreadMutex     mpModifiedGuard_;
    bool                   mpModifiedAny_;
    char *                 mpModifiedState_;
    int                    mpModifiedSamplesCount_;

    util::PthreadMutex     writingGuard_;
    char *                 writingMpModifiedState_;

    const ::std::string name_;

    /**
     * @brief name of frame scriber publisher scriber DO.
     */
    const ::std::string scriberName_;

    /**
     * @brief reference to frame scriber publisher scriber DO.
     */
    MonitorPointUpdate_var monUpdater_;

    static void autoWriterThread( MonitorPointSet & owner, 
                                  const double delayInS ); 

    boost::thread autoWriterThread_;

    // time at which last write began
    double  startWriteTime_;
    // time at which CORBA remote method was invoked in ScriberPublisher
    double  inScriberTime_;
    // time at which last write finished
    double  endWriteTime_;

    // Map to carry high water marks for adding monitor points
    ::std::map< MonitorValueType, unsigned int > typeHighWaterMarks_;
};



} } // namespace monitor, carma

#endif // CARMA_MONITOR_MONITOR_POINT_SET_h
