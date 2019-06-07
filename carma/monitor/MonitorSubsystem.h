#ifndef CARMA_MONITOR_MONITORSUBSYSTEM_H 
#define CARMA_MONITOR_MONITORSUBSYSTEM_H

/**
 * @file
 *
 * A generic monitor subsystem base class.
 *
 * @author: Steve Scott
 *
 * $CarmaCopyright$
 *
 */
 
#include <string>

#include "carma/monitor/MonitorSystemContainer.h"

namespace carma {

namespace dbms {
    class TagIDAuthority;
}

namespace monitor {


class MonitorPointSet;
class SystemFrameBuffer;


/**
 *
 * Abstract MonitorSubsystem base class.
 * The distinguishing feature of this monitor container is the 
 * associated Frame and the virtual configuration methods that must
 * be supplied by the derived class.
 * The configuration steps used internally are: <br>
 *  create hierarchy of typed monitor points - specific subsystem <br>
 *  set tagIDs and canonical names for all MPs - this class<br>
 *   (but it must lookup tagIDs using specific subsystem lookup)
 *  set other params for all MPs - specific subsystem <br>
 * The other params for the MPs may be set when the hierarchy is built
 * or driven from a table/file/database.
 *
 * Subsystem names are forced to have the first character UC, and all
 * others LC.
 *
 * After the derived MonitorSubsystem is instantiated it must be
 * configured with the configure() method.
 *
 */
class MonitorSubsystem : public MonitorSystemContainer {
public:

    /**
     * Constructor base subsystem configuration
     * The derived class object must be configure() after construction
     * The name is used to lookup the subsystem ID, so only specific
     * subsystem names are allowed.
     * @see SubsystemTagIDmap
     * @param subsystemName     monitor subsystem name
     * @param buffer pointer to storage on ACC; NULL if within subsystem
     * @param maxMonitorPoints storage to be allocated for monitor points 
     * @param maxSamples storage to be allocated for monitor sample values
     * @param archivePriority archive priority to set for all points
     * @param onlyDEFAULT if true, only set arcPriority for points that
     * have DEFAULT priority, otherwise set all of them
     */
    MonitorSubsystem( const std::string & subsystemName, 
                      SystemFrameBuffer * buffer = 0,
                      int                 maxMonitorPoints = 600,
                      int                 maxSamples = 1800,
                      ARCHIVE_PRIORITY    archivePriority = NORMAL,
                      bool                onlyDEFAULT = true );

    /**
     * Destructor
     */
    virtual ~MonitorSubsystem( );
        
    /**
     * %Configure does the complete setup of a monitor subsystem
     * The steps are:
     * <ul>
     * <li> create typed monitor points and hierarchy (@see createHierarchy)
     * <li> set identity for all monitor points (canonical name and tagID)
     *       (@see setIdentity)
     * <li> allocates space in Frame  (@see allocateAllMonitorPoints)
     * <li> sets any other monitor point attributes  (@see setMonitorPointAttributes)
     * <br> This can't be run in the constructor because calls pure virtual methods
     * </ul>
     * @return true if successful configuration
     */
    bool configure();

    /**
     * Create all typed monitor points and hierarchy
     * Every subsystem must define this
     * @see configure
     */
    virtual void createHierarchy() = 0;
    
    /**
     * Get the monitor point set
     * @return reference to the monitor point set
     */
    MonitorPointSet & getMonitorPointSet();



    /**
     * Reset validity flags of all data.
     * All monitor points that are not persistent have the 
     * validity set to INVALID_NO_DATA.
     */
    void resetValidity();

    /**
     * Average and ave validity is computed and set for each monitor point 
     * over all samples in the frame.
     */
    void updateFrameAverage();

    /**
     * Get the mjd associated with the frame 
     */
    double getFrameTime() const;
    
    /**
     * Get the mjd when the frame was published by the subsystem.
     * Only valid in the ACC.
     */
    double getPublishTime() const;

    /**
     * Get the mjd when the frame was received by the ACC.
     * Only valid in the ACC.
     */
    double getReceiveTime() const;

    /**
     * Time the last monitor point set write was started
     *
     * @return double - MJD when the monitor point set write was started
     */
    double getStartWriteTime() const ;
 
    /**
     * Time the last monitor point set write was finished
     *
     * @return double - MJD when the monitor point set write was finished
     */
    double getEndWriteTime() const ;
 
    /**
     * Time the last write was received on the ScriberPublisher.
     *
     * @return double - MJD when the monitor point set was rx'ed @ scriber
     */
    double getStartScriberWriteTime() const ;
 
    
    /**
     * The next few methods measure performance of monitor data transport.
     * They provide the following data:
     *
     * a) delay associated with the last autowriter thread to write 
     *    to the ScriberPublisher
     * b) time of the last write to the ScriberPublisher (manual or auto)
     * c) write delay associated with ScriberPublisher writing to the IPQ
     * d) actual time of writing to the IPQ
     *
     * All times and delays are doubles, measured in seconds. All delays
     * are measured from the corrected UTC half-second time tick.
     */


    /**
     * Offset from the half-second for the last auto writer thread.
     *
     * @return double - offset from the half-second, in seconds.
     */
    double getLastWriterDelay () const ;
 
    /**
     * Time (MJD) of the last write to the ScriberPublisher.
     *
     * @return double - MJD, as a double.
     */
    double getLastWriteTime () const ;
 
    /**
     * Offset from the half-second for writing this subsystem frame to
     * the (local) subsystem IPQ.
     *
     * @return double - MJD when the monitor point set is to be written 
     * to the IPQ.
     */
    double getScriberWriteDelay() const;
 
    /**
     * Time this frame of data was written to the subsystem IPQ.
     *
     * @return double - MJD when the monitor point set was written 
     *         to the IPQ.
     */
    double getScriberWriteTime() const;
 
   /*!
     * Resets all times associated with transport performance.
     * If these times are not reset, then the last set value remains 
     * even if no write/transport occurs.
     */
    void   resetTimes () ;
    
    /*! 
     * @brief method to configure and start the auto-write thread.
     *
     * This method configures and starts the thread for automatic writing.
     * This method is a combination of configureAutoWriter and
     * startAutoWriter. The parameter delay must be greater than zero.
     *
     * @parameter double delay in seconds. delay must be less than or equal to
     *            MAX_AUTO_WRITE_DELAY and greater than zero as the auto-writer 
     *            operates on a period of a half-second. 
     * @return none
     * @exception throws an ErrorException if delay > MAX_AUTO_WRITE_DELAY
     * @see MAX_AUTO_WRITE_DELAY
     * @see AUTO_WRITE_DELAY
     * @see MonitorPointSet::configureAutoWriter
     * @see MonitorPointSet::startAutoWriter
     */
    void startAutoWriter (double delay) const;

    /**
     * Stop the auto-write thread.
     *
     * This method stops the thread for automatic writing.
     * The thread makes a clean exit, completing any pending write.
     *
     * @return none
     */
    void stopAutoWriter() const ;

    /**
     * Test whether the auto-writer thread is alive (running).
     *
     * @return true if auto-writer thread is alive, false
     *                otherwise.
     */
    bool autoWriterIsAlive() const ;

    
    /** 
     * Form a name string that is a root followed by an integer,
     * e.g. Bima9.
     * This is a helper method used by subsystems to form their name.
     * @param name root name
     * @param number to append to the root
     */
    static std::string makeName(const std::string& name, int number);

    /** 
     * Return the size of the storage allocated for monitor points
     */
    int maxMonitorPoints() const ;
    
    /** 
     * Return the size of the storage allocated for monitor samples
     */
    int maxSamples() const ;
    
    // virtual, so docs have already been done...
    bool isSubsystem() const;
    
    /**
     * Get reference to the underlying storage
     *
     */  
    MonitorPointSet & monitorPointSet();

    /**
     * Dumps transport statistics table header, with legends for columns.
     *
     */
    static std::string transportHeaderToString () ;

    /**
     * Dumps transport statistics as a table, with write delays, and
     * transport/write times printed out as doubles, with fixed precision.
     *
     * @param canonical bool if true, prints canonical name otherwise uses
     *        leaf name.
     *
     * @return std::string returns transport statistics as a string
     */
    virtual std::string transportStatisticsToString (bool canonical = false) const;

    /**
     * Returns true if the monitor subsystem contains data that is current.
     * A read could make the data current, and hence make isCurrent return true.
     * If a read does not make isCurrent true, then the subsystem 
     * is probably broken.
     *
     * @return bool true if data in subsystem is current.
     */
    bool isCurrent () const;

           
    /**
     * Returns true if the monitor subsystem contains data that belongs to 
     * the timestamp <pre>frameCount</pre>.
     * 
     * @param frameCount long timestamp to match against timestamp for 
     *        data in MonitorSubsystem
     * @return bool true if data in subsystem has timestamp == frameCount.
     * @see ::carma::monitor::MonitorSubsystem::getFrameCount()
     */
    bool isCurrent (long frameCount) const;

    /*
     * Overridden methods of abstract MonitorSystemContainer methods.
     * Documentation supplied by base class.
     */
    unsigned int read();
    bool readNewest();
    bool readNewestConditionalCopy();
    void write( );
    void writeWithoutResettingValidities();
    int getFrameCount() const;

           
private:
    /**
     * Set the canonical name and tagID (from the canonical name).
     * All components in the hierarchy will have their canonical name set
     * and all monitor points will have their tagID's set as well.
     * @see configure
     */
    void setIdentity();
    
    void setMpTagIds( carma::dbms::TagIDAuthority & authority );

    /**
     * Allocate storage in the Frame for the monitor points and
     * their samples.
     * @see configure
     */
    void allocateAllMonitorPoints( );
    
    MonitorPointSet & monitorPointSet_;
    const int         maxMonitorPoints_;
    const int         maxSamples_;
    ARCHIVE_PRIORITY  archivePriority_;
    bool              onlyDEFAULT_;
};


}  // namespace carma::monitor
}  // namespace carma


#endif
