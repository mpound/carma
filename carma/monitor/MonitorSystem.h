#ifndef CARMA_MONITOR_MONITORSYSTEM_H 
#define CARMA_MONITOR_MONITORSYSTEM_H

/**
 * @file
 *
 * The monitor system base class.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorSystem.h,v 1.80 2014/06/20 15:55:43 mpound Exp $
 * $CarmaCopyright$
 *
 */
 
/** 
 * NOTE: If adding a new monitor system, don't forget to add it's name to 
 * dbms/subsystemnames.cc !
 */
#include <string>
#include <vector>
#include <iomanip>
#include <ios>

#include "carma/monitor/MonitorSubsystem.h"
// To get the default queueDepth...
#include "carma/monitor/SubsystemFrameBuffer.h"

namespace carma {
    namespace monitor {

// Forward class declarations
class AntennaCommon;
class AstroSubsystem;
class BimaSubsystem;
class C3gMax8PipelineSubsystem;
class C3gMax23PipelineSubsystem;
class CarmaSlcBandSubsystem;
class Carma3GBandSubsystem;
class CentralIfSubsystem;
class ControlSubsystem;
class DataflowSubsystem;
class DelayEngineSubsystem;
class FaultSubsystem;
class ImrSubsystem;
class AlarmSubsystem;
class LineLengthSubsystem;
class LoberotatorSubsystem;
class LoRefSubsystem;
class MasterClockSubsystem;
class OpacityMonitorSubsystem;
class OvroSubsystem;
class PhaseMonitorSubsystem;
class ProjectDatabaseManagerSubsystem;
class SignalPathSubsystem;
class SldcSubsystem;
class SlPipelineSubsystem;
class SystemStatusSubsystem;
class SzaSubsystem;
class TestSubsystem;
class VlbiSubsystem;
class WbdcSubsystem;
class WbcBandSubsystem;
class WbPipelineSubsystem;
class WeatherSubsystem;
class WbDataflowSubsystem;
class SlDataflowSubsystem;
class C3gDataflowSubsystem;
class WbRemapperSubsystem;
class SlRemapperSubsystem;
class C3gRemapperSubsystem;

class SystemFrameBuffer;
class SubsystemFrame;

/**
 *
 * Monitor system base class.
 *
 */
class MonitorSystem : public MonitorSystemContainer {
public:

    /**
     * Constructor base subsystem configuration
     * @param systemName     monitor system name
     * @param nSubsystems    number of subsystems
     * @param nMonitorPoints number of monitor points
     * @param nSamples       number of samples
     */
    MonitorSystem(const std::string& systemName, long nSubsystems, 
                    long nMonitorPoints, long nSamples,
                    int queueDepth=SubsystemFrameBuffer::kDefaultQueueDepth);

    /**
     * Destructor
     */
    virtual ~MonitorSystem() ;
        
    /**
     * @brief Get reference to an AntennaCommon container in an ovro or
     *        bima or sza subsystem.
     *
     * Convenience function. Gives the same answer as calling
     * ovro(x).antennaCommon() or bima(y).antennaCommon() or
     * sza(z).antennaCommon() for the appropriate value of z, y or z depending
     * on the value of index.
     *
     * @param index O based antenna index
     *              (i.e. 0-5 are ovro, 6-14 are bima, 15-22 are sza)
     */
     AntennaCommon& antennaCommon( int index ) const;

    /**
     * @brief Get reference to an Astro container.
     */
    AstroSubsystem& astro( ) const { return astro_; }

    /**
     * Get reference to a bima antenna subsystem
     * @param index antenna index  
     */  
    BimaSubsystem& bima(int index) const { return *(bimas_.at( index )); }

    /**
     * Get reference to carma 8 station 3g correlator pipeline.
     */  
    C3gMax8PipelineSubsystem& c3gMax8Pipeline( ) const { return c3gMax8Pipeline_; } 
    
    /**
     * Get reference to carma 23 station 3g correlator pipeline.
     */  
    C3gMax23PipelineSubsystem& c3gMax23Pipeline( ) const { return c3gMax23Pipeline_; } 

    /**
     * Get reference to the centalIF subsystem
     */  
    CentralIfSubsystem& centralIf() const { return centralIf_; }

    /**
     * Get reference to the control subsystem
     */  
    ControlSubsystem& control() const { return control_; }

    /**
     * Get reference to the control subsystem
     */  
    DataflowSubsystem& dataflow() const { return dataflow_; }
    
    /**
     * Get reference to the delay engine subsystem
     */  
    DelayEngineSubsystem& delay() const { return delayEngine_; }
    
    /**
     * Get reference to fault subsystem
     */
    FaultSubsystem & fault( ) const;

    /**
     * Get reference to imr subsystem
     */
    ImrSubsystem& imr() const { return imr_; };

    /**
     * Get reference to the alarm subsystem
     */  
    AlarmSubsystem& alarm() const { return alarm_; }
    
    /**
     * Get reference to the lineLength subsystem
     */  
    LineLengthSubsystem& lineLength() const { return lineLength_; }
    
    /**
     * Get reference to the loberotator subsystem
     */  
    LoberotatorSubsystem& loberotator() const { return loberotator_; }
    
    /**
     * Get reference to the LOref subsystem
     */  
    LoRefSubsystem& loRef() const { return loRef_; }
    
    /**
     * Get reference to the masterclock subsystem
     */  
    MasterClockSubsystem& masterclock() const { return masterclock_; }
    
    /**
     * Get reference to the opacityMonitor subsystem
     */  
    OpacityMonitorSubsystem& opacityMonitor() const { return opacityMonitor_; }
    
    /**
     * Get reference to ovro antenna subsystem
     * @param index antenna index  
     */  
    OvroSubsystem& ovro(int index) const { return *(ovros_.at( index )); }
    
    /**
     * Get reference to the phase monitor subsystem
     */  
    PhaseMonitorSubsystem& phaseMonitor() const { return phaseMonitor_; }

    /**
     * Get reference to the projectdatabase manager subsystem
     */
    ProjectDatabaseManagerSubsystem& projectDatabaseManager()
        const { return projectDatabaseManager_; }

    /** 
     * Get reference to the signal path subsystem.
     */
    SignalPathSubsystem & signalPath() const { return signalPath_; };

    /**
     * Get reference to a spectral line correlator band subsystem
     * @param index band index  
     */  
    CarmaSlcBandSubsystem & carmaSlcBand( const int index ) const {
        return *(carmaSlcBands_.at( index ));
    }

    /**
     * Get the number of carmaSlcBands
     */
    int getCarmaSlcBandCount() const ;

    /**
     * Get reference to a CARMA3G correlator band subsystem
     * @param index band index  
     */  
    Carma3GBandSubsystem & carma3gBand( const int index ) const {
        return *(carma3gBands_.at( index ));
    }

    /**
     * Get the number of carma3g bands
     */
    int getCarma3gBandCount() const ;

    /**
     * Get reference to the spectral line downconverter subsystem
     */  
    SldcSubsystem& sldc() const { return sldc_; }

    /**
     * Get reference to the spectral line pipeline subsystem
     */  
    SlPipelineSubsystem& slPipeline() const { return slPipeline_; }

    /**
     * Get reference to the system status subsystem
     */  
    SystemStatusSubsystem& systemStatus() const { return systemStatus_; }

    /**
     * Get reference to sza antenna subsystem
     * @param index antenna index  
     */  
    SzaSubsystem& sza(int index) const { return *(szas_.at( index )); }
    
    /**
     * Get reference to a wideband correlator band subsystem
     * @param index band index  
     */  
    WbcBandSubsystem & wbcBand( const int index ) const {
        return *(wbcBands_.at(index));
    }

    /**
     * Get the number of wbcBands
     */
    int getWbcBandCount() const ;
    
    /**
     * Get reference to wideband downconverter subsystem
     */  
    WbdcSubsystem& wbdc() const { return wbdc_; }

    /**
     * Get reference to the wideband line pipeline subsystem
     */  
    WbPipelineSubsystem& wbPipeline() const { return wbPipeline_; }

    /**
     * Get reference to the weather subsystem
     */  
    WeatherSubsystem& weather() const { return weather_; }
    
    /**
     * Get reference to the test subsystem
     */  
    TestSubsystem& test() const { return test_; }    
    
    /**
     * Get reference to the vlbi subsystem
     */  
    VlbiSubsystem& vlbi() const { return vlbi_; }
    
    /**
     * @return reference to WB data flow subsystem 
     * @param index Subsystem container index number = band number minus one
     */
    WbDataflowSubsystem & wbDataflow(const int index) const 
    { 
        return *( wbDataflow_.at(index) ); 
    }

    /**
     * @return reference to WB corr data remapper subsystem 
     * @param index Subsystem container index number = band number minus one
     */
    WbRemapperSubsystem & wbRemapper(const int index) const 
    { 
        return *( wbRemapper_.at(index) ); 
    }

    /**
     * @return reference to spectral data flow subsystem 
     * @param index Subsystem container index number = band number minus one
     */
    SlDataflowSubsystem & slDataflow(const int index) const 
    { 
        return *( slDataflow_.at(index) );
    }

    /**
     * @return reference to c3g data flow subsystem 
     * @param index Subsystem container index number = band number minus one
     */
    C3gDataflowSubsystem & c3gDataflow(const int index) const 
    { 
        return *( c3gDataflow_.at(index) );
    }

    /**
     * @return reference to spectral line corr data remapper subsystem 
     * @param index Subsystem container index number = band number minus one
     */
    SlRemapperSubsystem & slRemapper(const int index) const 
    { 
        return *( slRemapper_.at(index) ); 
    }

    /**
     * @return reference to c3g corr data remapper subsystem 
     * @param index Subsystem container index number = band number minus one
     */
    C3gRemapperSubsystem & c3gRemapper(const int index) const 
    { 
        return *( c3gRemapper_.at(index) ); 
    }

    /**
     * Get reference to the underlying storage
     */  
    SystemFrameBuffer& systemFrameBuffer() const { return systemFrameBuffer_; }

    /**
     * Dumps transport statistics as a table, with write delays, and
     * transport/write times printed out as doubles, with fixed precision.
     *
     * @param canonical bool if true, prints canonical name otherwise uses
     *        leaf name.
     */
    virtual std::string transportStatisticsToString (bool canonical = false) const;

    /**
     * Dumps transport statistics as a table, with write delays, and
     * transport/write times printed out as doubles, with fixed precision.
     *
     * @param o ostringstream stream into which the string is dumped
     */
    std::string addTransportTimesAsString (std::ostringstream o) const ;

    /**
     * Returns true if the monitor subsystem contains data that is current.
     * A read could make the data current, and hence make isCurrent return true.
     * If a read does not make isCurrent true, then the subsystem 
     * is probably broken.

     * @return bool true if subsystem is current, false otherwise.
     */
    bool isCurrent () const;

    /**
     * Returns number of valid subsystems.
     *
     * @return long number of subsystems that are current.
     */
    long getNumValidSubsystems () const;

    /**
     * Moves read pointer beyond top of queue so a read will block until the
     * next write. This method may be used to ensure that fresh data is read
     * on the next read() call.  Normally, a call to read will read the next 
     * unread element in its queue.
     */
    void resetQueue () ;

    /*
     * Overridden methods of abstract MonitorSystemContainer methods.
     * Documentation supplied by base class.
     */
    virtual unsigned int read();
    virtual bool readNewest();
    virtual bool readNewestIfStale();
    virtual bool readNewestConditionalCopy();
    virtual void write();
    virtual int getFrameCount() const;

   /**
    * Delay offset (in seconds) from the corrected UTC half-second when the
    * System frame is written to IPQ on the ACC.
    *
    * @return double offset from half-second specified in seconds.
    */
    double getCollatorWriteDelay() const;

   /**
    * MJD when this system frame was actually written to the IPQ.
    * It is guaranteed that this frame was written no earlier than this
    * time.
    *
    * @return double time in MJD format as specified by 
    *         ::carma::util::Time class.
    */
    double getCollatorWriteTime() const;
               
   /**
    * Sets MJD when this system frame was actually read by the fault system.
    * It is guaranteed that this frame was read no later than this
    * time.
    *
    * @param time  double time in MJD format as specified by 
    *        ::carma::util::Time class. Defaults to current time.
    */
    void setRawReadTime( double mjdTimestamp );
    void setRawReadTime( );
               
   /**
    * Gets MJD when this system frame was actually read by the fault system.
    * It is guaranteed that this frame was read no later than this
    * time.
    *
    * @return double time in MJD format as specified by 
    *         ::carma::util::Time class.
    */
    double getRawReadTime() const;
               
   /**
    * Sets MJD when this system frame was actually read by the fault system.
    * It is guaranteed that this frame was read no later than this
    * time.
    *
    * @param time  double time in MJD format as specified by 
    *        ::carma::util::Time class. Defaults to current time.
    */
    void setFinalWriteTime( double mjdTimestamp );
    void setFinalWriteTime( );
               
   /**
    * Gets MJD when this system frame was actually read by the fault system.
    * It is guaranteed that this frame was read no later than this
    * time.
    *
    * @return double time in MJD format as specified by 
    *         ::carma::util::Time class.
    */
    double getFinalWriteTime() const;
               
   /**
    * Resets collator write time - call clearAllTimes to conform
    * to name of similar routine in ::carma:monitor::MonitorSubsystem and 
    * allows for more times to be added.
    */
    void   resetTimes ();

   /**
    * Capacity related method - returns total subsystem capacity.
    *
    * @return long total # of subsystems system can accomodate
    */
   long    getTotalNumSubsystems() const;

   /**
    * Capacity related method - returns total monitor point capacity
    * across all subsystems.
    *
    * @return long total # of monitor points system can accomodate
    */
   long    getTotalNumMonitorPoints() const;

   /**
    * Capacity related method - returns total monitor sample capacity
    * across all subsystems.
    *
    * @return long total # of monitor samples system can accomodate
    */
   long    getTotalNumMonitorSamples() const;

   /**
    * Capacity related method - returns total size available in bytes
    * across all subsystems.
    *
    * @return size_t size of system frame in bytes
    */
   size_t    getTotalMonitorSystemSize() const;

   /**
    * Capacity usage related method - returns total allocated monitor point capacity
    * across all subsystems.
    *
    * @return long total # of monitor points allocated in monitor system object
    */
   long    getMaxNumMonitorPoints() const;

   /**
    * Capacity related method - returns total allocated monitor sample capacity
    * across all subsystems.
    *
    * @return long total # of monitor samples system can accomodate
    */
   long    getMaxNumMonitorSamples() const;

   /**
    * Capacity usage related method - returns total size allocated in bytes
    * across all subsystems.
    *
    * @return size_t allocated size of system frame in bytes
    */
   size_t    getMaxMonitorSystemSize() const;

   /**
    * Capacity usage related method - returns actual number of subsystems
    * allocated.
    *
    * @return long allocated # of subsystems in the monitor system object
    */
   long    getActualNumSubsystems() const;

   /**
    * Capacity usage related method - returns actual # of monitor points 
    * across all subsystems.
    *
    * @return long actual # of monitor points across al subsystems
    */
   long    getActualNumMonitorPoints() const;

   /**
    * Capacity usage related method - returns actual # of monitor points 
    * of type <pre>type</pre> across all subsystems.
    *
    * @param type const MonitorValueType type of monitor points of interest
    * @return long actual # of monitor points of type <pretype</pre> across all subsystems
    */
   long    getActualNumMonitorPoints (const MonitorValueType type) const;

   /**
    * Capacity usage related method - returns actual # of monitor points 
    * with multiple samples across all subsystems.
    *
    * @return long actual # of multiple sample monitor points across 
    * all subsystems
    */
   long    getActualNumMultiSampleMonitorPoints() const;

   /**
    * Capacity related method - returns actual # of monitor samples
    * across all subsystems.
    *
    * @return long actual # of monitor samples across all subsystems
    */
   long    getActualNumMonitorSamples() const;

   /**
    * Capacity related method - returns actual # of monitor samples
    * of type <pretype</pre> across all subsystems.
    *
    * @param type const MonitorValueType type of monitor samples of interest
    * @return long actual # of monitor samples of type <pre>type</pre> across all subsystems
    */
   long    getActualNumMonitorSamples (MonitorValueType type) const;

   /**
    * Capacity usage related method - returns actual size used in bytes
    * across all subsystems.
    *
    * @return size_t actual size of system frame in bytes
    */
   size_t    getActualMonitorSystemSize() const;

   /**
    * Capacity usage related method - returns total number of subsystems
    * allocated.
    *
    * @return long allocated # of subsystems in the monitor system object
    */
   long    getAllocatedNumSubsystems() const;

   /**
    * Capacity usage related method - returns allocated monitor point capacity
    * across all subsystems. 
    *
    * @return long total # of monitor points allocated in monitor system object
    */
   long    getAllocatedNumMonitorPoints() const;

   /**
    * Capacity related method - returns total monitor sample capacity
    * across all subsystems. May include "holes" in the sample arrays.
    *
    * @return long total # of monitor samples system can accomodate
    */
   long    getAllocatedNumMonitorSamples() const;

   /**
    * Capacity usage related method - returns total size allocated in bytes
    * across all subsystems. Sizing includes "holes".
    *
    * @return size_t allocated size of system frame in bytes
    */
   size_t    getAllocatedMonitorSystemSize() const;

   /**
    * Capacity usage related method - returns number of monitor points present
    * across all subsystems - number is obtained by iterating over all monitor
    * points, so method is likely to be expensive. 
    *
    * @return long total # of monitor points counted in monitor system object
    */
   long    getCountedNumMonitorPoints() const;

   /**
    * Capacity related method - returns number of monitor samples counted
    * across all subsystems. Number is obtained by iterating over all
    * monitor points, so method is likely to be expensive.
    *
    * @return long total # of monitor samples counted in the system 
    */
   long    getCountedNumMonitorSamples() const;

   /**
    * Capacity usage related method - returns total size counted, in bytes
    * across all subsystems. uses iterator to count monitor points and samples,
    * and computes size from these numbers.
    *
    * @return size_t "counted" size of system frame in bytes
    */
   size_t    getCountedMonitorSystemSize() const;

   /**
    * Get the underlying SystemFrameBuffer. For blackbelt use only.
    *
    * @return reference to the system frame buffer
    */
   SystemFrameBuffer&    getSystemFrameBuffer() const;

   /**
    * Get underlying subsystem by index.
    * @see getActualNumSubsystems
    */
   MonitorSubsystem & getChildSubsystem( int index ) const;

protected:

    SubsystemFrame & getChildSubsystemFrame( int index ) const;

private:
    /* Storage
     * Order of construction is critical - storage (in the form of
     * SystemFrameBuffer) must be constructed first, or the subsystems
     * will not have space to exist, and construction of all 
     * subsequent members will fail.
     * The order of storage intialization is determined by the order
     * of declaration below, *not* by the order of the constructor
     * initialization list.
     */
    SystemFrameBuffer&      systemFrameBuffer_;
        
    // Subsystems
    AstroSubsystem&             astro_;
    std::vector<BimaSubsystem*> bimas_;
    C3gMax8PipelineSubsystem&   c3gMax8Pipeline_;  
    C3gMax23PipelineSubsystem&  c3gMax23Pipeline_;  
    CentralIfSubsystem&         centralIf_;
    ControlSubsystem&           control_;
    DataflowSubsystem&          dataflow_;
    DelayEngineSubsystem&       delayEngine_;
    ImrSubsystem&               imr_;
    AlarmSubsystem&             alarm_;
    LineLengthSubsystem&        lineLength_;
    LoberotatorSubsystem&       loberotator_;
    LoRefSubsystem&             loRef_;
    MasterClockSubsystem&       masterclock_;
    OpacityMonitorSubsystem&    opacityMonitor_;
    std::vector<OvroSubsystem*> ovros_;
    PhaseMonitorSubsystem&   phaseMonitor_;
    ProjectDatabaseManagerSubsystem& projectDatabaseManager_;
    ::std::vector< CarmaSlcBandSubsystem * > carmaSlcBands_;
    ::std::vector< Carma3GBandSubsystem * > carma3gBands_;
    SignalPathSubsystem &    signalPath_;
    SldcSubsystem&           sldc_;
    SlPipelineSubsystem&     slPipeline_;
    SystemStatusSubsystem&   systemStatus_;
    ::std::vector< SzaSubsystem * > szas_;
    TestSubsystem&           test_;
    VlbiSubsystem&           vlbi_;
    ::std::vector< WbcBandSubsystem * > wbcBands_;
    WbdcSubsystem&           wbdc_;
    WbPipelineSubsystem&     wbPipeline_;
    WeatherSubsystem&        weather_;
    FaultSubsystem &         fault_;
    ::std::vector< WbDataflowSubsystem * > wbDataflow_;
    ::std::vector< SlDataflowSubsystem * > slDataflow_;
    ::std::vector< C3gDataflowSubsystem * > c3gDataflow_;
    ::std::vector< WbRemapperSubsystem * > wbRemapper_;
    ::std::vector< SlRemapperSubsystem * > slRemapper_;
    ::std::vector< C3gRemapperSubsystem * > c3gRemapper_;
};


//------------------------------------------------------------------


class RawCarmaMonitorSystem;

/**
 * Carma monitor system.
 * This instance of the CMS is written by the faultSystem after frame 
 * collation.  It consists of all subsystems though they may not have
 * been synchronized properly yet. Most programs written in the ACC will 
 * use this class.
 */
class CarmaMonitorSystem : public MonitorSystem {
protected:

    /**
     * Full monitor system with all subsystems
     * @param name of the system
     * @param queueDepth Depth of underlying IPQ.
     */
    CarmaMonitorSystem( std::string name,
                        int queueDepth );

public:

    /**
     * Full monitor system with all subsystems
     */
    CarmaMonitorSystem( );


    /**
     * Full monitor system with all subsystems
     * @param raw RawCarmaMonitorSystem object used to construct this - 
     *        resulting object is clone of raw.
     */
    explicit CarmaMonitorSystem(const RawCarmaMonitorSystem& raw) ;

   /**
    * Method to synchronize this object with another CarmaMonitorSystem 
    * object. Both objects must be commensurate, which means that both
    * objects support the same max # of subsystems, max total # of 
    * monitor points and max total # of samples. If the two are not 
    * commensurate, then this method throws a ::carma::util::ErrorException.
    *
    * @pre this.maxNumSubsystemFrames == src.maxNumSubsystemFrames
    *      &&  this.maxTotalMonitorPoints == src.maxTotalMonitorPoints
    *      &&  this.maxTotalSamples == src.maxTotalSamples
    * @post this == src
    * @pre this.maxNumSubsystemFrames != src.maxNumSubsystemFrames
    *      ||  this.maxTotalMonitorPoints != src.maxTotalMonitorPoints
    *      ||  this.maxTotalSamples != src.maxTotalSamples
    * @post exception ::carma::util::ErrorException
    * @param  src const CarmaMonitorSystem object - this object must be
    *         synchronized with src 
    * @return none
    * @exception ::carma::util::ErrorException
    */
    void   synchronize (const CarmaMonitorSystem& src) ;

};

/**
 * This is the full Carma monitor system before fault system processing
 * and immediately following collation in the frameCollator.
 */
class RawCarmaMonitorSystem : public CarmaMonitorSystem {

public:

    /**
     * Full monitor system with all subsystems, before fault processing
     * @param numSubsystems total number of subsystems allocated
     * @param numMonitorPoints total number of monitor points allocated
     * @param numSamples total number of samples allocated
     */
    RawCarmaMonitorSystem( );

};

/**
 * This is the Final Carma Monitor system after monitor point values from 
 * the correlator pipeline have been collated and synchronized with the
 * appropriate frame.  This version of the Carma Monitor System is written 
 * by the monitorPipelineSync application and read by the Monitor Average
 * Writer. 
 */
class FinalCarmaMonitorSystem : public CarmaMonitorSystem {
public:

    /**
     * Full monitor system with all subsystems.
     */
    FinalCarmaMonitorSystem( );
};




} }  // End namespace carma::monitor  


inline carma::monitor::FaultSubsystem &
carma::monitor::MonitorSystem::fault( ) const
{
    return fault_;
}


#endif
