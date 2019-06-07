/**
 * @file
 * Implementation of subscriber for monitor frame events.
 *
 * @author: N. S. Amarnath
 *
 * $CarmaCopyright$
 *
 */

#ifndef CARMA_MONITOR_FRAME_SUBSCRIBER_H
#define CARMA_MONITOR_FRAME_SUBSCRIBER_H

#include "carma/monitor/MonitorSystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/types.h"

#include <map>
#include <vector>

namespace carma {
namespace monitor {


class MonitorSystemQueueEntry;


/**
 * Indicates that a null event was returned by 
 * FrameSubscriber::retrieveNotification()
 *
 * @see FrameSubscriber::retrieveNotification
 */
class NullEventException : public ::carma::util::ErrorException {
  public:
    NullEventException (const ::std::string & message, 
                           const char* fileName, 
                           int lineNo) 
                   : ::carma::util::ErrorException (message, fileName, lineNo)
                   { }

    NullEventException (const ::std::ostringstream & message, 
                           const char* fileName, 
                           int lineNo) 
                   : ::carma::util::ErrorException (message, fileName, lineNo)
                   { }

    NullEventException (const ::carma::util::ErrorException& errorException)
                   : ::carma::util::ErrorException (errorException)
                   { }

    virtual ~NullEventException () throw() { }

    /**
     * @typedef MonitorSystemVec  std::vector of MonitorSystem pointers that
     * allow latency greater than a single frame duration (500 ms).
     * The members of this vector are always guaranteed to be
     * in time order with the highest frame count (newest time)
     * in the highest vector location.
    typedef ::std::vector< const carma::monitor::MonitorSystem * > MonitorSystemVec;
     */

    /**
     * @typedef non-const iterator for MonitorSystemVec
    typedef ::std::vector< const carma::monitor::MonitorSystem * >::iterator MonSysVecIterator;
     */


};



    /**
    * A class that accepts named events from a Push supplier.
    * This is the basis for event driven clients, which wish to
    * to block on "receive" call until an event arrives and process it.
    * NotificationConsumer provides a "callback" model, whereas
    * FrameSubscriber provides a "block and wait for event" model.
    */
    class FrameSubscriber
    {
      public:

       /**
        * Constructor
        * This constructor is used if this FrameSubscriber is to use the default
        * ORB and clean up and shut it down when this FrameSubscriber 
        * is destroyed.  In other words, the programmer wants the ORB shutdown
        * when this FrameSubscriber is destroyed.
        * @param delayInS The FrameSubscriber keeps a map of monitor systems
        *   (yes, monitor systems!) so that 
        * @param rawMode True if the monitor systems should be
        * instances of RawMonitorSystems, false if they should
        * be CarmaMonitorSystems. No default.
        */
        FrameSubscriber(double delayInS, bool   rawMode);

       /**
        * Destructor
        */
        ~FrameSubscriber();

        /*
         * Set scheduled first fire time for call to writeMonitorSystemToIPQ.
         * Should be called once and only once to synchronize internal timing.
         * @param firstFireTimeMJD First fire time as MJD.
         * @pre No collation takes place prior to call. 
         * @throw IllegalStateException if already called.
         * @see writeMonitorSystemToIPQ
         */
        void setFirstFireTime( double firstFireTimeMJD );


        /**
         * Write monitor system to IPQ.  
         * @pre setFirstFireTime must be called prior
         */
        void writeMonitorSystemToIPQ(double currentFireTimeMJD,
                                     double delayInSeconds,
			                         int    clearDelayInFrames);
        
        /**
         * Process a TransportSubsystemFrame.
         */
        void operator()( const carma::monitor::TransportSubsystemFrame & tsf );

      protected:

        /**
         * Called by push_structured_event. Prints performance data when 
         * debug (trace) is on.
         * 
         * @param timestamps array of doubles containing timestamps
         *        recorded as doubles using ::carma::util::Time::MJD()
         * @param numTimes int representing the number of timestamps in
         *                 the array <pre>timestamps</pre>
         *
         * @see ::carma::util::Time::MJD()
         */
        void    printTimes (double timestamps[], int numTimes) const;

     private:

        void writeMonitorStatsHoldingMsMapLock( MonitorSystem & ms );
        /*
         * Clears class variables containing subsystem and totals for various
         * error counts, including late, early, and missing frames.
         */
        void clearErrorCounts();
        
        bool rawMode_;

        typedef ::std::map< carma::util::frameType,
                MonitorSystemQueueEntry * > MonitorSystemMap;

        carma::util::PthreadMutex msMapMutex_;
        MonitorSystemMap msMap_;
        bool initialized_;
        carma::util::frameType lastWriteFrame_;

        typedef ::std::map< subsystemIDType, int > SubsystemCountMap;
        typedef ::std::map< subsystemIDType, 
                            carma::util::frameType > SubsystemFrameMap;


        carma::util::PthreadMutex subsystemDataMutex_;
        SubsystemCountMap receivedSubsystemFrameCount_;
        SubsystemCountMap receivedSubsystemDataCount_;
        SubsystemCountMap lateSubsystemFrameCount_;
        SubsystemCountMap earlySubsystemFrameCount_;
        SubsystemCountMap missedSubsystemFrameCount_;
        SubsystemCountMap missedDataFrameCount_;
        SubsystemFrameMap lastReceivedSubsystemFrames_;
        
        int totalFrames_;
        int totalMissedDataFrames_;
        int totalEarlyFrames_;
        int totalLateFrames_;
        int totalMissedFrames_;

        int outOfOrderSubsystemFrames_;
        int duplicateSubsystemFrames_;
        int erroneousNotifications_;

        // Fill msMap_ with ((int)(2*delay)+1) monitor systems.
        // The delay is the writeDelay param to the frameCollator,  
        // currently 600 (0.600sec) in carmaCore.xml
        void allocateMonitorSystems(double delayInS);
        void deallocateMonitorSystems();
        void rebuildMapHoldingLock( carma::util::frameType newHeadFrame );
        void checkForWriteDiscontinuityHoldingLock( const double currentFireTimeMJD );
        
        // There are some subsystems that are not counted
        bool isMissedDataCountingEnabled(unsigned int subsystemID);
        
        // Subsystem has a timestamp MP that can be used to check transport
        bool subsystemHasTimestamp(unsigned int subsystemID);

    }; // End class FrameSubscriber

   } // End namespace monitor
} // End namespace carma

#endif // CARMA_MONITOR_FRAME_SUBSCRIBER_H
