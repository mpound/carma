/**
 * @file
 *
 * Updates delay engine periodically - incorporates a timer thread.
 *
 * Author: N. S. Amarnath
 * 
 */

#ifndef CARMA_SUBARRAY_TRACKER_THREAD_H
#define CARMA_SUBARRAY_TRACKER_THREAD_H

#include <vector>
#include <sys/time.h>
#include "carma/monitor/WeatherSubsystem.h"
#include "carma/control/SubarrayControlImpl.h"

#include "carma/interferometry/DelayEngine.h"

#include "carma/services/Atmosphere.h"
#include "carma/services/Pressure.h"
#include "carma/services/Temperature.h"
#include "carma/services/Velocity.h"


namespace carma  {

namespace util {

class PeriodicTimer;

}

namespace control  {


/**
 * Implementation of SAT as thread. See design document for CARMA Control 
 * package for details. 
 *
 * Critical method is updateTracking - all the work is done there, invoked
 * periodically by action. Period is deteremined by TRACKER_UPDATE_INTERVAL
 * which is currently set to 20 seconds.  The update is synchronized to
 * aboslute ,kd time, an integral number of TRACKER_UPDATE_INTERVALS.
 * The quadratic interpolator will be filled with 3 samples that are
 * -0.5T, +0.5T, and 1.5T from the current time, allowing interpolation from 
 * 0T to 1.0T without ever being near the boundary where precision could 
 * cause an error.
 *
 * TrackerThread::updateTracking sends topocentric RA/Dec to antennas
 * for RA/Dec specified sources, and Az/El for Az/El specified targets, in
 * addition to Ut1-Utc corrections. It also sends geocentric Ra/Dec for 
 * each antenna) to delay engine for RA/Dec specified sources and Az/El 
 * for each antenna to delay engine for Az/El specified targets.
 */
class SubarrayControlImpl::TrackerThread {
    
public:

    static const unsigned long 
        TRACKER_DEFAULT_UPDATE_INTERVAL = 20; //in seconds
    static const unsigned long 
        TRACKER_DEFAULT_WEATHER_UPDATE_INTERVAL = 4 * 60; //in seconds (4 min)
    //In seconds (8 min)    
    static const unsigned long 
        TRACKER_DEFAULT_ANTENNA_TRACK_UPDATE_INTERVAL = 8 * 60; 

// TESTING ONLY TO SPEED UP TRACKING CYCLE!
//    static const unsigned long 
//        TRACKER_DEFAULT_ANTENNA_TRACK_UPDATE_INTERVAL = 1 * 60; 


        /**
         * Constructor
         * @brief Constructs tracker thread for a set of antennas, a
         * specified delay engine and (possibly) an initial source.
         *
         * @param carmaMonitor, reference to full CARMA monitor system - used 
         *        to read in fresh values from the monitor stream.
         * @param period, const long periodicity of tracker thread firing
         *        set to TRACKER_DEFAULT_UPDATE_INTERVAL. Measured in seconds.
         * @param antennaWeatherPeriod, const long periodicity of weather update
         *        to antennas, set to TRACKER_DEFAULT_WEATHER_UPDATE_INTERVAL. 
         *        Measured in seconds.
         * @param delayEngineWeatherPeriod, const long periodicity of weather 
         *        update to delay engine, set to 
         *        TRACKER_DEFAULT_WEATHER_UPDATE_INTERVAL. Measured in seconds.
         * @param antennaTrackPeriod, const long periodicity of tracking update
         *        to antennas, set to 
         *        TRACKER_DEFAULT_ANTENNA_TRACK_UPDATE_INTERVAL. 
         *        Measured in seconds.
         */
        explicit TrackerThread (
            SubarrayControlImpl & saCI,
            monitor::CarmaMonitorSystem & carmaMonitor,
            const unsigned long period = TRACKER_DEFAULT_UPDATE_INTERVAL,
            const unsigned long antennaWeatherPeriod = 
                TRACKER_DEFAULT_WEATHER_UPDATE_INTERVAL,
            const unsigned long delayEngineWeatherPeriod = 
                TRACKER_DEFAULT_WEATHER_UPDATE_INTERVAL,
            const unsigned long antennaTrackPeriod = 
                TRACKER_DEFAULT_ANTENNA_TRACK_UPDATE_INTERVAL );

        /**
         * Destructor
         */
        ~TrackerThread();

        /**
         * Returns tracker thread period - set during construction.
         * Value is returned in seconds.
         *
         * @return const long value of thread period in seconds.
         */
        long getTrackerPeriod () const ;

        /**
         * Returns antenna tracker thread period - set during construction.
         * Value is returned in seconds.
         *
         * @return const long value of antenna tracking period in seconds.
         */
        long getAntennaTrackPeriod () const ;

        /**
         * Returns antenna weather update period - set during construction.
         * Value is returned in seconds.
         *
         * @return const long value of antenna weather update period in seconds.
         */
        long getAntennaWeatherPeriod () const ;

        /**
         * Returns delay engine weather update period - set during construction.
         * Value is returned in seconds.
         *
         * @return const long value of delay engine weather update period 
         * in seconds.
         */
        long getDelayEngineWeatherPeriod () const ;

        /**
         * Returns interferometry interpolation interval as a fraction of a day.
         *
         * @return const double value of interferometry interpolation 
         * interval, as a fraction of a day.
         */
        double getInterpolationInterval () const ;

        /**
         * Returns antenna tracking interpolation interval as a 
         * fraction of a day.
         *
         * @return const double value of antenna interpolation interval, 
         *         as a fraction of a day.
         */
        double getAntInterpolationInterval () const ;

        /**
         * Set time of last antenna tracking position update.
         * Input truncated to align with the effective interval.
         * @param mjd The time to set the ant track update to.
         */
        void setLastAntUpdate(double mjd);

        /**
         * @return time of last antenna tracking position update as an MJD.
         */
        double getLastAntUpdate( ) const;

        /**
         * Set time of last interferometry sky position update.
         * Input truncated to align with the effective interval.
         *
         * @param mjd The time to set the interferometry update to.
         */
        void setLastIntferUpdate(double mjd);

        /**
         * @return The time of last interferometry sky position update, 
         * as an MJD
         */
        double getLastIntferUpdate( ) const;

        /**
         * Returns internal CARMA Monitor System object. 
         *
         * @return monitor::CarmaMonitorSystem object reference
         */
        monitor::CarmaMonitorSystem& getMonitorSystem () const ;
        
        /**
         * Thread functor method.  To be passed in to boost::thread constructor.
         */
        void operator()();

        /**
         * Calls updateTracking on each antenna, and then
         * calls computeDelay on delay engine. setAntennaLOFreq
         * should already have been called by constructor of
         * SubarrayControlImpl.
         */
        void updateTracking();

  private:

        /**
         * Returns next timer fire time in wall clock time (as seen by 
         * the OS). Uses period_ to decide next fire time.
         *
         * @return ::timeval timeval structure, with seconds and
         *         and microseconds.
         */
        struct ::timeval getNextTimerInstant() const ;

        /**
         * Private data member - reference to CARMA monitor system. Used
         * to update and send weather/environmental information to 
         * antennas and delay engine.
         */
        monitor::CarmaMonitorSystem & carmaMonitor_;

        /**
         * Private data member that determines the period (in seconds)
         * of this thread. Is a constant that is set at construction time.
         * By default it is equal to TRACKER_DEFAULT_UPDATE_INTERVAL.
         */
        const unsigned long period_;
         
        /**
         * Private data member that determines the period (in seconds)
         * that this thread updates weather information for antennas. 
         * Is a constant that is set at construction time.
         * By default it is equal to TRACKER_DEFAULT_WEATHER_UPDATE_INTERVAL.
         */
        const unsigned long antennaWeatherPeriod_;
         
        /**
         * Private data member that determines the period (in seconds)
         * that this thread updates weather information for delay engine. 
         * Is a constant that is set at construction time.
         * By default it is equal to TRACKER_DEFAULT_WEATHER_UPDATE_INTERVAL.
         */
        const unsigned long delayEngineWeatherPeriod_;
         
        /**
         * Private data member that determines the period (in seconds)
         * that this thread updates tracking information for all antennas. 
         * Is a constant that is set at construction time.
         * By default it is equal to 
         * TRACKER_DEFAULT_ANTENNA_TRACK_UPDATE_INTERVAL.
         */
        const unsigned long antennaTrackPeriod_;
         
        /**
         * Private data member that determines the period (in number of cycles)
         * that this thread updates weather information for antennas. 
         * Is a constant that is set at construction time.
         * By default it is equal to 
         * TRACKER_DEFAULT_WEATHER_UPDATE_INTERVAL/period_.
         */
        const unsigned long antennaWeatherCycleCount_;
         
        /**
         * Private data member that determines the period (in number of cycles)
         * that this thread updates weather information for delay engine. 
         * Is a constant that is set at construction time.
         * By default it is equal to 
         * TRACKER_DEFAULT_WEATHER_UPDATE_INTERVAL/period_.
         */
        const unsigned long delayEngineWeatherCycleCount_;
         
        /**
         * Private data member that determines the period (in number of cycles)
         * that this thread updates tracking information for all antennas. 
         * Is a constant that is set at construction time.
         * By default it is equal to 
         * TRACKER_DEFAULT_ANTENNA_TRACK_UPDATE_INTERVAL/period_.
         */
        const unsigned long antennaTrackCycleCount_;
         
        /**
         * Private data member that determines the interpolation interval
         * for the subarray tracker - coupled to the period. The value
         * of this variable is equal to the period_ expressed as a fraction
         * a day.
         */
        const double interpInterval_;
        
        double lastAntUpdate_;
        double lastIntferUpdate_;
         
        /**
         * Private data member used to calculate time to
         * next firing of timer
         */
        struct ::timeval nextFiringTime_;

        /**
         * PeriodicTimer Timer instance used to get timer pulse
         */
        util::PeriodicTimer * timer_;

        SubarrayControlImpl & saCI_;
        
        /**
         * Read the latest weather data from the Weather monitor system.
         * Update SAT internal weather data by passing monitor data 
         * through the Atmosphere safe routines, guaranteeing reasonable
         * weather values.
         */
        void updateWeatherSafely();
        
        // Weather data.
        services::Atmosphere atm_;
        services::Temperature   airTemp_;
        services::Temperature   dewTemp_;
        services::Pressure      atmPressure_;
        services::Velocity      windSpeed_;
        // 0 degrees = north, 90 degrees = east
        services::Angle         windDirection_;
        double                  relHumid_;
};


}  // End namespace carma::control
}  // End namespace carma


#endif  // CARMA_SUBARRAY_TRACKER_THREAD_H
