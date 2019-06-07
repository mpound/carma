/**
 * $Id: SubarrayTrackerThread.cc,v 1.79 2014/04/02 23:10:53 iws Exp $ 
 * 
 * Delay engine update thread that incorporates a timer.  
 * 
 * Author: N. S. Amarnath
 * 
 * Version: $Revision: 1.79 $ * $Date: 2014/04/02 23:10:53 $ 
 */


#include <iomanip>
#include <ios>
#include <pthread.h>
#include <sys/time.h>

#include "carma/util/Backtrace.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/loggingUtils.h"
#include "carma/util/PeriodicTimer.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/control/SubarrayTrackerThread.h"
#include "carma/control/AntennaControls.h"
#include "carma/control/DriveHandle.h"
#include "carma/services/Atmosphere.h"
#include "carma/interferometry/DelayEngine.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/services/DecAngle.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::interferometry;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace log4cpp;


namespace {

const Trace::TraceLevel kTraceLevel = Trace::TRACE4;

const string subarrayTrackerName = "SubarrayTrackerThread";


string
getInitialNdcString( const SubarrayControlImpl & saCI ) {
    return saCI.getAlphanumericName() + " SAT";
}


}  // namespace < anonymous >


// public

SubarrayControlImpl::TrackerThread::TrackerThread (
    SubarrayControlImpl & saCI,
    CarmaMonitorSystem &  carmaMonitor,
    const unsigned long   period,
    const unsigned long   antennaWeatherPeriod,
    const unsigned long   delayEngineWeatherPeriod,
    const unsigned long   antennaTrackPeriod ) :
carmaMonitor_ (carmaMonitor),
period_(period),
antennaWeatherPeriod_ (antennaWeatherPeriod),
delayEngineWeatherPeriod_ (delayEngineWeatherPeriod),
antennaTrackPeriod_ (antennaTrackPeriod),
antennaWeatherCycleCount_ (antennaWeatherPeriod/period),
delayEngineWeatherCycleCount_ (delayEngineWeatherPeriod/period),
antennaTrackCycleCount_ (antennaTrackPeriod/period),
interpInterval_(static_cast<double>(period)/Time::SECONDS_PER_DAY),
lastAntUpdate_( 0.0 ),
lastIntferUpdate_( 0.0 ),
timer_ (0),
saCI_( saCI ),
airTemp_(Atmosphere::DEFAULT_AIR_TEMP,"K"),
dewTemp_(Atmosphere::DEFAULT_DEW_POINT,"K"), // DEFAULT_DEW_POINT assumes 
                                             // DEFAULT_RH and DEFAULT_AIR_TEMP
atmPressure_(Atmosphere::DEFAULT_ATM_PRESSURE,"mbar"),
windSpeed_(0.0,"mph"),
windDirection_(0.0,"degrees"),
relHumid_(Atmosphere::DEFAULT_RH)
{
    nextFiringTime_ = getNextTimerInstant();
    timer_          = new PeriodicTimer(nextFiringTime_);
    timer_->ResetNextFireAbsoluteTime (nextFiringTime_);
}


SubarrayControlImpl::TrackerThread::~TrackerThread( )
try {
    delete timer_;
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


long
SubarrayControlImpl::TrackerThread::getTrackerPeriod () const
{
    return period_;
}

long
SubarrayControlImpl::TrackerThread::getAntennaTrackPeriod () const
{
    return antennaTrackPeriod_;
}

long
SubarrayControlImpl::TrackerThread::getAntennaWeatherPeriod () const
{
    return antennaWeatherPeriod_;
}


long
SubarrayControlImpl::TrackerThread::getDelayEngineWeatherPeriod () const
{
    return delayEngineWeatherPeriod_;
}


double
SubarrayControlImpl::TrackerThread::getInterpolationInterval () const
{
    return interpInterval_;
}

double
SubarrayControlImpl::TrackerThread::getAntInterpolationInterval () const
{
    return interpInterval_*antennaTrackCycleCount_;
}

void
SubarrayControlImpl::TrackerThread::setLastAntUpdate(double mjd)
{
    lastAntUpdate_ = mjd;
}

double
SubarrayControlImpl::TrackerThread::getLastAntUpdate() const
{
    return lastAntUpdate_;
}


void
SubarrayControlImpl::TrackerThread::setLastIntferUpdate(double mjd)
{
    lastIntferUpdate_ = mjd;
}

double
SubarrayControlImpl::TrackerThread::getLastIntferUpdate() const
{
    return lastIntferUpdate_;
}


CarmaMonitorSystem &
SubarrayControlImpl::TrackerThread::getMonitorSystem () const 
{
    return carmaMonitor_;
}


// protected


struct ::timeval
SubarrayControlImpl::TrackerThread::getNextTimerInstant() const
{
    struct ::timeval  nextFireTime;
    // The periods must be synchronous with absolute MJD, not absolute
    // Unix time (MJD1970)
    const double mjd1970 = Time::MJD1970*Time::SECONDS_PER_DAY;

    timerclear(&nextFireTime);
    gettimeofday(&nextFireTime, 0);
    double now  = nextFireTime.tv_sec + 1E-6*nextFireTime.tv_usec;  // unix in secs
    double nextmjd = period_ * (round((now+mjd1970)/period_) + 1) ; // mjd n secs
    double next    = nextmjd - mjd1970;  // unix time
    int iNext      = static_cast<int>(next);
    nextFireTime.tv_sec   = iNext ;
    nextFireTime.tv_usec  = 0;

    return nextFireTime;
}



void 
SubarrayControlImpl::TrackerThread::updateWeatherSafely()
{
    const ScopedLogNdc ndc( "SAT::updateWeatherSafely" );
    CARMA_CPTRACE(kTraceLevel, "Entering");
    carmaMonitor_.readNewest();
    WeatherSubsystem& weather = carmaMonitor_.weather();
    MonitorPoint::VALIDITY valid = weather.ambientTemperature().getValidity(); 
    if (    valid == MonitorPoint::INVALID_HW_BAD
         || valid == MonitorPoint::INVALID_NO_DATA 
         || valid == MonitorPoint::INVALID_NO_HW
       ) 
    {
        // in the case of invalid monitor points, do nothing.
        // the last good measured weather data will be retained.
        return;
    }

    double ambTemp = weather.ambientTemperature().getValue();

    // we shouldnt have to do this, but want to check
    // where we is getting junk from.
    if ( isnan( ambTemp ) ) {
        programLogWarnIfPossible("Ambient temperature is NaN");
        Backtrace bt;
        bt.captureNoThrow();
        string btText = bt.formattedAsString( "    ", "\n" );
        logMultipleLines(Program::getLogger(), Priority::ERROR, btText);
        return;
    }

    double dewTemp = weather.dewpointTemperature().getValue();
    if ( isnan( dewTemp ) ) {
    programLogWarnIfPossible("DewPoint temperature is NaN");
    Backtrace bt;
    bt.captureNoThrow();
    string btText = bt.formattedAsString( "    ", "\n" );
    logMultipleLines(Program::getLogger(), Priority::ERROR, btText);
    return;
    }
    double atmPres = weather.pressure().getValue();
    if ( isnan( atmPres ) ) {
    programLogWarnIfPossible("Atm Pressure is NaN");
    Backtrace bt;
    bt.captureNoThrow();
    string btText = bt.formattedAsString( "    ", "\n" );
    logMultipleLines(Program::getLogger(), Priority::ERROR, btText);
    return;
    }
    double wind    = weather.windSpeed().getValue();
    if ( isnan( wind ) ) {
    programLogWarnIfPossible("Wind speed is NaN");
    Backtrace bt;
    bt.captureNoThrow();
    string btText = bt.formattedAsString( "    ", "\n" );
    logMultipleLines(Program::getLogger(), Priority::ERROR, btText);
    return;
    }
    // % relative humidity
    double humidity = weather.humidity().getValue(); 
    if ( isnan( humidity ) ) {
    Backtrace bt;
    bt.captureNoThrow();
    string btText = bt.formattedAsString( "    ", "\n" );
    logMultipleLines(Program::getLogger(), Priority::ERROR, btText);
    return;
    }

    airTemp_.reset( ambTemp, "Celsius") ;
    dewTemp_.reset( dewTemp, "Celsius" );
    atmPressure_.reset( atmPres, "mbar");
    windSpeed_.reset( wind ,"mph" );

    // pass the weather data through Atmosphere's safe routines
    // to ensure that client gets reasonble weather data.
    airTemp_     = atm_.safeAirTemperature( airTemp_ );
    atmPressure_ = atm_.safeAtmPressure( atmPressure_ );
    relHumid_    = atm_.safeRelativeHumidity ( humidity );
    windSpeed_   = atm_.safeWindSpeed( windSpeed_ );
    dewTemp_     = atm_.safeDewPoint( dewTemp_ );

    // check that dewTemp_ is internally self-consistent.
    bool consistent 
    = atm_.isConsistentDewPoint( dewTemp_, airTemp_, relHumid_);
    if ( ! consistent ) {
    ostringstream dp;
    dp << " Dew point temperature ["
       << dewTemp_
       << "] inconsistent with air temperature ["
       << airTemp_
       << "] and relative humidity ["
       << relHumid_
       << "]. Recomputed value: [" 
       ;
    double dpK = atm_.computeDewPoint( airTemp_.kelvin(), relHumid_ );
    dewTemp_.reset( dpK, "K" );
    dp << dewTemp_ << "].";
    programLogWarnIfPossible( dp.str() );
    }

    windDirection_.reset(weather.windDirection().getValue(), "degrees");

    CARMA_CPTRACE(kTraceLevel, "Exiting");
}

void
SubarrayControlImpl::TrackerThread::updateTracking( )
{
    const ScopedLogNdc ndc( "SAT::updateTracking" );

    const double now = Time::MJD();

    CARMA_CPTRACE(kTraceLevel, 
        " begins: mjd=" 
        << setiosflags(ios::fixed)
        << setprecision(5) << now
        << " secsInDay="  << setprecision(2) 
        << Time::SECONDS_PER_DAY*(now-static_cast<int>(now)) );

    // static method variables are only assigned the first time around.
    // this really should be in an unnamed namespace
    static unsigned long antennaWeatherUpdateCounter = 0;

    const bool updateAntennaWeather = (antennaWeatherUpdateCounter == 0) ;

    const double antInterpInterval = getAntInterpolationInterval();
    // in steady state we should be 1.5 interpIntervals ahead of 
    // current time
    // The QI will then contain -0.5, +0.5, +1.5 (see class documentation).
    // We need times for antennas and for interferometry tracking.
    const double mjdAnt    = (now + 1.5*antInterpInterval);
    const double mjdIntfer = (now + 1.5*interpInterval_);
    const bool updateAntennaTracks = 
        (now - lastAntUpdate_) > (antInterpInterval - interpInterval_/2);
    const bool updateInterferometryPositions  = 
        (now - lastIntferUpdate_) > interpInterval_/2;
    if (updateAntennaTracks) setLastAntUpdate(now);
    if (updateInterferometryPositions)  setLastIntferUpdate(now);
    // want just the fractional day part for printing
    CARMA_CPTRACE(kTraceLevel,  " mjd="             
        << setiosflags(ios::fixed)
        << setprecision(5) << now
        << " intferInterval=" << setprecision(5) << interpInterval_
        << " mjdIntfer="      << setprecision(5) << mjdIntfer
        << "=day+"  << setprecision(1)
        << Time::SECONDS_PER_DAY * (mjdIntfer - floor(mjdIntfer))
        << "sec" );

    updateWeatherSafely();

    // Note: this does NOT cause a discontinuity in the delays,
    // under the assumption that weather varies smoothly
    CARMA_CPTRACE( kTraceLevel, "SAT calling getDelayEngine" );
    DelayEngine * de = saCI_.getDelayEngine();
    CARMA_CPTRACE( kTraceLevel, "SAT calling DE->setWeather" );
    de->setWeather(airTemp_, atmPressure_, relHumid_);

    typedef SubarrayControlImpl::DriveGroup DriveGroup;

    const DriveGroup driveGroup = saCI_.getDriveGroup( "< SAT >" );
        
    if ( updateAntennaWeather ) { 
        ostringstream weatherMsg;
        weatherMsg << "Sending weather to drives ";

        DriveGroup::const_iterator h = driveGroup.begin( );
        const DriveGroup::const_iterator hEnd = driveGroup.end( );

        for ( ; h != hEnd; ++h ) {
            DriveHandle * const driveHandle = *h;
            if ( driveHandle == 0 )
                continue;

            weatherMsg << driveHandle->getCarmaAntennaNo( ) << " ";
        } 

        weatherMsg << ": [RH = "
            << relHumid_ << " , Tatm = "
            << airTemp_.celsius() << " C , Tdew ="
            << dewTemp_.celsius() << " C , Patm = "
            << atmPressure_.millibar() << " mbar , Vwind ="
            << windSpeed_.mph() << " mph, WindDir = "
            << windDirection_.degrees() << " deg ]";

        //programLogInfo( weatherMsg.str() );
    }

    DriveGroup::const_iterator i = driveGroup.begin( );
    const DriveGroup::const_iterator iEnd = driveGroup.end( );
       
    for ( ; i != iEnd; ++i ) {
    DriveHandle * const driveHandle = *i;
    
    if ( driveHandle == 0 )
        continue;
        
    const unsigned short carmaAntNo = driveHandle->getCarmaAntennaNo( );
        
    string suffix;
    {
        ostringstream oss;
        oss << " on carma antenna #" << carmaAntNo;
        suffix = oss.str( );
    }

    // update the weather parameters for the drive before updating
    // the source position, so that the drive's internal refraction 
    // correction will get the latest weather.
    if ( updateAntennaWeather ) {
        CARMA_CPTRACE( kTraceLevel, "calling updateWeather" << suffix );

        driveHandle->updateWeather ( now, 
                     airTemp_,
                     atmPressure_,
                     relHumid_,
                     dewTemp_, 
                     windSpeed_,
                     windDirection_,
                     false 
                    );

        CARMA_CPTRACE( kTraceLevel,
               "returned from updateWeather" << suffix );
    }


    if (updateAntennaTracks) { 
        CARMA_CPTRACE( kTraceLevel,
               " calling updateTracking" << suffix );
        driveHandle->updateTracking( mjdAnt );
        CARMA_CPTRACE( kTraceLevel,
               "returned from updateTracking" << suffix );
    }

    // This branch is currently always true since our delay engine 
    // interval is equal to the thread cycle time.  But leave it in, 
    // if for some reason we change one timescale or the other. 
    if (updateInterferometryPositions)  { 

    //@TODO REFACTOR THIS WITH SubarrayControl::renewDelays()
        
        // Note this is a copy of the driveHandle's ephemeris, 
        // to we are not messing with the time of it's 
        // private member Ephemeris.

        Ephemeris ephem = driveHandle->getEphemeris();
        ephem.setMJD( mjdIntfer );

        // Depending on whether the drive is in AZEL or RADEC
        // mode, we will make different calls the the delay engine.
        // If IDLE mode, make no call to the delay engine.
        DriveHandle::SourceMode mode = driveHandle->getSourceMode();
        const string sourceName = driveHandle->getSourceName() ;

        switch ( mode ) {

        case DriveHandle::SOURCE_MODE_RA_DEC :
        {
        //
        //  Set the new mjd of ephemeris.
        //  Get apparent ra and dec pointing centers for this 
        //  antenna at mjd from ephemeris.
        //
        const double antennaRa  = ephem.getRa();
        const double antennaDec = ephem.getDec();
        string rastr = StringUtils::hms(antennaRa,2);
        string decstr = DecAngle(antennaDec,"radians").dms(2);
        
        CARMA_CPTRACE( kTraceLevel,
            " - DE->setAntennaRaDec("
            << carmaAntNo << "," 
            << setprecision(10) 
            << mjdIntfer     << ","
            << rastr      << ","
            << decstr     << ","
            << rastr   << ","
            << decstr  << ","
            << sourceName
            << ")"
        );
            
        // Note delay phase and tracking offsets will get set 
        // to zero inside this call for calculation of delay.
        // This is okay because we are passing in the total
        // ra/dec which include offsets.
        CARMA_CPTRACE( kTraceLevel, " SAT: SOURCEMODE RADEC CALL TO DE");
        de->setAntennaRaDec( carmaAntNo,
                     mjdIntfer,
                     antennaRa, antennaDec,
                     antennaRa, antennaDec,
                     false,
                     sourceName
                     );
        }
        break;

        case DriveHandle::SOURCE_MODE_AZ_EL:
         // for calculating delays, 
         // choose az and el between 0-2PI, 0-PI respectively
         // [param=true]
        CARMA_CPTRACE( kTraceLevel, " SAT: SOURCEMODE AZEL CALL TO DE");
        {
        double azRadians = 
            ephem.getSource().getXCoordinate().radians(true);
        double elRadians = 
            ephem.getSource().getYCoordinate().radians(true);
        de->setAntennaAzEl( carmaAntNo,
                     mjdIntfer,
                     azRadians, elRadians,
                     azRadians, elRadians
                     );
        }
        break;

         case DriveHandle::SOURCE_MODE_IDLE:
         // Do nothing for IDLE or unrecognized mode.
        break;
         default:
         // Complain if mode is unrecognized.
        ostringstream os;
        os << ": Unrecognized drive mode: " << mode <<". Ignoring it.";
        programLogErrorIfPossible( os.str() );
        break;
        } // end switch


        /*
###########################################################
        const double antennaRa  = delayEphem.getRa();
        const double antennaDec = delayEphem.getDec();
        DriveHandle::SourceMode mode = driveHandle->getSourceMode();
        de->setAntennaRaDec( carmaAntNo, mjdIntfer,
            antennaRa, antennaDec, antennaRa, antennaDec, false,
            saCI_.sourceName() );
###########################################################
    */

        }
    } // DriveHandle loop

    // ONLY UPDATE DOPPLER TRACKING ONCE!
    if (updateAntennaTracks) { 
        // Doppler frequency interpolator is updated on the
        // antenna 8 minute cycle.  This is a convenient timescale.
        // See Tracking Update document.
        CARMA_CPTRACE( kTraceLevel, " calling extendDopplerInterpolator" );
        saCI_.extendDopplerInterpolator( mjdAnt );
        CARMA_CPTRACE( kTraceLevel, "returned from extendDopplerInterpolator" );
    }

    if (updateInterferometryPositions)  { 
        // This must happen on a 20 second timescale to keep
        // the frequency error under 500 Hz. See Tracking Update
        // document.  Note if our finest spectral resolution (channel)
        // goes below 5kHz, this update interval must drop
        // commensurately.
        saCI_.updateLo1DopplerFrequency(mjdIntfer);
        // This needs to be done every 8 minutes, but uses the results of
        // the updated LO1, hence its placement here
        if (updateAntennaTracks) { 
            saCI_.updateLo2DopplerFrequency();
        }
    }
    

    if ( saCI_.isInitialized() ) {
        const DelayFrameVec delayFrames = de->computeDelays();
        saCI_.broadcastDelayData( delayFrames );
    }

    if (antennaWeatherCycleCount_ > 0)  {
        antennaWeatherUpdateCounter = 
            (antennaWeatherUpdateCounter +1) % antennaWeatherCycleCount_;
    }

    // finally, update all the relevant monitor points.
    saCI_.updateShortTermMonitorPoints(now);
    
    /*
    if ( updateAntennaTracks ) {
        programLogInfoIfPossible(
            "SAT last ant update was set to " +
            Time::getTimeString( now, 1 ) );
    }
    */
}


void
SubarrayControlImpl::TrackerThread::operator( ) ( )
{
    const ScopedLogNdc ndc( "SAT::operator()()" );

    while ( true ) {

        CARMA_CPTRACE( Trace::TRACE1, "Calling updateTracking()" );

        try {

            boost::this_thread::interruption_point();

            boost::recursive_mutex::scoped_lock scopelock(saCI_.trackerThreadSyncMutex_);

            updateTracking();
            
            boost::this_thread::interruption_point();
        } catch ( ... ) {
            try {
                programLogErrorIfPossible(
                        "Caught exception syncing and/or calling updateTracking: " +
                        getStringForCaught() );
            } catch ( ... ) {
                // Just stifle any exception
            }
        }


        CARMA_CPTRACE( Trace::TRACE1, "Calculating next timer firing time" );

        // fire again on next timing interval
        nextFiringTime_ = getNextTimerInstant();

        CARMA_CPTRACE( Trace::TRACE1, "Waiting on timer_" );

        boost::this_thread::interruption_point();

        timer_->ResetNextFireAbsoluteTimeAndWait( nextFiringTime_ );

        boost::this_thread::interruption_point();

        CARMA_CPTRACE( Trace::TRACE1, "End" );
    } // while ( true )
}
