/**
 * Class to access weather data from WS and DPS
 *
 * @author: Peter Teuben
 * @author: Ira W. Snyder
 * @version: $Id: Weather.cc,v 1.35 2013/01/30 19:22:15 iws Exp $
 */

#include <iostream>
#include <sstream>
using namespace std;

#include <carma/util/Time.h>
#include <carma/util/Trace.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

#include <carma/services/Physical.h>
using namespace carma::services;
using namespace carma::services::constants;

#include <carma/environment/Weather.h>
using namespace carma::environment;

typedef carma::monitor::WeatherSubsystem::WeatherModeMonitorPointEnum WeatherModeEnum;

namespace carma {
namespace environment {

Weather::Weather(const std::string &WSdev, const std::string &DPSdev,
                 bool useWS, bool useDPS, bool reopen, bool emulate)
    : WSdev_(WSdev)
    , DPSdev_(DPSdev)
    , useWS_(useWS)
    , useDPS_(useDPS)
    , reopen_(reopen)
    , emulate_(emulate)
    , ws_()
    , dps_()
{
    // intentionally left empty
}

void Weather::copyWeatherStation(carma::monitor::WeatherSubsystem &wss)
{
    const carma::util::frameType currentFrame = carma::util::Time::computeCurrentFrame();
    const int MAX_FRAMES_LATE = 10;

    WSData data;
    this->ws_->getLatestData(data);

    // exit the function early if we have stale data
    const int frameDifference = currentFrame - data.timestamp;
    if (frameDifference > MAX_FRAMES_LATE) {
        std::ostringstream oss;
        oss << "weather station data stale: " << frameDifference << " frames late";
        programLogErrorIfPossible(oss.str());
        return;
    }

    /**
     * The following two constants are taken from ALMA memo 40,
     * appendix A.
     * http://www.alma.nrao.edu/memos/html-memos/abstracts/abs040.html
     *
     * TAU(225) = BETA_225 * mmH2O + OXYGEN_OPACITY
     * Eventually we can substitute a more sophisticated opacity
     * model, subclasses environment::OpacityModel.
     */

    /*
     * Constant to convert millimeters of water to opacity at 225 GHz, neper/mm
     */
    const float BETA_225 = 0.06;

    /* Constant oxygen opacity, neper */
    const float OXYGEN_OPACITY = 0.005;

    /* calculate tau225 */
    const double mmh2o = data.waterColumn;
    const double tau225 = BETA_225 * mmh2o + OXYGEN_OPACITY;

    // generic weather values
    wss.ambientTemperature().setValue(data.ambientTemperature);
    wss.ambientTempF().setValue(data.ambientTemperature*1.8+32);
    wss.dewpointTemperature().setValue(data.dewpointTemperature);
    wss.pressure().setValue(data.pressure);
    wss.humidity().setValue(data.humidity);
    wss.windSpeed().setValue(data.windSpeed);
    wss.windDirection().setValue(data.windDirection);
    wss.peakWindSpeed().setValue(data.peakWindSpeed);
    wss.averageWindSpeed().setValue(data.averageWindSpeed);
    wss.averageWindDirection().setValue(data.averageWindDirection);
    wss.waterDensity().setValue(data.waterVaporDensity);
    wss.precipWater().setValue(mmh2o);
    wss.tau225().setValue(tau225);
    wss.batteryVoltage().setValue(data.battery);

    // weather station specific data
    wss.weatherStation().ambientTemperature().setValue(data.ambientTemperature);
    wss.weatherStation().dewpointTemperature().setValue(data.dewpointTemperature);
    wss.weatherStation().humidity().setValue(data.humidity);
    wss.weatherStation().wSTime().setValue(data.weatherStationTime);
}

void Weather::copyDewPointSensor(carma::monitor::WeatherSubsystem &wss)
{
    const carma::util::frameType currentFrame = 
            carma::util::Time::computeCurrentFrame();
    const int MAX_FRAMES_LATE = 10;

    DPSData data;
    this->dps_->getLatestData(data);

    // exit the function early if we have stale data
    const int frameDifference = currentFrame - data.timestamp;
    if (frameDifference > MAX_FRAMES_LATE) {
        std::ostringstream oss;
        oss << "dew point sensor data stale: " << frameDifference 
            << " frames late";
        programLogErrorIfPossible(oss.str());
        return;
    }

    // exit the function early if the dew point sensor is in a
    // heating cycle (or has a fault)
    //
    // This will automatically fall back on the weather station for
    // these values, which is less accurate, but much better than
    // invalid data
    if (!data.valid) {
        std::ostringstream oss;
        oss << "dew point sensor data invalid: heat cycle?";
        programLogErrorIfPossible(oss.str());
        return;
    }

    // override generic weather values
    wss.ambientTemperature().setValue(data.ambientTemperature);
    wss.ambientTempF().setValue(data.ambientTemperature*1.8+32);
    wss.dewpointTemperature().setValue(data.dewpointTemperature);
    wss.humidity().setValue(data.humidity);

    // dew point sensor specific data
    wss.dewPointSensor().ambientTemperature().setValue(data.ambientTemperature);
    wss.dewPointSensor().dewpointTemperature().setValue(data.dewpointTemperature);
    wss.dewPointSensor().humidity().setValue(data.humidity);
}

void Weather::copyToMonitorSystem(carma::monitor::WeatherSubsystem &wss)
{
    /* copy the data from the weather station (always used) */
    this->copyWeatherStation(wss);

    /* copy the data from the dew point sensor */
    if (this->useDPS_) {
        this->copyDewPointSensor(wss);
        wss.weatherMode().setValue(WeatherModeEnum::DEW_POINT_SENSOR);
    } else {
        wss.weatherMode().setValue(WeatherModeEnum::WEATHER_STATION);
    }

    /* if we are emulating, this is a special mode */
    if (this->emulate_) {
        wss.weatherMode().setValue(WeatherModeEnum::EMULATE);
    }
}

void Weather::startThreads()
{
    // the weather station is always used
    this->ws_ = boost::shared_ptr<WS>(new WS(WSdev_, reopen_, emulate_));
    ws_->startThreads();

    if (this->useDPS_) {
        this->dps_ = boost::shared_ptr<DPS>(new DPS(DPSdev_, reopen_, emulate_));
        dps_->startReaderThread();
    }
}

} // namespace environment
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 et: */
