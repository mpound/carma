#ifndef CARMA_ENVIRONMENT_WEATHER_H
#define CARMA_ENVIRONMENT_WEATHER_H

/*
 * Weather Reader (Weather Station + Dew Point Sensor)
 *
 * @author: Peter Teuben
 * @author: Ira W. Snyder
 * @version: $Id: Weather.h,v 1.26 2011/05/05 16:43:01 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <carma/environment/WS.h>
#include <carma/environment/DPS.h>
#include <carma/services/Atmosphere.h>
#include <carma/util/Time.h>
#include <carma/monitor/WeatherSubsystem.h>

#include <boost/shared_ptr.hpp>

namespace carma {
namespace environment {

class Weather
{
public:
    /**
     * @brief Weather constructor
     * @param WSdev     serial port device for weather station
     * @param DPSdev    serial port device for dew point sensor
     * @param useWS     use weather station for ambient, dewpoint, and humidity
     * @param useDPS    use dew point sensor for ambient, dewpoint, and humidity
     * @param reopen    re-open the serial port after each cycle
     * @param emulate   emulate the devices
     */
    Weather(const std::string &WSdev, const std::string &DPSdev,
            bool useWS = true, bool useDPS = true,
            bool reopen = false, bool emulate = true);

    void copyToMonitorSystem(carma::monitor::WeatherSubsystem &wss);
    void startThreads();

private:
    void copyWeatherStation(carma::monitor::WeatherSubsystem &wss);
    void copyDewPointSensor(carma::monitor::WeatherSubsystem &wss);

    const std::string WSdev_;
    const std::string DPSdev_;
    const bool useWS_;
    const bool useDPS_;
    const bool reopen_;
    const bool emulate_;

    boost::shared_ptr<WS> ws_;
    boost::shared_ptr<DPS> dps_;
};

} // namespace environment
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 et: */
#endif // CARMA_ENVIRONMENT_WEATHER_H
