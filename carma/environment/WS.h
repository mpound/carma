#ifndef CARMA_ENVIRONMENT_WS_H
#define CARMA_ENVIRONMENT_WS_H

#include <vector>

#include <boost/shared_ptr.hpp>
#include <boost/circular_buffer.hpp>

#include <carma/util/types.h>
#include <carma/util/Time.h>
#include <carma/util/PthreadMutex.h>
#include <carma/util/AutoPthreadQuitAndJoinGroup.h>
#include <carma/services/Atmosphere.h>

/**
 * Weather Station Controller
 *
 * @author: Ira Snyder
 * @verios: $Id: WS.h,v 1.24 2013/01/30 19:22:15 iws Exp $
 *
 * $CarmaCopyright$
 */

namespace carma {
namespace environment {

class WSSerialAccessor;

/*----------------------------------------------------------------------------*/
/* Weather Station Data                                                       */
/*----------------------------------------------------------------------------*/

struct WSData
{
    WSData();

    carma::util::frameType timestamp;
    float ambientTemperature;
    float dewpointTemperature;
    float pressure;
    float humidity;
    float windSpeed;
    float peakWindSpeed;
    float averageWindSpeed;
    float averageWindDirection;
    float windDirection;
    float battery;
    float waterVaporDensity;
    float waterColumn;
    std::string weatherStationTime;
};

/*----------------------------------------------------------------------------*/
/* Weather Station Reader                                                     */
/*----------------------------------------------------------------------------*/

class WS
{
public:
    WS(const std::string &device, bool reopen = false, bool emulate = false);

    void getLatestData(struct WSData &data);
    void startThreads();

    static void readerThreadEP(WS &This);
    static void writerThreadEP(WS &This);

private:
    void addRegisterData(const int regnum, const float regval);
    bool parseData(const char *buf, ssize_t bytes);
    void readerNormalMode();
    void readerEmulateMode();
    void writerNormalMode();

    const std::string device_;
    const bool reopen_;
    const bool emulate_;
    carma::util::AutoPthreadQuitAndJoinGroup group_;
    carma::services::Atmosphere atm_;
    boost::shared_ptr<WSSerialAccessor> serial_;

    boost::circular_buffer<float> windSpeeds_;
    boost::circular_buffer<float> windDirections_;

    carma::util::PthreadMutex mutex_;
    WSData data_;
};

} // namespace environment
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 et: */
#endif // CARMA_ENVIRONMENT_WS_H
