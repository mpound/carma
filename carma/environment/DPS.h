#ifndef CARMA_ENVIRONMENT_DPS_H
#define CARMA_ENVIRONMENT_DPS_H

/*
 * Dew Point Sensor class
 *
 * @author: Ira W. Snyder
 * @version: $Id: DPS.h,v 1.14 2011/05/04 20:31:42 iws Exp $
 *
 * $CarmaCopyright$
 */

#include <string>

#include <carma/util/types.h>
#include <carma/util/PthreadMutex.h>
#include <carma/util/AutoPthreadQuitAndJoinGroup.h>
#include <carma/services/Atmosphere.h>

namespace carma {
namespace environment {

/*----------------------------------------------------------------------------*/
/* Dew Point Sensor Data Container                                            */
/*----------------------------------------------------------------------------*/

struct DPSData
{
    DPSData();

    carma::util::frameType timestamp;
    float ambientTemperature;
    float dewpointTemperature;
    float humidity;
    bool valid;
};

/*----------------------------------------------------------------------------*/
/* Dew Point Sensor Reader                                                    */
/*----------------------------------------------------------------------------*/

class DPS
{
public:
    DPS(const std::string &device, bool reopen = false, bool emulate = false);

    void getLatestData(struct DPSData &data);
    void startReaderThread();

    static void readerThreadEP(DPS &This);

private:
    bool parseData(const char *buf, ssize_t bytes);
    void readerNormalMode();
    void readerEmulateMode();

    const std::string device_;
    const bool reopen_;
    const bool emulate_;
    carma::util::AutoPthreadQuitAndJoinGroup group_;
    carma::services::Atmosphere atm_;

    carma::util::PthreadMutex mutex_;
    DPSData data_;
};

} // namespace environment
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 et: */
#endif // CARMA_ENVIRONMENT_DPS_H
