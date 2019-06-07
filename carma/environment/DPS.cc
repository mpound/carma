/**
 * Dew Point Sensor Reader Implementation
 *
 * @author Peter Teuben
 * @author Ira W. Snyder
 */

#include <iostream>
#include <cmath>

#include <boost/regex.hpp>

#include <carma/util/Time.h>
#include <carma/util/Program.h>
#include <carma/util/Logger.h>
#include <carma/util/Trace.h>
#include <carma/util/ScopedPthreadMutexLock.h>
#include <carma/util/StartPthread.h>
#include <carma/util/programLogging.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/environment/DPS.h>
#include <carma/environment/SerialPortAccessor.h>
#include <carma/services/Physical.h>

using namespace std;
using namespace carma::util;
using namespace carma::environment;
using namespace carma::services;

namespace carma {
namespace environment {

/*----------------------------------------------------------------------------*/
/* Override the SerialPortAccessor class for DPS settings                     */
/*----------------------------------------------------------------------------*/

/*
 * Dew Point Sensor is a read-only device which automatically
 * sends data every 2 seconds.
 *
 * The serial port settings are:
 * 600 baud, 8N1
 * canonical mode, terminated by newlines (default)
 */
class DPSSerialAccessor : public SerialPortAccessor
{
public:
    DPSSerialAccessor(const std::string &device);

protected:
    void setup(struct termios *tio);
};

DPSSerialAccessor::DPSSerialAccessor(const std::string &device)
    : SerialPortAccessor(device, O_RDONLY | O_NONBLOCK | O_NOCTTY)
{
    // intentionally left empty
}

void DPSSerialAccessor::setup(struct termios *tio)
{
    // clear termios
    memset(tio, 0, sizeof(*tio));

    // 600 8N1, no RTS/CTS, ignore modem control lines
    tio->c_cflag = B600 | CS8 | CLOCAL | CREAD | HUPCL;
    tio->c_iflag = IGNPAR;
    tio->c_oflag = IGNPAR;

    /* set input mode (canonical, no echo, ...) */
    tio->c_lflag = ICANON | NOFLSH;

    /* inter-character timer unused */
    tio->c_cc[VTIME] = 0;

    /* blocking read until 1 char has been rx'd */
    tio->c_cc[VMIN] = 1;
}

/*----------------------------------------------------------------------------*/
/* Dew Point Sensor Data                                                      */
/*----------------------------------------------------------------------------*/

DPSData::DPSData()
    : timestamp(0)
    , ambientTemperature(0.0)
    , dewpointTemperature(0.0)
    , humidity(0.0)
    , valid(false)
{
    // intentionally left empty
}

/*----------------------------------------------------------------------------*/
/* Dew Point Sensor Reader                                                    */
/*----------------------------------------------------------------------------*/

DPS::DPS(const std::string &device, bool reopen, bool emulate)
    : device_(device)
    , reopen_(reopen)
    , emulate_(emulate)
    , group_()
    , atm_()
    , mutex_()
    , data_()
{
    // intentionally left empty
}

void DPS::getLatestData(DPSData &data)
{
    ScopedPthreadMutexLock lock(this->mutex_);
    data = this->data_;
}

void DPS::startReaderThread()
{
    ::pthread_t thread = StartPthreadWithRef(this->readerThreadEP, *this);
    this->group_.insert(thread);
}

#define CPTRACE1(args...) CPTRACE(Trace::TRACE1, args)

static double celsius(double f) { return (f - 32.0) * 5.0 / 9.0; }

static std::string buf_as_hex(const char *buf, ssize_t len)
{
    std::ostringstream oss;

    for (int i = 0; i < len; i++)
        oss << "%.2x " << (uint8_t)buf[i];

    return oss.str();
}

bool DPS::parseData(const char *buf, ssize_t bytes)
{
    /*
     * This may look bad, but it is not too hard to understand. It
     * is broken into several parts. Each part may be parsed out into
     * a group (the "what" variable).
     *
     * - whitespace
     * - a literal "\x02"
     * - a literal "T"
     * - a plus or minus sign           GROUP #1
     * - a literal space
     * - a number with optional dot     GROUP #2
     * - a literal "/D"
     * - a plus or minus sign           GROUP #3
     * - a literal space
     * - a number with optional dot     GROUP #4
     * - a literal space
     * - a literal "P", "H", or "F"     GROUP #5
     * - two hex digits (checksum)
     * - a literal "\x03"
     * - whitespace
     *
     * The lines returned by the weather station look like:
     * "\x02T+ 46.2/D- 05.4 P60\x03"
     *
     * This corresponds to:
     * - ambient temperature of +46.2 degF
     * - dewpoint temperature of -5.4 degF
     * - normal mode (P == normal, H == heat, F == fault)
     * - checksum 0x60
     */
    const boost::regex re("\\s*\x02T([+-]) ([0-9.]+)/D([+-]) ([0-9.]+) ([PHF])[0-9A-F]{2}\x03\\s*");
    const std::string input(buf, bytes);
    boost::smatch what;
    std::string s;

    // if the match fails, return false immediately
    if (boost::regex_match(input, what, re) == false)
        return false;

    // it matched, parse the data
    float atempF, dtempF;

    s = what[1].str() + what[2].str();
    atempF = atof(s.c_str());
    CPTRACE1("amb temp degF: " + s);

    s = what[3].str() + what[4].str();
    dtempF = atof(s.c_str());
    CPTRACE1("dew temp degF: " + s);

    const std::string status = what[5].str();
    CPTRACE1("status: " + status);

    // convert to celsius
    float atempC, dtempC;
    atempC = celsius(atempF);
    dtempC = celsius(dtempF);

    // calculate humidity
    // arguments first need to be converted from Celsius to Kelvin
    const float atempK = atempC - carma::services::constants::Physical::ABS_ZERO;
    const float dtempK = dtempC - carma::services::constants::Physical::ABS_ZERO;
    const float humidity = this->atm_.computeHumidity(atempK, dtempK);

    // save the data for readers
    ScopedPthreadMutexLock lock(this->mutex_);
    this->data_.timestamp = carma::util::Time::computeCurrentFrame();
    this->data_.ambientTemperature = atempC;
    this->data_.dewpointTemperature = dtempC;
    this->data_.humidity = humidity;
    this->data_.valid = (status == "P");

    return true;
}

void DPS::readerEmulateMode()
{
    // set some initial emulated values
    this->data_.ambientTemperature = 5.0;
    this->data_.dewpointTemperature = 10.0;
    this->data_.humidity = 15.0;

    float count = 0;
    const float TWOPI = 2.0*M_PI;
    const float AMPLITUDE = 5.0;
    const float WEATHER_SLEEP = 2.0;
    while (true) {
        // sleep 2 seconds to emulate the normal time interval
        sleep( WEATHER_SLEEP );

        // lock and update the data
        ScopedPthreadMutexLock lock(this->mutex_);
        this->data_.timestamp = carma::util::Time::computeCurrentFrame();

        // sinusoid with 1 hour period, to keep these values from
        // incrementing forever to non-physical values.
        float arg = count *TWOPI;
        float increment = AMPLITUDE * std::sin( arg );
        this->data_.ambientTemperature  =  5 + increment;
        this->data_.dewpointTemperature = 10 + increment;
        this->data_.humidity            = 15 + increment;
        this->data_.valid = true;
        count += (WEATHER_SLEEP/3600.0);
        if ( count >= 1.0) count -= 1.0;
    }
}

void DPS::readerNormalMode()
{
    const std::string &dev = this->device_;
    DPSSerialAccessor serial(dev);

    // open the device
    try {
        serial.open();
    } catch (...) {
        std::ostringstream oss;
        oss << "failed to open " << dev << ": " << getStringForCaught();
        programLogErrorIfPossible(oss.str());
    }

    // run forever
    while (true) {
        try {
            char buf[256];
            ssize_t bytes;

            // close the device if it should be re-opened every
            // iteration of the loop
            if (this->reopen_)
                serial.close();

            // this is ignored if the device is already open
            serial.open();

            // wait up to 5 seconds to recv output
            bytes = serial.recv(buf, sizeof(buf), 5000);
            if (bytes <= 0) {
                programLogErrorIfPossible("error: no bytes received from DPS");
                continue;
            }

            // parse the data
            bool b = this->parseData(buf, bytes);
            if (b == false) {
                programLogErrorIfPossible("error: unable to parse input string");
                programLogErrorIfPossible("hex: " + buf_as_hex(buf, bytes));
                programLogErrorIfPossible("str: " + std::string(buf, bytes));
                continue;
            }

        } catch (...) {
            std::ostringstream oss;
            oss << "error: " << getStringForCaught();
            programLogErrorIfPossible(oss.str());
        }
    }
}

void DPS::readerThreadEP(DPS &This)
{
    if (This.emulate_)
        This.readerEmulateMode();
    else
        This.readerNormalMode();
}

} // namespace environment
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 et: */
