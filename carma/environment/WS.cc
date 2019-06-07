/**
 * Weather Station Reader Implementation
 *
 * @author: Peter Teuben
 * @author: Ira W. Snyder
 * @version: $Id: WS.cc,v 1.47 2013/08/22 13:16:42 mpound Exp $
 */
#include <iostream>
#include <iomanip>
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
#include <carma/environment/WS.h>
#include <carma/environment/SerialPortAccessor.h>
#include <carma/services/Physical.h>

using namespace std;
using namespace carma::environment;
using namespace carma::services;
using namespace carma::util;

static const int WEATHER_SLEEP = 3;

// approx 10 minutes worth of samples, to match the MPML documentation for
// the monitor points derived from this data
static const size_t MAX_WIND_MEASUREMENTS = 10 * 60 / 3;

#define CPTRACE1(args...) CPTRACE(Trace::TRACE1, args)

namespace carma {
namespace environment {

/*----------------------------------------------------------------------------*/
/* Weather Station Data Container                                             */
/*----------------------------------------------------------------------------*/

WSData::WSData()
    : timestamp(0)
    , ambientTemperature(0.0)
    , dewpointTemperature(0.0)
    , pressure(0.0)
    , humidity(0.0)
    , windSpeed(0.0)
    , peakWindSpeed(0.0)
    , averageWindSpeed(0.0)
    , averageWindDirection(0.0)
    , windDirection(0.0)
    , battery(0.0)
    , waterVaporDensity(0.0)
    , waterColumn(0.0)
    , weatherStationTime("00:00:00")
{
    // intentionally left empty
}

/*----------------------------------------------------------------------------*/
/* Weather Station Serial Port Accessor                                       */
/*----------------------------------------------------------------------------*/

class WSSerialAccessor : public SerialPortAccessor
{
    public:
        WSSerialAccessor(const std::string &device);

    private:
        void setup(struct termios *tio);
};

WSSerialAccessor::WSSerialAccessor(const std::string &device)
    : SerialPortAccessor(device, O_RDWR | O_NOCTTY | O_NONBLOCK)
{
    // intentionally left empty
}

void WSSerialAccessor::setup(struct termios *tio)
{
	// clear termios
	memset(tio, 0, sizeof(*tio));

	//tio->c_cflag = B115200 | CRTSCTS | CS8 | CLOCAL | CREAD;
	tio->c_cflag = B9600 | CS8 | CLOCAL | CREAD | HUPCL;
	tio->c_iflag = IGNPAR;
	tio->c_oflag = IGNPAR;

	/* set input mode (canonical mode, no echo, ...) */
	tio->c_lflag = NOFLSH | ICANON;

	/* inter-character timer unused */
	tio->c_cc[VTIME] = 0;

	/* blocking read until 1 char has been rx'd */
	tio->c_cc[VMIN] = 1;
}

/*----------------------------------------------------------------------------*/
/* Local Helper Functions                                                     */
/*----------------------------------------------------------------------------*/

static float deg2rad(const float val)
{
    return val * M_PI / 180.0;
}

static float rad2deg(const float val)
{
    return val * 180.0 / M_PI;
}

static float CtoK(const float val)
{
    return val - carma::services::constants::Physical::ABS_ZERO;
}

static float KtoC(const float val)
{
    return val + carma::services::constants::Physical::ABS_ZERO;
}

/*----------------------------------------------------------------------------*/
/* Weather Station Reader                                                     */
/*----------------------------------------------------------------------------*/

WS::WS(const std::string &device, bool reopen, bool emulate)
    : device_(device)
    , reopen_(reopen)
    , emulate_(emulate)
    , group_()
    , atm_()
    , serial_()
    , windSpeeds_(MAX_WIND_MEASUREMENTS)
    , windDirections_(MAX_WIND_MEASUREMENTS)
    , mutex_()
    , data_()
{
    // intentionally left empty
}

void WS::getLatestData(struct WSData &data)
{
    // handle everything under the lock
    ScopedPthreadMutexLock lock(this->mutex_);

    float averageWindDirection = 0.0;
    float averageWindSpeed = 0.0;
    float peakWindSpeed = 0.0;
    float xsum = 0.0;
    float ysum = 0.0;

    const size_t nwinds = std::min(this->windSpeeds_.size(), this->windDirections_.size());
    for (size_t i = 0; i < nwinds; i++) {
        const float speed = this->windSpeeds_.at(i);
        const float dir = this->windDirections_.at(i);

        averageWindSpeed += speed;
        peakWindSpeed = std::max(peakWindSpeed, speed);

        xsum += speed * cos(deg2rad(dir));
        ysum += speed * sin(deg2rad(dir));
    }

    if (nwinds > 0) {
        averageWindSpeed /= nwinds;
        averageWindDirection = rad2deg(atan2(ysum, xsum));
    }

    if (averageWindDirection < 0.0)
        averageWindDirection += 360.0;

    // convert Celcius to Kelvin
    const float ambientTemperature = this->data_.ambientTemperature;
    const float humidity = this->data_.humidity;
    const float dewpointTemperature = KtoC(atm_.computeDewPoint(CtoK(ambientTemperature), humidity));

    // calculate water column
    const float waterColumn = atm_.waterColumn(CtoK(ambientTemperature), humidity);

    // save data
    this->data_.averageWindDirection = averageWindDirection;
    this->data_.averageWindSpeed = averageWindSpeed;
    this->data_.peakWindSpeed = peakWindSpeed;
    this->data_.dewpointTemperature = dewpointTemperature;
    this->data_.waterColumn = waterColumn;

    // copy data
    data = this->data_;
}

void WS::startThreads()
{
    ::pthread_t thread;

    // no emulation, open the serial port
    if (!this->emulate_) {
        serial_ = boost::shared_ptr<WSSerialAccessor>(new WSSerialAccessor(device_));
        try {
            serial_->open();
        } catch (...) {
            std::ostringstream oss;
            oss << "error: unable to open serial port: " << getStringForCaught();
            programLogErrorIfPossible(oss.str());

            // rethrow
            throw;
        }
    }

    // always need the reader thread
    thread = StartPthreadWithRef(this->readerThreadEP, *this);
    this->group_.insert(thread);

    // only need the writer thread in non-emulate mode
    if (!this->emulate_) {
        thread = StartPthreadWithRef(this->writerThreadEP, *this);
        this->group_.insert(thread);
    }
}

void WS::readerThreadEP(WS &This)
{
    if (This.emulate_)
        This.readerEmulateMode();
    else
        This.readerNormalMode();
}

void WS::writerThreadEP(WS &This)
{
    This.writerNormalMode();
}

bool WS::parseData(const char *buf, ssize_t bytes)
{
    const std::string input(buf, bytes);

    // match the empty string
    {
        const boost::regex re("\\s*");
        boost::smatch what;
        if (boost::regex_match(input, what, re)) {
            CPTRACE1("parse: found empty string");
            return true;
        }
    }

    // match anything beginning with "MODE" or "LOG" or "*" or ">"
    // with any set of trailing characters and whitespace
    {
        const boost::regex re("\\s*(MODE|LOG|\\*|>).*\\s*");
        boost::smatch what;
        if (boost::regex_match(input, what, re)) {
            CPTRACE1("parse: found mode/log/prompt string");
            return true;
        }
    }

    // match registers
    {
        const boost::regex re("\\s*(\\d{2}):([+-][0-9.]+)\\s*");
        boost::smatch what;
        if (boost::regex_match(input, what, re)) {
            const int regnum = atoi(what[1].str().c_str());
            const float regval = atof(what[2].str().c_str());

            // print a trace message
            std::ostringstream oss;
            oss << "parse: register " << regnum << " value=" << regval;
            CPTRACE1(oss.str());

            // handle the register data
            this->addRegisterData(regnum, regval);
            return true;
        }
    }

    // fallback: no matches
    return false;
}

void WS::addRegisterData(const int regnum, const float regval)
{
    ScopedPthreadMutexLock lock(this->mutex_);
    WSData &data = this->data_;

    switch (regnum) {
    case 1:
        data.battery = regval;
        break;
    case 3:
        data.windSpeed = regval;
        windSpeeds_.push_back(regval);
        break;
    case 4:
        data.windDirection = regval;
        windDirections_.push_back(regval);
        break;
    case 5:
        data.pressure = regval;
        break;
    case 6:
        data.ambientTemperature = regval;
        break;
    case 7:
        data.humidity = regval;
        break;
    case 14:
        data.waterVaporDensity = regval;
        break;
    case 20:
        // unused register, update the timestamp when we see it.
        // we work this way because it comes after all other registers
        // that we use: it is a good-enough indicator that we got the
        // whole set
        data.timestamp = Time::computeCurrentFrame();
        data.weatherStationTime = Time::getTimeString(data.timestamp, 0);
        break;
    default:
        // unused register
        break;
    }
}

static std::string buf_as_hex(const char *buf, ssize_t len)
{
    std::ostringstream oss;

    for (int i = 0; i < len; i++) {
        // Yes, all of these bullshit casts are needed to defeat the
        // C++ compiler's operator overloading of the stream insertion
        // operators.
        //
        // The first cast avoids a sign extension during the second cast,
        // which avoids (0x80 turning into 0xffffff80).
        //
        // The second cast is necessary to force the compiler to output
        // the byte as a hex number. If the type is left as an unsigned
        // char, the compiler will output the text representation rather
        // than the hex number.
        const unsigned char c = static_cast<unsigned char>(buf[i]);
        const unsigned int v = static_cast<unsigned int>(c);
        oss << std::setw(2) << std::setfill('0') << std::hex << v << " ";
    }

    return oss.str();
}

void WS::readerNormalMode()
{
    WSSerialAccessor &serial = *(this->serial_.get());

    while (true) {
        try {
            char buf[256];
            ssize_t bytes;

            // wait up to 5 seconds to recv output
            bytes = serial.recv(buf, sizeof(buf), 5000);
            if (bytes <= 0) {
                programLogErrorIfPossible("error: no bytes received from WS");
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

void WS::readerEmulateMode()
{
    WSData &data = this->data_;

    /* set some initial values */
    data.timestamp = Time::computeCurrentFrame();
    data.weatherStationTime = Time::getTimeString(data.timestamp, 0);

    float count = 0;
    const float TWOPI = 2.0*M_PI;
    const float AMPLITUDE = 5.0;
    while (true) {
        // sleep 3 seconds to emulate the normal time interval
        sleep(WEATHER_SLEEP);

        // lock and update the data
        ScopedPthreadMutexLock lock(this->mutex_);
        data.timestamp = Time::computeCurrentFrame();
        data.weatherStationTime = Time::getTimeString(data.timestamp, 0);

        // sinusoid with 1 hour period, to keep these values from
        // incrementing forever to non-physical values.
        float arg = count * TWOPI;
        float increment = AMPLITUDE * std::sin( arg );
        data.ambientTemperature   = 5 + increment; // same as DPS.cc
        data.dewpointTemperature  = 10.0 + increment; // same as DPS.cc
        data.humidity  =     15.0 + increment; // same as DPS.cc
        data.pressure  =     1000.0 + increment;
        data.windSpeed =     4.0 + increment;
        data.windDirection = 5.0 + increment;
        data.battery       = 6.0 + 0.1*increment;
        data.waterVaporDensity = 7 + increment;
        count += ( WEATHER_SLEEP/3600.0 );
        if ( count >= 1.0 ) count -= 1.0;

        this->windSpeeds_.push_back(data.windSpeed);
        this->windDirections_.push_back(data.windDirection);
    }
}

void WS::writerNormalMode()
{
    WSSerialAccessor &serial = *(this->serial_.get());
    std::string s;

    while (true) {
        try {
            // sleep 3 seconds to allow the weather station to calculate
            sleep(WEATHER_SLEEP);

            // Telecommunications Command State (CR10X 5-5 p75)
            s = "*0\r";
            serial.send_n(s.c_str(), s.size());

            // Remote Keyboard State (CR10X 5-5 p75)
            s = "7H\r";
            serial.send_n(s.c_str(), s.size());

            // View Input Storage Mode (CR10X OV-10 p22)
            s = "*6\r";
            serial.send_n(s.c_str(), s.size());

            // Read all 28 registers
            for (int i = 0; i < 28; i++) {
                s = "\r";
                serial.send_n(s.c_str(), s.size());

                // unnecessary, but nice to the weather station
                usleep(5 * 1000);
            }
        } catch (...) {
            std::ostringstream oss;
            oss << "error in write thread: " << getStringForCaught();
            programLogErrorIfPossible(oss.str());
        }
    }
}

} // namespace environment
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 et: */
