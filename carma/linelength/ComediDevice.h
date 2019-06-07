#ifndef CARMA_LINELENGTH_COMEDIDEVICE_H
#define CARMA_LINELENGTH_COMEDIDEVICE_H

#include <carma/util/PthreadMutex.h>
#include <carma/util/PthreadCond.h>

#include <boost/circular_buffer.hpp>
#include <boost/shared_ptr.hpp>
#include <comedilib.h>
#include <vector>

namespace carma {
namespace linelength {

/*
 * Simple container to hold all information about a sample from the DAQ card,
 * as obtained by the comedi library.
 *
 * The samples are already converted to volts using the units specified when
 * the capture was configured.
 *
 * TODO FIXME: Is this a better data structure?
 * typedef std::vector<double> DoubleVector;
 * std::vector<DoubleVector> scans;
 *
 * Then each vector in 'scans' has equal length, and is exactly comparable to
 * a single scan from the hardware. Maybe this makes the higher level software
 * easier to reason about. No weird index manipulation and multiplications
 * are needed!
 */
struct ComediSample
{
    ComediSample(const unsigned int nchannels, const unsigned int nsamples);
    std::vector<double> calculateAverage() const;

    const unsigned int nchannels;
    const unsigned int nsamples;

    std::vector<double> samples;
    std::vector<bool> dio;
};

typedef boost::shared_ptr<struct ComediSample> ComediSamplePtr;

/*
 * Class to control the comedi library/driver connected to a NI PXE-6031E
 * Data Acquisition card. Only the features necessary for use by CARMA are
 * exposed.
 *
 * No Digital I/O write capability has been exposed, since no CARMA applications
 * have found it necessary. If you do find this necessary in the future, I
 * suggest that this file be turned into a simple set of routines (not a class)
 * which wrap some comedi functionality. Then leave the rest to the user. The
 * comedi library is very simple to use once you read the documentation.
 *
 * This driver attempts to let you have full control of the streaming data
 * access from the card by exposing the raw comedi interface. Anything other
 * interface just looked like intentional obfuscation.
 *
 * You have to know how the card is wired up to use this class.
 */
class ComediDevice
{
    public:
    // Open the specified device file and configure the comedi device
    ComediDevice(const std::string &name);
    ~ComediDevice();

    // Find the optimal comedi range index which contains the specified min
    // and max voltages.
    int findRange(const double min, const double max);

    // Use external reference with specified period (default to 10MHz)
    void setupExternalReference(unsigned int period_ns = 100);

    // Configure device for operation and begin data capture
    void startCapture(comedi_cmd *cmd, unsigned int nsamples);

    // Wait for next sample (comprised of one or more scans) to complete or
    // timeout. Samples are converted to physical units before they are
    // returned from this function.
    ComediSamplePtr waitForNextSample(const struct timespec &ts);

    // thread entry point
    static void thread(ComediDevice &This) { This.run(); };

    private:

    // thread data acquisition function
    void run();

    // error handling
    void clearExistingData();
    void restartCapture();

    // data acquisition
    void readSamples();
    void readDIO(ComediSamplePtr buf);
    void convertSamples(ComediSamplePtr buf);

    const std::string filename_;
    comedi_t *device_;
    comedi_cmd *cmd_;
    bool capture_started_;

    // saved ranges for automatic unit conversion
    std::vector<comedi_range *> ranges_;
    std::vector<lsampl_t> maxdatas_;

    // samples which are currently being accumulated
    unsigned int nchannels_;
    unsigned int nsamples_;
    std::vector<sampl_t> samples_;

    // circular buffer to hold several pending samples
    boost::circular_buffer<ComediSamplePtr> circbuf_;
    carma::util::PthreadMutex mutex_;
    carma::util::PthreadCond cond_;
};

} // namespace linelength
} // namespace carma

#endif /* CARMA_LINELENGTH_COMEDIDEVICE_H */

/* vim: set ts=4 sts=4 sw=4 et tw=92: */
