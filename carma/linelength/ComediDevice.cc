#include <carma/linelength/ComediDevice.h>
using namespace carma::linelength;

#include <carma/util/ScopedPthreadMutexLock.h>
#include <carma/util/programLogging.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ScopedLogNdc.h>
#include <carma/util/ThreadQuit.h>
using namespace carma::util;

#include <time.h>

/* ---------------------------------------------------------------------------*/
/* ComediSample class                                                         */
/* ---------------------------------------------------------------------------*/

ComediSample::ComediSample(const unsigned int nchannels, const unsigned int nsamples)
	: nchannels(nchannels)
	, nsamples(nsamples)
{
	samples.reserve(nchannels * nsamples);
	dio.reserve(8);
}

std::vector<double> ComediSample::calculateAverage() const
{
	// skip work if no averaging is needed
	if (this->nsamples == 1)
		return this->samples;

	// accumulate
	std::vector<double> avgData(this->nchannels, 0.0);
	for (size_t i = 0; i < this->samples.size(); i++) {
		const unsigned int avgIndex = i % this->nchannels;
		avgData.at(avgIndex) += this->samples.at(i);
	}

	// divide by the number of samples, producing the average
	for (size_t i = 0; i < avgData.size(); i++) {
		avgData.at(i) /= this->nsamples;
	}

	return avgData;
}

/* ---------------------------------------------------------------------------*/
/* Local Helper Functions                                                     */
/* ---------------------------------------------------------------------------*/

static std::string getComediError()
{
	return comedi_strerror(comedi_errno());
}

static void closeComediAndError(comedi_t *device, const std::string &msg)
{
	if (device != NULL)
		comedi_close(device);

	programLogErrorIfPossible(msg);
	throw CARMA_ERROR(msg);
}

/*
 * Open and configure comedi device.
 *
 * If any part of this function fails, the comedi device is closed appropriately
 * so that we don't leak any resources.
 */
static comedi_t *openComediDevice(const std::string &name)
{
	comedi_t *device = NULL;
	std::ostringstream oss;

	// open the device
	device = comedi_open(name.c_str());
	if (device == NULL) {
		oss << "comedi_open: " << name << ": " << getComediError();
		closeComediAndError(device, oss.str());
	}

	// get the comedi device name
	const char *board = comedi_get_board_name(device);
	if (board == NULL) {
		oss << "comedi_get_board_name: " << name << ": " << getComediError();
		closeComediAndError(device, oss.str());
	}

	// make sure the device is a NI PXI-6031E
	if (std::string(board) != "pxi-6031e") {
		oss << "comedi device " << name << "(" << board << ") is not a pxi-6031e";
		closeComediAndError(device, oss.str());
	}

	// Initialize all PFI channels to be inputs, except for channels:
	// 2: CONVERT monitor output
	// 7: STARTSCAN monitor output
	//
	// These channels are automatically pulsed by the hardware at the
	// appropriate times.
	const int PFI_SUBDEVICE = 7;
	const int nchans = comedi_get_n_channels(device, PFI_SUBDEVICE);
	for (int i = 0; i < nchans; i++) {
		unsigned int direction = COMEDI_INPUT;
		if (i == 2 || i == 7)
			direction = COMEDI_OUTPUT;

		if (comedi_dio_config(device, PFI_SUBDEVICE, i, direction) < 0) {
			oss << "comedi_dio_config: " << name << ": " << getComediError();
			closeComediAndError(device, oss.str());
		}
	}

	return device;
}

/* ---------------------------------------------------------------------------*/
/* ComediDevice class                                                         */
/* ---------------------------------------------------------------------------*/

ComediDevice::ComediDevice(const std::string &name)
	: filename_(name)
	, device_(openComediDevice(name))
	, cmd_(NULL)
	, capture_started_(false)
	, ranges_()
	, maxdatas_()
	, samples_()
	, circbuf_(32)
	, mutex_()
	, cond_()
{
	// intentionally left empty
}

ComediDevice::~ComediDevice()
{
	if (device_ != NULL) {
		comedi_close(device_);
		device_ = NULL;
	}
}

int ComediDevice::findRange(const double min, const double max)
{
	const unsigned int AI_SUBDEVICE = 0;

	int ret = comedi_find_range(device_, AI_SUBDEVICE, 0, UNIT_volt, min, max);
	if (ret == -1) {
		std::ostringstream oss;
		oss << "comedi_find_range: " << getComediError();
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	return ret;
}

void ComediDevice::setupExternalReference(unsigned int period_ns)
{
	const unsigned int RTSI_SUBDEVICE = 10;
	lsampl_t insnData[3];
	comedi_insn insn;

	memset(&insn, 0, sizeof(insn));

	insn.insn = INSN_CONFIG;
	insn.n = 3;
	insn.data = insnData;
	insn.subdev = RTSI_SUBDEVICE;

#if defined(COMEDI_RTSI_CLOCK_MODE_SLAVE)
	/*
	 * The ancient carma-modified comedi package (version 0.7.22-carma1) had
	 * added support for using RTSI as a clock source. This code did not make
	 * it upstream as-is.
	 *
	 * This code should get compiled only when using the ancient comedi
	 * package. Otherwise, use the newer code below.
	 *
	 * We're lucky that Dave MacMahon used a C pre-processor macro for this
	 * value. The newer version uses an enum, which cannot be detected by
	 * the C pre-processor.
	 */
	insnData[0] = INSN_CONFIG_SET_RTSI_CLOCK_MODE;
	insnData[1] = COMEDI_RTSI_CLOCK_MODE_SLAVE;
	insnData[2] = period_ns;
#else
	/*
	 * Use this code for newer comedi packages, including the newer in-kernel
	 * "staging" comedi support. This is untested, but a manual audit of the
	 * kernel code paths in question show that this should work (as of 2.6.36).
	 */
	insnData[0] = INSN_CONFIG_SET_CLOCK_SRC;
	insnData[1] = NI_MIO_RTSI_CLOCK;
	insnData[2] = period_ns;
#endif

	if (comedi_do_insn(device_, &insn) < 0) {
		std::ostringstream oss;
		oss << "comedi_do_insn: unable to setup external reference with period: "
			<< period_ns << " ns: " << getComediError();
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

void ComediDevice::startCapture(comedi_cmd *cmd, unsigned int nsamples)
{
	int ret;

	// data capture is now stopped
	{
		carma::util::ScopedPthreadMutexLock lock(this->mutex_);
		this->capture_started_ = false;
	}

	// save number of samples
	nsamples_ = nsamples;
	nchannels_ = cmd->chanlist_len;
	cmd_ = cmd;

	// locate and save each voltage range for later conversion
	ranges_.clear();
	maxdatas_.clear();
	for (unsigned int i = 0; i < cmd->chanlist_len; i++) {
		const int chan = cmd->chanlist[i];
		comedi_range *range = comedi_get_range(device_, cmd->subdev, CR_CHAN(chan), CR_RANGE(chan));
		if (range == NULL) {
			std::ostringstream oss;
			oss << "comedi_get_range: " << getComediError();
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}

		lsampl_t maxdata = comedi_get_maxdata(device_, cmd->subdev, CR_CHAN(chan));
		if (maxdata == 0) {
			std::ostringstream oss;
			oss << "comedi_get_maxdata: " << getComediError();
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}

		ranges_.push_back(range);
		maxdatas_.push_back(maxdata);
	}

	// clear out any existing samples, and reserve space in the vector
	samples_.clear();
	samples_.reserve(nsamples_ * nchannels_);

	// cancel any outstanding transactions
	const unsigned int AI_SUBDEVICE = 0;
	comedi_cancel(device_, AI_SUBDEVICE);

	// Test the command for validity
	//
	// The comedi library will adjust the command parameters into acceptable
	// limits. Therefore we log the failure, but don't quit.
	ret = comedi_command_test(device_, cmd);
	if (ret != 0) {
		std::ostringstream oss;
		oss << "comedi_command_test (first try): ret=" << ret << ": " << getComediError();
		programLogInfoIfPossible(oss.str());
	}

	// Test the command for validity, again
	//
	// The command should be in good shape this time, unless it was horribly
	// wrong.
	ret = comedi_command_test(device_, cmd);
	if (ret != 0) {
		std::ostringstream oss;
		oss << "comedi_command_test error: ret=" << ret << ": " << getComediError();
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	// run the command on the hardware
	ret = comedi_command(device_, cmd);
	if (ret < 0) {
		std::ostringstream oss;
		oss << "comedi_command: " << getComediError();
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	// data capture is now started
	{
		carma::util::ScopedPthreadMutexLock lock(this->mutex_);
		this->capture_started_ = true;
	}
}

ComediSamplePtr ComediDevice::waitForNextSample(const struct timespec &ts)
{
	carma::util::ScopedPthreadMutexLock lock(this->mutex_);
	ComediSamplePtr buf;

	while (this->circbuf_.empty()) {
		ThreadQuitTestSelf();

		// Wait for the specified amount of time
		//
		// If we were not waked up, then we timed out. That means
		// we should exit the loop with a NULL pointer.
		const bool waked = this->cond_.TimedWait(this->mutex_, ts);
		if (!waked)
			return buf;

		ThreadQuitTestSelf();
	}

	// we have some data, return it
	buf = this->circbuf_.front();
	this->circbuf_.pop_front();
	return buf;
}

void ComediDevice::run()
{
	const ScopedLogNdc ndc("ComediDevice::run " + filename_);

	// dump any existing data on the floor
	this->clearExistingData();

	while (true) {
		try {
			ThreadQuitTestSelf();

			// capture is not started yet: sleep and retry
			{
				carma::util::ScopedPthreadMutexLock lock(this->mutex_);
				if (!this->capture_started_) {
					struct timespec ts;
					ts.tv_sec = 0;
					ts.tv_nsec = 20 * 1000 * 1000; // 20ms
					nanosleep(&ts, NULL);
					continue;
				}
			}

			// read the raw data samples
			this->readSamples();

			ThreadQuitTestSelf();
		} catch (...) {
			if (CaughtExceptionIsThreadQuitRequestedError()) {
				programLogInfoIfPossible("thread quit requested");
				return;
			}

			std::ostringstream oss;
			oss << "ComediDevice::run exception: " << getStringForCaught();
			programLogErrorIfPossible(oss.str());

			// sleep for a while to slow down logging
			sleep(10);
		}
	}
}

void ComediDevice::clearExistingData()
{
	// make sure the circular buffer is empty
	{
		carma::util::ScopedPthreadMutexLock lock(this->mutex_);
		this->circbuf_.clear();

		// changed the state of the circular buffer, signal waiters
		// NOTE: MUST hold the mutex for defined behavior!
		this->cond_.Signal();
	}

	// make sure we don't have any stale data
	samples_.clear();
}

void ComediDevice::restartCapture()
{
	programLogWarnIfPossible("unknown problem: restart capture");

	// dump any existing data on the floor
	this->clearExistingData();

	// re-issue the configuration and start the command process
	this->startCapture(this->cmd_, this->nsamples_);
}

void ComediDevice::readSamples()
{
	// sampl_t are 2 bytes, 16Kbyte should be plenty
	sampl_t rawbuf[8192];

	// read some data
	const ssize_t status = read(comedi_fileno(device_), rawbuf, sizeof(rawbuf));

	// error
	if (status < 0) {
		std::ostringstream oss;
		oss << "comedi read: " << strerror(errno);
		programLogWarnIfPossible(oss.str());
		this->restartCapture();
		return;
	}

	// no bytes
	if (status == 0) {
		std::ostringstream oss;
		oss << "comedi read: no bytes";
		programLogWarnIfPossible(oss.str());

		// empty the accumulator for sample data
		samples_.clear();

		return;
	}

	// read some bytes
	const int n = status / sizeof(sampl_t);
	for (int i = 0; i < n; i++) {

		// add the sample we just read
		samples_.push_back(rawbuf[i]);

		// a full scan worth of samples has been read
		if (samples_.size() == nchannels_ * nsamples_) {
			ComediSamplePtr buf(new struct ComediSample(nchannels_, nsamples_));

			// convert all samples to physical units
			this->convertSamples(buf);

			// read the DIO channels into the buffer
			this->readDIO(buf);

			// push the sample into the circular buffer under lock
			{
				carma::util::ScopedPthreadMutexLock lock(this->mutex_);
				this->circbuf_.push_back(buf);
				this->cond_.Signal();
			}

			// free all existing samples so we can start again
			samples_.clear();
		}
	}
}

void ComediDevice::readDIO(ComediSamplePtr buf)
{
	const unsigned int DIO_SUBDEVICE = 2;

	// figure out how many channels the device has
	const int nchannels = comedi_get_n_channels(device_, DIO_SUBDEVICE);
	if (nchannels < 0) {
		std::ostringstream oss;
		oss << "comedi_get_n_channels: " << getComediError();
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	// read each digital i/o
	for (int i = 0; i < nchannels; i++) {
		unsigned int val;
		const ssize_t status = comedi_dio_read(device_, DIO_SUBDEVICE, i, &val);
		if (status < 0) {
			std::ostringstream oss;
			oss << "comedi_dio_read: channel=" << i << ": " << getComediError();
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}

		buf->dio.push_back(val);
	}
}

void ComediDevice::convertSamples(ComediSamplePtr buf)
{
	for (unsigned int i = 0; i < nsamples_; i++) {
		for (unsigned int j = 0; j < nchannels_; j++) {
			const sampl_t sampl = samples_.at(i * nchannels_ + j);
			comedi_range *range = ranges_.at(j);
			const lsampl_t maxdata = maxdatas_.at(j);

			const double dst = comedi_to_phys(sampl, range, maxdata);
			buf->samples.push_back(dst);
		}
	}
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
