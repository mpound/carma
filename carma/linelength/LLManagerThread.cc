#include <carma/monitor/LineLengthSubsystem.h>
using carma::monitor::LineLengthSubsystem;

#include <carma/linelength/LLManagerThread.h>
using namespace carma::linelength;

#include <carma/util/Time.h>
#include <carma/util/Trace.h>
#include <carma/util/StartPthread.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/FrameAlignedTimer.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

#include <fstream>
#include <limits>
using namespace std;

#include <boost/foreach.hpp>

#define NUM_ANT_CHANS   24
#define NUM_REF_CHANS   3

// trace helpers
#define CPTRACE1(args...) CPTRACE(Trace::TRACE1, ##args)
#define CPTRACE6(args...) CPTRACE(Trace::TRACE6, ##args)

/*----------------------------------------------------------------------------*/
/* Helper Functions                                                           */
/*----------------------------------------------------------------------------*/

static void checkAntennaNumber(const std::string &fn, const unsigned short &ant)
{
    if (ant < 1 || ant > NUM_ANT_CHANS) {
        std::ostringstream oss;
        oss << fn << ": antenna number " << ant
            << " out of range [1," << NUM_ANT_CHANS << "]";
        throw CARMA_ERROR(oss.str());
    }
}

static void checkSynthesizerNumber(const std::string &fn, const unsigned short &synth)
{
    if (synth < 1 || synth > NUM_REF_CHANS) {
        std::ostringstream oss;
        oss << fn << ": synthesizer number " << synth
            << "out of range [1," << NUM_REF_CHANS << "]";
        throw CARMA_ERROR(oss.str());
    }
}

static void writeRawDataSnapshot(const std::string &filename, ComediSamplePtr p)
{
    std::ofstream eo(filename.c_str());

    eo << "# " << Time::MJD() << "\n";

    unsigned long len = 0L;
    BOOST_FOREACH(const double val, p->samples) {
        eo << val << " ";
        len++;
        if ((len % p->nchannels) == 0)
            eo << "\n";
    }

    eo.flush();
    eo.close();
}

// Helper to make sure that non-sensical lengths are rejected
static double sanitizeLength(const double realLength)
{
    std::numeric_limits<double> doubleInfo;

    // make sure sensical lengths are returned
    const double checkLength = fabs(realLength);
    if (checkLength > 50000.0)
        return 0.0;
    else if (doubleInfo.has_infinity && checkLength == doubleInfo.infinity())
        return 0.0;
    else if (doubleInfo.has_quiet_NaN && checkLength == doubleInfo.quiet_NaN())
        return 0.0;
    else if (doubleInfo.has_signaling_NaN && checkLength == doubleInfo.signaling_NaN())
        return 0.0;
    else if (isnan(checkLength) || isinf(checkLength))
        return 0.0;

    return realLength;
}

/*----------------------------------------------------------------------------*/
/* LLManagerData Class Implementation                                         */
/*----------------------------------------------------------------------------*/

LLManagerData::LLManagerData()
    : initialized(NUM_ANT_CHANS, false)
    , length(NUM_ANT_CHANS, 0.0)
    , offsetPhase(NUM_ANT_CHANS, 0.0)
    , synthNumber(NUM_ANT_CHANS, 1)
    , synthFreq(NUM_REF_CHANS, 0.0)
{
    // NOTE: channel 24 is a spare slot: it is initialized by default
    initialized.at(24 - 1) = true;
}

/*----------------------------------------------------------------------------*/
/* LLManagerThread Class Implementation                                       */
/*----------------------------------------------------------------------------*/

LLManagerThread::LLManagerThread(const std::string &ni0, const std::string &ni1, const double awdelay)
    : _ni0Worker(ni0)
    , _ni1Worker(ni1)
    , _awdelay(awdelay)
{
    programLogDebugIfPossible("LLManagerThread ctor");
}

LLManagerThread::~LLManagerThread()
{
    programLogDebugIfPossible("LLManagerThread dtor");
}

void
LLManagerThread::setOffsetPhase(const unsigned short ant, const double cycles)
{
    ScopedPthreadMutexLock lock(_updateGuard);
    checkAntennaNumber("setOffsetPhase", ant);
    const int index = ant - 1;

    _managerData.offsetPhase.at(index) = cycles;
}

void
LLManagerThread::setAntennaLORef(const unsigned short ant, const unsigned short synth)
{
    ScopedPthreadMutexLock lock(_updateGuard);
    checkAntennaNumber("setAntennaLORef", ant);
    checkSynthesizerNumber("setAntennaLORef", synth);
    const int index = ant - 1;

    _managerData.synthNumber.at(index) = synth;
}

void
LLManagerThread::setLORefFreq(const unsigned short synth, const double freq_hz)
{
    ScopedPthreadMutexLock lock(_updateGuard);
    checkSynthesizerNumber("setLORefFreq", synth);
    const int index = synth - 1;

    _managerData.synthFreq.at(index) = freq_hz;
}

void
LLManagerThread::setNominalLineLength(const unsigned short ant, const double length_ns)
{
    ScopedPthreadMutexLock lock(_updateGuard);
    checkAntennaNumber("setNominalLineLength", ant);

    /*
     * Check that the length is sane
     *
     * Negative length is obviously bogus. A maximum length of 25000 ns
     * corresponds to ~7.5 kilometers, which is much longer than our
     * longest fiber.
     */
    if (length_ns < 0.0 || length_ns > 25000.0) {
        std::ostringstream oss;
        oss << "setNominalLineLength: length " << length_ns << " is outside range [0,25000] ns";
        throw CARMA_ERROR(oss.str());
    }

    const int index = ant - 1;
    _managerData.length.at(index) = length_ns;
    _managerData.initialized.at(index) = true;
}

void LLManagerThread::takeBoard0Snapshot(const std::string &filename)
{
    LLBoard0DataPtr buf = _ni0Worker.getNewestData();

    // no data available, exit immediately
    if (!buf.get())
        throw CARMA_ERROR("No data available");

    writeRawDataSnapshot(filename, buf->rawData);
}

void LLManagerThread::takeBoard1Snapshot(const std::string &filename)
{
    LLBoard1DataPtr buf = _ni1Worker.getNewestData();

    // no data available, exit immediately
    if (!buf.get())
        throw CARMA_ERROR("No data available");

    writeRawDataSnapshot(filename, buf->rawData);
}

void LLManagerThread::startWorkerBoard0()
{
    _group.insert(StartPthreadWithRef(LLWorkerBoard0Thread::thread, _ni0Worker));
}

void LLManagerThread::startWorkerBoard1()
{
    _group.insert(StartPthreadWithRef(LLWorkerBoard1Thread::thread, _ni1Worker));
}

void LLManagerThread::writeBoard0MP(LineLengthSubsystem &LLmon)
{
    LLBoard0DataPtr b0Data = _ni0Worker.getNewestData();
    if (!b0Data.get())
        return;

    // grab internal data under lock
    struct LLManagerData manData;
    {
        ScopedPthreadMutexLock lock(_updateGuard);
        manData = _managerData;
    }

    // handle antenna-based monitor points
    for (int ant = 1; ant <= NUM_ANT_CHANS; ant++) {
        const int index = ant - 1;

        // synthesizer number and index
        const unsigned short synthNumber = manData.synthNumber.at(index);
        const int synthIndex = synthNumber - 1;

        // Each antenna complex value has this computation performed to
        // do ??? to it. What this does exactly is unknown: it was merely
        // copied from the original code.
        const std::complex<double> refC = b0Data->refComplex.at(synthIndex);
        const std::complex<double> antC = b0Data->antComplex.at(index);

        const double tmprp = antC.real() * refC.real() + antC.imag() * refC.imag();
        const double tmpip = antC.imag() * refC.real() - antC.real() * refC.imag();

        const std::complex<double> newC(tmprp, tmpip);

        LLmon.initialized(index).setValue(manData.initialized.at(index));
        LLmon.phaseRms(index).setValue(b0Data->antPhaseRMS.at(index));
        LLmon.refChan(index).setValue(synthNumber);

        // This is some sort of normalization to keep the amplitude in the range
        // [-1, 1]. The constant 20.0 may come from the number of frequency bins
        // produced by the FFT. However, this guess may be incorrect. The
        // scaling factor was merely copied from the original code.
        //
        // The division changes only the amplitude. The phase is unchanged.
        {
            const std::complex<float> cf(newC / (abs(refC) * 20.0));
            LLmon.phaseComplex(index).setValue(cf);
        }

        const double antPhase = arg(newC);
        LLmon.phase(index).setValue(antPhase);

        // phase measured (in cycles)
        const double phaseMeasured = antPhase / (2.0 * M_PI);
        const double synthGHz = manData.synthFreq.at(synthIndex) / 1e9;

        // TODO FIXME:
        //
        // I'm fairly certain that the " - 0.5" at the end of phasePredicted
        // is bogus. It is canceled out by the fmod(... + 1.5, 1.0) on the dPhi
        // line. Remove them and double check.
        const double offsetPhase = manData.offsetPhase.at(index);
        const double phasePredicted = fmod(offsetPhase + 2.0 * manData.length.at(index) * synthGHz + 0.5, 1.0) - 0.5;
        const double dPhi = fmod(phaseMeasured - phasePredicted + 1.5, 1.0) - 0.5;
        const double realLength = manData.length.at(index) + dPhi / (2.0 * synthGHz);

        // make sure sensical lengths are returned
        LLmon.lineLength(index).setValue(sanitizeLength(realLength));
    }

    // handle reference-based monitor points
    for (int ref = 1; ref <= NUM_REF_CHANS; ref++) {
        const int index = ref - 1;

        const double phase = arg(b0Data->refComplex.at(index));
        LLmon.phaseRef(index).setValue(phase);
        LLmon.phaseRefRms(index).setValue(b0Data->refPhaseRMS.at(index));

        // This is some sort of normalization to keep the amplitude in the range
        // [-1, 1]. The constant 20.0 may come from the number of frequency bins
        // produced by the FFT. However, this guess may be incorrect. The
        // scaling factor was merely copied from the original code.
        //
        // The division changes only the amplitude. The phase is unchanged.
        {
            const std::complex<float> cf(b0Data->refComplex.at(index) / 20.0);
            LLmon.phaseRefComplex(index).setValue(cf);
        }

        LLmon.offsetFreqLocked(index).setValue(b0Data->refLockInfo.at(index));
    }
}

void LLManagerThread::writeBoard1MP(LineLengthSubsystem &LLmon)
{
    LLBoard1DataPtr b1Data = _ni1Worker.getNewestData();
    if (!b1Data.get())
        return;

    // handle antenna-based monitor points
    for (int ant = 1; ant <= NUM_ANT_CHANS; ant++) {
        const int index = ant - 1;

        LLmon.optPwr(index).setValue(b1Data->antopt.at(index));
        LLmon.agc(index).setValue(b1Data->antagc.at(index));
    }

    // handle reference-based monitor points
    for (int ref = 1; ref <= NUM_REF_CHANS; ref++) {
        const int index = ref - 1;

        LLmon.agcRef(index).setValue(b1Data->refagc.at(index));
        LLmon.refRFPower(index).setValue(b1Data->refpower.at(index));
    }
}

void LLManagerThread::run()
{
    CPTRACE1(" Launching Monitor Thread");

    carma::monitor::LineLengthSubsystem LLmon;
    FrameAlignedTimer framer;

    LLmon.startAutoWriter(_awdelay);
    framer.ResetNextFireTime();

    while (true) {
        try {
            ThreadQuitTestSelf();
            framer.WaitForNextFireTime();
            ThreadQuitTestSelf();
            CPTRACE6("      *POW* Got monitor frame timer fire");

            LLmon.timestamp().setValue(Time::MJD());
            LLmon.online().setValue(true);

            this->writeBoard0MP(LLmon);
            this->writeBoard1MP(LLmon);
        } catch (...) {
            if (CaughtExceptionIsThreadQuitRequestedError()) {
                programLogInfoIfPossible("LLManagerThread: exit requested");
                LLmon.stopAutoWriter();
                return;
            }

            std::ostringstream oss;
            oss << "LLManagerThread: exception: " << getStringForCaught();
            programLogErrorIfPossible(oss.str());
        }
    } // end while (true)
}

// vim: set expandtab ts=4 sts=4 sw=4:
