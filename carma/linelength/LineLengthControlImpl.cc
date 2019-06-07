#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
using namespace carma::util;

#include <carma/linelength/LineLengthControlImpl.h>
using namespace carma::linelength;

LineLengthControlImpl::LineLengthControlImpl(LLManagerThread &llManager)
    : manager_(llManager)
{
    programLogInfoIfPossible("LineLengthControlImpl ctor");
}

LineLengthControlImpl::~LineLengthControlImpl()
{
    programLogInfoIfPossible("LineLengthControlImpl dtor");
}

void
LineLengthControlImpl::setOffsetPhase(const CORBA::UShort ant, const CORBA::Double cycles)
try {
    std::ostringstream oss;
    oss << "setOffsetPhase(" << ant << ", " << cycles << ")";
    programLogInfoIfPossible(oss.str());

    this->manager_.setOffsetPhase(ant, cycles);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

void
LineLengthControlImpl::setAntennaLORef(const CORBA::UShort ant, const CORBA::UShort synth)
try {
    std::ostringstream oss;
    oss << "setAntennaLORef(" << ant << ", " << synth << ")";
    programLogInfoIfPossible(oss.str());

    this->manager_.setAntennaLORef(ant, synth);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

void
LineLengthControlImpl::setLORefFreq(const CORBA::UShort synth, const CORBA::Double freq_hz)
try {
    std::ostringstream oss;
    oss << "setLORefFreq(" << synth << ", " << freq_hz << ")";
    programLogInfoIfPossible(oss.str());

    this->manager_.setLORefFreq(synth, freq_hz);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

void
LineLengthControlImpl::setNominalLineLength(const CORBA::UShort ant, const CORBA::Double length_ns)
try {
    std::ostringstream oss;
    oss << "setNominalLineLength(" << ant << ", " << length_ns << ")";
    programLogInfoIfPossible(oss.str());

    this->manager_.setNominalLineLength(ant, length_ns);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

void
LineLengthControlImpl::takeBoard0Snapshot(const char *fullPathAndFileName)
try {
    std::ostringstream oss;
    oss << "takeBoard0Snapshot(" << fullPathAndFileName << ")";
    programLogInfoIfPossible(oss.str());

    this->manager_.takeBoard0Snapshot(fullPathAndFileName);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

void
LineLengthControlImpl::takeBoard1Snapshot(const char *fullPathAndFileName)
try {
    std::ostringstream oss;
    oss << "takeBoard1Snapshot(" << fullPathAndFileName << ")";
    programLogInfoIfPossible(oss.str());

    this->manager_.takeBoard1Snapshot(fullPathAndFileName);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// vim: set expandtab ts=4 sts=4 sw=4:
