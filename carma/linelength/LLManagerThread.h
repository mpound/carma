#ifndef LLMANAGERTHREAD_H
#define LLMANAGERTHREAD_H
// $Id: LLManagerThread.h,v 1.11 2013/01/07 21:35:27 iws Exp $

#include <carma/linelength/LLWorkerBoard0Thread.h>
#include <carma/linelength/LLWorkerBoard1Thread.h>

#include <carma/util/AutoPthreadQuitAndJoinGroup.h>
#include <carma/util/ScopedPthreadMutexLock.h>

// forward declaration
namespace carma {
namespace monitor {
    class LineLengthSubsystem;
} // namespace carma::monitor
} // namespace carma

namespace carma {
namespace linelength {

struct LLManagerData {
    // constructor
    LLManagerData();

    // per-antenna data
    std::vector<bool> initialized;
    std::vector<double> length;
    std::vector<double> offsetPhase;
    std::vector<unsigned short> synthNumber;

    // per-synthesizer data
    std::vector<double> synthFreq;
};

class LLManagerThread
{
    public:
    LLManagerThread(const std::string &ni0, const std::string &ni1, const double awdelay);
    ~LLManagerThread();

    // CORBA methods
    void setOffsetPhase(const unsigned short ant, const double cycles);
    void setAntennaLORef(const unsigned short ant, const unsigned short synth);
    void setLORefFreq(const unsigned short synth, const double freq_hz);
    void setNominalLineLength(const unsigned short ant, const double length_ns);

    void takeBoard0Snapshot(const std::string &filename);
    void takeBoard1Snapshot(const std::string &filename);

    // Misc Public Interface
    void startWorkerBoard0();
    void startWorkerBoard1();

    // Thread
    static void thread(LLManagerThread &This) { This.run(); };
    void run();

    private:
    carma::util::AutoPthreadQuitAndJoinGroup _group;
    LLWorkerBoard0Thread _ni0Worker;
    LLWorkerBoard1Thread _ni1Worker;

    // monitor system
    const double _awdelay;

    void writeBoard0MP(carma::monitor::LineLengthSubsystem &LLmon);
    void writeBoard1MP(carma::monitor::LineLengthSubsystem &LLmon);

    // all data members below are protected by this mutex
    mutable carma::util::PthreadMutex _updateGuard;
    struct LLManagerData _managerData;
};

} // namespace carma::linelength
} // namespace carma

#endif // LLMANAGERTHREAD_H
// vim: set expandtab ts=4 sts=4 sw=4:
