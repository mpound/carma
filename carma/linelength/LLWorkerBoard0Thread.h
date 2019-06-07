#ifndef LLWORKERBOARD0THREAD_H
#define LLWORKERBOARD0THREAD_H
// $Id: LLWorkerBoard0Thread.h,v 1.12 2013/01/07 21:36:41 iws Exp $

#include <carma/util/PthreadMutex.h>
#include <carma/util/types.h>

#include <carma/linelength/ComediDevice.h>

#include <boost/shared_ptr.hpp>

#include <complex>
#include <vector>

namespace carma {
namespace linelength {

struct LLBoard0Data
{
    // timestamp of when this data was captured
    carma::util::frameType timestamp;
    ComediSamplePtr rawData;

    // Complex Data contains both amplitude and phase information
    // amplitude: abs(std::complex<double>);
    // phase: arg(std::complex<double>);

    // Antenna Data (24 antennas)
    std::vector< std::complex<double> > antComplex;
    std::vector<double> antPhaseRMS;

    // Reference Data (3 references)
    std::vector< std::complex<double> > refComplex;
    std::vector<double> refPhaseRMS;

    // Reference Lock Status
    std::vector<bool> refLockInfo;
};

typedef boost::shared_ptr<LLBoard0Data> LLBoard0DataPtr;

class LLWorkerBoard0Thread
{
    public:
    LLWorkerBoard0Thread(const std::string &device);
    ~LLWorkerBoard0Thread();

    // Thread
    static void thread(LLWorkerBoard0Thread &This) { This.run(); };
    void run();

    // Data Interface
    LLBoard0DataPtr getNewestData() const;

    private:
    const std::string device_;

    // Data buffer
    mutable carma::util::PthreadMutex mutex_;
    LLBoard0DataPtr data_;
};

} // namespace carma::linelength
} // namespace carma

#endif // LLWORKERBOARD0THREAD_H
// vim: set expandtab ts=4 sts=4 sw=4:
