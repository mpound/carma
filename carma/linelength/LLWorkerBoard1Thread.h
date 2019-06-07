#ifndef LLWORKERBOARD1THREAD_H
#define LLWORKERBOARD1THREAD_H

#include <carma/util/PthreadMutex.h>
#include <carma/util/types.h>

#include <carma/linelength/ComediDevice.h>

#include <boost/shared_ptr.hpp>

#include <vector>

namespace carma {
namespace linelength {

// Simple structure to hold a sample of LineLength Board1 Data
struct LLBoard1Data
{
    // timestamp of when this data was captured
    carma::util::frameType timestamp;
    ComediSamplePtr rawData;

    // 24 channels of AGC and Optical Power data
    std::vector<double> antagc;
    std::vector<double> antopt;

    // 3 channels of reference AGC and RF Power data
    std::vector<double> refagc;
    std::vector<double> refpower;
};

typedef boost::shared_ptr<struct LLBoard1Data> LLBoard1DataPtr;

class LLWorkerBoard1Thread
{
    public:
    LLWorkerBoard1Thread(const std::string &device);
    ~LLWorkerBoard1Thread();

    // Thread
    static void thread(LLWorkerBoard1Thread &This) { This.run(); };
    void run();

    // Data Interface
    LLBoard1DataPtr getNewestData() const;

    private:
    // device file to use
    const std::string device_;

    // data buffer
    mutable carma::util::PthreadMutex mutex_;
    LLBoard1DataPtr buffer_;
};

} // namespace carma::linelength
} // namespace carma

#endif // LLWORKERBOARD1THREAD_H
// vim: set expandtab ts=4 sts=4 sw=4:
