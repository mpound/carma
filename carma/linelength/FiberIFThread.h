#ifndef CARMA_LINELENGTH_FIBERIFTHREAD_H
#define CARMA_LINELENGTH_FIBERIFTHREAD_H

#include <carma/util/PthreadMutex.h>

#include <boost/circular_buffer.hpp>
#include <boost/shared_ptr.hpp>

#include <vector>

namespace carma {
namespace linelength {

// Simple structure to hold a sample of LineLength Board1 Data
struct FiberIFData
{
    // 24 channels of Polarization #1 IF data (mW)
    std::vector<double> pol1;
    // 24 channels of Polarization #2 IF data (mW)
    std::vector<double> pol2;
};

typedef boost::shared_ptr<struct FiberIFData> FiberIFDataPtr;

class FiberIFThread
{
    public:
    FiberIFThread(const std::string &device);
    ~FiberIFThread();

    // Thread
    static void thread(FiberIFThread &This) { This.run(); };
    void run();

    // Data Interface
    bool empty() const;
    size_t size() const;
    FiberIFDataPtr getData();

    private:
    // device file to use
    const std::string device_;

    // data buffer
    mutable carma::util::PthreadMutex mutex_;
    boost::circular_buffer<FiberIFDataPtr> buffer_;
};

} // namespace carma::linelength
} // namespace carma

#endif // CARMA_LINELENGTH_FIBERIFTHREAD_H
// vim: set expandtab ts=4 sts=4 sw=4:
