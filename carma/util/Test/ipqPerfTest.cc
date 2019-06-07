/**
 * Test program for ipq performance.
 *
 *@author: Steve Scott
 * 23 Apr, 2004
 *
 * $Id: ipqPerfTest.cc,v 1.10 2012/01/13 17:04:19 iws Exp $
 *
 */

// Keyword setup...
//
// @version     $Revision: 1.10 $ $Date: 2012/01/13 17:04:19 $
//
// @usage     See keywords
//
// @key startSize 100 int
//      smallest shmem size in KB
//
// @key nSizes 5 int
//      Number of different sizes to test: these increase by factors of 2 from
//      the startSize
//
// @key reps 100 int
//      Number of frames to do in test
//
// @key verbose false bool
//      Turns on additional output
//
// @description
// Program to test timing of memcpy and IPQ reads and writes.
// The displayed sizes are in KB and rates are in milliseconds/MB
//
// @logger TEST_FACILITY carma.test.util.ipqPerfTest
//


#include <functional>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>

#include <string.h>

#include "carma/util/IPQbuffer.h"
#include "carma/util/Time.h"
#include "carma/util/Program.h"

using namespace carma::util;
using namespace std;


class IPQ: public IPQbuffer {
public:
    IPQ(char*         localElement,
        int           elementSize,
        const std::string& filename,
        bool          isCreator = false,
        int           nElements = 0):
            IPQbuffer(localElement, elementSize, filename, isCreator, nElements)
    {
        init();
    }
    void write() { IPQbuffer::write(); }
}; 

// Times are in microseconds 
class IPQperf {
public:
    /// @param size of IPQ element or memcopy in bytes
    IPQperf(int size): size_(size) {
    }
    
    void runTest() {
        const int nMems = 5;
        char* mem[nMems];
        IPQ* ipq[nMems];
        for(int i=0; i<nMems; i++) {
            mem[i] = new char[size_];
            ipq[i] = new IPQ(mem[i], size_, "/perftest.ipq", true, 10);
        }
        double t1;
        double t2;
        
        t1 = Time::MJD();
        ipq[0]->write();
        t2 = Time::MJD();
        ipqWriteTime_ = Time::MICROSECONDS_PER_DAY*(t2 - t1);

        t1 = Time::MJD();
        ipq[1]->readNewest();
        t2 = Time::MJD();
        ipqReadNewestTime_ = Time::MICROSECONDS_PER_DAY*(t2 - t1);
        
        t1 = Time::MJD();
        memcpy(mem[2], mem[3], size_);
        t2 = Time::MJD();
        memcopyTime_ = Time::MICROSECONDS_PER_DAY*(t2 - t1);
        
        t1 = Time::MJD();
        ipq[4]->read();
        t2 = Time::MJD();
        ipqReadTime_ = Time::MICROSECONDS_PER_DAY*(t2 - t1);
       
        for(int i=0; i<nMems; i++) {
            delete[] mem[i];
            delete   ipq[i];
        }
    }

    double memcopy() {return memcopyTime_ ;}
    double ipqWrite() { return ipqWriteTime_; }
    double ipqRead() { return ipqReadTime_;}
    double ipqReadNewest() { return ipqReadNewestTime_; }
private:
    double memcopyTime_;
    double ipqWriteTime_;
    double ipqReadTime_;
    double ipqReadNewestTime_;
    const int size_;
};

// Class to accumulate timing statistics
class Stats {
public:
    Stats(double editFilter = 0.0):editFilter_(editFilter) {
        min_=1000000;
        max_=-1000000;
        sum_=ave_=0;
        cout.setf(ios::fixed);
        uptodate_ = true;
        debug_ = false;
    }
    int operator+=(double val) {
        uptodate_ = false;
        lastSamp_ = val;
        ave_ = sum_/nSamps_;
        sample_.push_back(val);
        return sample_.size();
    }
    double min(){ update(); return min_; }
    double max(){ update(); return max_; }
    double ave(){ update(); return ave_; }
    double median(){ update(); return median_; }
    double last(){ return lastSamp_; }
    double nsamps(){ return sample_.size(); }
    double editFilter() { return editFilter_; }
    void summary(string label, int size, int precision=2) {
        update();
        cout << setw(13) << left << label
             << setprecision(precision)            
             << " Size="    << setw(4) << left << size 
             << " Min="     << setw(4) << left << min_ 
             << " Max="     << setw(4) << left << max_ 
             << " Ave="     << setw(4) << left << ave_ 
             << " Median="  << setw(4) << left << median_ 
             << endl;
    }
private:
    double min_;
    double max_;
    double sum_;
    double ave_;
    double median_;
    double editFilter_;
    double lastSamp_;
    int    nSamps_;
    bool   uptodate_;
    vector<double> sample_;
    bool   debug_;

    // Required to be used by iterator later on
    // Must extend from the binary_function
    class EditFilter : public binary_function<Stats*, double, bool> {
    public:
        // Returns true if value is too far away from the mean
        bool operator() (Stats* s, double val) const {
            return (fabs(val-s->median())/s->median()) > s->editFilter();
        }
    };
    
    // Compute the median
    void   calcMedian() {
        int size = sample_.size();
        if (size == 0) return;
        if (size & 1) median_ = sample_[size/2];  // Odd number samples
        else          median_ = (sample_[size/2-1] + sample_[size/2])/2;
        if (debug_) cout << "Calcmedian:" << size << "  " << median_ << endl;
    }
    
    // Compute the mode, remove outlying samples, recompute mode, do stats
    void   update() {
        if (uptodate_) return;
        uptodate_ = true;
        sort(sample_.begin(), sample_.end());
        calcMedian();
        if (editFilter_ > 0) {
            sample_.erase(
                remove_if(sample_.begin(), sample_.end(),
                    bind1st(EditFilter(), this)), 
                sample_.end()
            );
            // In case the edit changed things (usually not much!)
            calcMedian(); 
        }
        sum_ = 0;
        min_=1000000;
        max_=-1000000;
        ave_=0;
        vector<double>::iterator pos;
        for (pos=sample_.begin(); pos < sample_.end(); ++pos) {
            sum_ += *pos;
            min_ = std::min(min_, *pos);
            max_ = std::max(max_, *pos);
        }
        ave_ = sum_/sample_.size();
    }
};


int Program::main() {
    cout.setf(ios::fixed);   

    int startSize = getIntParameter("startSize"); // KB
    int nSizes    = getIntParameter("nSizes");
    int nReps     = getIntParameter("reps");
    bool verbose  = getBoolParameter("verbose");

    // Fractional deviation from the median to be tossed    
    double filter = 0.3;
    
    cout << "Sizes are in KB, performance in millisecs per MB\n"
         << "Statistics computed over " << nReps << " repetitions" << endl;
    
    for(int s=0; s<nSizes; s++) {
        Stats memcopy(filter);
        Stats ipqWrite(filter);
        Stats ipqRead(filter);
        Stats ipqReadNewest(filter);
        int size = (int)ldexp(startSize*1000.0, s);  // In bytes

        for(int i=0; i<nReps; i++) {
            IPQperf ipqperf = IPQperf(size);
            ipqperf.runTest();
            memcopy       += 1000*ipqperf.memcopy()/size; // msec/MB
            ipqWrite      += 1000*ipqperf.ipqWrite()/size;
            ipqRead       += 1000*ipqperf.ipqRead()/size;
            ipqReadNewest += 1000*ipqperf.ipqReadNewest()/size;
            if (verbose) cout    
                << "Size:" << setw(4) << setprecision(0) << size/1000
                << setprecision(2)
                << " memcopy="    << memcopy.last()
                << " ipqWrit="    << ipqWrite.last()
                << " ipqRead="    << ipqRead.last()
                << " ipqReadNew=" << ipqReadNewest.last()
                << endl;
        }

        ipqWrite.summary("IPQwrite",           size/1000);
        memcopy.summary("Memcpy",              size/1000);
        ipqRead.summary("IPQread",             size/1000);
        ipqReadNewest.summary("IPQreadNewest", size/1000);
        cout << endl;
    }

   cout << "Exiting ipq performance test"<<endl;
   return EXIT_SUCCESS;

}












