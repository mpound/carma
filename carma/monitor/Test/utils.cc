

/**
 *
 * Utilities for monitoring tests
 * These will eventually be replaced by routines in carma/util
 * @author: Steve Scott
 *
 * $Id: utils.cc,v 1.5 2005/09/01 18:52:14 mpound Exp $
 * $CarmaCopyright$
 *
 */


using namespace std;
using namespace carma::util;



// Class to accumulate timing statistics
class Stats {
public:
    Stats() {
        min_=1000000;
        max_=-1000000;
        sum_=ave_=0;
        nSamps_=0;
        cout.setf(ios::fixed);
    }
    int operator+=(double val) {
        lastSamp_ = val;
        sum_ += val;
        min_ = std::min(min_, val);
        max_ = std::max(max_, val);
        nSamps_++;
        ave_ = sum_/nSamps_;
        return nSamps_;
    }
    double min(){ return min_; }
    double max(){ return max_; }
    double ave(){ return ave_; }
    double last(){ return lastSamp_; }
    double nsamps(){ return nSamps_; }
    void summary(string label) {
        cout << setw(25) << left << label
             << setprecision(0)            
             << " Min=" << setw(4) << left << min_ 
             << "  Max=" << setw(4) << left << max_ 
             << "  Ave=" << setw(4) << left << ave_ 
             << endl;
    }
private:
    double min_;
    double max_;
    double sum_;
    double ave_;
    double lastSamp_;
    int    nSamps_;
};

// Time offset in milliseconds; can be negative
void sleepUntilNextFrame(int offset=0) {
    // Wake up right on the next frame
    double delay = Time::computeTimeToNextFrame();
    delay += 0.001*offset;	
    if (delay < 0)delay += 0.500;
    timespec sleepTime, remainingTime;
    sleepTime.tv_sec  = static_cast<int>(delay); // truncated time, in seconds
    // fractional part, in nanoseco:    
    sleepTime.tv_nsec = static_cast<int>(1e9*(delay - sleepTime.tv_sec)); 
    
    if (nanosleep (&sleepTime, &remainingTime) == -1)  {
        // If return early, keep sleeping
        while (nanosleep (&remainingTime, &remainingTime) == -1);
    }
} 


   
