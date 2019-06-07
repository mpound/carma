#ifndef LLRMSTRACKER_H
#define LLRMSTRACKER_H
// $Id: RMSTracker.h,v 1.3 2011/08/30 23:12:39 iws Exp $

#include <carma/util/ScopedPthreadMutexLock.h>
#include <carma/util/programLogging.h>

#include <boost/foreach.hpp>

#include <deque>
#include <vector>
#include <iostream>
#include <algorithm>

#include <math.h>

namespace carma {
namespace linelength {

class RMSTracker
{
    public:
    RMSTracker(unsigned int max = 500)
        : _max(max)
        , _Sx(0.0)
        , _SSx(0.0)
        , _Sxx(0.0)
        , _Ssize(0.0)
        , _values()
        , _squares()
        { /* empty */ };
    ~RMSTracker() { /* empty */ };

    unsigned int size() const {
        carma::util::ScopedPthreadMutexLock lock(_lock);
        return static_cast<unsigned int>(_values.size());
    };

    void add(const double value) {
        carma::util::ScopedPthreadMutexLock lock(_lock);
        const unsigned int vs = _values.size();

        if (isnan(value) || isinf(value)) {
            carma::util::programLogWarnIfPossible("RMSTracker::add got inf or nan, ignore");
            return;
        }

        if (vs == _max) {
            const double v = _values.back();
            const double Sv = _squares.back();

            _Sx -= v;
            _Sxx -= Sv;

            _values.pop_back();
            _squares.pop_back();
        }

        const double s = value * value;

        _Sx += value;
        _Sxx += s;

        _values.push_front(value);
        _squares.push_front(s);

        _Ssize = _values.size();

        const double tSx = _Sx / _Ssize;
        _SSx = tSx * tSx;
    };

    double rms() const {
        carma::util::ScopedPthreadMutexLock lock(_lock);
        return sqrt((_Sxx / _Ssize) - _SSx);
    };

    // As defined in Numerical Recipes
    // this is the RMS Mean or sqrt((1/N-1)(sum((xj-xm)^2))
    double rmsMean() const {
        carma::util::ScopedPthreadMutexLock lock(_lock);

        const double xm = _Sx / _Ssize;
        double ssx = 0.;

        BOOST_FOREACH(const double value, _values) {
            const double sxm = value - xm;
            ssx += (sxm * sxm);
        }

        const double ret = sqrt(ssx / (_Ssize - 1));
        if (isnan(ret) || isinf(ret))
            return 0.0;

        return ret;
    }

    double mean() const {
        carma::util::ScopedPthreadMutexLock lock(_lock);
        return _Sx / static_cast<double>(_values.size());
    }

    double median() const {
        std::vector<double> v;

        {
            carma::util::ScopedPthreadMutexLock lock(_lock);
            v.resize(_values.size());
            copy(_values.begin(), _values.end(), v.begin());
        }

        if (v.empty())
            return 0.0;

        sort(v.begin(), v.end());
        return v.at(v.size() / 2);
    }

    double instantaneous() const {
        carma::util::ScopedPthreadMutexLock lock(_lock);
        return _values.front();
    }

    private:
    const unsigned int _max;

    double _Sx;         // running sum of v for v in values
    double _SSx;        // (_Sx / _Ssize) ** 2
    double _Sxx;        // running sum of (v ** 2) for v in values
    double _Ssize;      // values.size()

    std::deque<double> _values;
    std::deque<double> _squares;

    mutable carma::util::PthreadMutex _lock;
};

} // namespace carma::linelength
} // namespace carma

#endif // LLRMSTRACKER_H
// vim: set expandtab ts=4 sts=4 sw=4:
