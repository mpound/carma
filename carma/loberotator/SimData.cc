
/**
 * @file
 * Control method definitions for Loberotator class 
 *
 * @author Steve Scott
 *
 * $Id: SimData.cc,v 1.2 2005/07/12 14:35:30 scott Exp $
 *
 * $CarmaCopyright$
 */

#include <math.h>
#include <sstream>
#include <iostream>
#include "carma/loberotator/SimData.h"
#include "carma/util/ErrorException.h"

using namespace std;
using namespace carma::loberotator;


SimData::SimData(double minVal, double maxVal) :
    minVal_(minVal), maxVal_(maxVal),
    range_(maxVal - minVal), x_(0)
{
    if (range_ == 0.0) {
        throw CARMA_ERROR("Range cannot be equal to zero");
    }
}

SimData::SimData(double minVal, double maxVal, double off, double rate) :
    minVal_(minVal), maxVal_(maxVal), range_(maxVal - minVal),
    off_(off), rate_(rate), x_(0)
{
    if (range_ == 0.0) {
        throw CARMA_ERROR("Range cannot be equal to zero");
    }
}

void SimData::setRate(double rate) 
{ 
    rate_ = rate; 
}

void SimData::setOffset(double offset) 
{ 
    off_ = offset; 
}

double SimData::simData(double x)
{
    x_ = x;
    return  minVal_ +  fmod(off_ + x*rate_, range_);
}

double SimData::simData()
{
    x_++;
    return simData(x_);
}


// ------------------------------ Integer -------------------------

SimIntegerData::SimIntegerData(int minVal, int maxVal) :
    minVal_(minVal), maxVal_(maxVal), range_(maxVal - minVal + 1), 
    start_(minVal), dwell_(1), count_(0)
{
}

SimIntegerData::SimIntegerData(int minVal, int maxVal, int start, int dwell) :
    minVal_(minVal), maxVal_(maxVal), range_(maxVal - minVal + 1),
    start_(start), dwell_(dwell), count_(0)
{
    if (dwell <= 0) {
        ostringstream o;
        o << "Dwell (" << dwell << ") must be greater than 0";
        throw CARMA_ERROR(o);
    }
}

void SimIntegerData::setDwell(int dwell) 
{ 
    if (dwell <= 0) {
        ostringstream o;
        o << "Dwell (" << dwell << ") must be greater than 0";
        throw CARMA_ERROR(o);
    }
    dwell_ = dwell; 
}

void SimIntegerData::setOffset(int start) 
{ 
    if ((start < minVal_) || (start > maxVal_)) {
        ostringstream o;
        o << "Start value (" << start << ") out of range ["
          << minVal_ << "-" << maxVal_ << "]";
        throw CARMA_ERROR(o);
    }
    start_ = start; 
}

int SimIntegerData::simData()
{
    return minVal_ +  (start_ - minVal_  + count_++/dwell_)%range_;
}

