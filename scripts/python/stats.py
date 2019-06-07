#
# @author Steve Scott
#
# $Id: stats.py,v 1.2 2005/06/20 02:43:05 teuben Exp $
#
# Computes mean and standard deviation
#

import math


class Stats(object):
    """Accumulate samples and compute mean and std dev on demand"""
    def __init__(self) :
        self.clear()

    def clear(self):
        "Clear out all samples and start from scratch"
        self.nSamp_  = 0
        self.sumX_   = 0.0
        self.sumXX_  = 0.0
                        
    def add(self, x):
        "Add a sample"
        self.nSamp_  += 1
        self.sumX_   += x
        self.sumXX_  += x*x

    def samps(self) :
        "Return the number of samples"
        return self.nSamp_
        
    def mean(self) :
        "Return the mean value of the samples"
        if self.nSamp_ == 0: 
            raise Exception, "Stats::mean(): number of samples=0"
        return self.sumX_/self.nSamp_

    def variance(self) :
        "Return the variance of the samples"
        if self.nSamp_ == 0: 
            raise Exception, "Stats::variance(): number of samples=0"
        return self.sumXX_/self.nSamp_

    def rms(self) :
        "Return the standard deviation of the samples"
        if self.nSamp_ == 0: 
            raise Exception, "Stats::rms(): number of samples=0"
        tmp = self.variance()-self.mean()*self.mean()
        # due to roundoff tmp could just be a tad negative
        if tmp < 0: return 0
        return math.sqrt(tmp)
        
    def error(self) :
        "Return the error in the mean"
        if self.nSamp_ <= 1: 
            raise Exception, "Stats::error(): number of samples<=1"
        return self.rms()/math.sqrt(self.nSamp_ - 1)

         

