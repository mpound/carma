/*
 * Easy Timing and Benchmarking Objects
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <iomanip>
#include <cstdio>

#include <carma/util/TimedBenchmark.h>

#include <boost/date_time/special_defs.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

using namespace boost::posix_time;
using namespace carma::util;

void TimedBenchmark::reset()
{
	this->start_time = boost::date_time::not_a_date_time;
	this->stop_time = boost::date_time::not_a_date_time;
}

void TimedBenchmark::start()
{
	this->start_time = microsec_clock::local_time();
}

void TimedBenchmark::stop()
{
	this->stop_time = microsec_clock::local_time();
}

std::string TimedBenchmark::print() const
{
	std::ostringstream oss;
	time_duration td;

	td = this->stop_time - this->start_time;
	oss << td.total_microseconds() << " us"
	    << " (" << std::fixed << std::setprecision(3)
	    << this->milliseconds() << " ms)";

	return oss.str();
}

double TimedBenchmark::milliseconds() const
{
	time_duration td;

	td = this->stop_time - this->start_time;
	return td.total_microseconds() / 1000.0;
}

EasyClock::EasyClock()
	: time(boost::date_time::not_a_date_time)
{
	/* intentionally left empty */
}

void EasyClock::read()
{
	this->time = microsec_clock::local_time();
}

double EasyClock::milliseconds(const EasyClock &start, const EasyClock &stop)
{
	time_duration td;

	td = stop.time - start.time;
	return td.total_microseconds() / 1000.0;
}

