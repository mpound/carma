/*
 * Easy Clock and Benchmark Utility Objects
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef TIMED_BENCHMARK_H
#define TIMED_BENCHMARK_H

#include <boost/date_time/posix_time/posix_time.hpp>

namespace carma {
namespace util {

class TimedBenchmark
{
	public:
		void reset();
		void start();
		void stop();
		std::string print() const;
		double milliseconds() const;

	private:
		boost::posix_time::ptime start_time;
		boost::posix_time::ptime stop_time;
};

class EasyClock
{
	public:
		EasyClock();

		/* capture a time reading */
		void read();

		/* convert to milliseconds */
		static double milliseconds(const EasyClock &lhs, const EasyClock &rhs);

	private:
		boost::posix_time::ptime time;
};

}} // namespace carma::util
#endif /* TIMED_BENCHMARK_H */
