/**
 * $Id: tBitmask.cc,v 1.1 2012/10/29 21:36:39 iws Exp $
 *
 * @author Ira W. Snyder
 *
 * @version $Revision: 1.1 $
 * @description
 * Test program for MonitorPointEnum bitmask support
 *
 * @noKeys
 * @logger TEST_FACILITY carma.test.monitor.tBitmask
 */

#include <carma/monitor/TestSubsystem.h>
#include <carma/util/Program.h>
#include <boost/io/ios_state.hpp>
#include <iostream>
#include <iomanip>

int carma::util::Program::main()
{
	typedef carma::monitor::TestSubsystem::MpbitmaskMonitorPointEnum MPBMPE;
	carma::monitor::TestSubsystem ts;

	// start auto writer
	ts.startAutoWriter(0.100);

	long val = MPBMPE::NONE;
	for (int i = 0; i < 30; i++) {
		if (i == 5)
			val |= MPBMPE::BIT1;

		if (i == 10)
			val |= MPBMPE::BIT2;

		if (i == 15)
			val |= MPBMPE::BIT3;

		if (i == 20)
			val &= ~MPBMPE::BIT2;

		if (i == 25)
			val &= ~MPBMPE::BIT1;

		ts.box().mpbitmask().setValue(static_cast<MPBMPE::MPBITMASK>(val));

		boost::io::ios_all_saver guard(std::cout);
		std::cout << "TEST BITMASK: i=" << i
			<< " val=0x" << std::setw(8) << std::setfill('0') << std::hex << val
			<< " str=" << ts.box().mpbitmask().getValueToString()
			<< std::endl;

		// 500 ms
		usleep(500 * 1000);
	}

	return EXIT_SUCCESS;
}

/* vim: set ts=4 sts=4 sw=4 noet: */
