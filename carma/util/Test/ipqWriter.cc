//
// @version	$Revision: 1.15 $ $Date: 2008/02/12 21:02:28 $
//
// @usage	use it
//
// @description
//  Test program for IPQ writing. Run in conjunction with
//  ipqReader
//
// @key	depth        200   int     depth of the queue in elements
// @key	dump         1     int     dump every nth element write (0 = none)
// @key	interactive  true  bool    whether to run in interactive mode or not
// @key	num          0     int     number of elements to write (0 = infinite)
// @key	period       0.5   double  number of seconds between element writes
// @key to           0   i   test offset for queues
//
// @logger TEST_FACILITY carma.test.util.ipqWriter
//

#include <iomanip>
#include <stdexcept>
#include <cmath>

#include "carma/util/Test/ipqWriterReaderData.h"
#include "carma/util/ScopedSingleCharIoMode.h"
#include "carma/util/IPQwriter.h"
#include "carma/util/IPQfileWriter.h"
#include "carma/util/Time.h"
#include "carma/util/Program.h"
#include "carma/util/PeriodicTimer.h"


int
carma::util::Program::main( ) {
	const int depth = getIntParameter( "depth" );
	const int dumpEveryNth = getIntParameter( "dump" );
	const bool interactiveMode = getBoolParameter( "interactive" );
	const int num = getIntParameter( "num" );
	const double periodInSeconds = getDoubleParameter( "period" );
	const unsigned int to = getIntParameter("to");
	
	if ( periodInSeconds < 0.0 )
		throw std::runtime_error( "period cannot be negative" );
	
	if ( dumpEveryNth < 0 )
		throw std::runtime_error( "dump cannot be negative" );

	if ( num < 0 )
		throw std::runtime_error( "num cannot be negative" );
	
	struct ::timespec period;
	
	period.tv_sec = static_cast< int >( ::floor( periodInSeconds ) );
	period.tv_nsec = static_cast< int >( ::ceil( (periodInSeconds - period.tv_sec) * 1.0e+9 ) );
	
    std::cout.setf( std::ios::fixed );

    // Create the data queue
    carma::util::IPQwriter< carma::util::test::Element >
		ipq(carma::util::test::fname, true, depth, to);

	std::cout << "element size: " << sizeof( carma::util::test::Element ) << std::endl;
	std::cout << "queue depth: " << ipq.getQueueSize() << std::endl;

	if ( interactiveMode ) {
		std::cout << "Enter 'q' to exit or anything else to write a new element"
		          << std::endl;
	}
	
	{
		carma::util::ScopedSingleCharIoMode singleCharIoMode;
		carma::util::PeriodicTimer timer( period );
		
		ipq.i = 0;
		Time time;

		if ( periodInSeconds > 0.0 ) {
			struct ::timeval now;
		
			::gettimeofday( &now, 0 );
			
			timer.ResetNextFireAbsoluteTime( now );
		}

		// Loop, modifying and writing data
		// Each loop writes a new element of data

		unsigned long long numThusFar = 0;
		
		while ( (num == 0) || (numThusFar < static_cast<unsigned>(num)) ) {
			if ( interactiveMode ) {
				const char c = getchar( );

				if ( (c == 'q') || (c == 'Q') )
					break;
			} else if ( periodInSeconds > 0.0 )
				timer.WaitForNextFireTime( );

			// Change data
			const int writtenI = ++(ipq.i);
			ipq.mjd = time.MJD( );

			try {
				// Write out a new element
				ipq.write( );
			
				++numThusFar;
				
				if ( (dumpEveryNth != 0) && ((numThusFar % dumpEveryNth) == 0) ) {
					std::cout << "Write #" << numThusFar << ": i="
							  << std::setw( 3 ) << writtenI
							  << std::endl;
				}
			} catch ( const std::exception & e ) {
				std::cerr << "IPQwrite err: " << e.what( ) << std::endl;
			}
		}
    }

   std::cout << "Exiting ipq test writer" << std::endl;
   
   return EXIT_SUCCESS;
}
