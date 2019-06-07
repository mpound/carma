/**
 * @version $Revision: 1.2 $
 * @usage @autogen
 *
 * @description
 *  RTD runner. This program will simulate the RTD Java client by sending
 *  an initialize message, followed by an update message every two seconds.
 *  All messages go to stdout. All debugging messages go to stderr.
 *
 *  A common way to use this program will be to run it in a pipeline like this:
 *  rtdrunner | rtdalarm | rtddecoder
 *
 * @noKeys
 *
 * @logger DEFAULT_FACILITY carma.rtd.runner
 */

#include <carma/ui/rtd/common/ReaderWithTimeout.h>
#include <carma/ui/rtd/common/ProtoBufUtil.h>
#include <carma/ui/rtd/common/RTD.pb.h>

#include <carma/util/Program.h>

#include <google/protobuf/text_format.h>

#include <sys/time.h>

#include <iostream>
#include <string>

using namespace carma::ui::rtd;

int carma::util::Program::main()
{
	std::cerr << "rtdrunner: using 2 second loop, Control-C to exit" << std::endl;
	::rtdproto::UIMessageRequest req;

	req.set_code(::rtdproto::REQ_INITIALIZE);
	req.set_sleeptime(2000);

	std::cerr << "rtdrunner: sending INITIALIZE message" << std::endl;
	serializeMessageToStdout(req, false);

	while (true) {
		sleep(2);

		req.Clear();
		req.set_code(::rtdproto::REQ_UPDATE);
		req.set_sleeptime(2000);

		std::cerr << "rtdrunner: sending UPDATE message, then sleeping" << std::endl;
		serializeMessageToStdout(req, false);
	}

	return EXIT_SUCCESS;
}

/* vim: set ts=4 sts=4 sw=4 noet: */
