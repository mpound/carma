/**
 * @version $Revision: 1.4 $
 * @usage @autogen
 *
 * @description
 *  RTD Wire Protocol Decoder
 *
 * @noKeys
 *
 * @logger DEFAULT_FACILITY carma.rtd.decoder
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
	struct ::timeval tv;
	tv.tv_sec = 3600;
	tv.tv_usec = 0;

	ReaderWithTimeout reader(tv);

	std::cerr << "rtddecoder: will wait " << tv.tv_sec << " seconds for input"
		<< " before quitting" << std::endl;
	std::cerr << "rtddecoder: press Control-C to quit earlier" << std::endl;

	while (true) {
		std::string bytes;
		reader.getBytes(bytes);

		if (reader.getErrorCode() != ReaderWithTimeout::SUCCESS) {
			std::cerr << "ERROR: reading from stdin failed:"
				<< " code=" << reader.getErrorCode()
				<< " msg=" << bytes << std::endl;
			return EXIT_FAILURE;
		}

		std::cerr << "REPLY MESSAGE RECEIVED: " << bytes.size()
			<< " bytes long (compressed)" << std::endl;

		std::string uncompressed;
		decompressZLIB(bytes, uncompressed);

		std::cerr << "REPLY MESSAGE RECEIVED: " << uncompressed.size()
			<< " bytes long (uncompressed)" << std::endl;

		::rtdproto::UIMessageReply repl;
		if (!repl.ParseFromString(uncompressed)) {
			std::cerr << "ERROR: failed to parse protocol buffer from string" << std::endl;
			return EXIT_FAILURE;
		}

		// print the protocol buffer to std::cerr
		{
			std::string str;
			if (!google::protobuf::TextFormat::PrintToString(repl, &str)) {
				std::cerr << "ERROR: printing protocol buffer to string failed" << std::endl;
				return EXIT_FAILURE;
			}

			std::cerr << str << std::endl;
		}
	}

	std::cerr << "rtddecoder: exiting on timeout" << std::endl;
	return EXIT_SUCCESS;
}

/* vim: set ts=4 sts=4 sw=4 noet: */
