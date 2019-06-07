#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
#include <carma/util/SyslogRedirector.h>

#include <iostream>
#include <cstdio>
#include <string>

/*
 * @usage @autogen
 *
 * @description
 * Test program for the carma/util/SyslogRedirector functions
 *
 * @key crash false bool crash the program via a double free error
 *
 * @logger TEST_FACILITY carma.test.util.tSyslogRedirector
 */

int carma::util::Program::main()
try {
	carma::util::forceGlibcErrorsToStderr();
	carma::util::redirectFdToSyslog(STDOUT_FILENO, "stdout");
	carma::util::redirectFdToSyslog(STDERR_FILENO, "stderr");

	/* send some output to stdout */
	std::printf("hello world from stdout via printf\n");
	std::cout << "hello world from stdout via std::cout" << std::endl;

	/* send some output to stderr */
	std::fprintf(stderr, "hello world from stderr via fprintf\n");
	std::cerr << "hello world from stderr via std::cerr" << std::endl;

	/* Very long lines will be split up by the internal implementation of the
	 * redirector. We should be able to see this. :)
	 */
	const std::string s(8192, 'a');
	std::cerr << "long string: " << s << std::endl;

	/* and now a glibc crash message (double free) */
	if (getBoolParameter("crash")) {
		void *ptr = malloc(5);
		free(ptr);
		free(ptr);
	}

	return EXIT_SUCCESS;
} catch (...) {
	/* stdout/stderr are likely redirected, so logs are the only option! */
	carma::util::programLogErrorIfPossible(carma::util::getStringForCaught());
	return EXIT_FAILURE;
}
