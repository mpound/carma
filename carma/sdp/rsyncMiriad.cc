//
// @version $Revision: 1.4 $
//
// @usage @autogen
//
// @description
//  MIRIAD-aware rsync
//
// @key daemon false bool
// Run in daemon mode
//
// @key sleep 30 int
// Time between each daemon iteration
//
// @key src "" string
// The source directory (in rsync host:/path/ format)
//
// @key dst "" string
// The destination directory (in rsync host:/path/ format)
//
// @key ssh true bool
// Use the "-e ssh" rsync option
//
// @key bwlimit 0 int
// rsync bandwidth limit (in KBPS)
//
// @key delete false bool
// Use the "--delete" rsync option (DANGEROUS)
//
// @logger MONITOR_FACILITY carma.sdp.rsyncMiriad
//

/*
 * MIRIAD-aware rsync
 */

#include <iostream>
#include <cstdio>
#include <sstream>

#include <boost/regex.hpp>

#include <unistd.h>
#include <glib.h>

#include <carma/util/Program.h>
#include <carma/util/loggingUtils.h>
#include <carma/util/programExtras.h>
#include <carma/util/programLogging.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
using namespace carma::util;

#include <carma/corba/Server.h>

static const std::string rsyncCommand = "rsync";
static const std::string rsyncOptions = "-ah --stats";

struct options {
	bool daemon;
	bool sshOption;
	bool deleteOption;
	unsigned int sleepSeconds;
	unsigned int bwlimit;

	std::string src;
	std::string dst;
};

static void log_command_output(const std::string &stdoutdata, const std::string &stderrdata)
{
	log4cpp::Category &logger = getProgramLogger();

	// stdout
	if (!stdoutdata.empty()) {
		logMultipleLines(logger, log4cpp::Priority::INFO, "stdout:\n" + stdoutdata);
	}

	// stderr
	if (!stderrdata.empty()) {
		logMultipleLines(logger, log4cpp::Priority::INFO, "stderr:\n" + stderrdata);
	}
}

static void check_exit_status(const int status)
{
	if (WIFEXITED(status)) {
		const int child_status = WEXITSTATUS(status);
		if (child_status != 0) {
			std::ostringstream oss;
			oss << "unsucessful exit status: " << child_status;
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}

		// normal success status
		return;
	}

	if (WIFSIGNALED(status)) {
		const int signo = WTERMSIG(status);
		std::ostringstream oss;
		oss << "child terminated by signal number: " << signo;
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

static std::string create_rsync_command(const struct options &opts, const std::string &extraOptions)
{
	std::ostringstream cmd;
	cmd << rsyncCommand << " " << rsyncOptions;

	cmd << " -f 'exclude /lost+found/'";

	if (opts.sshOption) {
		cmd << " -e ssh";
	}

	if (opts.deleteOption) {
		cmd << " --delete";
	}

	if (opts.bwlimit != 0) {
		cmd << " --bwlimit=" << opts.bwlimit;
	}

	cmd << extraOptions;

	cmd << " " << opts.src
		<< " " << opts.dst;

	return cmd.str();
}

static void simple_spawn_command(const std::string &cmd, std::string &stdoutdata, std::string &stderrdata)
{
	// log what we are about to run
	{
		std::ostringstream oss;
		oss << "Run Command: " << cmd;
		programLogInfoIfPossible(oss.str());
	}

	// spawn the command
	char *stdout_p = NULL;
	char *stderr_p = NULL;
	int exit_status = 0;
	GError *err = NULL;
	const bool ret = g_spawn_command_line_sync(cmd.c_str(), &stdout_p, &stderr_p, &exit_status, &err);
	if (ret == FALSE) {
		std::ostringstream oss;
		oss << "spawn failed: " << err->message;
		g_clear_error(&err);

		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	// save and free the output strings
	stdoutdata = stdout_p;
	stderrdata = stderr_p;
	g_free(stdout_p);
	g_free(stderr_p);

	// log the output
	log_command_output(stdoutdata, stderrdata);

	// check the exit status
	check_exit_status(exit_status);
	programLogInfoIfPossible("command completed successfully");
}

static void check_rsync_version(const struct options &opts)
{
	const std::string cmd = rsyncCommand + " --version";
	std::string stdout_s;
	std::string stderr_s;

	simple_spawn_command(cmd, stdout_s, stderr_s);

	// Parse the data from stdout and see if it contains the "append"
	// feature. This feature is needed for optimal performance. If the
	// append feature is missing, there will be much more disk I/O.
	const boost::regex re("\\<append\\>");
	if (!boost::regex_search(stdout_s, re)) {
		const std::string msg = "rsync missing append support";
		programLogErrorIfPossible(msg);
		throw CARMA_ERROR(msg);
	}
}

static void run_rsync_visdata(const struct options &opts)
{
	std::ostringstream extras;
#if 1
	// Temporarily disable MIRIAD visdata optimization due to corruption
	// issues. The sdpFiller is currently not writing out visdata in a
	// strictly append-only way. It re-fills several times per track, and
	// changes values as it goes.
	extras << " -f 'include */'"
		   << " -f 'include visdata'"
		   << " -f 'exclude *'";
#else
	extras << " -f 'include */'"
		   << " -f 'include visdata'"
		   << " -f 'exclude *'"
		   << " --append";
#endif

	const std::string cmd = create_rsync_command(opts, extras.str());
	std::string stdout_s;
	std::string stderr_s;

	simple_spawn_command(cmd, stdout_s, stderr_s);
}

static void run_rsync_otherdata(const struct options &opts)
{
	const std::string cmd = create_rsync_command(opts, " -f 'exclude visdata'");
	std::string stdout_s;
	std::string stderr_s;

	simple_spawn_command(cmd, stdout_s, stderr_s);
}

static void run_single_iteration(const struct options &opts)
{
	run_rsync_otherdata(opts);
	run_rsync_visdata(opts);
}

static void run_daemon_mode(const struct options &opts)
{
	Program &program = Program::getProgram();
	program.getCorbaServer().run(true);

	while (!program.imrTerminationRequested()) {
		// run a single iteration of the rsync
		try {
			run_single_iteration(opts);
		} catch (...) {
			std::ostringstream oss;
			oss << "ERROR: " << getStringForCaught();
			programLogErrorIfPossible(oss.str());
		}

		// check for termination at one second intervals
		for (unsigned int i = 0; i < opts.sleepSeconds; i++) {
			if (program.imrTerminationRequested()) {
				programLogInfoIfPossible("IMR termination requested");
				return;
			}

			sleep(1);
		}
	}
}

int Program::main()
try {
	struct options opts;
	opts.daemon = getBoolParameter("daemon");
	opts.sshOption = getBoolParameter("ssh");
	opts.deleteOption = getBoolParameter("delete");
	opts.sleepSeconds = getIntParameter("sleep");
	opts.bwlimit = getIntParameter("bwlimit");
	opts.src = getStringParameter("src");
	opts.dst = getStringParameter("dst");

	// check source parameter
	if (opts.src.empty()) {
		const std::string msg = "required parameter src= not specified";
		programLogErrorIfPossible(msg);
		std::cerr << msg << std::endl;
		return EXIT_FAILURE;
	}

	// check destination parameter
	if (opts.dst.empty()) {
		const std::string msg = "required parameter dst= not specified";
		programLogErrorIfPossible(msg);
		std::cerr << msg << std::endl;
		return EXIT_FAILURE;
	}

	// make certain that paths have a trailing slash
	opts.src += "/";
	opts.dst += "/";

	// check that rsync has append support
	check_rsync_version(opts);

	// run the program
	//
	// Daemon mode should always keep running, even in the face of
	// rsync errors
	//
	// Single-shot mode should exit with the appropriate exit status
	if (opts.daemon) {
		run_daemon_mode(opts);
	} else {
		run_single_iteration(opts);
	}

	return EXIT_SUCCESS;
} catch (...) {
	std::ostringstream oss;
	oss << "ERROR: " << getStringForCaught();
	programLogErrorIfPossible(oss.str());
	std::cerr << oss.str() << std::endl;
	return EXIT_FAILURE;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
