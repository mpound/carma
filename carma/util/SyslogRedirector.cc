#include <carma/util/SyslogRedirector.h>
#include <carma/util/programLogging.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ScopedLogNdc.h>

#include <boost/shared_ptr.hpp>

#include <sstream>
#include <string>

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <syslog.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>
#include <fcntl.h>
#include <signal.h>

using namespace carma::util;

/* -------------------------------------------------------------------------- */
/* Message Buffer                                                             */
/* -------------------------------------------------------------------------- */

/*
 * This message buffer can handle partial lines written to the buffer each time
 * the msgbuf_add() function is called. It writes each line to syslog as a
 * new line character is found.
 *
 * The maximum line length supported is 4096 characters.
 *
 * We don't want to risk any dynamic memory allocations while using this code,
 * since it can be called during a crash (for example, during glibc's error
 * report caused by heap corruption). Therefore we roll our own message buffer.
 */

struct msgbuf {
	char msg[4096];
	int pos;
};

static void msgbuf_init(struct msgbuf *mb)
{
	memset(mb->msg, 0, sizeof(mb->msg));
	mb->pos = 0;
}

static bool msgbuf_full(struct msgbuf *mb)
{
	return mb->pos == (sizeof(mb->msg) - 1);
}

static void msgbuf_flush(struct msgbuf *mb)
{
	if (mb->pos) {
		/* ensure NULL termination */
		mb->msg[mb->pos] = '\0';

		/*
		 * Log the message. The INFO level was chosen as we have no other
		 * information about the contents of the log message.
		 */
		programLogInfoIfPossible(mb->msg);

		/* reset the start position in the message buffer */
		mb->pos = 0;
	}
}

static void msgbuf_add(struct msgbuf *mb, const char * const buf, const ssize_t len)
{
	int i;

	for (i = 0; i < len; i++) {
		const char c = buf[i];

		/* null or new line character: log the message now */
		if (c == '\0' || c == '\n') {
			msgbuf_flush(mb);
			continue;
		}

		/* full message buffer: log a partial message now */
		if (msgbuf_full(mb)) {
			msgbuf_flush(mb);
		}

		/* copy the current character into the message buffer */
		mb->msg[mb->pos] = c;
		mb->pos++;
	}
}

/* -------------------------------------------------------------------------- */
/* Thread Creation                                                            */
/* -------------------------------------------------------------------------- */

struct thread_struct {
	pthread_barrier_t barrier;
	std::string fdname;
	int fd;
};

typedef boost::shared_ptr<struct thread_struct> ThreadStructPtr;

static void *syslog_thread_entry(void *arg)
{
	const ThreadStructPtr ts = *((ThreadStructPtr *)arg);
	const std::string threadname = ts->fdname + "-redir";
	const ScopedLogNdc ndc(threadname);
	struct msgbuf mb;

	msgbuf_init(&mb);

	/* try to set the thread name, but keep going on failure */
#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 12)
	if (pthread_setname_np(pthread_self(), threadname.c_str())) {
		std::ostringstream oss;
		oss << threadname << ": pthread_setname_np: " << strerror(errno);
		programLogWarnIfPossible(oss.str());
	}
#endif
	/* signal to the main thread that we have started */
	pthread_barrier_wait(&ts->barrier);

	while (true) {
		char buf[4096];
		const ssize_t bytes = read(ts->fd, buf, sizeof(buf));

		/* end of file */
		if (bytes == 0) {
			std::ostringstream oss;
			oss << threadname << ": read: end of file reached";
			programLogInfoIfPossible(oss.str());
			break;
		}

		/* error */
		if (bytes == -1) {
			std::ostringstream oss;
			oss << threadname << ": read: error: " << strerror(errno);
			programLogErrorIfPossible(oss.str());
			break;
		}

		/* write the bytes to the log */
		msgbuf_add(&mb, buf, bytes);
	}

	{
		std::ostringstream oss;
		oss << threadname << ": exit";
		programLogInfoIfPossible(oss.str());
	}

	return NULL;
}

/* -------------------------------------------------------------------------- */
/* Signal Handler                                                             */
/* -------------------------------------------------------------------------- */

/*
 * Handler for SIGABRT
 *
 * Sleep when the abort() function is called so that the logging thread has some
 * time to actually log the error, rather than racing with the SIGABRT.
 */
static void abrthandler(int signo)
{
	/* give the logger a few seconds to catch up */
	sleep(1);
}

/* -------------------------------------------------------------------------- */
/* Syslog Redirection                                                         */
/* -------------------------------------------------------------------------- */

void carma::util::forceGlibcErrorsToStderr()
{
	/* force glibc errors to go to stderr rather than the controlling tty */
	setenv("LIBC_FATAL_STDERR_", "1", 1);
}

void carma::util::redirectFdToSyslog(int fd, const std::string &fdname)
{
	int pipefd[2];
	pthread_t thread;
	ThreadStructPtr ts(new struct thread_struct);

	/* save file descriptor name */
	ts->fdname = fdname;

	/* create a pthread_barrier to handle thread startup order issues */
	if (pthread_barrier_init(&ts->barrier, NULL, 2)) {
		std::ostringstream oss;
		oss << "pthread_barrier_init: " << strerror(errno);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 12)
	/* create a pipe between threads */
	if (pipe2(pipefd, O_CLOEXEC) == -1) {
		std::ostringstream oss;
		oss << "pipe2: " << strerror(errno);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
#else
	if (pipe(pipefd) == -1) {
		std::ostringstream oss;
		oss << "pipe: " << strerror(errno);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
#endif


	/* read end of the pipe goes into the struct */
	ts->fd = pipefd[0];

	/* setup stderr to go to the write end of the pipe */
	if (dup2(pipefd[1], fd) == -1) {
		std::ostringstream oss;
		oss << "dup2: " << strerror(errno);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	/* start the logger thread */
	if (pthread_create(&thread, NULL, &syslog_thread_entry, &ts) == -1) {
		std::ostringstream oss;
		oss << "pthread_create: " << strerror(errno);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	/*
	 * install a handler for SIGABRT to allow the writer thread to handle any
	 * errors before the program quits
	 */
	struct sigaction act;
	memset(&act, 0, sizeof(act));
	act.sa_handler = abrthandler;

	if (sigaction(SIGABRT, &act, NULL)) {
		std::ostringstream oss;
		oss << "sigaction: " << strerror(errno);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	/* wait for thread startup to complete */
	pthread_barrier_wait(&ts->barrier);
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
