/*
 * Fault System Email Reporting Service
 *
 * Copyright (c) 2010-2011 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <list>
#include <vector>

#include <string.h>

#include <ace/OS.h>
#include <ace/Pipe.h>
#include <ace/Process.h>

#include <boost/foreach.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/local_time_adjustor.hpp>
#include <boost/date_time/c_local_time_adjustor.hpp>

#include <carma/fault/EmailManager.h>

#include <carma/util/ThreadQuit.h>
#include <carma/util/ScopedLogNdc.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
#include <carma/util/Program.h>
#include <carma/util/Time.h>
using namespace carma::util;

#include <carma/services/Table.h>

/*----------------------------------------------------------------------------*/
/* Fixed ACE_Program_Options class override                                   */
/*----------------------------------------------------------------------------*/

/*
 * The ACE_Process_Options class is broken, this is a workaround
 *
 * When processing a command line already in argv[] form, the
 * command_line() method turns it into a string, then re-parses
 * it into an argv[] again. This loses important information, and
 * breaks when single quotes are part of an argument.
 *
 * This class works around it by implementing the command_line_fixed()
 * method, which does the right thing.
 */
class Fixed_Process_Options : public ACE_Process_Options
{
	public:
		Fixed_Process_Options();
		int command_line_fixed(const char *argv[]);
};

Fixed_Process_Options::Fixed_Process_Options()
	: ACE_Process_Options()
{
	/* all functionality in base class */
}

int Fixed_Process_Options::command_line_fixed(const char *argv[])
{
	int i = 0;

	while (i < (MAX_COMMAND_LINE_OPTIONS - 1) && argv[i] != 0) {
		command_line_argv_[i] = const_cast<ACE_TCHAR *>(argv[i]);
		i++;
	}

	command_line_argv_[i] = 0;
	command_line_argv_calculated_ = 1;
	return 0;
}

/*----------------------------------------------------------------------------*/
/* Email Sender                                                               */
/*----------------------------------------------------------------------------*/

/*
 * Send an email
 *
 * This calls the external mutt program to send the email to the correct
 * address. It sends a pre-formatted email message.
 *
 * The mutt mailer was chosen since it adds the headers needed to be compatible
 * with a wider majority of email clients. This includes the hypermail archive
 * program. The mailx mailer does not add these headers, nor does it provide
 * a simple way to do so.
 *
 * This was actually quite difficult to get right. The implementation took
 * several hours of debugging with strace before it would actually send the
 * message. Don't mess with it unless you know how the lowlevel file descriptor
 * syscalls actually work!
 */
static int sendEmail(const std::string &address, const std::string &subject,
					 const std::string &body)
{
	Fixed_Process_Options options;
	ACE_Process process;
	ACE_Pipe pipe;
	ssize_t size;
	pid_t pid;
	int ret;

	/*
	 * Create the true argv[] vector for execve()
	 */
	const char *argv[] = {
		"mutt",
		"-y",
		"-b",
		"archive_rtsalarm@mmarray.org",
		"-s",
		subject.c_str(),
		address.c_str(),
		NULL,
	};

	/*
	 * Create the UNIX pipe
	 */
	ret = pipe.open();
	if (ret < 0) {
		std::ostringstream oss;

		oss << "pipe open failed: " << strerror(errno);
		programLogErrorIfPossible(oss.str());
		return ret;
	}

	/*
	 * Prepare the process options
	 *
	 * This sets up the argument vector and file descriptors
	 * for the to-be-spawned process.
	 *
	 * Note that the set_handles() method duplicates the file descriptors,
	 * and so they must be closed with release_handles().
	 */
	options.command_line_fixed(argv);
	options.set_handles(pipe.read_handle(), ACE_INVALID_HANDLE, ACE_INVALID_HANDLE);

	/*
	 * We want the pipe to remain open over the spawn (the exec() syscall).
	 * Therefore, we need to make sure that the write end of the pipe is
	 * closed by the child process.
	 *
	 * Setting the CLOEXEC flag forces the kernel to close the fd on exec()
	 * for us, so we don't have to do anything fancy.
	 *
	 * If we had forgotten to close the write end of the pipe on exec(), the
	 * child process will block forever while reading it's stdin, which is hooked
	 * to the read end of the pipe. This means it will never finish and we'll hang.
	 *
	 * An alternative method would be to fill and then close the pipe here, before
	 * the exec() happens. The set_handles() method has duplicated the read side
	 * of the pipe, and the kernel will buffer the data. Most kernel implementations
	 * only buffer up to 1 page of data, which can be shorter than the message
	 * length. Hence why we go through the trouble: arbitrary length messages.
	 */
	ret = ACE_OS::fcntl(pipe.write_handle(), F_SETFD, FD_CLOEXEC);
	if (ret < 0) {
		std::ostringstream oss;

		oss << "fcntl failed: " << strerror(errno);
		programLogErrorIfPossible(oss.str());

		options.release_handles();
		pipe.close();
		return ret;
	}

	/*
	 * Start the child process
	 */
	pid = process.spawn(options);
	if (pid < 0) {
		std::ostringstream oss;

		oss << "spawn failed: " << strerror(errno);
		programLogErrorIfPossible(oss.str());

		options.release_handles();
		pipe.close();
		return pid;
	}

	/*
	 * Write the body of the message to the child stdin
	 */
	size = pipe.send_n(body.c_str(), body.size());
	if (size < 0) {
		std::ostringstream oss;

		oss << "write failed: " << strerror(errno);
		programLogErrorIfPossible(oss.str());

		options.release_handles();
		pipe.close();
		return size;
	}

	/*
	 * The set_handles() method duplicates the file descriptors,
	 * so we must close them now that we're done spawning the
	 * child process.
	 *
	 * Also, we're done with the pipe, so close it's file descriptors
	 * too.
	 */
	options.release_handles();
	pipe.close();

	/*
	 * Wait for the child process to complete.
	 */
	pid = process.wait(&ret);
	if (pid < 0) {
		std::ostringstream oss;

		oss << "wait failed: " << strerror(errno);
		programLogErrorIfPossible(oss.str());
		return pid;
	}

	/* success */
	return 0;
}

/*----------------------------------------------------------------------------*/
/* Date and Time Helpers                                                      */
/*----------------------------------------------------------------------------*/

static std::string locDateTime(const carma::util::frameType frame)
{
	using namespace boost::date_time;
	using namespace boost::posix_time;
	using namespace boost::gregorian;

	// CARMA is UTC-8 (Pacific Time)
	typedef local_adjustor<ptime, -8, us_dst> us_pacific;

	ptime utc(from_time_t(Time::gettime_t(frame)));
	ptime loc = us_pacific::utc_to_local(utc);

	std::ostringstream oss;

	oss << to_iso_extended_string(loc.date())
		<< " "
		<< to_simple_string(loc.time_of_day());

	// this sucks
	if (frame % 2 == 0)
		oss << ".000";
	else
		oss << ".500";

	return oss.str();
}

static std::string utcDateTime(const carma::util::frameType frame)
{
	return Time::getDateTimeString(frame, 3, "%F");
}

static std::string utcTime(const carma::util::frameType frame)
{
	return Time::getTimeString(frame, 3);
}

/*----------------------------------------------------------------------------*/
/* File Access Helpers                                                        */
/*----------------------------------------------------------------------------*/

static std::string slurpfile(const std::string &filename)
{
	std::ifstream input(filename.c_str(), std::ios::in | std::ios::binary);
	std::string output;
	bool done = false;
	char buf[4096];

	/* if the state is not immediately good, the file didn't exist */
	if (!input.good()) {
		std::ostringstream oss;

		oss << "open file for input failed: " << filename;
		throw CARMA_ERROR(oss.str());
	}

	/* read the entire contents of the file */
	while (!done) {
		memset(buf, 0, sizeof(buf));
		done = !(input.read(buf, sizeof(buf)));

		output += buf;
	}

	return output;
}

/*----------------------------------------------------------------------------*/
/* The EmailBin class                                                         */
/*----------------------------------------------------------------------------*/

EmailBin::EmailBin(const struct EmailBinSettings &settings)
	: settings_(settings)
	, regex_(settings.regex, boost::regex::icase | boost::regex::extended)
	, hysteresisFrames_(settings.hysteresis * 2)
	, templateBody_(slurpfile(settings.filename))
	, faults_()
	, lastActiveFrame_(0)
	, shouldSendEmail_(false)
{
	/*
	 * Some people adding email addresses to the fault system email
	 * configuration cannot be counted on to type valid email addresses.
	 * This code will raise an error when invalid email addresses are
	 * typed into the configuration.
	 *
	 * This regex gets about 99% of valid email addresses:
	 * http://www.regular-expressions.info/regexbuddy/email.html
	 */
	const boost::regex re("\\b[A-Z0-9._%-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}\\b", boost::regex::icase | boost::regex::perl);
	if (!boost::regex_match(settings.address, re)) {
		std::ostringstream oss;
		oss << "Invalid email address: \"" << settings.address << "\""
			<< " on line with MonitorPoint regex: \"" << settings.regex << "\"";
		throw CARMA_ERROR(oss.str());
	}
}

/*
 * Clear internal state for the next iteration
 *
 * This function should be called before attempting to use
 * the object.
 */
void EmailBin::clear()
{
	this->faults_.clear();
	this->shouldSendEmail_ = false;
}

bool EmailBin::addFault(const int frame, const std::string &name)
{
	/* Check that this fault matches the regular expression */
	if (!this->matchesRegex(name)) {
		return false;
	}

	/*
	 * We have found a fault that matches our regular expression. Calculate
	 * the number of frames since this email was last activated.
	 *
	 * If we have exceeded the ratelimit period, then we should send email.
	 */
	int numFrames = frame - this->lastActiveFrame_;
	if (numFrames > this->hysteresisFrames_) {
		this->shouldSendEmail_ = true;
	}

	/*
	 * Add this fault to the list of faults that should be sent in the
	 * email. Also update the last activated frame. This will make sure
	 * that the ratelimit code keeps us from sending an email if the
	 * alarm has re-appeared too quickly.
	 */
	this->faults_.push_back(name);
	this->lastActiveFrame_ = frame;

	/* should we stop processing */
	return this->settings_.stop_processing;
}

bool EmailBin::shouldSendEmail() const
{
	return this->shouldSendEmail_;
}

std::string EmailBin::formatMessageSubject(const carma::util::frameType frame) const
{
	return "Fault System Alarm -- " + utcTime(frame) + " UTC";
}

/*
 * Generate the email text
 *
 * The fault system wants to add some various text to the email
 * depending on the faults, timestamp, etc. This function takes
 * the template and generates the email.
 */
std::string EmailBin::formatMessageBody(const carma::util::frameType frame) const
{
	const StringList &faults = this->faults_;
	std::ostringstream oss;

	/* append the file contents */
	oss << this->templateBody_;

	/* append the list of monitor points */
	oss << "\n\n"
		<< "List of monitor points that caused this email to be sent:"
		<< "\n";

	BOOST_FOREACH(const StringList::value_type &fault, faults) {
		oss << fault << "\n";
	}

	/* append the fault system footer */
	oss << "\n\n"
		<< "This email automatically generated by the CARMA Fault System\n"
		<< "Monitor Frame: " << frame << "\n"
		<< "Timestamp: " << utcDateTime(frame) << " UTC\n"
		<< "Timestamp: " << locDateTime(frame) << " LOCAL\n";

	return oss.str();
}

/*
 * Get the filename used to send the email
 *
 * This is used for convenient logging only. Nothing else.
 */
std::string EmailBin::getFileName() const
{
	return this->settings_.filename;
}

/*
 * Get the email address used to send the email
 *
 * This is used for convenient logging only. Nothing else.
 */
std::string EmailBin::getEmailAddress() const
{
	return this->settings_.address;
}

/*
 * Check if a text string matches the compiled regex
 */
bool EmailBin::matchesRegex(const std::string &name) const
{
	return boost::regex_match(name, this->regex_, boost::regex_constants::match_nosubs);
}

/*----------------------------------------------------------------------------*/
/* The EmailManager class                                                     */
/*----------------------------------------------------------------------------*/

EmailManager::EmailManager(const std::string &tab, const int holdoff)
	: frames_processed_(0)
	, frames_holdoff_(holdoff * 2)
	, emailList_()
	, queue_()
{
	carma::services::Table table(tab);

	/* loop through each regex and build an EmailBin out of it */
	for (int i = 0; i < table.getNrows(); i++) {
		struct EmailBinSettings settings;

		settings.regex = table.getColumn("regex").at(i);
		settings.filename = Program::getConfFile(table.getColumn("file").at(i));
		settings.address = table.getColumn("address").at(i);
		settings.stop_processing = table.getBoolColumn("stop").at(i);
		settings.hysteresis = table.getIntColumn("hysteresis").at(i);

		EmailBinPtr bin(new EmailBin(settings));
		this->emailList_.push_back(bin);
	}
}

/*
 * Add all of the faults for a given frame
 *
 * This is the "input" side of the queue. We generate a FaultQueueElem object
 * and push it onto the queue. The "output" thread will read it out at its
 * leisure.
 */
void EmailManager::addFaults(const int frame, const DagMLNodeList faults)
{
	FaultQueue &queue = this->queue_;
	struct FaultQueueElem elem;

	elem.frame = frame;
	BOOST_FOREACH(const DagMLNodeList::value_type &node, faults) {
		elem.faults.push_back(node->getName());
	}

	/* add to the queue */
	queue.push(elem);
}

/*
 * This is the "output" side of the queue. We read FaultQueueElem objects
 * off the queue and send email if necessary.
 *
 * This totally decouples the fault system from the time it takes to spawn
 * the mailx program to send email. Very nice behavior. :)
 */
void EmailManager::emailThreadEP(EmailManager &This)
{
	const ScopedLogNdc ndc("EmailManager::emailThread");
	FaultQueue &queue = This.queue_;

	/* run the mainloop forever */
	while (true) {
		/* run an iteration of the email system mainloop */
		try {
			ThreadQuitTestSelf();

			struct FaultQueueElem elem;
			queue.wait_and_pop(elem);

			ThreadQuitTestSelf();

			This.processElement(elem);

		} catch (...) {
			if (CaughtExceptionIsThreadQuitRequestedError()) {
				programLogInfoIfPossible("thread quit requested");
				return;
			}

			std::ostringstream oss;
			oss << "ERROR: " << getStringForCaught();
			programLogErrorIfPossible(oss.str());
		}
	}
}

void EmailManager::processElement(struct FaultQueueElem &elem)
{
	EmailBinList &emailList = this->emailList_;
	const StringList &names = elem.faults;
	const frameType frame = elem.frame;

	/* check if we should hold off processing to wait for system start */
	this->frames_processed_++;
	if (this->frames_processed_ <= this->frames_holdoff_)
		return;

	/* clear all bins for this iteration */
	BOOST_FOREACH(EmailBinPtr &bin, emailList) {
		bin->clear();
	}

	/* loop over each name in the list of faults */
	BOOST_FOREACH(const StringList::value_type &name, names) {

		/* loop over each email bin */
		BOOST_FOREACH(EmailBinPtr &bin, emailList) {
			bool stop_processing = false;

			/*
			 * Attempt to add the fault
			 *
			 * The bin knows whether it should stop processing at this point,
			 * or let us continue on to more email bins. This helps to support
			 * both multiple emails per alarm and a catchall bin at the end of
			 * the list.
			 */
			stop_processing = bin->addFault(frame, name);
			if (stop_processing)
				break;
		}
	}

	/* check all bins to see if we should send email */
	BOOST_FOREACH(EmailBinPtr &bin, emailList) {
		if (bin->shouldSendEmail()) {
			const std::string address = bin->getEmailAddress();
			const std::string subject = bin->formatMessageSubject(frame);
			const std::string body = bin->formatMessageBody(frame);
			const std::string file = bin->getFileName();

			std::ostringstream oss;
			oss << "sending email:"
				<< " frame=" << frame
				<< " file=" << file
				<< " address=" << address;

			programLogInfoIfPossible(oss.str());
			sendEmail(address, subject, body);
		}
	}
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
