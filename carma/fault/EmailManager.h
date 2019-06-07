/*
 * Fault System Error Reporting via Email
 *
 * Copyright (c) 2010-2011 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#ifndef EMAIL_MANAGER_H
#define EMAIL_MANAGER_H

#include <queue>
#include <list>

#include <boost/shared_ptr.hpp>
#include <boost/regex.hpp>

#include <carma/fault/DagMLNode.h>

#include <carma/util/ConcurrentQueue.h>
#include <carma/util/types.h>

typedef std::list<std::string> StringList;
typedef boost::shared_ptr<class EmailBin> EmailBinPtr;

/* Holds the settings for the EmailBin class */
struct EmailBinSettings
{
	std::string regex;
	std::string filename;
	std::string address;
	bool stop_processing;
	int hysteresis; /* in seconds */
};

class EmailBin
{
	public:
		EmailBin(const struct EmailBinSettings &settings);

		/* clear internal state at the beginning of each frame */
		void clear();

		/* add a fault in the given frame */
		bool addFault(const int frame, const std::string &name);

		/* should we send email */
		bool shouldSendEmail() const;

		/* message contents */
		std::string formatMessageSubject(const carma::util::frameType frame) const;
		std::string formatMessageBody(const carma::util::frameType frame) const;

		/* get some settings */
		std::string getFileName() const;
		std::string getEmailAddress() const;

	protected:

		bool matchesRegex(const std::string &name) const;

		/* the settings we were given */
		const struct EmailBinSettings settings_;

		/* the regular expression and hysteresis frames */
		boost::regex regex_;
		const int hysteresisFrames_;

		/* email template body */
		const std::string templateBody_;

		/* active data that changes per-frame */
		StringList faults_;
		int lastActiveFrame_;
		bool shouldSendEmail_;
};

class EmailManager
{
	public:
		EmailManager(const std::string &tab, const int holdoff);

		/* add the faults for a given frame */
		void addFaults(const int frame, const DagMLNodeList faults);

		/* thread entry point */
		static void emailThreadEP(EmailManager &This);

	protected:

		/* private structure for internal queueing */
		struct FaultQueueElem {
			int frame;
			StringList faults;
		};

		typedef carma::util::ConcurrentQueue<struct FaultQueueElem> FaultQueue;
		typedef std::list<EmailBinPtr> EmailBinList;

		/* support for startup holdoff */
		unsigned int frames_processed_;
		unsigned int frames_holdoff_;

		EmailBinList emailList_;
		FaultQueue queue_;

		/* process a queue element */
		void processElement(struct FaultQueueElem &elem);
};

#endif /* EMAIL_MANAGER_H */

/* vim: set ts=4 sts=4 sw=4 noet tw=92: */
