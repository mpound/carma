//
// @version $Revision: 1.9 $
//
// @usage @autogen
//
// @description
//  utility to dump fault system information
//
// @key astroband "1" string
//  The astrobands for which to dump the fault information. Valid values are
//  any numbers 1-24, comma seperated, or the special keyword "all", which is
//  equivalent to all astrobands.
//
// @key type "alarm" string
//  The type of data to dump. Valid values are "alarm", "blankflag",
//  "alarmdisable", "alarmhistory" and "all". They correspond to faults
//  which are causing alarms, blanking/flagging, disabled alarms, and
//  alarm history, respectively.
//
// @logger MONITOR_FACILITY carma.fault.dumpFaults
//

/*
 * Utility to dump the fault system output
 *
 * Copyright (c) 2010 Ira W. Snyder <iws@ovro.caltech.edu>
 */

#include <iostream>
#include <cstdio>
#include <string>
#include <sstream>
#include <algorithm>

#include <boost/foreach.hpp>

#include <carma/fault/Constants.h>
#include <carma/fault/FaultTransport.h>

#include <carma/monitor/MonitorSystem.h>
using namespace carma::monitor;

#include <carma/util/types.h>
#include <carma/util/Time.h>
#include <carma/util/StringUtils.h>
#include <carma/util/Program.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/ErrorException.h>
using namespace carma::util;

class DumpFaults
{
	public:
		DumpFaults();

		void read();
		void readNewest();

		/* argument parsing */
		int setAstroband(std::string s);
		int setType(std::string s);

		/* print just the required info */
		void print();

		/* print arbitrary info */
		void printMetadata();
		void printBlankFlagFaults(const int band);
		void printAlarmFaults();
		void printAlarmDisabled();
		void printAlarmHistory();

		void printHeaderString(const std::string &s, const int band) const;
		void printHeaderString(const std::string &s) const;
		void printNameString(const std::string &s) const;

	protected:
		FaultTransportReader reader;

		std::list<int> astrobands_;
		std::vector<std::string> types_;
};

DumpFaults::DumpFaults()
	: reader()
{
	/* intentionally left empty */
}

void DumpFaults::read()
{
	this->reader.read();
}

void DumpFaults::readNewest()
{
	this->reader.readNewest();
}

int DumpFaults::setAstroband(std::string s)
{
	/* lowercase the string */
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);

	/* chomp whitespace */
	StringUtils::trimWhiteSpaceInplace(s);

	/* special string "all" */
	if (s == "all") {
		for (int i = 1; i <= NUM_ASTROBANDS; i++)
			this->astrobands_.push_back(i);

		return 0;
	}

	/* tokenize based on commas */
	std::vector<std::string> tokens;
	StringUtils::tokenizeInplace(tokens, s, ",");

	if (tokens.empty()) {
		std::ostringstream oss;

		oss << "parameter \"astroband\" is the empty string";
		throw CARMA_ERROR(oss.str());
	}

	BOOST_FOREACH(const std::string &token, tokens) {
		std::istringstream iss(token);
		int num;

		/* convert the token to an int */
		if (!(iss >> num)) {
			std::ostringstream oss;

			oss << "unable to parse \"" << num << "\" as an integer";
			throw CARMA_ERROR(oss.str());
		}

		/* check the range */
		if (num < 1 || num > NUM_ASTROBANDS) {
			std::ostringstream oss;
			oss << "astroband number " << num << "is out of range [1,"
				<< NUM_ASTROBANDS << "]";
			throw CARMA_ERROR(oss.str());
		}

		this->astrobands_.push_back(num);
	}

	return 0;
}

int DumpFaults::setType(std::string s)
{
	/* lowercase the string */
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);

	/* chomp whitespace */
	StringUtils::trimWhiteSpaceInplace(s);

	/* tokenize based on commas */
	std::vector<std::string> tokens;
	StringUtils::tokenizeInplace(tokens, s, ",");

	if (tokens.empty()) {
		std::ostringstream oss;

		oss << "parameter \"type\" is the empty string";
		throw CARMA_ERROR(oss.str());
	}

	BOOST_FOREACH(const std::string &token, tokens) {
		/* check all of the valid possibilities */
		if (token == "alarm" || token == "alarmdisable")
			continue;

		if (token == "alarmhistory")
			continue;

		if (token == "blankflag" || token == "all")
			continue;

		std::ostringstream oss;

		oss << "type is invalid: \"" << token << "\"\n"
			<< "Valid choices are: alarm, alarmdisable, alarmhistory, blankflag, all";
		throw CARMA_ERROR(oss.str());
	}

	BOOST_FOREACH(const std::string &token, tokens) {
		this->types_.push_back(token);
	}

	return 0;
}

void DumpFaults::print()
{
	std::vector<std::string> types = this->types_;

	bool doPrintAlarmHistory = false;
	bool doPrintAlarmDisabled = false;
	bool doPrintAlarmFaults = false;
	bool doPrintBlankFlag = false;

	/* figure out exactly what we are supposed to print */
	BOOST_FOREACH(const std::string &type, types) {
		if (type == "all") {
			doPrintAlarmFaults = true;
			doPrintAlarmHistory = true;
			doPrintAlarmDisabled = true;
			doPrintBlankFlag = true;
		}

		if (type == "alarm")
			doPrintAlarmFaults = true;

		if (type == "alarmdisable")
			doPrintAlarmDisabled = true;

		if (type == "alarmhistory")
			doPrintAlarmHistory = true;

		if (type == "blankflag")
			doPrintBlankFlag = true;
	}

	/* metadata first */
	this->printMetadata();

	/* print blankflag info for each astroband, if requested */
	if (doPrintBlankFlag) {
		const std::list<int> &astrobands = this->astrobands_;
		BOOST_FOREACH(const int astroband, astrobands) {
			this->printBlankFlagFaults(astroband);
		}
	}

	/* print alarm data, if requested */
	if (doPrintAlarmFaults)
		this->printAlarmFaults();

	if (doPrintAlarmDisabled)
		this->printAlarmDisabled();

	if (doPrintAlarmHistory)
		this->printAlarmHistory();
}

void DumpFaults::printMetadata()
{
	carma::util::frameType frame = this->reader.getCmsFrameNumber();
	uint64_t cycle = this->reader.getFaultCycleNumber();

	std::cout << std::string(80, '-') << std::endl;
	std::cout << "Cycle " << cycle << " (Frame " << frame << ": "
			  << carma::util::Time::getDateTimeString(frame, 3, "%d %b %Y")
			  << ")" << std::endl;
	std::cout << std::string(80, '-') << std::endl;
}

void DumpFaults::printBlankFlagFaults(const int astroband)
{
	InputFaultList faults;
	uint32_t number;

	this->printHeaderString("Input Blank/Flag Faults", astroband);

	this->reader.getComplexInputFaults(astroband, number, faults);
	if (faults.empty()) {
		this->printNameString("none");
		return;
	}

	BOOST_FOREACH(const InputFaultList::value_type &fault, faults) {
		std::ostringstream oss;
		oss << "Band=" << fault.band
			<< " Input=" << fault.input
			<< " MP=" << fault.name;

		this->printNameString(oss.str());
	}

	if (number > 0) {
		std::ostringstream oss;
		oss << "and " << number << " more";
		this->printNameString(oss.str());
	}
}

void DumpFaults::printAlarmFaults()
{
	StringList names;
	uint32_t number;

	this->printHeaderString("Alarm Faults");

	this->reader.getAlarmFaults(number, names);
	if (names.empty()) {
		this->printNameString("none");
		return;
	}

	BOOST_FOREACH(const StringList::value_type &name, names) {
		this->printNameString(name);
	}

	if (number > 0) {
		std::ostringstream oss;
		oss << "and " << number << " more";
		this->printNameString(oss.str());
	}
}

void DumpFaults::printAlarmDisabled()
{
	StringList names;
	uint32_t number;

	this->printHeaderString("Disabled Alarms");

	this->reader.getAlarmDisabled(number, names);
	if (names.empty()) {
		this->printNameString("none");
		return;
	}

	BOOST_FOREACH(const StringList::value_type &name, names) {
		this->printNameString(name);
	}

	if (number > 0) {
		std::ostringstream oss;
		oss << "and " << number << " more";
		this->printNameString(oss.str());
	}
}

void DumpFaults::printAlarmHistory()
{
	StringList names;
	uint32_t number;

	this->printHeaderString("Alarm History");

	this->reader.getAlarmHistory(number, names);
	if (names.empty()) {
		this->printNameString("none");
		return;
	}

	BOOST_FOREACH(const StringList::value_type &name, names) {
		this->printNameString(name);
	}

	if (number > 0) {
		std::ostringstream oss;
		oss << "and " << number << " more";
		this->printNameString(oss.str());
	}
}

void DumpFaults::printHeaderString(const std::string &s, const int astroband) const
{
	std::cout << "\n--- " << s << " for Astroband " << astroband << " ---" << std::endl;
}

void DumpFaults::printHeaderString(const std::string &s) const
{
	std::cout << "\n--- " << s << " ---" << std::endl;
}

void DumpFaults::printNameString(const std::string &s) const
{
	std::cout << "    " << s << std::endl;
}

int Program::main ()
{
	try {
		/*
		 * Force creation of a CarmaMonitorSystem so that any on-the-fly
		 * tag id's can be resolved correctly into their string names.
		 */
		const std::auto_ptr<CarmaMonitorSystem> cms(new CarmaMonitorSystem);

		DumpFaults dump;

		/* parse parameters */
		dump.setAstroband(getStringParameter("astroband"));
		dump.setType(getStringParameter("type"));

		/* read the newest data */
		dump.readNewest();

		while (true) {

			/* blocking read */
			dump.read();

			/* print the correct data */
			dump.print();

			/* and some blank lines */
			std::cout << std::endl << std::endl;
		}
	} catch (...) {
		std::cerr << "ERROR: " << getStringForCaught() << std::endl;
		exit(EXIT_FAILURE);
	}

	return 0;
}

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
