/*
 * CARMA Project Database Utilities
 */

#include <carma/observertools/PDB_Util.h>

#include <carma/util/corbaSequenceUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>
#include <carma/util/StringUtils.h>

#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/foreach.hpp>

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

using namespace carma::observertools;
using namespace carma::util;

/* -------------------------------------------------------------------------- */
/* Private Helper Methods                                                     */
/* -------------------------------------------------------------------------- */

static double stringToDouble(const std::string &s)
{
	try {
		return boost::lexical_cast<double>(s);
	} catch (boost::bad_lexical_cast const &) {
		std::ostringstream oss;
		oss << "unable to convert string to double: " << s;
		throw CARMA_ERROR(oss.str());
	}
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

/* -------------------------------------------------------------------------- */
/* ProjectId Class                                                            */
/* -------------------------------------------------------------------------- */

ProjectId::ProjectId()
	: project("")
	, obsblock("")
	, subobsblock("")
	, trial(0)
{
	// intentionally left empty
}

ProjectId obsblockIdToProjectId(const std::string &obsblockId)
{
	if (StringUtils::isEmptyOrContainsWhiteSpace(obsblockId)) {
		std::ostringstream oss;
		oss << "ObsblockId [" << obsblockId << "] is empty or contains white space";
		throw CARMA_ERROR(oss.str());
	}

	const std::vector<std::string> v = StringUtils::tokenize(obsblockId, ".");
	const unsigned short size = v.size();
	if (size < 3) {
		std::ostringstream oss;
		oss << "Not enough tokens in obsblockID: " << obsblockId;
		throw CARMA_ERROR(oss.str());
	}

	if (size > 4) {
		std::ostringstream oss;
		oss << "Too many tokens in obsblockID: " << obsblockId;
		throw CARMA_ERROR(oss.str());
	}

	ProjectId p;
	p.project = v.at(0);
	p.obsblock = v.at(1);

	int trial = 0;
	if (size == 4) {
		p.subobsblock = v.at(2);
		trial = StringUtils::stringToInt(v.at(3));
	} else {
		trial = StringUtils::stringToInt(v.at(2));
	}

	if (trial <= 0) {
		std::ostringstream oss;
		oss << "Trial [" << trial << "] must be greater than zero.";
		throw CARMA_ERROR(oss.str());
	}

	p.trial = trial;

	return p;
}

std::string projectIdToObsblockId(const ProjectId &pid)
{
	std::ostringstream oss;
	oss << pid.project << "." << pid.obsblock;
	if (!pid.subobsblock.empty()) {
		oss << "." << pid.subobsblock;
	}

	oss << "." << pid.trial;
	return oss.str();
}

/* -------------------------------------------------------------------------- */
/* ItemValue Helpers                                                          */
/* -------------------------------------------------------------------------- */

ItemValue makeItemValue(const std::string &n, const std::string &v)
{
	ItemValue iv;
	iv.name = n.c_str();
	iv.value = v.c_str();

	return iv;
}

std::string itemValueToString(const ItemValue &iv)
{
	std::ostringstream oss;
	oss << iv.name << ":" << iv.value;
	return oss.str();
}

std::string itemValueVectorToString(const std::vector<ItemValue> &vec)
{
	std::vector<std::string> strVec;
	BOOST_FOREACH(const ItemValue &iv, vec) {
		strVec.push_back(itemValueToString(iv));
	}

	return boost::algorithm::join(strVec, ",");
}

std::string itemValueSequenceToString(const ItemValueSequence &seq)
{
	const std::vector<ItemValue> itemValueVec = convertSequenceToVector<ItemValue>(seq);
	return itemValueVectorToString(itemValueVec);
}

void itemValueError(const std::string &msg, const ItemValue &iv)
{
	std::ostringstream oss;
	oss << msg << ": " << itemValueToString(iv);
	programLogErrorIfPossible(oss.str());
	throw CARMA_ERROR(oss.str());
}

/* -------------------------------------------------------------------------- */
/* Identifier Helpers                                                         */
/* -------------------------------------------------------------------------- */

bool identifierIsNone(const std::string &identifier)
{
	return StringUtils::lowASCIIAlphaNumericToUpper(identifier) == "NONE";
}

bool identifierIsNoneOrEmpty(const std::string &identifier)
{
	return identifierIsNone(identifier) || identifier.empty();
}

/* -------------------------------------------------------------------------- */
/* RA and DEC Conversion Methods                                              */
/* -------------------------------------------------------------------------- */

double raConvert(const std::string &raInput)
{
	const double degToRad = 0.017453293;
	double hours, minutes, seconds = 0;
	std::string ra(raInput);

	// a native double number was provided
	if (ra.find(" ") == std::string::npos && ra.find(":") == std::string::npos) {
		return stringToDouble(ra);
	}

	boost::algorithm::trim(ra);

	std::string::size_type location = ra.find(":");
	if (location != std::string::npos) {
		// we are using ":" as a delimiter
		location = ra.find(":");
		hours = stringToDouble(ra.substr(0, location));
		ra = ra.substr(location + 1);
		location = ra.find(":");

		if (location == std::string::npos) {
			minutes = stringToDouble(ra);
			seconds = 0.0;
		} else {
			minutes = stringToDouble(ra.substr(0, location));
			ra = ra.substr(location + 1);
			location = ra.find(":");

			if (location == std::string::npos && !ra.empty()) {
				seconds = stringToDouble(ra);
			}
		}
	} else {
		// we are using " " as a delimiter
		hours = stringToDouble(ra.substr(0, location));
		ra = ra.substr(location + 1);
		location = ra.find(" ");

		if (location == std::string::npos) {
			minutes = stringToDouble(ra);
			seconds = 0.0;
		} else {
			minutes = stringToDouble(ra.substr(0, location));
			ra = ra.substr(location + 1);
			location = ra.find(" ");

			if (location == std::string::npos && !ra.empty()) {
				seconds = stringToDouble(ra);
			}
		}
	}

	hours += minutes / 60.0 + seconds / 3600.0;
	return hours * 15.0 * degToRad;
}

double decConvert(const std::string &decInput)
{
	const double degToRad = 0.017453293;
	double degrees, minutes, seconds = 0;
	std::string dec(decInput);

	// a native double number was provided
	if (dec.find(" ") == std::string::npos && dec.find(":") == std::string::npos) {
		return stringToDouble(dec);
	}

	boost::algorithm::trim(dec);

	const bool negative = (dec.find("-") != std::string::npos);

	std::string::size_type location = dec.find(":");
	if (location != std::string::npos) {
		// we are using ":" as a delimiter
		location = dec.find(":");
		degrees = stringToDouble(dec.substr(0, location));
		dec = dec.substr(location + 1);
		location = dec.find(":");

		if (location == std::string::npos) {
			minutes = stringToDouble(dec);
			seconds = 0.0;
		} else {
			minutes = stringToDouble(dec.substr(0, location));
			dec = dec.substr(location + 1);
			location = dec.find(":");

			if (location == std::string::npos && !dec.empty()) {
				seconds = stringToDouble(dec);
			}
		}
	} else {
		// we are using " " as a delimiter
		degrees = stringToDouble(dec.substr(0, location));
		dec = dec.substr(location + 1);
		location = dec.find(" ");

		if (location == std::string::npos) {
			minutes = stringToDouble(dec);
			seconds = 0.0;
		} else {
			minutes = stringToDouble(dec.substr(0, location));
			dec = dec.substr(location + 1);
			location = dec.find(" ");

			if (location == std::string::npos && !dec.empty()) {
				seconds = stringToDouble(dec);
			}
		}
	}

	if (degrees < 0.0 || negative) {
		degrees -= minutes / 60.0 - seconds / 3600.0;
	} else {
		degrees += minutes / 60.0 + seconds / 3600.0;
	}

	return degrees * degToRad;
}

/* -------------------------------------------------------------------------- */
/* File Utilities                                                             */
/* -------------------------------------------------------------------------- */

std::string readFile(const std::string &filename)
{
	std::ifstream fin(filename.c_str(), std::ios::binary);
	if (!fin.good()) {
		std::ostringstream oss;
		oss << "Unable to read file: " << filename;
		throw CARMA_ERROR(oss.str());
	}

	std::ostringstream oss;
	oss << fin.rdbuf();
	fin.close();

	return oss.str();
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
