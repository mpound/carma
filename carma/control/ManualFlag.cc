/*
 * Control Subsystem Manual Blank/Flag/Birdie support configuration file
 * parser.
 */

#include <sstream>
#include <string>
#include <locale>

#include <boost/shared_ptr.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/gregorian/parsers.hpp>

#include <carma/services/Table.h>

#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>

#include <carma/control/ManualFlag.h>

typedef carma::monitor::ManualFlagPreferenceMonitorPointEnum MFPrefMPE;

/*
 * Parse and validate a band/input integer from the configuration file.
 */
static int parseInt(const std::string &s)
{
	std::istringstream iss(s);
	int num;

	// special value 255 means "don't care"
	if (s == "X" || s == "x")
		return 255;

	if (!(iss >> num)) {
		std::ostringstream oss;
		oss << "could not parse \"" << s << "\" as an int";
		throw CARMA_ERROR(oss.str());
	}

	// validate the range
	if (num < 0) {
		std::ostringstream oss;
		oss << "invalid band/input (negative): " << s;
		throw CARMA_ERROR(oss.str());
	} else if (num == 0) {
		std::ostringstream oss;
		oss << "invalid band/input (zero): " << s;
		throw CARMA_ERROR(oss.str());
	} else if (num > 32) {
		std::ostringstream oss;
		oss << "invalid band/input (too high): " << s;
		throw CARMA_ERROR(oss.str());
	}

	return num;
}

static MFPrefMPE::MANUALFLAGPREFERENCE parsePreference(const std::string &s)
{
	if (s == "BLANK") {
		return MFPrefMPE::BLANK;
	} else if (s == "FLAG") {
		return MFPrefMPE::FLAG;
	} else if (s == "BIRDIE") {
		return MFPrefMPE::BIRDIE;
	} else {
		std::ostringstream oss;
		oss << "could not parse \"" << s << "\" as a preference";
		throw CARMA_ERROR(oss.str());
	}
}

static boost::posix_time::ptime parseDate(const std::string &s)
{
	// std::locale constructor will free this object for us
	boost::posix_time::time_input_facet *facet = new boost::posix_time::time_input_facet("%Y%b%d:%H:%M:%S");
	//std::locale::locale loc(std::locale::classic(), facet);
	std::locale loc(std::locale::classic(), facet);

	std::istringstream iss(s);
	iss.imbue(loc);

	boost::posix_time::ptime t;
	iss >> t;

	if (t == boost::posix_time::not_a_date_time) {
		std::ostringstream oss;
		oss << "unable to parse \"" << s << "\" as a date-time";
		throw CARMA_ERROR(oss.str());
	}

	return t;
}

namespace carma {
namespace control {

ManualFlag::ManualFlag()
	: start(boost::posix_time::not_a_date_time)
	, end(boost::posix_time::not_a_date_time)
	, band(255)
	, input1(255)
	, input2(255)
	, preference(MFPrefMPE::NONE)
{
	// intentionally left empty
}

std::vector<ManualFlagPtr> parseManualFlagTable(const std::string &filename, const bool currentOnly)
{
	using namespace boost::posix_time;

	const ptime now = second_clock::local_time();
	const carma::services::Table table(filename);
	std::vector<ManualFlagPtr> vec;

	for (int i = 0; i < table.getNrows(); i++) {
		ManualFlagPtr mf(new ManualFlag());
		std::string colname;
		std::string value;

		try {
			colname = "startdatetime";
			value = table.getColumn(colname).at(i);
			mf->start = parseDate(value);

			colname = "enddatetime";
			value = table.getColumn(colname).at(i);
			mf->end = parseDate(value);

			colname = "band";
			value = table.getColumn(colname).at(i);
			mf->band = parseInt(value);

			colname = "input1";
			value = table.getColumn(colname).at(i);
			mf->input1 = parseInt(value);

			colname = "input2";
			value = table.getColumn(colname).at(i);
			mf->input2 = parseInt(value);

			colname = "preference";
			value = table.getColumn(colname).at(i);
			mf->preference = parsePreference(value);
		} catch (...) {
			std::ostringstream oss;
			oss << "unable to parse value for column name: \"" << colname << "\""
				<< " (row: " << i << ")"
				<< " (value: " << value << ")"
				<< " message: " << carma::util::getStringForCaught();
			throw CARMA_ERROR(oss.str());
		}

		// check for bogus datetimes
		if (mf->start >= mf->end) {
			std::ostringstream oss;
			oss << "start datetime (" << mf->start << ") occurs before"
				<< " end datetime (" << mf->end << ")";
			throw CARMA_ERROR(oss.str());
		}

		// check inputs
		if (mf->input1 == 255 && mf->input2 != 255) {
			std::ostringstream oss;
			oss << "input1 is unspecified (X)"
				<< " while input2 is specified (" << mf->input2 << ")";
			throw CARMA_ERROR(oss.str());
		} else if (mf->input1 != 255 && mf->input2 != 255 && mf->input1 > mf->input2) {
			std::ostringstream oss;
			oss << "input1 (" << mf->input1 << ")"
				<< " is greater than input 2 (" << mf->input2 << ")";
			throw CARMA_ERROR(oss.str());
		}

		// check for current entries only
		if (currentOnly && !(mf->start <= now && mf->end >= now)) {
			continue;
		}

		vec.push_back(mf);
	}

	return vec;
}

} // namespace carma::control
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
