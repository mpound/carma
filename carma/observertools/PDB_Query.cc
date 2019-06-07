/*
 * CARMA Project Database Query
 */

#include <carma/observertools/PDB_Query.h>
#include <carma/observertools/QueryTags.h>
#include <carma/observertools/PDB_BSON_Convert.h>
#include <carma/observertools/PDB_Enum.h>
#include <carma/observertools/PDB_Util.h>

#include <carma/util/corbaSequenceUtils.h>
#include <carma/util/ErrorException.h>
#include <carma/util/programLogging.h>
#include <carma/util/StringUtils.h>

#include <mongo/util/time_support.h>

#include <boost/lexical_cast.hpp>
#include <boost/foreach.hpp>

#include <algorithm>
#include <sstream>
#include <string>
#include <vector>

using namespace carma::observertools;
using namespace carma::util;

/* -------------------------------------------------------------------------- */
/* ItemValue to MongoDB Query Conversion Helpers                              */
/* -------------------------------------------------------------------------- */

struct DoubleRange {
	bool fromSingleValue;	// was originally a single value
	double actualValue;		// actual single value, no threshold

	double min;				// minimum value in range
	double max;				// maximum value in range

	DoubleRange();			// constructor
};

DoubleRange::DoubleRange()
	: fromSingleValue(false)
	, actualValue(0.0)
	, min(0.0)
	, max(0.0)
{
	// intentionally left empty
}

template <typename T>
static void checkMinLessThanMax(const ItemValue &iv, const T min, const T max)
{
	if (!(min <= max)) {
		std::ostringstream oss;
		oss << "minimum is larger than maximum: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

static void checkDoubleLimits(const double value, const double minLimit, const double maxLimit)
{
	if (value < minLimit || value > maxLimit) {
		std::ostringstream oss;
		oss << "value outside accepted range: (" << minLimit << "," << maxLimit << "): "
			<< value;
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

/*
 * Quote a regular expression such that the metacharacters have no meaning.
 * This provides a way to do a text search in MongoDB, while still being
 * able to search for things like "H13CO+".
 *
 * MetaCharacters list taken from:
 * http://php.net/manual/en/function.preg-quote.php
 */
static std::string preg_quote(const std::string &s)
{
	const std::string metachars(".\\+*?[^]$(){}=!<>|:-");
	std::string result;

	BOOST_FOREACH(const char c, s) {
		if (metachars.find(c) != std::string::npos) {
			result += '\\';
		}

		result += c;
	}

	return result;
}

/*
 * Convert an ItemValue into a search for a string literal anywhere inside
 * a string field in the database. This uses a regex internally, but the
 * value passed from the user is escaped to not use any special characters.
 */
static mongo::BSONObj iv2str(const std::string &fieldName, const ItemValue &iv)
{
	const std::string value(iv.value);
	const struct mongo::BSONRegEx regex(preg_quote(value), "i");
	return BSON(fieldName << regex);
}

// Convert ItemValue into MongoDB search for Calibrator Type
static mongo::BSONObj iv2caltype(const std::string &fieldName, const ItemValue &iv)
{
	const std::string allowed("FGPBORA");
	const std::string value = StringUtils::lowASCIIAlphaNumericToUpper(std::string(iv.value));

	BOOST_FOREACH(const char c, value) {
		if (allowed.find(c) == std::string::npos) {
			std::ostringstream oss;
			oss << "invalid calibrator type (must be one of: " << allowed << "): "
				<< itemValueToString(iv);
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}
	}

	// create a regex with the entire range of values requested
	const struct mongo::BSONRegEx regex("[" + value + "]", "i");
	return BSON(fieldName << regex);
}

/*
 * TODO FIXME:
 *
 * You might consider having all of the iv2XXX() methods return a
 * mongo::BSONObj rather than strings/doubles/ints. This would make
 * all of their API signatures consistent, which would be good.
 */

// Convert ItemValue value part to integer
static int iv2int(const ItemValue &iv)
{
	try {
		return boost::lexical_cast<int>(iv.value);
	} catch (boost::bad_lexical_cast const &) {
		std::ostringstream oss;
		oss << "could not parse as integer: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

// Convert ItemValue value part to bool
static bool iv2bool(const ItemValue &iv)
{
	const std::string value(StringUtils::lowASCIIAlphaNumericToLower(std::string(iv.value)));

	if (value == "1" || value == "t" || value == "true") {
		return true;
	}

	if (value == "0" || value == "f" || value == "false") {
		return false;
	}

	{
		std::ostringstream oss;
		oss << "could not parse as bool: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

// Convert ItemValue to string representing ProjectStatus enumeration
static std::string iv2status(const ItemValue &iv)
{
	std::vector<std::string> vec;
	vec.push_back("COMPLETE");
	vec.push_back("INCOMPLETE");

	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		std::ostringstream oss;
		oss << "could not parse as status: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	return value;
}

// Convert ItemValue to string representing ProjectStatus enumeration
static std::string iv2trialstatus(const ItemValue &iv)
{
	std::vector<std::string> vec;
	vec.push_back("COMPLETE");
	vec.push_back("INCOMPLETE");
	vec.push_back("RUNNING");

	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		std::ostringstream oss;
		oss << "could not parse as trial status: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	return value;
}

// Convert ItemValue to string representing ObsCategory enumeration
static std::string iv2category(const ItemValue &iv)
{
	const std::vector<std::string> vec = getEnumNames(getObsCategoryMap());
	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		std::ostringstream oss;
		oss << "could not parse as category: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	return value;
}

// Convert ItemValue value part to double
static double iv2double(const ItemValue &iv)
{
	try {
		return boost::lexical_cast<double>(iv.value);
	} catch (boost::bad_lexical_cast const &) {
		std::ostringstream oss;
		oss << "could not parse as double: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

/*
 * A common pattern in the original code was to allow two types of values:
 * 1) single value
 * 2) two values, comma separated, representing a range
 *
 * For case #1, a small uncertainty was added, and the value was therefore turned
 * into a narrow range around the original value.
 *
 * For case #2, a no uncertainty is added, the user is assumed to know exactly
 * what they want.
 *
 * Any additional error checking can be done in other functions, this is just
 * a parser for a common case.
 */
static DoubleRange getDoubleRange(const ItemValue &iv)
{
	const std::string name(iv.name);
	const std::string value(iv.value);
	const size_t commaPos = value.find(",");
	DoubleRange range;

	// no comma, create a range with a small uncertainty
	if (commaPos == std::string::npos) {
		const double tolerance = 0.0001;
		const double d = iv2double(iv);

		range.fromSingleValue = true;
		range.actualValue = d;
		range.min = d - tolerance;
		range.max = d + tolerance;
	} else {
		const ItemValue ivMin = makeItemValue(name, value.substr(0, commaPos));
		const ItemValue ivMax = makeItemValue(name, value.substr(commaPos + 1));

		range.fromSingleValue = false;
		range.actualValue = 0.0;
		range.min = iv2double(ivMin);
		range.max = iv2double(ivMax);
	}

	return range;
}

// Convert ItemValue to one double or a pair of doubles
static mongo::BSONObj iv2doublerange(const std::string &fieldName, const ItemValue &iv)
{
	const DoubleRange range = getDoubleRange(iv);
	double min = range.min;
	double max = range.max;

	if (range.fromSingleValue) {
		// ensure values are sane
		if (min < 0.0)
			min = 0.0;

		if (max < 0.0)
			max = 0.0;
	}

	checkMinLessThanMax(iv, min, max);

	return BSON(fieldName << mongo::GTE << min << fieldName << mongo::LTE << max);
}

static mongo::BSONObj iv2doubletime(const std::string &fieldName, const ItemValue &iv)
{
	const DoubleRange range = getDoubleRange(iv);

	if (range.fromSingleValue) {
		return BSON(fieldName << mongo::GTE << range.actualValue);
	}

	const double min = range.min;
	const double max = range.max;

	checkMinLessThanMax(iv, min, max);

	return BSON(fieldName << mongo::GTE << min << fieldName << mongo::LTE << max);
}

/*
 * Create the query for a grade-style object
 *
 * For grade-style objects, the original code allowed us to search in three ways:
 * 1) everything >= N: provided by value "N"
 * 2) everything <= N: provided by value "M,N" (where M == N)
 * 3) min <= VALUE <= MAX: provided by value "M,N" (where M < N)
 *
 * It is a somewhat strange way to specify the parameters, but it is what the
 * original author chose, so we are stuck with it forever.
 */
static mongo::BSONObj iv2doublegrade(const std::string &fieldName, const ItemValue &iv)
{
	const DoubleRange range = getDoubleRange(iv);
	const double min = range.min;
	const double max = range.max;

	// A single value was given. Therefore search for condition: fieldValue >= N
	if (range.fromSingleValue) {
		const double d = range.actualValue;
		return BSON(fieldName << mongo::GTE << d);
	}

	// Two values were given, but they are equal. Therefore, search
	// for condition: fieldValue <= N
	if (min == max) {
		return BSON(fieldName << mongo::LTE << min);
	}

	checkMinLessThanMax(iv, min, max);

	// Not equal, the user provided a real range. Therefore, search
	// for condition: fieldValue >= min && fieldValue <= max
	return BSON(fieldName << mongo::GTE << min << fieldName << mongo::LTE << max);
}

/**
 * Query for an object which has both "low" and "high" properties.
 *
 * @param fieldName the name of the object field to search for (ex: requestedHaCoverage)
 * @param iv the ItemValue object passed into the query
 * @param minLimit the minimum limit for the values passed in (ex: -12.0)
 * @param maxLimit the maximum limit for the values passed in (ex: 12.0)
 */
static mongo::BSONObj iv2doublehighlow(const std::string &fieldName,
		const ItemValue &iv, const double minLimit, const double maxLimit)
{
	const DoubleRange range = getDoubleRange(iv);
	const std::string fieldNameLow = fieldName + ".low";
	const std::string fieldNameHigh = fieldName + ".high";

	if (range.fromSingleValue) {
		const double d = range.actualValue;

		// check limits
		checkDoubleLimits(d, minLimit, maxLimit);

		return BSON(fieldNameLow << mongo::LTE << d << fieldNameHigh << mongo::GTE << d);
	}

	const double min = range.min;
	const double max = range.max;

	// check limits
	checkDoubleLimits(min, minLimit, maxLimit);
	checkDoubleLimits(max, minLimit, maxLimit);

	// Three cases here: draw it out on paper to understand fully
	// 1) min limit is inside the object low/high bounds, max limit can be above high
	// 2) max limit is inside the object low/high bounds, min limit can be below low
	// 3) min limit below low, max limit above high (object totally within bounds)
	return mongo::OR(
		BSON(fieldNameLow << mongo::LTE << min << fieldNameHigh << mongo::GTE << min),
		BSON(fieldNameLow << mongo::LTE << max << fieldNameHigh << mongo::GTE << max),
		BSON(fieldNameLow << mongo::GTE << min << fieldNameHigh << mongo::LTE << max)
	);
}

// Convert ItemValue to MongoDB search for RA Coverage
static mongo::BSONObj iv2racoverage(const std::string &fieldName, const ItemValue &iv)
{
	// RA is in radians
	const double minLimit = 0.0;
	const double maxLimit = 2 * M_PI;

	return iv2doublehighlow(fieldName, iv, minLimit, maxLimit);
}

// Convert ItemValue to MongoDB search for HA Coverage
static mongo::BSONObj iv2hacoverage(const std::string &fieldName, const ItemValue &iv)
{
	// HA is in hours
	const double minLimit = -12.0;
	const double maxLimit = 12.0;

	return iv2doublehighlow(fieldName, iv, minLimit, maxLimit);
}

// Convert ItemValue to MongoDB search for Allocated Time
static mongo::BSONObj iv2allocatedtime(const std::string &fieldName, const ItemValue &iv)
{
	const DoubleRange range = getDoubleRange(iv);
	const std::string fieldNameMin = fieldName + ".min";
	const std::string fieldNameMax = fieldName + ".max";

	if (range.fromSingleValue) {
		const double d = range.actualValue;
		return BSON(fieldNameMin << mongo::LTE << d << fieldNameMax << mongo::GTE << d);
	}

	const double min = range.min;
	const double max = range.max;

	// Three cases here: draw it out on paper to understand fully
	// 1) min limit is inside the object low/high bounds, max limit can be above high
	// 2) max limit is inside the object low/high bounds, min limit can be below low
	// 3) min limit below low, max limit above high (object totally within bounds)
	return mongo::OR(
		BSON(fieldNameMin << mongo::LTE << min << fieldNameMax << mongo::GTE << min),
		BSON(fieldNameMin << mongo::LTE << max << fieldNameMax << mongo::GTE << max),
		BSON(fieldNameMin << mongo::GTE << min << fieldNameMax << mongo::LTE << max)
	);
}

/*
 * Search for a single-value field with two cases:
 * 1) single value: used as a lower bound: all documents with "field >= value"
 * 2) range value: used as bounds: all documents where "min <= field <= max"
 */
static mongo::BSONObj iv2doublelowerlimit(const std::string &fieldName, const ItemValue &iv)
{
	const DoubleRange range = getDoubleRange(iv);

	// single value means "lower bound"
	if (range.fromSingleValue) {
		const double d = range.actualValue;
		return BSON(fieldName << mongo::GTE << d);
	}

	// pair of values means within the range
	const double min = range.min;
	const double max = range.max;
	return BSON(fieldName << mongo::GTE << min << fieldName << mongo::LTE << max);
}

/*
 * Search for a single-value field with two cases:
 * 1) single value: used as a upper bound: all documents with "field <= value"
 * 2) range value: used as bounds: all documents where "min <= field <= max"
 */
static mongo::BSONObj iv2doubleupperlimit(const std::string &fieldName, const ItemValue &iv)
{
	const DoubleRange range = getDoubleRange(iv);

	// single value means "upper bound"
	if (range.fromSingleValue) {
		const double d = range.actualValue;
		return BSON(fieldName << mongo::LTE << d);
	}

	// pair of values means within the range
	const double min = range.min;
	const double max = range.max;
	return BSON(fieldName << mongo::GTE << min << fieldName << mongo::LTE << max);
}

// Convert ItemValue to string representing ObsType enumeration
static std::string iv2observationtype(const ItemValue &iv)
{
	const std::vector<std::string> vec = getEnumNames(getObsTypeMap());
	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		std::ostringstream oss;
		oss << "invalid value for enumeration: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	return value;
}

// Convert ItemValue to string representing ObsLikelihood enumeration
static std::string iv2obslikelihood(const ItemValue &iv)
{
	const std::vector<std::string> vec = getEnumNames(getObsLikelihoodMap());
	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		std::ostringstream oss;
		oss << "invalid value for enumeration: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	return value;
}

// Convert ItemValue to string representing Receiver Band
static std::string iv2receiverband(const ItemValue &iv)
{
	const std::vector<std::string> vec = getReceiverBandVec();
	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		std::ostringstream oss;
		oss << "invalid value for enumeration: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	return value;
}

// Convert ItemValue to MongoDB search for ImgVsSnr
static mongo::BSONObj iv2imgsnr(const std::string &fieldName, const ItemValue &iv)
{
	const std::vector<std::string> vec = getImgVsSnrVec();
	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		std::ostringstream oss;
		oss << "invalid value for enumeration: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	return BSON(fieldName << value);
}

// Convert ItemValue to string representing array configuration
static std::string iv2arrayconfiguration(const ItemValue &iv)
{
	const std::vector<std::string> vec = getArrayConfigurationVec();
	const std::string value(iv.value);

	if (std::find(vec.begin(), vec.end(), value) == vec.end()) {
		std::ostringstream oss;
		oss << "invalid value for enumeration: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	return value;
}

// Convert ItemValue with string representing a date in the following formats:
// 1) YYYY-mm-ddTHH:MM:SS
// 2) YYYY-mm-dd
//
// If (end == true) and format #2 was specified, then automatically set the
// hours, minutes, and seconds to the last minute of the day.
static struct mongo::Date_t parseDate(const ItemValue &iv, const bool end)
{
	const std::string value = std::string(iv.value);
	char *ret = NULL;
	struct tm date;

	// clear out the date object
	memset(&date, 0, sizeof(date));

	// full datetime format
	ret = strptime(value.c_str(), "%Y-%m-%dT%H:%M:%S", &date);
	if (ret != NULL && *ret == '\0') {
		const time_t tt = mktime(&date);
		if (tt == -1) {
			std::ostringstream oss;
			oss << "unable to convert to time_t (long format): " << itemValueToString(iv);
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}

		const int64_t ms = static_cast<int64_t>(tt) * 1000;
		const struct mongo::Date_t mongodate(ms);

		return mongodate;
	}

	// clear out the date object
	memset(&date, 0, sizeof(date));

	// date only format
	ret = strptime(value.c_str(), "%Y-%m-%d", &date);
	if (ret != NULL && *ret == '\0') {
		if (end) {
			date.tm_hour = 23;
			date.tm_min = 59;
			date.tm_sec = 59;
		}

		const time_t tt = mktime(&date);
		if (tt == -1) {
			std::ostringstream oss;
			oss << "unable to convert to time_t (short format): " << itemValueToString(iv);
			programLogErrorIfPossible(oss.str());
			throw CARMA_ERROR(oss.str());
		}

		const int64_t ms = static_cast<int64_t>(tt) * 1000;
		const struct mongo::Date_t mongodate(ms);

		return mongodate;
	}

	// error
	{
		std::ostringstream oss;
		oss << "unable to parse ISO 8601 datetime from: " << itemValueToString(iv);
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}
}

// Convert ItemValue to MongoDB search for dates
static mongo::BSONObj iv2date(const std::string &fieldName, const ItemValue &iv)
{
	const std::string fieldNameStart = fieldName + ".start";
	const std::string fieldNameEnd = fieldName + ".end";

	const std::string name(iv.name);
	const std::string value(iv.value);
	const size_t commaPos = value.find(",");

	if (commaPos == std::string::npos) {
		// no comma, parse directly
		const struct mongo::Date_t date = parseDate(iv, false);

		return BSON(fieldNameStart << mongo::LTE << date
				<< fieldNameEnd << mongo::GTE << date);
	}

	const ItemValue ivMin = makeItemValue(name, value.substr(0, commaPos));
	const ItemValue ivMax = makeItemValue(name, value.substr(commaPos + 1));

	const struct mongo::Date_t min = parseDate(ivMin, false);
	const struct mongo::Date_t max = parseDate(ivMax, true);

	// Somewhat strangely, the entire observation must be within the datetime
	// range that was specified. Having part of the observation there does not count.
	return BSON(fieldNameStart << mongo::GTE << min << fieldNameEnd << mongo::LTE << max);
}

// Convert ItemValue to MongoDB search for LST
static mongo::BSONObj iv2lst(const std::string &fieldName, const ItemValue &iv)
{
	const DoubleRange range = getDoubleRange(iv);
	const std::string fieldNameStart = fieldName + ".lstStart";
	const std::string fieldNameEnd = fieldName + ".lstEnd";

	if (range.fromSingleValue) {
		const double d = range.actualValue;
		return BSON(fieldNameStart << mongo::LTE << d << fieldNameEnd << mongo::GTE << d);
	}

	const double min = range.min;
	const double max = range.max;

	if (min <= max) {
		return mongo::OR(
			BSON(fieldNameStart << mongo::LTE << min << fieldNameEnd << mongo::GTE << min),
			BSON(fieldNameStart << mongo::LTE << max << fieldNameEnd << mongo::GTE << max),
			BSON(fieldNameStart << mongo::GTE << min << fieldNameEnd << mongo::LTE << max)
		);
	} else {
		return mongo::OR(
			BSON(fieldNameEnd << mongo::GTE << min),
			BSON(fieldNameStart << mongo::LTE << max),
			BSON(fieldNameEnd << mongo::LTE << fieldNameStart)
		);
	}
}

/*
 * Create query for an int range
 *
 * Two options available:
 * 1) straight equality: single integer in value
 * 2) range: min/max of range in value, comma separated
 */
static mongo::BSONObj iv2intrange(const std::string &fieldName, const ItemValue &iv)
{
	const std::string name(iv.name);
	const std::string value(iv.value);
	const size_t commaPos = value.find(",");

	if (commaPos == std::string::npos) {
		// no comma, parse directly
		const int i = iv2int(iv);
		return BSON(fieldName << i);
	}

	const ItemValue ivMin = makeItemValue(name, value.substr(0, commaPos));
	const ItemValue ivMax = makeItemValue(name, value.substr(commaPos + 1));

	const int min = iv2int(ivMin);
	const int max = iv2int(ivMax);

	checkMinLessThanMax(iv, min, max);

	return BSON(fieldName << mongo::GTE << min << fieldName << mongo::LTE << max);
}

// Convert an ItemValue into a Right Ascension (RA) query, with error checking
static mongo::BSONObj iv2ra(const std::string &fieldName, const ItemValue &iv)
{
	const DoubleRange range = getDoubleRange(iv);

	double min = range.min;
	double max = range.max;

	if (range.fromSingleValue) {
		// ensure values are sane
		if (min < 0.0)
			min += 2 * M_PI;

		if (max > 2 * M_PI)
			max -= 2 * M_PI;
	} else {
		checkDoubleLimits(min, 0.0, 2 * M_PI);
		checkDoubleLimits(max, 0.0, 2 * M_PI);
	}

	if (min > max) {
		double tmp = max;
		max = min;
		min = tmp;
	}

	return BSON(fieldName << mongo::GTE << min << fieldName << mongo::LTE << max);
}

// Convert an ItemValue into a Declination (DEC) query, with error checking
static mongo::BSONObj iv2dec(const std::string &fieldName, const ItemValue &iv)
{
	const DoubleRange range = getDoubleRange(iv);

	double min = range.min;
	double max = range.max;

	if (range.fromSingleValue) {
		// ensure values are sane
		if (min < -M_PI_2)
			min = -M_PI_2;

		if (max > M_PI_2)
			max = M_PI_2;
	} else {
		checkDoubleLimits(min, -M_PI_2, M_PI_2);
		checkDoubleLimits(max, -M_PI_2, M_PI_2);
	}

	checkMinLessThanMax(iv, min, max);
	return BSON(fieldName << mongo::GTE << min << fieldName << mongo::LTE << max);
}

// Convert ItemValue into MongoDB Correlator Frequency search
static mongo::BSONObj iv2corfreq(const std::string &fieldName, const ItemValue &iv)
{
	const std::string fieldNameMin = fieldName + ".minfreq";
	const std::string fieldNameMax = fieldName + ".maxfreq";
	const DoubleRange range = getDoubleRange(iv);

	if (range.fromSingleValue) {
		const double d = range.actualValue;
		return BSON(fieldNameMin << mongo::LTE << d << fieldNameMax << mongo::GTE << d);
	}

	const double min = range.min;
	const double max = range.max;

	return mongo::OR(
		BSON(fieldNameMin << mongo::LTE << min << fieldNameMax << mongo::GTE << min),
		BSON(fieldNameMin << mongo::LTE << max << fieldNameMax << mongo::GTE << max),
		BSON(fieldNameMin << mongo::GTE << min << fieldNameMax << mongo::LTE << max)
	);
}

/*
 * Filter the input vector to contain only entries within the given
 * set of keys.
 *
 * @param vec the entire input query
 * @param keys the permitted set of keys
 * @return a subset (possibly empty) of the input query
 */
static std::vector<ItemValue> filterItemValues(
		const std::vector<ItemValue> &vec,
		const std::set<std::string> &keys)
{
	std::vector<ItemValue> result;

	BOOST_FOREACH(const ItemValue &iv, vec) {
		const std::string name(iv.name);

		// skip keys that are not part of the query for projects
		if (keys.find(name) == keys.end()) {
			continue;
		}

		// add the ItemValue to the result set
		result.push_back(iv);
	}

	return result;
}

/* -------------------------------------------------------------------------- */
/* Project Object                                                             */
/* -------------------------------------------------------------------------- */

static std::set<std::string> getProjectKeys()
{
	std::set<std::string> keys;

	keys.insert(QueryTags::PROJECT);
	keys.insert(QueryTags::NOTPROJECT);
	keys.insert(QueryTags::PROJECTSTATUS);
	keys.insert(QueryTags::CALLFORPROPOSALS);
	keys.insert(QueryTags::TOTALTIME);
	keys.insert(QueryTags::TITLE);
	keys.insert(QueryTags::NUMBEROFINVESTIGATORS);
	keys.insert(QueryTags::NAME);
	keys.insert(QueryTags::PIAFFILIATION);
	keys.insert(QueryTags::AFFILIATION);
	keys.insert(QueryTags::COIAFFILIATION);
	keys.insert(QueryTags::PIISUS);
	keys.insert(QueryTags::ISUS);
	keys.insert(QueryTags::COIISUS);
	keys.insert(QueryTags::TOO);
	keys.insert(QueryTags::KEYPROJECT);
	keys.insert(QueryTags::CATEGORY);
	keys.insert(QueryTags::ABSTRACT);
	keys.insert(QueryTags::PARENTPROJECT);
	keys.insert(QueryTags::CHILDPROJECT);

	return keys;
}

/*
 * Take all user provided query parameters and turn them into a
 * MongoDB compatible BSON query object.
 */
static mongo::BSONObj createProjectQuery(const std::vector<ItemValue> &vec)
{
	const std::vector<ItemValue> myVec = filterItemValues(vec, getProjectKeys());
	std::vector<mongo::BSONObj> objects;

	BOOST_FOREACH(const ItemValue &iv, myVec) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		if (name == QueryTags::PROJECT) {
			objects.push_back(BSON("projectID" << value));
		} else if (name == QueryTags::NOTPROJECT) {
			if (value == "commissioning") {
				// The query notProject:commissioning does not denote a real project,
				// instead it denotes a class of projects. This is changed into a
				// simple boolean query on a new field that did not exist before.
				objects.push_back(BSON("commissioning" << false));
			} else {
				// This actually denotes a project not to search for
				objects.push_back(BSON("projectID" << mongo::NE << value));
			}
		} else if (name == QueryTags::PROJECTSTATUS) {
			objects.push_back(BSON("projectStatus" << iv2status(iv)));
		} else if (name == QueryTags::CALLFORPROPOSALS) {
			objects.push_back(BSON("callForProposals.term" << value));
		} else if (name == QueryTags::TOTALTIME) {
			objects.push_back(iv2doublerange("totalTime", iv));
		} else if (name == QueryTags::TITLE) {
			objects.push_back(iv2str("title", iv));
		} else if (name == QueryTags::NUMBEROFINVESTIGATORS) {
			objects.push_back(BSON("investigators.numberOfInvestigators" << iv2int(iv)));
		} else if (name == QueryTags::NAME) {
			objects.push_back(
				mongo::OR(
					iv2str("investigators.PI.name", iv),
					iv2str("investigators.CoI.name", iv)
				)
			);
		} else if (name == QueryTags::PIAFFILIATION) {
			objects.push_back(iv2str("investigators.PI.affiliation", iv));
		} else if (name == QueryTags::AFFILIATION) {
			objects.push_back(
				mongo::OR(
					iv2str("investigators.PI.affiliation", iv),
					iv2str("investigators.CoI.affiliation", iv)
				)
			);
		} else if (name == QueryTags::COIAFFILIATION) {
			objects.push_back(iv2str("investigators.CoI.affiliation", iv));
		} else if (name == QueryTags::PIISUS) {
			objects.push_back(BSON("investigators.PI.US" << iv2bool(iv)));
		} else if (name == QueryTags::ISUS) {
			objects.push_back(
				mongo::OR(
					BSON("investigators.PI.US" << iv2bool(iv)),
					BSON("investigators.CoI.US" << iv2bool(iv))
				)
			);
		} else if (name == QueryTags::COIISUS) {
			objects.push_back(BSON("investigators.CoI.US" << iv2bool(iv)));
		} else if (name == QueryTags::TOO) {
			objects.push_back(BSON("targetOfOpportunity" << iv2bool(iv)));
		} else if (name == QueryTags::KEYPROJECT) {
			objects.push_back(BSON("keyProject" << iv2bool(iv)));
		} else if (name == QueryTags::CATEGORY) {
			objects.push_back(BSON("category" << iv2category(iv)));
		} else if (name == QueryTags::ABSTRACT) {
			objects.push_back(iv2str("abstract", iv));
		} else {
			std::ostringstream oss;
			oss << "unknown ItemValue: " << itemValueToString(iv);
			throw CARMA_ERROR(oss.str());
		}
	}

	// We should logically AND all of these objects together
	{
		mongo::BSONObjBuilder builder;
		BOOST_FOREACH(const mongo::BSONObj &obj, objects) {
			builder.appendElements(obj);
		}

		return builder.obj();
	}
}

/* -------------------------------------------------------------------------- */
/* Obsblock Object                                                            */
/* -------------------------------------------------------------------------- */

static std::set<std::string> getObsblockKeys()
{
	std::set<std::string> keys;

	keys.insert(QueryTags::OBSBLOCK);
	keys.insert(QueryTags::OBSBLOCKSTATUS);
	keys.insert(QueryTags::EXCEEDTAC);
	keys.insert(QueryTags::ALLOCATEDTIME);
	keys.insert(QueryTags::PRIORITY);
	keys.insert(QueryTags::LIKELIHOOD);
	keys.insert(QueryTags::TOTALOBSERVEDTIME);
	keys.insert(QueryTags::REMAININGTIME);
	keys.insert(QueryTags::REQUESTEDHACOVERAGE);
	keys.insert(QueryTags::REQUESTEDRACOVERAGE);
	keys.insert(QueryTags::OBSERVATIONTYPE);
	keys.insert(QueryTags::RECEIVERBAND);
	keys.insert(QueryTags::RESTFREQUENCY);
	keys.insert(QueryTags::ARRAYCONFIGURATION);
	keys.insert(QueryTags::ISFLEX);

	return keys;
}

/*
 * Take all user provided query parameters and turn them into a
 * MongoDB compatible BSON query object.
 */
static mongo::BSONObj createObsblockQuery(const std::vector<ItemValue> &vec)
{
	const std::vector<ItemValue> myVec = filterItemValues(vec, getObsblockKeys());
	std::vector<mongo::BSONObj> objects;

	BOOST_FOREACH(const ItemValue &iv, myVec) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		if (name == QueryTags::OBSBLOCK) {
			objects.push_back(BSON("obsblockID" << value));
		} else if (name == QueryTags::OBSBLOCKSTATUS) {
			objects.push_back(BSON("obsblockStatus" << iv2status(iv)));
		} else if (name == QueryTags::EXCEEDTAC) {
			objects.push_back(BSON("exceedTAC" << iv2bool(iv)));
		} else if (name == QueryTags::ALLOCATEDTIME) {
			objects.push_back(iv2allocatedtime("allocatedTime", iv));
		} else if (name == QueryTags::PRIORITY) {
			objects.push_back(iv2doublerange("priority", iv));
		} else if (name == QueryTags::LIKELIHOOD) {
			objects.push_back(BSON("obsLikelihood" << iv2obslikelihood(iv)));
		} else if (name == QueryTags::TOTALOBSERVEDTIME) {
			objects.push_back(iv2doubletime("totalObservedTime", iv));
		} else if (name == QueryTags::REMAININGTIME) {
			objects.push_back(iv2doubletime("remainingTime", iv));
		} else if (name == QueryTags::REQUESTEDHACOVERAGE) {
			objects.push_back(iv2hacoverage("requestedHaCoverage", iv));
		} else if (name == QueryTags::REQUESTEDRACOVERAGE) {
			objects.push_back(iv2racoverage("requestedRaCoverage", iv));
		} else if (name == QueryTags::OBSERVATIONTYPE) {
			objects.push_back(BSON("observationType" << iv2observationtype(iv)));
		} else if (name == QueryTags::RECEIVERBAND) {
			objects.push_back(BSON("receiverBand" << iv2receiverband(iv)));
		} else if (name == QueryTags::RESTFREQUENCY) {
			objects.push_back(iv2doublerange("restFrequency", iv));
		} else if (name == QueryTags::ARRAYCONFIGURATION) {
			objects.push_back(BSON("arrayConfiguration" << iv2arrayconfiguration(iv)));
		} else if (name == QueryTags::ISFLEX) {
			objects.push_back(BSON("isFlex" << iv2bool(iv)));
		} else {
			std::ostringstream oss;
			oss << "unknown ItemValue: " << itemValueToString(iv);
			throw CARMA_ERROR(oss.str());
		}
	}

	// We should logically AND all of these objects together
	{
		mongo::BSONObjBuilder builder;
		BOOST_FOREACH(const mongo::BSONObj &obj, objects) {
			builder.appendElements(obj);
		}

		return builder.obj();
	}
}

/* -------------------------------------------------------------------------- */
/* SubObsblock Object                                                         */
/* -------------------------------------------------------------------------- */

static std::set<std::string> getSubObsblockKeys()
{
	std::set<std::string> keys;

	keys.insert(QueryTags::SUBOBSBLOCK);
	keys.insert(QueryTags::SUBOBSBLOCKOBSERVEDTIME);
	keys.insert(QueryTags::SUBOBSBLOCKSTATUS);

	return keys;
}

/*
 * Take all user provided query parameters and turn them into a
 * MongoDB compatible BSON query object.
 */
static mongo::BSONObj createSubObsblockQuery(const std::vector<ItemValue> &vec)
{
	const std::vector<ItemValue> myVec = filterItemValues(vec, getSubObsblockKeys());
	std::vector<mongo::BSONObj> objects;

	BOOST_FOREACH(const ItemValue &iv, myVec) {
		const std::string name(iv.name);
		const std::string value(iv.value);

		if (name == QueryTags::SUBOBSBLOCK) {
			objects.push_back(BSON("subObsblockID" << value));
		} else if (name == QueryTags::SUBOBSBLOCKOBSERVEDTIME) {
			objects.push_back(iv2doublerange("subObsblockObservedTime", iv));
		} else if (name == QueryTags::SUBOBSBLOCKSTATUS) {
			objects.push_back(BSON("subObsblockStatus" << iv2status(iv)));
		} else {
			std::ostringstream oss;
			oss << "unknown ItemValue: " << itemValueToString(iv);
			throw CARMA_ERROR(oss.str());
		}
	}

	// We should logically AND all of these objects together
	{
		mongo::BSONObjBuilder builder;
		BOOST_FOREACH(const mongo::BSONObj &obj, objects) {
			builder.appendElements(obj);
		}

		return builder.obj();
	}
}

/* -------------------------------------------------------------------------- */
/* Trial Object                                                               */
/* -------------------------------------------------------------------------- */

static std::set<std::string> getTrialKeys()
{
	std::set<std::string> keys;

	keys.insert(QueryTags::TRIAL);
	keys.insert(QueryTags::TRIALSTATUS);
	keys.insert(QueryTags::TRIALOBSERVATIONLENGTH);
	keys.insert(QueryTags::TRIALOBSERVATIONDATE);
	keys.insert(QueryTags::TRIALOBSERVEDLST);
	keys.insert(QueryTags::FASTSWITCH);
	keys.insert(QueryTags::AVERAGEPHASE);
	keys.insert(QueryTags::AVERAGEOPACITY);
	keys.insert(QueryTags::DQAOVERALLGRADE);
	keys.insert(QueryTags::OBSGRADE);
	keys.insert(QueryTags::NUMBEROFPOINTINGS);
	keys.insert(QueryTags::POINTINGOFFSETS);
	keys.insert(QueryTags::NUMBEROFANTENNAS);
	keys.insert(QueryTags::MOLECULE);
	keys.insert(QueryTags::TRANSITION);
	keys.insert(QueryTags::SOURCENAME);
	keys.insert(QueryTags::SRCRA);
	keys.insert(QueryTags::SRCDEC);
	keys.insert(QueryTags::CALRA);
	keys.insert(QueryTags::CALDEC);
	keys.insert(QueryTags::VELOCITY);
	keys.insert(QueryTags::VELTYPE);
	keys.insert(QueryTags::SELFCALIBRATABLE);
	keys.insert(QueryTags::SRCOBSERVATIONLENGTH);
	keys.insert(QueryTags::CALOBSERVATIONLENGTH);
	keys.insert(QueryTags::CALIBRATORNAME);
	keys.insert(QueryTags::CALIBRATORTYPE);
	keys.insert(QueryTags::NUMBEROFWINDOWS);
	keys.insert(QueryTags::BANDWIDTH);
	keys.insert(QueryTags::RESOLUTION);
	keys.insert(QueryTags::NUMBEROFCHANNELS);
	keys.insert(QueryTags::FREQ);
	keys.insert(QueryTags::IMGVSSNR);
	keys.insert(QueryTags::MAXCALTIME);
	keys.insert(QueryTags::MAXCALRMS);
	keys.insert(QueryTags::MAXTSYS);
	keys.insert(QueryTags::MINANTS);
	keys.insert(QueryTags::MAXOPACITY);
	keys.insert(QueryTags::MAXRMSPATH);
	keys.insert(QueryTags::MAXDECOR);
	keys.insert(QueryTags::REQUIREDSRCRMS);

	keys.insert(QueryTags::TOR);
	keys.insert(QueryTags::ENDTOR);

	return keys;
}

/*
 * Take all user provided query parameters and turn them into a
 * MongoDB compatible BSON query object.
 */
static mongo::BSONObj createTrialQueryHelper(
		std::vector<ItemValue>::const_iterator &it,
		const std::vector<ItemValue>::const_iterator &itEnd,
		const int orCount)
{
	// storage for all BSONObj created during this (sub)query for later AND or OR
	std::vector<mongo::BSONObj> objects;

	for (/* none */; it < itEnd; it++) {
		const ItemValue &iv = *it;
		const std::string name(iv.name);
		const std::string value(iv.value);

		// TOR is the start of "Trial-OR" which logically OR's together conditions
		// until the nearest ENDTOR is reached
		if (name == QueryTags::TOR) {
			// skip this element
			it++;

			// create the subQuery recursively
			const mongo::BSONObj subQuery = createTrialQueryHelper(it, itEnd, orCount + 1);
			objects.push_back(subQuery);
			continue;
		}

		// ENDTOR is the end of "Trial-OR"
		if (name == QueryTags::ENDTOR) {
			// check for badness in the query itself
			if (orCount <= 0) {
				std::ostringstream oss;
				oss << "Received extra ENDTOR without matching TOR";
				programLogErrorIfPossible(oss.str());
				throw CARMA_ERROR(oss.str());
			}

			break;
		}

		// Handle all normal types
		if (name == QueryTags::TRIAL) {
			objects.push_back(BSON("trialID" << iv2int(iv)));
		} else if (name == QueryTags::TRIALSTATUS) {
			objects.push_back(BSON("trialStatus" << iv2trialstatus(iv)));
		} else if (name == QueryTags::TRIALOBSERVATIONLENGTH) {
			objects.push_back(iv2doublerange("trialObservationLength", iv));
		} else if (name == QueryTags::TRIALOBSERVATIONDATE) {
			objects.push_back(iv2date("trialObservationDate", iv));
		} else if (name == QueryTags::TRIALOBSERVEDLST) {
			objects.push_back(iv2lst("trialObservedLST", iv));
		} else if (name == QueryTags::FASTSWITCH) {
			objects.push_back(BSON("fastSwitch" << iv2bool(iv)));
		} else if (name == QueryTags::AVERAGEPHASE) {
			objects.push_back(iv2doublegrade("grade.averagePhase", iv));
		} else if (name == QueryTags::AVERAGEOPACITY) {
			objects.push_back(iv2doublegrade("grade.averageOpacity", iv));
		} else if (name == QueryTags::DQAOVERALLGRADE) {
			objects.push_back(iv2doublegrade("grade.DQAOverallGrade", iv));
		} else if (name == QueryTags::OBSGRADE) {
			objects.push_back(iv2doublegrade("grade.obsGrade", iv));
		} else if (name == QueryTags::NUMBEROFPOINTINGS) {
			objects.push_back(iv2intrange("numberOfPointings", iv));
		} else if (name == QueryTags::POINTINGOFFSETS) {
			objects.push_back(BSON("pointingOffsets" << iv2double(iv)));
		} else if (name == QueryTags::NUMBEROFANTENNAS) {
			objects.push_back(iv2intrange("numberOfAntennas", iv));
		} else if (name == QueryTags::MOLECULE) {
			objects.push_back(iv2str("target.molecule", iv));
		} else if (name == QueryTags::TRANSITION) {
			objects.push_back(iv2str("target.transition", iv));
		} else if (name == QueryTags::SOURCENAME) {
			objects.push_back(iv2str("objects.source.sourceName", iv));
		} else if (name == QueryTags::SRCRA) {
			objects.push_back(iv2ra("objects.source.RA", iv));
		} else if (name == QueryTags::SRCDEC) {
			objects.push_back(iv2dec("objects.source.DEC", iv));
		} else if (name == QueryTags::CALRA) {
			objects.push_back(iv2ra("objects.calibrator.RA", iv));
		} else if (name == QueryTags::CALDEC) {
			objects.push_back(iv2dec("objects.calibrator.DEC", iv));
		} else if (name == QueryTags::VELOCITY) {
			objects.push_back(iv2doublerange("objects.source.velocity", iv));
		} else if (name == QueryTags::VELTYPE) {
			objects.push_back(iv2str("objects.source.veltype", iv));
		} else if (name == QueryTags::SELFCALIBRATABLE) {
			objects.push_back(BSON("objects.source.selfcalibratable" << iv2bool(iv)));
		} else if (name == QueryTags::SRCOBSERVATIONLENGTH) {
			objects.push_back(iv2doublerange("objects.source.observationLength", iv));
		} else if (name == QueryTags::CALOBSERVATIONLENGTH) {
			objects.push_back(iv2doublerange("objects.calibrator.observationLength", iv));
		} else if (name == QueryTags::CALIBRATORNAME) {
			objects.push_back(iv2str("objects.calibrator.calibratorName", iv));
		} else if (name == QueryTags::CALIBRATORTYPE) {
			objects.push_back(iv2caltype("objects.calibrator.type", iv));
		} else if (name == QueryTags::NUMBEROFWINDOWS) {
			objects.push_back(iv2intrange("correlator.numberOfWindows", iv));
		} else if (name == QueryTags::BANDWIDTH) {
			objects.push_back(iv2doublerange("correlator.window.bandwidth", iv));
		} else if (name == QueryTags::RESOLUTION) {
			objects.push_back(iv2doublerange("correlator.window.resolution", iv));
		} else if (name == QueryTags::NUMBEROFCHANNELS) {
			objects.push_back(iv2intrange("correlator.window.numberOfChannels", iv));
		} else if (name == QueryTags::FREQ) {
			objects.push_back(iv2corfreq("correlator.window", iv));
		} else if (name == QueryTags::IMGVSSNR) {
			objects.push_back(iv2imgsnr("constraints.imgVsSnr", iv));
		} else if (name == QueryTags::MAXCALTIME) {
			objects.push_back(iv2doublelowerlimit("constraints.gainCalibrator.maxTime", iv));
		} else if (name == QueryTags::MAXCALRMS) {
			objects.push_back(iv2doublelowerlimit("constraints.gainCalibrator.maxRms", iv));
		} else if (name == QueryTags::MAXTSYS) {
			objects.push_back(iv2doublelowerlimit("constraints.maxSystemTemperature", iv));
		} else if (name == QueryTags::MINANTS) {
			objects.push_back(iv2doubleupperlimit("constraints.minNumberOfAntennas", iv));
		} else if (name == QueryTags::MAXOPACITY) {
			objects.push_back(iv2doublelowerlimit("constraints.maxOpacity", iv));
		} else if (name == QueryTags::MAXRMSPATH) {
			objects.push_back(iv2doublelowerlimit("constraints.maxRmsPathLength", iv));
		} else if (name == QueryTags::MAXDECOR) {
			objects.push_back(iv2doublelowerlimit("constraints.maxDecorrelationRatio", iv));
		} else if (name == QueryTags::REQUIREDSRCRMS) {
			objects.push_back(iv2doublelowerlimit("constraints.requiredSourceRms", iv));
		} else {
			std::ostringstream oss;
			oss << "unknown ItemValue: " << itemValueToString(iv);
			throw CARMA_ERROR(oss.str());
		}
	}

	// Check for missing ENDTOR at the end of the query
	if (it == itEnd && orCount > 0) {
		std::ostringstream oss;
		oss << "Missing ENDTOR (too many TOR, too few ENDTOR)";
		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	// We should logically OR all of these objects together
	if (orCount > 0) {
		mongo::BSONArrayBuilder builder;
		BOOST_FOREACH(const mongo::BSONObj &obj, objects) {
			builder.append(obj);
		}

		return BSON("$or" << builder.arr());
	}

	// We should logically AND all of these objects together
	{
		mongo::BSONObjBuilder builder;
		BOOST_FOREACH(const mongo::BSONObj &obj, objects) {
			builder.appendElements(obj);
		}

		return builder.obj();
	}
}

static mongo::BSONObj createTrialQuery(const std::vector<ItemValue> &vec)
{
	const std::vector<ItemValue> myVec = filterItemValues(vec, getTrialKeys());

	std::vector<ItemValue>::const_iterator it = myVec.begin();
	const std::vector<ItemValue>::const_iterator itEnd = myVec.end();
	const int orCount = 0;

	// call the helper function, which does the real work
	const mongo::BSONObj obj = createTrialQueryHelper(it, itEnd, orCount);
	return obj;
}

/* -------------------------------------------------------------------------- */
/* MongoDB Split-Database Query / Join Engine                                 */
/* -------------------------------------------------------------------------- */

/*
 * Metadata for the query of each object type.
 *
 * This is a significant part of the query joiner, as described below.
 */

struct ObjectQueryMetadata {
	std::vector<ItemValue> queryParams;				// Query Parameters
	std::vector<mongo::BSONObj> queryResults;		// Query Results
	bool queryFinished;								// Was the query run?

	ObjectQueryMetadata();							// constructor
};

ObjectQueryMetadata::ObjectQueryMetadata()
	: queryParams()
	, queryResults()
	, queryFinished(false)
{
	// intentionally left empty
}

struct QueryMetadata {
	struct ObjectQueryMetadata project;
	struct ObjectQueryMetadata obsblock;
	struct ObjectQueryMetadata subobsblock;
	struct ObjectQueryMetadata trial;

	// database access
	const PDB_DB_Params &db;

	// Constructor
	QueryMetadata(const PDB_DB_Params &db);

private:
	// No copying
	QueryMetadata(const QueryMetadata &rhs);
	QueryMetadata& operator=(const QueryMetadata &rhs);
};

QueryMetadata::QueryMetadata(const PDB_DB_Params &db)
	: db(db)
{
	// intentionally left empty
}

/*
 * Generate a query limit expression.
 *
 * This uses the $in functionality of MongoDB to limit the amount of
 * results which are returned by a query. On import, each object has
 * all of the parent object ids stored into various keys.
 *
 * We use the set of values to generate a MongoDB query expression like:
 * { key: { $in: [val1, val2, val3, ...] } }
 */
static mongo::BSONObj generateQueryLimiter(const std::string &key,
		const std::set<std::string> &values)
{
	mongo::BSONArrayBuilder builder;
	builder.append(values);

	const mongo::BSONArray arr = builder.arr();

	return BSON(key << BSON("$in" << arr));
}

/*
 * Identifier Limits
 *
 * These are the various identifier fields which are present in each object.
 * Upon handling a user provided query parameter, the identifiers from the
 * result set are saved into this object. This object can then be used to
 * limit the output of future queries.
 */
struct IdentifierLimits {
	std::set<std::string> project;		// completeTrialID
	std::set<std::string> obsblock;		// completeObsblockID
	std::set<std::string> subobsblock;	// completeSubObsblockID
	std::set<std::string> trial;		// completeTrialID
};

/*
 * Run a query on the "projects" collection, respecting limits.
 */
static IdentifierLimits runProjectQuery(QueryMetadata &metadata, const IdentifierLimits &limits)
{
	mongo::BSONObjBuilder builder;

	// completeProjectID limit
	if (!limits.project.empty()) {
		builder.appendElements(generateQueryLimiter("completeProjectID", limits.project));
	}

	// Done with limits, add the actual query parameters
	builder.appendElements(createProjectQuery(metadata.project.queryParams));

	// Now we have the actual query string itself
	const mongo::BSONObj query = builder.obj();
	//programLogInfoIfPossible("project query: " + query.toString());

	// Storage for the revised (stricter) limits
	IdentifierLimits newLimits;

	// Run the query, saving the objects and limits as we go
	const std::string collection = metadata.db.PROJECTS;
	const std::auto_ptr<mongo::DBClientCursor> cursor = metadata.db.conn->query(collection, query);
	while (cursor->more()) {
		const mongo::BSONObj obj = cursor->next();
		newLimits.project.insert(obj.getStringField("completeProjectID"));

		metadata.project.queryResults.push_back(obj.copy());
	}

	metadata.project.queryFinished = true;
	return newLimits;
}

/*
 * Run a query on the "obsblocks" collection, respecting limits
 */
static IdentifierLimits runObsblockQuery(QueryMetadata &metadata, const IdentifierLimits &limits)
{
	mongo::BSONObjBuilder builder;

	// completeProjectID limit
	if (!limits.project.empty()) {
		builder.appendElements(generateQueryLimiter("completeProjectID", limits.project));
	}

	// completeObsblockID limit
	if (!limits.obsblock.empty()) {
		builder.appendElements(generateQueryLimiter("completeObsblockID", limits.obsblock));
	}

	// Done with limits, add the actual query parameters
	builder.appendElements(createObsblockQuery(metadata.obsblock.queryParams));

	// Now we have the actual query string itself
	const mongo::BSONObj query = builder.obj();
	//programLogInfoIfPossible("obsblock query: " + query.toString());

	// Storage for the revised (stricter) limits
	IdentifierLimits newLimits;

	// Run the query, saving the objects and limits as we go
	const std::string collection = metadata.db.OBSBLOCKS;
	const std::auto_ptr<mongo::DBClientCursor> cursor = metadata.db.conn->query(collection, query);
	while (cursor->more()) {
		const mongo::BSONObj obj = cursor->next();
		newLimits.project.insert(obj.getStringField("completeProjectID"));
		newLimits.obsblock.insert(obj.getStringField("completeObsblockID"));

		metadata.obsblock.queryResults.push_back(obj.copy());
	}

	metadata.obsblock.queryFinished = true;
	return newLimits;
}

/*
 * Run a query on the "subobsblock" collection, respecting limits
 */
static IdentifierLimits runSubObsblockQuery(QueryMetadata &metadata, const IdentifierLimits &limits)
{
	mongo::BSONObjBuilder builder;

	// completeProjectID limit
	if (!limits.project.empty()) {
		builder.appendElements(generateQueryLimiter("completeProjectID", limits.project));
	}

	// completeObsblockID limit
	if (!limits.obsblock.empty()) {
		builder.appendElements(generateQueryLimiter("completeObsblockID", limits.obsblock));
	}

	// completeSubObsblockID limit
	if (!limits.subobsblock.empty()) {
		builder.appendElements(generateQueryLimiter("completeSubObsblockID", limits.subobsblock));
	}

	// Done with limits, add the actual query parameters
	builder.appendElements(createSubObsblockQuery(metadata.subobsblock.queryParams));

	// Now we have the actual query string itself
	const mongo::BSONObj query = builder.obj();
	//programLogInfoIfPossible("subobsblock query: " + query.toString());

	// Storage for the revised (stricter) limits
	IdentifierLimits newLimits;

	// Run the query, saving the objects and limits as we go
	const std::string collection = metadata.db.SUBOBSBLOCKS;
	const std::auto_ptr<mongo::DBClientCursor> cursor = metadata.db.conn->query(collection, query);
	while (cursor->more()) {
		const mongo::BSONObj obj = cursor->next();
		newLimits.project.insert(obj.getStringField("completeProjectID"));
		newLimits.obsblock.insert(obj.getStringField("completeObsblockID"));
		newLimits.subobsblock.insert(obj.getStringField("completeSubObsblockID"));

		metadata.subobsblock.queryResults.push_back(obj.copy());
	}

	metadata.subobsblock.queryFinished = true;
	return newLimits;
}

/*
 * Run a query on the "trials" collection, respecting limits
 */
static IdentifierLimits runTrialQuery(QueryMetadata &metadata, const IdentifierLimits &limits)
{
	mongo::BSONObjBuilder builder;

	// completeProjectID limit
	if (!limits.project.empty()) {
		builder.appendElements(generateQueryLimiter("completeProjectID", limits.project));
	}

	// completeObsblockID limit
	if (!limits.obsblock.empty()) {
		builder.appendElements(generateQueryLimiter("completeObsblockID", limits.obsblock));
	}

	// completeSubObsblockID limit
	if (!limits.subobsblock.empty()) {
		builder.appendElements(generateQueryLimiter("completeSubObsblockID", limits.subobsblock));
	}

	// completeTrialID limit
	if (!limits.trial.empty()) {
		builder.appendElements(generateQueryLimiter("completeTrialID", limits.trial));
	}

	// Done with limits, add the actual query parameters
	builder.appendElements(createTrialQuery(metadata.trial.queryParams));

	// Now we have the actual query string itself
	const mongo::BSONObj query = builder.obj();
	//programLogInfoIfPossible("trial query: " + query.toString());

	// Storage for the revised (stricter) limits
	IdentifierLimits newLimits;

	// Run the query, saving the objects and limits as we go
	const std::string collection = metadata.db.TRIALS;
	const std::auto_ptr<mongo::DBClientCursor> cursor = metadata.db.conn->query(collection, query);
	while (cursor->more()) {
		const mongo::BSONObj obj = cursor->next();
		newLimits.project.insert(obj.getStringField("completeProjectID"));
		newLimits.obsblock.insert(obj.getStringField("completeObsblockID"));
		newLimits.subobsblock.insert(obj.getStringField("completeSubObsblockID"));
		newLimits.trial.insert(obj.getStringField("completeTrialID"));

		metadata.trial.queryResults.push_back(obj.copy());
	}

	metadata.trial.queryFinished = true;
	return newLimits;
}

/*
 * Once we have the results from our four queries, we may still have some
 * unwanted results. This occurs due to the fact that we can not impose all
 * query conditions at once across the four disjoint collections.
 *
 * Thus, the earlier queries may have returned some objects that we do not
 * need in the result set. So we should go through and throw them away.
 *
 * The queryResults vector in the OneQueryMetadata object will be overwritten
 * with the reduced version where the unwanted results have been removed.
 */
static void removeUnwantedResults(ObjectQueryMetadata &metadata, const IdentifierLimits &limits)
{
	const std::string completeProjectID = "completeProjectID";
	const std::string completeObsblockID = "completeObsblockID";
	const std::string completeSubObsblockID = "completeSubObsblockID";
	const std::string completeTrialID = "completeTrialID";

	std::vector<mongo::BSONObj> results;
	BOOST_FOREACH(const mongo::BSONObj &obj, metadata.queryResults) {

		// skip objects where completeProjectID not in limit set
		if (!limits.project.empty() && obj.hasField(completeProjectID)) {
			const std::string pid = obj.getStringField(completeProjectID);
			if (limits.project.find(pid) == limits.project.end())
				continue;
		}

		// skip objects where completeObsblockID not in limit set
		if (!limits.obsblock.empty() && obj.hasField(completeObsblockID)) {
			const std::string oid = obj.getStringField(completeObsblockID);
			if (limits.obsblock.find(oid) == limits.obsblock.end())
				continue;
		}

		// skip objects where completeSubObsblockID not in limit set
		if (!limits.subobsblock.empty() && obj.hasField(completeSubObsblockID)) {
			const std::string sid = obj.getStringField(completeSubObsblockID);
			if (limits.subobsblock.find(sid) == limits.subobsblock.end())
				continue;
		}

		// skip objects where completeTrialID not in limit set
		if (!limits.trial.empty() && obj.hasField(completeTrialID)) {
			const std::string tid = obj.getStringField(completeTrialID);
			if (limits.trial.find(tid) == limits.trial.end())
				continue;
		}

		// add to result set
		results.push_back(obj);
	}

	// assign the new vector without the unwanted results
	metadata.queryResults = results;
}

/*
 * A somewhat complicated algorithm in order to get to the complete result set
 * with as few queries as possible. This turns out to be exactly 4 queries.
 *
 * We start at the highest level object class that has a user provided query
 * available. The resulting identifiers from the result set are used to limit
 * the results of later queries.
 *
 * Once we have reached the lowest object class with a user provided query, then
 * we have all of the information we need. This is stored as the identifier
 * limits.
 *
 * At this point, we go backwards and run the query all object classes where
 * the user did not provide an explicit query.
 *
 * At this point, we have all of the objects in the result set. However, we may
 * have some extra objects that do not belong in the result set. This is due to
 * the use of separate collections for each object type.
 *
 * Therefore, we use our known good limits to throw away any of these objects
 * that we don't need. This leaves us with the true result set.
 */
static void runCompleteQuery(QueryMetadata &metadata)
{
	IdentifierLimits limits;

	// The first thing to do is race downwards. We find the highest level object
	// that we can where the user specified some query parameters and run the
	// query using their parameters. They may not have specified any IDs, which
	// makes our life difficult.
	//
	// Once we have run that query, we then have some IDs. We can use the IDs
	// to limit the scope of the queries on the way down. The limits will get
	// stricter and stricter as we go.

	// if the user specified a project-object query, execute it
	if (!metadata.project.queryParams.empty()) {
		limits = runProjectQuery(metadata, limits);

		if (metadata.project.queryResults.empty()) {
			programLogInfoIfPossible("no project results: null result");
			return;
		}
	}

	// if the user specified a obsblock-object query, execute it
	if (!metadata.obsblock.queryParams.empty()) {
		limits = runObsblockQuery(metadata, limits);

		if (metadata.obsblock.queryResults.empty()) {
			programLogInfoIfPossible("no obsblock results: null result");
			return;
		}
	}

	// if the user specified a subobsblock-object query, execute it
	if (!metadata.subobsblock.queryParams.empty()) {
		limits = runSubObsblockQuery(metadata, limits);

		if (metadata.subobsblock.queryResults.empty()) {
			programLogInfoIfPossible("no subobsblock results: null result");
			return;
		}
	}

	// if the user specified a trial-object query, execute it
	if (!metadata.trial.queryParams.empty()) {
		limits = runTrialQuery(metadata, limits);

		if (metadata.trial.queryResults.empty()) {
			programLogInfoIfPossible("no trial results: null result");
			return;
		}
	}

	// At this point, the most strict limits have been found. We need to work our
	// way backwards, running any queries that we didn't do originally. This way we
	// have the strictest limits and have the least amount of stuff to do manually.
	//
	// We do not need to update the limits at each step, they are already as strict
	// as they are going to get!

	// trial query was never run, we need to run it now
	if (!metadata.trial.queryFinished) {
		runTrialQuery(metadata, limits);
	}

	// subobsblock query was never run, we need to run it now
	if (!metadata.subobsblock.queryFinished) {
		runSubObsblockQuery(metadata, limits);
	}

	// obsblock query was never run, we need to run it now
	if (!metadata.obsblock.queryFinished) {
		runObsblockQuery(metadata, limits);
	}

	// project query was never run, we need to run it now
	if (!metadata.project.queryFinished) {
		runProjectQuery(metadata, limits);
	}

	// At this point, we have all of the results from our four queries. However,
	// due to the disjoint nature of the four collections, we may have some
	// results that do not belong in the final output. They need to be removed.
	removeUnwantedResults(metadata.project, limits);
	removeUnwantedResults(metadata.obsblock, limits);
	removeUnwantedResults(metadata.subobsblock, limits);
	removeUnwantedResults(metadata.trial, limits);
}

/* -------------------------------------------------------------------------- */
/* Validation Methods                                                         */
/* -------------------------------------------------------------------------- */

// Append the contents of one set to another set
static void appendSetToSet(const std::set<std::string> &in, std::set<std::string> &out)
{
	out.insert(in.begin(), in.end());
}

/*
 * Check that a query ItemValueSequence only contains known parameters
 */
static void checkQueryParameters(const std::vector<ItemValue> &vec)
{
	std::set<std::string> keys;

	// append all keys from all object types
	appendSetToSet(getProjectKeys(), keys);
	appendSetToSet(getObsblockKeys(), keys);
	appendSetToSet(getSubObsblockKeys(), keys);
	appendSetToSet(getTrialKeys(), keys);

	BOOST_FOREACH(const ItemValue &iv, vec) {
		const std::string name(iv.name);

		if (std::find(keys.begin(), keys.end(), name) == keys.end()) {
			std::ostringstream oss;
			oss << "ItemValue has invalid name: " << name;
			throw CARMA_ERROR(oss.str());
		}
	}
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

PDB_Query::PDB_Query(const PDB_DB_Params &db, const ItemValueSequence &theQuery)
	: db_(db)
	, queryParams_(convertSequenceToVector<ItemValue>(theQuery))
{
	// check that each ItemValue in the sequence is recognized
	checkQueryParameters(queryParams_);
}

PDB_Query::PDB_Query(const PDB_DB_Params &db, const std::vector<ItemValue> &theQuery)
	: db_(db)
	, queryParams_(theQuery)
{
	// check that each ItemValue in the sequence is recognized
	checkQueryParameters(queryParams_);
}

std::vector<Project> PDB_Query::run() const
{
	const std::vector<ItemValue> &vec = queryParams_;
	QueryMetadata metadata(db_);

	// filter out the parameters specific to each type of query
	metadata.project.queryParams = filterItemValues(vec, getProjectKeys());
	metadata.obsblock.queryParams = filterItemValues(vec, getObsblockKeys());
	metadata.subobsblock.queryParams = filterItemValues(vec, getSubObsblockKeys());
	metadata.trial.queryParams = filterItemValues(vec, getTrialKeys());

	// run the query
	runCompleteQuery(metadata);

	// merge the results together
	CORBA_Object_Merge merge;
	merge.projects = convertBSONProjects(metadata.project.queryResults);
	merge.obsblocks = convertBSONObsblocks(metadata.obsblock.queryResults);
	merge.subobsblocks = convertBSONSubObsblocks(metadata.subobsblock.queryResults);
	merge.trials = convertBSONTrials(metadata.trial.queryResults);

	return mergeCORBAObjects(merge);
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
