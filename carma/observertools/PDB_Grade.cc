#include <carma/observertools/PDB_Grade.h>

#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/programLogging.h>
#include <carma/util/StringUtils.h>

#include <boost/foreach.hpp>

#include <sstream>
#include <map>

using namespace carma::util;

/* -------------------------------------------------------------------------- */
/* Types                                                                      */
/* -------------------------------------------------------------------------- */

typedef std::map<std::string, float> GradeMapType;
typedef std::map<float, std::string> ReverseGradeMapType;

/* -------------------------------------------------------------------------- */
/* Private Methods                                                            */
/* -------------------------------------------------------------------------- */

static GradeMapType getGradeMap()
{
	GradeMapType m;

	m.insert(std::make_pair("A+", 100.0));
	m.insert(std::make_pair("A",  95.0));
	m.insert(std::make_pair("A-", 92.0));
	m.insert(std::make_pair("B+", 88.0));
	m.insert(std::make_pair("B",  85.0));
	m.insert(std::make_pair("B-", 82.0));
	m.insert(std::make_pair("C+", 78.0));
	m.insert(std::make_pair("C",  75.0));
	m.insert(std::make_pair("C-", 72.0));
	m.insert(std::make_pair("D+", 68.0));
	m.insert(std::make_pair("D",  65.0));
	m.insert(std::make_pair("D-", 62.0));
	// these are both 30.0 on purpose
	m.insert(std::make_pair("E",  30.0));
	m.insert(std::make_pair("F",  30.0));

	return m;
}

static ReverseGradeMapType getReverseGradeMap()
{
	const GradeMapType gm = getGradeMap();
	ReverseGradeMapType m;

	// Note for entry for E_STR will get overwritten
	// with F_STR since they share the same key.
	// This is what we want actually.
	BOOST_FOREACH(const GradeMapType::value_type &element, gm) {
		const std::string s = element.first;
		const float f = element.second;
		m.insert(std::make_pair(f, s));
	}

	return m;
}

/* -------------------------------------------------------------------------- */
/* Public Methods                                                             */
/* -------------------------------------------------------------------------- */

namespace carma {
namespace observertools {

std::string convertNumericGradeToLetter(const float grade)
{
	if (grade < 0 || grade > 100) {
		std::ostringstream oss;
		oss << "Bad input grade [" << grade << "]."
			<< " It must be numeric between [0-100].";

		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	// get pointer to first key that is equal or greater than the grade
	const ReverseGradeMapType reverseMap = getReverseGradeMap();
	ReverseGradeMapType::const_iterator it = reverseMap.lower_bound(grade);
	if (it == reverseMap.end()) {
		it--;
	}

	return it->second;
}

float convertLetterGradeToNumeric(const std::string &letter)
{
	const std::string ug = StringUtils::lowASCIIAlphaNumericToUpper(letter);
	const GradeMapType gradeMap = getGradeMap();

	GradeMapType::const_iterator it = gradeMap.find(ug);
	if (it == gradeMap.end()) {
		std::ostringstream oss;
		oss << "Bad grade given [" << letter << "],"
			<< " it must be a letter grade [";

		BOOST_FOREACH(const GradeMapType::value_type &element, gradeMap) {
			oss << element.first << ", ";
		}

		oss << "]";

		programLogErrorIfPossible(oss.str());
		throw CARMA_ERROR(oss.str());
	}

	return it->second;
}

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 noet tw=112: */
