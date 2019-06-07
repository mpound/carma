//
// @version $Revision: 1.4 $ $Date: 2014/11/03 18:36:06 $
//
// @usage @autogen
// @noKeys
//
// @description
// Unit test for the carma/observertools/PDB_Grade.h methods
//
// @logger TEST_FACILITY carma.test.observertools.tGradeUtils
//

#include <carma/observertools/PDB_Grade.h>

#include <carma/util/programLogging.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/Program.h>

#include <boost/test/floating_point_comparison.hpp>
#include <boost/foreach.hpp>

#include <iostream>
#include <sstream>
#include <string>

using namespace carma::observertools;
using namespace carma::util;

namespace {

void testStringRoundtrip(const std::string &input)
{
    const float numeric = convertLetterGradeToNumeric(input);
    const std::string result = convertNumericGradeToLetter(numeric);

    if (result != input) {
        std::ostringstream oss;
        oss << "roundtrip failure: input=" << input
            << " did not match output=" << result
            << " (numeric=" << numeric << ")";

        std::cerr << oss.str() << std::endl;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    std::cout << "roundtrip success: input=" << input << std::endl;
}

void testNumericConversion(const float numeric, const std::string &expectedResult)
{
    const std::string result = convertNumericGradeToLetter(numeric);

    if (result != expectedResult) {
        std::ostringstream oss;
        oss << "numeric to string failure: numeric input=" << numeric
            << " expectedResult=" << expectedResult
            << " actualResult=" << result;

        std::cerr << oss.str() << std::endl;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    std::cout << "numeric success: input=" << numeric << " result=" << result << std::endl;
}

void testStringConversion(const std::string &letter, const float expectedResult)
{
    using boost::test_tools::percent_tolerance;
    using boost::test_tools::check_is_close;

    const float result = convertLetterGradeToNumeric(letter);

    const bool withinTolerance = check_is_close(result, expectedResult, percent_tolerance(1.0));
    if (!withinTolerance) {
        std::ostringstream oss;
        oss << "string to numeric failure: string input=" << letter
            << " expectedResult=" << expectedResult
            << " actualResult=" << result;

        std::cerr << oss.str() << std::endl;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    std::cout << "string success: input=" << letter << " result=" << result << std::endl;
}

void printRangeOfNumericConversions()
{
    std::string lastGrade("X");
    const std::string X("X");
    const float step = 0.1;
    float f;

    for (f = 0; f <= 100; f += step) {
        const std::string g = convertNumericGradeToLetter(f);
        if (lastGrade != g) {
            if (lastGrade != X)
            std::cout << (f - step) << std::endl;
            std::cout << g << ": " << f << " to ";
            lastGrade = g;
        }
    }

    std::cout << (f - step) << std::endl;
}

}  // namespace < anonymous >


int Program::main()
try {
    printRangeOfNumericConversions();

    testStringRoundtrip("A+");
    testStringRoundtrip("A");
    testStringRoundtrip("C-");
    testStringRoundtrip("D");

    testStringConversion("A+", 100.0);
    testStringConversion("A", 95.0);
    testStringConversion("C-", 72.0);
    testStringConversion("D", 65.0);

    testNumericConversion(100.0, "A+");
    testNumericConversion(95.0, "A");
    testNumericConversion(72.0, "C-");
    testNumericConversion(65.0, "D");

    // TODO FIXME: add some expected failure test cases

    std::cout << "success" << std::endl;
    return EXIT_SUCCESS;

} catch (...) {
    std::ostringstream oss;
    oss << "Exception: " << getStringForCaught();

    std::cerr << oss.str() << std::endl;
    programLogErrorIfPossible(oss.str());
    return EXIT_FAILURE;
}

/* vim: set ts=4 sts=4 sw=4 et: */
