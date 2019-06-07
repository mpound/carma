/*
 * Utilities to handle grade conversion to/from floats/letters
 */

#ifndef PDB_GRADE_H
#define PDB_GRADE_H

#include <string>

namespace carma {
namespace observertools {

/**
 * Convert a numeric grade (0-100) to a string grade (A+, A, A-, B+, etc.)
 *
 * @param grade the input numeric grade
 * @throw UserException if grade < 0 || grade > 100
 */
std::string convertNumericGradeToLetter(const float grade);

/**
 * Convert a string grade (A+, A, A-, B+, etc.) to a numeric grade (0-100)
 *
 * @param letter the input string grade
 */
float convertLetterGradeToNumeric(const std::string &letter);

} // namespace carma::observertools
} // namespace carma

#endif /* PDB_GRADE_H */

/* vim: set ts=4 sts=4 sw=4 noet tw=92: */
