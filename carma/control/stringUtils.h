#ifndef CARMA_CONTROL_STRING_UTILS_H
#define CARMA_CONTROL_STRING_UTILS_H

#include <string>

namespace carma {
namespace control {

/**
 * Adds escape-quote  \"  
 * to input string if it contains space or comma.
 * @param s The input string to check
 * @return \" s \" if it contains space or comma, s otherwise
 */
::std::string escapeAndQuoteStringAsNeeded( const ::std::string & s );

//@TODO should write a containsChar(string & s) in util::StringUtils
/**
 * @return true if the input string contains the dot character ('.'),
 * which is used as a field separator for obsblock IDs,  false otherwise
 * @param s The input string to check
 */
bool containsDot(const ::std::string & s);

/**
 * @return true if the input string contains the asterisk character ('*'),
 * which causes problems with quality and other shell parsing.
 * @param s The input string to check
 */
bool containsAsterisk(const ::std::string & s);

/**
 * @return true if the input string contains single or double quotes
 * which causes problems for data transfer/archive ingestion
 * @param s The input string to check
 */
bool containsQuote(const ::std::string & s );

/**
 * @return true if the input string contains astroheader reserve
 * words "write", "done", "xml"; false otherwise.
 * comparison is case-insensitive.
 * @param s The input string to check
 */
bool containsReserveWords(const ::std::string & s);

/**
 * @return the list of reserve words for obsblocks
 */
::std::string listReserveWords();

/**
 * @return true if the input string contains the a white space character
 * or is an empty string, false otherwise
 * these are illegal chars for an obsblock ID.
 * @param s The input string to check
bool isEmptyOrContainsWhiteSpace( const ::std::string & s);
 */

}  // namespace carma::control
}  // namespace carma




#endif
