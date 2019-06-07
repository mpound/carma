/**
 *
 * @file
 * Common string functions
 *
 * @author Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#ifndef CARMA_UTIL_STRINGUTILS_H
#define CARMA_UTIL_STRINGUTILS_H

#include <string>
#include <set>
#include <vector>
#include <sstream>

namespace carma {
    namespace util {

    /**
     * digest type
     * <ui>
     * <li>MD5 md5</li>
     * <li>SHA1 sha1</li>
     * </ul>
     */
    typedef enum {
        MD5,
        SHA1
    } DigestType;

    /**
     * @relatesalso carma::util::StringUtils
     * convert a vector of elements to a string by appending the
     * string representation of each element separated by the specified
     * delimiter
     * @example a vector<int> of elements 4 5 2 -> "4, 5, 2"
     * @example a vector<string> of elements "aa" "kk" "b" -> "aa, kk, b"
     * @param v the vector of elements
     * @param delimiter the string to use to seperate the elements
     * @return the string representation
     */
     template <typename T> ::std::string vectorToString
         (const ::std::vector<T>& v, const ::std::string& delimiter=", ") {
         ::std::ostringstream os;
         int size = v.size();
         for(int i=0; i<size; i++) {
             os << v[i];
             if(i < size-1) {
                 os << delimiter;
             }
         }
         return os.str();
     }


    /**
     * @relatesalso carma::util::StringUtils
     * convert a set of elements to a string by appending the
     * string representation of each element separated by the specified
     * delimiter
     * @example a set<int> of elements 4 5 2 -> "4, 5, 2"
     * @example a set<string> of elements "aa" "kk" "b" -> "aa, kk, b"
     * @param s the set of elements
     * @param delimiter the string to use to seperate the elements
     * @return the string representation
     */
     template <typename T> ::std::string setToString
         (const ::std::set<T>& s, const ::std::string& delimiter=", ") {
         ::std::ostringstream os;
         int size = s.size();
	 typename ::std::set<T>::iterator iter;
         int count = 0;
         for(iter=s.begin(); iter != s.end(); iter++) {
             os << *iter;
             if(count < size-1) {
                 os << delimiter;
             }
             count++;
         }
         return os.str();
     }


/**
 * Common string functions
 * Additional string-related functions can be added here.
 * This class contains no state.
 *
 */
class StringUtils {
public:

    /**
     * @typedef sortType
     * Sort type for sort( std::string, sortType ) method
     */
    typedef enum sortTypeEnum {
     /** do an ascending sort */
        ASCENDING_SORT,
     /** do an descending sort */
        DESCENDING_SORT
    } sortType;


    /**
    ** Default constructor
    */
    StringUtils();

    /**
    ** Destructor
    */
    virtual ~StringUtils();

    /**
     * convert a string containing low ASCII alphanumeric and punctionation
     * characters (unicode 0x20 to 0x7E) to upper case
     * @param str The string to be converted
     * @return the converted string
     * @throws out_of_range if any character in the input string is not in the
     *         range 0x20 to 0x7E
     */
    static ::std::string lowASCIIAlphaNumericToUpper(const ::std::string& str);

    /**
     * In place conversion of a vector of string to lower case
     * @param v The vector<string> to be converted
     * @see lowASCIIAlphaNumericToLower(const ::std::string& str);
     */
    static void toLower(::std::vector< ::std::string > & v);

    /**
     * In place conversion of  a vector of string to upper case
     * @param v The vector<string> to be converted
     * @see lowASCIIAlphaNumericToUpper(const ::std::string& str);
     */
    static void toUpper(::std::vector< ::std::string > & v);

    /**
     * convert a string containing low ASCII alphanumeric and punctionation
     * characters (unicode 0x20 to 0x7E) to lower case
     * @param str The string to be converted
     * @return the converted string
     * @throws out_of_range if any character in the input string is not in the
     *         range 0x20 to 0x7E
     */
    static ::std::string lowASCIIAlphaNumericToLower(const ::std::string& str);

    /**
     * erase specified characters from a string
     * @param str the string from which to remove characters
     * @param chars characters to remove
     * @return string with the specified characters erased
     */
    static ::std::string erase(const ::std::string& str,
                             const ::std::vector<char>& chars);

    /**
     * erase specified characters from the beginning and end of a string
     * @param str the string from which to remove characters
     * @param chars characters to remove from the beginning and end
     */
    static void trimInplace( ::std::string &       str,
                             const ::std::string & chars );

    /**
     * erase specified characters from the beginning and end of a string
     * @param str the string from which to remove characters
     * @param chars String with characters to remove from the beginning and end
     * @return string with the specified characters trimmed
     */
    static ::std::string trim( const ::std::string & str,
                               const ::std::string & chars );


    /**
     * Erase whitespace (' ',\n,\r,\t) from beginning and end of string.
     * This method calls trim with the above set of chars.
     * @param str the string from which to remove characters
     */
    static void trimWhiteSpaceInplace( ::std::string & str );

    /**
     * Erase whitespace (' ',\n,\r,\t) from beginning and end of string.
     * This method calls
     * StringUtils::trim(const ::std::string&,const ::std::vector<char>&)
     * with an appropriate
     * set of chars.
     * @param str the string from which to remove characters
     * @return string with the white space trimmed
     */
    static ::std::string trimWhiteSpace( const ::std::string & str );


    /**
     * collapse consecutive repeats of a single character in specified
     * character list to a single character
     * <pre>
     * "abc         def" -> "abc def"
     * </pre>
     * @param str the string from which to collapse characters
     * @param chars characters to collapse
     * @return string with the specified characters collapsed
     */
    static ::std::string collapse(const ::std::string& str,
                            const ::std::vector<char>& chars);

    /**
     * collapse consecutive repeats of a single character in specified
     * character list to a single character
     * <pre>
     * "abc         def" -> "abc def"
     * </pre>
     * @param str the string from which to collapse characters
     * @param c repeated character to collapse
     * @return string with the specified characters collapsed
     */
    static ::std::string collapse(const ::std::string& str, char c);

    /**
     * replace a substring within a string with another substring
     * @param str the string
     * @param origSubstr the original substring to replace
     * @param replSubstr the replacement substring
     * @return the resulting string
     */
    static ::std::string replace(const ::std::string& str,
                               const ::std::string& origStr,
                               const ::std::string& replStr);

    /**
     * split a string into substrings based on a delimiter character set
     * @param tokens vector that will be filled in with the split substrings
     * @param str the input string to tokenize
     * @param delimiterSet the delimiter character set to use to tokenize
     *                     the string
     * @param skipNull skip null tokens if true
     */
    static void
    tokenizeInplace( ::std::vector< ::std::string > & tokens,
                     const ::std::string &            str,
                     const ::std::string &            delimiterSet = " ",
                     bool                             skipNull = true );

    /**
     * split a string into substrings based on a delimiter character set
     * @param str the input string to tokenize
     * @param delimiterSet the delimiter character set to use to tokenize
     *                     the string
     * @param skipNull skip null tokens if true
     * @return vector of split substrings
     */
    static ::std::vector< ::std::string >
    tokenize( const ::std::string & str,
              const ::std::string & delimiterSet = " ",
              bool                  skipNull = true );

    /**
     * split a string into substrings based on a delimiter character set
     * stopping if a maximum number of substrings is found
     * @param tokens vector that will be filled in with the split substrings
     * @param str the input string to tokenize
     * @param maxTokens maximum number of tokens to stop after
     * @param delimiterSet the delimiter character set to use to tokenize
     *                     the string
     * @param skipNull skip null tokens if true
     */
    static void
    tokenizeNInplace( ::std::vector< ::std::string > & tokens,
                      const ::std::string &            str,
                      const size_t                     maxTokens,
                      const ::std::string &            delimiterSet = " ",
                      bool                             skipNull = true );

    /**
     * split a string into substrings based on a delimiter character set
     * stopping if a maximum number of substrings is found
     * @param str the input string to tokenize
     * @param maxTokens maximum number of tokens to stop after
     * @param delimiterSet the delimiter character set to use to tokenize
     *                     the string
     * @param skipNull skip null tokens if true
     * @return vector of split substrings
     */
    static ::std::vector< ::std::string >
    tokenizeN( const ::std::string & str,
               const size_t          maxTokens,
               const ::std::string & delimiterSet = " ",
               bool                  skipNull = true );

    static bool miniGlob(const char *pattern, const char *checkString);

    /**
     * convert a string representation of an integer to an int
     * @code
     * stringToInt("5893");
     * @endcode
     * returns 5893
     * @code
     * stringToInt("5893z");
     * @endcode
     * throws a carma::util::IllegalArgumentException
     * @param str the string representation of the int
     * @return the integer represented by the string
     * @throws carma::util::IllegalArgumentException if @p str isn't really
     *         the string representation of an int
     */
    static int stringToInt(const ::std::string& str);

    /**
     * convert a number into sexadecimal HMS/DMS notation, with or without a sign
     * [+/-]HH:MM:SS.ss  or [+/-]DDD:MM:SS.sss  (precision# digits)
     */

    static ::std::string  hms(double angle, int precision=2);
    static ::std::string  dms(double angle, int precision=3);
    static ::std::string shms(double angle, int precision=2);
    static ::std::string sdms(double angle, int precision=3);
    static ::std::string sexa(double angle, bool sign, bool hms, int precision);

    /**
     * compute the specified message digest for the specified string
     * @param str the string for which to compute the digest
     * @param digestType the type of digest to compute
     * @return the message digest
     */
    static ::std::string
    computeMessageDigest( const ::std::string & str,
                          DigestType            digestType );



    /**
     * Compares two strings, ignoring case considerations. Two strings are
     * considered equal ignoring case if they are of the same length, and
     * corresponding characters in the two strings are equal ignoring case.
     * @param aString First string to compare
     * @param anotherString Second string to compare
     * @return true if the strings are equal ignoring case, false otherwise.
     */
    static bool equalsIgnoreCase(const std::string & aString,
                                 const std::string & anotherString);

    /**
     * Checks that the input string is made up of only the
     * characters from a given list. Match is case-insensitive.
     * @param str the string to check
     * @param allowedChars string containing characters allowed in str
     * @return true if the string is made up of only characters in the
     * chars vector, false otherwise.
     * @return true if str is empty, because empty is a proper subset of
     * anything.
     * @return false if allowedChars is empty and str is anything except empty
     *
     */
    static bool containsOnly( const ::std::string & str,
                              const ::std::string & allowedChars );
    /**
     * @return the input string sorted lexigraphically.
     * @param str The string to sort
     * @param direction Enum value indicating whether to do an
     * ascending or descending sort.
     * Default is ascending sort.
     */
    static ::std::string sort( const ::std::string & str,
                               const sortType direction = ASCENDING_SORT);

    /**
     * @return the input string after eliminating adjacent duplicate characters.
     * Check is case-sensitive, e.g. 'U' is not a duplicate of 'u'.
     * Example: input = abcdcefffghgi, output = abcdcefghgi
     */
    static ::std::string uniq( const ::std::string & str);

    /**
     * @return the input string after eliminating <i>any</i> duplicate characters.
     * Check is case-sensitive, e.g. 'U' is not a duplicate of 'u'.
     * Example: input = abcdcefffghgi, output = abcdefghi
     */
    static ::std::string reallyUniq( const ::std::string & str);

    /**
     * @return true if the input string contains the a white space character
     * or is an empty string, false otherwise
     * these are illegal chars for an obsblock ID.
     * @param s The input string to check
     */
    static bool isEmptyOrContainsWhiteSpace( const ::std::string & s);
};



}}
#endif  // CARMA_UTIL_STRINGUTILS_H
