#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/String.h"

#include <iostream>
#include <sstream>
#include <errno.h>

using namespace std;
using namespace sza::util;

const std::string String::emptyString_("");
const std::string String::whiteSpace_(" \t");

/**.......................................................................
 * Constructor.
 */
String::String() 
{
  initialize();
}

String::String(unsigned int iVal, bool convertNonAscii) 
{
  initialize();
  std::ostringstream os;
  os << iVal;
  str_   = os.str();
  ucvec_ = stringToBytes(str_, convertNonAscii);
}

String::String(const std::string& str, bool convertNonAscii) 
{
  initialize();

  // If there are non-ascii characters to be converted, convert to
  // bytes, then back to string.  Though this seems redundant, it
  // allows printable characters to be represented by their ASCII
  // codes.  Those codes will be converted to their printable ASCII
  // equivalents by bytesToString(), allowing predictable comparison
  // to other strings treated in the same way.

  ucvec_    = stringToBytes(str, convertNonAscii);
  str_      = bytesToString(ucvec_, convertNonAscii);
  convertNonAscii_ = convertNonAscii;
}

String::String(const std::vector<unsigned char>& ucvec, bool convertNonAscii)
{
  initialize();
  ucvec_    = ucvec;
  str_      = bytesToString(ucvec, convertNonAscii);
  convertNonAscii_ = convertNonAscii;
}

String::String(const std::vector<char>& cvec, bool convertNonAscii)
{
  initialize();
  ucvec_.resize(cvec.size());

  unsigned nChar = cvec.size();
  std::ostringstream os;

  for(unsigned iChar=0; iChar < nChar; iChar++) {

    ucvec_[iChar] = cvec[iChar];

    if(!isprint(ucvec_[iChar]) && convertNonAscii) {
      os << "(0x" << (unsigned int)ucvec_[iChar] << ")";
    } else {
      os << ucvec_[iChar];
    }

  }

  str_ = os.str();
  convertNonAscii_ = convertNonAscii;
}

void String::initialize()
{
  convertNonAscii_ = false;
  iStart_ = 0;
}

/**.......................................................................
 * Destructor.
 */
String::~String() {}

/**.......................................................................
 * Strip all occurrences of the characters in stripStr from a target
 * string.
 */
void String::strip(std::string& targetStr, const std::string& stripStr)
{
  // For each char in the strip string, remove all occurrences in the
  // target string

  for(unsigned istrip=0; istrip < stripStr.size(); istrip++)
    strip(targetStr, stripStr[istrip]);
}

/**.......................................................................
 * Strip all occurrences of the characters in stripStr from a target
 * string.
 */
void String::strip(const std::string& stripStr)
{
  strip(str_, stripStr);
}

/**.......................................................................
 * Strip all occurrences of a character from a target string.
 */
void String::strip(std::string& targetStr, char stripChar)
{
  bool erased;

  do {
    erased = false;
    
    std::string::size_type idx;
    idx = targetStr.find(stripChar);
    
    if(idx != std::string::npos) {
      targetStr.erase(idx, 1);
      erased = true;
    }
    
  } while(erased);
}

/**.......................................................................
 * Strip all occurrences of a character from a target string.
 */
void String::strip(char stripChar)
{
  strip(str_, stripChar);
}

/**.......................................................................
 * Return true if our string contains the target character
 */
bool String::contains(char c)
{
  std::string::size_type idx;

  idx = str_.find(c);

  return idx != std::string::npos;
}

/**.......................................................................
 * Return true if our string contains the target string
 */
bool String::contains(string s)
{
  std::string cmp = convert(s);

  std::string::size_type idx;

  idx = str_.find(cmp);

  return idx != std::string::npos;
}

/**.......................................................................
 * Return true if the (unsearched) remainder of our string contains
 * the target string
 */
bool String::remainderContains(string s)
{
  std::string cmp = convert(s);

  std::string::size_type idx;

  idx = str_.find(cmp, iStart_);

  return idx != std::string::npos;
}

std::string String::convert(char& c)
{
  std::ostringstream os;

  if(!isprint(c) && convertNonAscii_) {
    os << "(0x" << (unsigned int)c << ")";
  } else {
    os << c;
  }

  return os.str();
}

std::string String::convert(string& s)
{
  std::vector<unsigned char> ucvec = stringToBytes(s, convertNonAscii_);
  return bytesToString(ucvec, convertNonAscii_);
}

bool String::matches(unsigned char c, std::string matchSet)
{
  for(unsigned iEl=0; iEl < matchSet.size(); iEl++)
    if(c == matchSet[iEl])
      return true;

  return false;
}

/**......................................................................
 * Assignment operators
 */
void String::operator=(const std::string& str)
{
  iStart_ = 0;
  str_    = str;
}

void String::operator=(const String str)
{
  iStart_ = str.iStart_;
  str_    = str.str_;
}

bool String::operator==(std::string str)
{
  return str_ == str;
}

bool String::operator==(String str)
{
  return str_ == str.str_;
}

char& String::operator[](unsigned int index)
{
  return str_[index];
}

bool String::operator!=(String str)
{
  return str_ != str.str_;
}

bool String::operator<(String& str)
{
  unsigned thisLen = str_.size();
  unsigned thatLen = str.size();
  unsigned minLen = (thisLen < thatLen) ? thisLen : thatLen;

  if(*this == str)
    return false;

  char c1, c2;
  for(unsigned i=0; i < minLen; i++) {
    c1 = str_[i];
    c2 = str[i];

    if(c1 == c2)
      continue;

    if(islower(c1)) {
      if(islower(c2)) {
	return c1 < c2;
      } else {
	return true;
      }
    } else {
      if(isupper(c2)) {
	return c1 < c2;
      } else {
	return false;
      }
    }
  }

  // Else the strings are equal to the minimum length.  Then the
  // shorted string will be alphabetized first

  return thisLen < thatLen;
}

/**.......................................................................
 * Allows cout << String
 */
std::ostream& sza::util::operator<<(std::ostream& os, String str)
{
  os << str.str_;
  return os;
}

String String::findFirstInstanceOf(std::string stop)
{
  iStart_ = 0;
  return findNextInstanceOf("", false, stop, true);
}

String String::findFirstInstanceOf(std::string start, std::string stop)
{
  iStart_ = 0;
  return findNextInstanceOf(start, true, stop, true);
}

String String::findNextInstanceOf(std::string stop)
{
  return findNextInstanceOf("", false, stop, true);
}

String String::findNextInstanceOf(std::string start, std::string stop)
{
  return findNextInstanceOf(start, true, stop, true);
}

String String::findNextString()
{
  return findNextInstanceOf(" ", false, " ", false, true);
}

/**.......................................................................
 * Search a string for substrings separated by the specified start
 * and stop strings.  
 *
 * If useStart = true, then we will search for the start string first, then
 *                     search for the stop string, and return everything 
 *                     between them.
 * 
 *                     otherwise, we will just search for the end string
 *                     and return everything up to it.
 */
String String::findNextInstanceOf(std::string start, bool useStart, 
				  std::string stop,  bool useStop, bool consumeStop)
{
  String retStr;
  std::string::size_type iStart=0, iStop=0;

  if(iStart_ == std::string::npos) {
    return retStr;
  }

  // If we are searching for the next string separated by the start
  // and stop strings, then start by searching for the start string

  if(useStart) {

    iStart_ = str_.find(start, iStart_);

    // Return empty string if we hit the end of the string without
    // finding a match

    if(iStart_ == std::string::npos) {
      return retStr;
    }
    
    // Else start the search for the end string at the end of the
    // string we just found

    iStart_ += start.size();
  }

  // Now search for the stop string

  iStop = str_.find(stop, iStart_);

  // If we insist that the stop string is present, return an empty
  // string if it wasn't

  if(useStop) {
    if(iStop == std::string::npos) {
      iStart_ -= start.size();
      return retStr;
    }
  }

  // Else match anything up to the stop string, if it was found, or
  // the end of the string if it wasn't

  retStr = str_.substr(iStart_, iStop-iStart_);

  // We will start the next search at the _beginning_ of the stop
  // string, so just increment iStart_ to point to the first char of
  // the stop string (or end of string, if the stop string wasn't found)
 
  iStart_ = iStop;

  // If consumeStop = true, then start the next search just _after_
  // the stop string (if it was found).

  if(consumeStop && iStop != std::string::npos) {
    iStart_ += stop.size();
  }

  return retStr;
}

bool String::atEnd()
{
  return iStart_ == std::string::npos;
}

int String::toInt()
{
  return toInt(str_);
}

int String::toInt(std::string str)
{
  int iVal = strtol(str.c_str(), NULL, 10);

  if(iVal==0 && errno==EINVAL)
    ThrowError("Cannot convert '" << str << "' to a valid integer");
  
  return iVal;
}

float String::toFloat()
{
  float fVal = strtof(str_.c_str(), NULL);

  if(errno==EINVAL)
    ThrowError("Cannot convert '" << str_ << "' to a valid float");

  return fVal;
}

double String::toDouble()
{
  double dVal = strtod(str_.c_str(), NULL);

  if(errno==EINVAL)
    ThrowError("Cannot convert '" << str_ << "' to a valid double");

  return dVal;
}

bool String::isEmpty()
{
  return str_.size() == 0;
}

/**.......................................................................
 * Return the next string separated by any of the chars in separators
 */
String String::findNextStringSeparatedByChars(std::string separators, 
					      bool matchEndOfString)
{
  String retStr("");
  unsigned iStart=0, iStop=0;
  unsigned iEl=0;

  for(iEl=iStart_; iEl < str_.size(); iEl++) {
    if(matches(str_[iEl], separators)) {
      break;
    }
  }

  // If we hit the end of the string without finding a match, return
  // an empty string, and don't advance the start counter.  Unless the
  // separator string was the empty string, in which case we will
  // match end of string too

  if(iEl == str_.size() && !matchEndOfString)
    return retStr;

  // Else extract the portion of the string that matches

  retStr = str_.substr(iStart_, iEl-iStart_);

  // Now consume any trailing characters that also match, including whitespace

  for(unsigned iMatch = iEl; iMatch < str_.size(); iMatch++) {
    if(matches(str_[iMatch], separators) || matches(str_[iMatch], whiteSpace_))
      iEl++;
    else
      break;
  }

  // Advance iStart_ to the first non-matching character

  iStart_ = iEl;

  // And return the matched substring

  return retStr;
}

unsigned String::size()
{
  return str_.size();
}

void String::resetToBeginning()
{
  iStart_ = 0;
}

/**.......................................................................
 * Replace all occurrences of a character with a replacement character
 */
void String::replace(char stripChar, char replaceChar)
{
  replace(str_, stripChar, replaceChar);
}

/**.......................................................................
 * Replace all occurrences of a character with a replacement character
 */
void String::replace(std::string& targetStr, char stripChar, char replaceChar)
{
  bool replaced;

  do {
    replaced = false;
    
    std::string::size_type idx;
    idx = targetStr.find(stripChar);
    
    if(idx != std::string::npos) {
      targetStr[idx] = replaceChar;
      replaced = true;
    }
    
  } while(replaced);
}

String String::toLower()
{
  String retStr = str_;

  for(unsigned i=0; i < size(); i++) {
    retStr[i] = tolower(retStr[i]);
  }

  return retStr;
}

String String::capitalized()
{
  String retStr = capitalized(str_);
  return retStr;
}

String String::firstToLower()
{
  String retStr = firstToLower(str_);
  return retStr;
}

String String::toUpper()
{
  String retStr = str_;

  for(unsigned i=0; i < size(); i++) {
    retStr[i] = toupper(retStr[i]);
  }

  return retStr;
}

String String::firstToUpper()
{
  String retStr = firstToUpper(str_);
  return retStr;
}

std::string String::toLower(std::string str)
{
  std::string retStr(str);

  for(unsigned i=0; i < str.size(); i++) {
    retStr[i] = tolower(retStr[i]);
  }

  return retStr;
}

std::string String::capitalized(std::string str)
{
  std::string retStr(str);

  for(unsigned i=0; i < str.size(); i++) {
    if(i==0) {
      retStr[i] = toupper(retStr[i]);
    } else {
      retStr[i] = tolower(retStr[i]);
    }
  }

  return retStr;
}

std::string String::toUpper(std::string str)
{
  std::string retStr(str);

  for(unsigned i=0; i < str.size(); i++) {
    retStr[i] = toupper(retStr[i]);
  }

  return retStr;
}

std::string String::firstToLower(std::string str)
{
  std::string retStr(str);
  retStr[0] = tolower(retStr[0]);
  return retStr;
}

std::string String::firstToUpper(std::string str)
{
  std::string retStr(str);
  retStr[0] = toupper(retStr[0]);
  return retStr;
}

/**.......................................................................
 * Convert from string representation to a byte representation.
 *
 * If convertNonAscii == true, convert string representations of
 * non-ascii bytes to bytes
 */
std::string String::bytesToString(std::vector<unsigned char> ucvec, bool convertNonAscii)
{
  unsigned nChar = ucvec.size();
  std::ostringstream os;

  for(unsigned iChar=0; iChar < nChar; iChar++) {
 
    if(!isprint(ucvec[iChar]) && convertNonAscii) {
      os << "(0x" << (unsigned int)ucvec[iChar] << ")";
    } else {
      os << ucvec[iChar];
    }

  }

  return os.str();
}

/**.......................................................................
 * Convert from string representation to a byte representation.
 *
 * If convertNonAscii == true, convert string representations of
 * non-ascii bytes to bytes
 */
std::string String::bytesToString(std::vector<char> cvec, bool convertNonAscii)
{
  unsigned nChar = cvec.size();
  std::ostringstream os;

  for(unsigned iChar=0; iChar < nChar; iChar++) {
 
    if(!isprint(cvec[iChar]) && convertNonAscii) {
      os << "(0x" << (unsigned int)cvec[iChar] << ")";
    } else {
      os << cvec[iChar];
    }

  }

  return os.str();
}

/**.......................................................................
 * Convert from string representation to a byte representation.
 *
 * If convertNonAscii == true, convert string representations of
 * non-ascii bytes to bytes
 */
std::vector<unsigned char> String::stringToBytes(std::string s, 
						 bool convertNonAscii)
{
  std::vector<unsigned char> ucvec;

  if(s.size() > 0) {

    // string::size() returns the size of the string including the
    // terminating NULL character.  Set the size to size()-1 so that
    // this terminator is no interpreted as an extra byte
    
    unsigned nChar = s.size();
    unsigned iChar = 0;
    
    do {
      
      unsigned char c = s[iChar];
      
      if(c == '(' && convertNonAscii) {
	parseNonAscii(s, iChar, nChar, ucvec);
      } else {
	ucvec.push_back(c);
	++iChar;
      }
      
    } while(iChar < nChar);
  }


  return ucvec;
}

/**.......................................................................
 * Parse a string representation of a non-ascii byte into a byte
 */
void String::parseNonAscii(std::string& s, 
			   unsigned int& iChar, unsigned int nChar,
			   std::vector<unsigned char>& ucvec)
{
  unsigned iCurr = iChar;
  std::ostringstream os;

  if(++iCurr < nChar && s[iCurr] == '0') {

    if(++iCurr < nChar && s[iCurr] == 'x') {

      while(++iCurr < nChar && s[iCurr] != ')') {
	os << s[iCurr];
      }

      // If we found an end-brace, convert the decimal number to the
      // equivalent byte

      if(s[iCurr] == ')') {
	unsigned int iVal = (unsigned int)toInt(os.str());
	ucvec.push_back((unsigned char)iVal);
	iChar = ++iCurr;
	return;
      } else {
	ThrowError("String ended before conversion was complete");
      }
    }
    
  } else {
    return;
  }
}

std::vector<unsigned char> String::getData()
{
  return stringToBytes(str_, convertNonAscii_);
}

String String::remainder()
{
  if(iStart_ >= str_.size()) {
    return String("");
  } else {
    return  str_.substr(iStart_, str_.size()-iStart_);
  }
}

/**.......................................................................
 * Advance the start pointer of this string to the first non-whitespace char
 */
void String::advanceToNextNonWhitespaceChar()
{
  for(; iStart_ < str_.size(); iStart_++) 
    if(!matches(str_[iStart_], whiteSpace_)) 
      break;
}

void String::wrapTo(unsigned nChar, unsigned nIndent) 
{
  std::ostringstream os;
  unsigned count = 0;
  for(unsigned i=0; i < str_.size(); i++) {

    // If we're indenting, indent the start of the string too

    if(i==0) {
      for(unsigned iIndent=0; iIndent < nIndent; iIndent++) {
	os << ' ';
      }
    }

    //------------------------------------------------------------
    // Continue adding characters until we reach the wrap limit (or
    // encounter a newline)
    //------------------------------------------------------------

    if(count < nChar) {

      //------------------------------------------------------------
      // If the next character is a newline, reset the counter
      //------------------------------------------------------------
      
      if(str_[i] == '\n') {
	os << str_[i];
	for(unsigned iIndent=0; iIndent < nIndent; iIndent++) {
	  os << ' ';
	}
	  
	count = 0;
      } else {
	os << str_[i];
	count++;
      }

      //------------------------------------------------------------
      // Else we have reached the wrap limit.  Add a newline, reset the
      // counter before adding the current character (if the current
      // character is already a newline, don't add two)
      //------------------------------------------------------------

    } else {

      if(str_[i] == '\n') {
	os << str_[i];
	for(unsigned iIndent=0; iIndent < nIndent; iIndent++) {
	  os << ' ';
	}
	count = 0;
      } else {
	os << std::endl;
	for(unsigned iIndent=0; iIndent < nIndent; iIndent++) {
	  os << ' ';
	}
	os << str_[i];
	count = 1;
      }
    }

  }

  str_ = os.str();
}
