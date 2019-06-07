#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/RegExpParser.h"
#include <cstring>

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Constructor.
 */
RegExpParser::RegExpParser(string regExpString) 
{
  regExpString_ = regExpString;
}

/**.......................................................................
 * Destructor.
 */
RegExpParser::~RegExpParser() {}

/*.......................................................................
 * This is the top level matching routine. It returns 1 if the string
 * regexp matches the string, string - otherwise it returns 0.
 * The final argument is an error flag. It is normally 0 but when there
 * has been a syntax error it becomes 1.
 */
bool RegExpParser::matches(string matchString)
{
  // Get the C-string corresponding to the input string

  char* matchCString  = (char*)matchString.c_str();

  // Re-initialize the regexp pointer to the start of the string

  regExpPtr_ = (char*)regExpString_.c_str();

  try {
    bool retVal = matches(matchCString);
    return retVal;
  } catch(...) {
    return false;
  }
}

/**.......................................................................
 * Private recursion method for matching a string
 */
bool RegExpParser::matches(char* matchString, bool incrementRegExpPtr)
{
  bool wasEscaped;  /* Returned by getNextChar() to report that the */
                    /* character that it returned was escaped */
  char *start_ptr;  /* Start pointer of regexp for matchSingle() */
  char *end_ptr;    /* End pointer of regexp for matchSingle() */
  char *tail;       /* Pointer to tail part of some string not yet processed */
  char new_char;    /* Current char returned by getNextChar() */
  int match_test;   /* 1 or 0 depending on whether success is measured */
                    /* by a match or by no match */

  // Parse regExpPtr_.

  for(;;) {

    new_char=getNextChar(regExpPtr_, (incrementRegExpPtr ? &regExpPtr_ : &tail), wasEscaped);
    
    // Interpret new regexp character.

    if(!wasEscaped) {
      switch (new_char) {
	
	// Stop at end of regexp string. If the string is also
	// exhausted then the match has succeded.

      case '\0':
	return (*matchString == '\0') ? 1:0;
	break;
	
	// Start of a single character regexp.

      case '[':
	
	// Keep a record of where the inside of the [] regexp started.

	start_ptr = regExpPtr_;
	
	// Locate the matching (un-escaped) ']'.

	for(;;) {
	  new_char=getNextChar(regExpPtr_, &regExpPtr_, wasEscaped);
	  if(new_char== '\0' || (new_char==']' && !wasEscaped) )
	    break;
	};
	
	// If not found, report error and return.

	if(new_char == '\0') 
	  ThrowError("Syntax error: \'[\' not matched in regExpPtr_");
	
	// Record the pointer to the last char in the [] regExpPtr_.

	end_ptr = regExpPtr_-2;
	
	// If the first character of the regExpPtr_ is an
	// un-escaped '^' then the result of the one-char
	// regExpPtr_ match is to be complemented.

	new_char=getNextChar(start_ptr, &tail, wasEscaped);
	if(new_char == '^' && !wasEscaped) {
	  start_ptr=tail;
	  match_test=1;
	}
	else {
	  match_test=0;
	};
	
	// Make sure that the [] expression contains something.

	if(end_ptr < start_ptr) 
	  ThrowError("Syntax error: Empty [] regExpPtr_ encountered");

	// If the character following the closing ']' is an unescaped
	// '*', then the single-character regExpPtr_ must match 0
	// or more characters. Otherwise it should match exactly one.

	new_char=getNextChar(regExpPtr_, &tail, wasEscaped);
	if(new_char=='*' && !wasEscaped) {
	  regExpPtr_ = tail;
	  for(;;) {
	    if(matches(matchString)) {
	      return true;
	    }	      
	    if(*matchString == '\0')
	      return false;
	    if(matchSingle(start_ptr, end_ptr, *(matchString++))
	       == match_test)
	      return false;
	  };
	}
	else {
	  
	  // A '+' following a [] expression says that it should match
	  // exactly one character. This is only really required when
	  // one wants to a following '*' not to be associated with
	  // the [].

	  if(new_char=='+' && !wasEscaped) regExpPtr_=tail;
	  if(*matchString == '\0')
	    return false;
	  if(matchSingle(start_ptr, end_ptr, *(matchString++))
	     == match_test)
	    return false;
	};
	continue;
	
	// '.' matches any character in string.

      case '.':
	if(*matchString == '\0')
	  return false;
        matchString++;
	continue;
	
	// Zero or more characters in matchString.

      case '*':
	
	// Further '*'s are redundant.

	while(getNextChar(regExpPtr_, &tail, wasEscaped)=='*'
	      && !wasEscaped) regExpPtr_=tail;
	
	// End of regExpPtr_ matchString? If so the rest of
	// matchString definitely matches.

	if(*regExpPtr_ == '\0') {
	  return true;
	}
	
	// Now we don't know where the next part of the regExpPtr_
	// will continue in matchString, so keep recursively calling
	// match() for each subsequent character in matchString until
	// we hit the end of matchString (in which case the match has
	// failed) or match() returns success.

	while(*matchString != '\0'  && *regExpPtr_ != '\0') {

	  if(matches(matchString++)) {
	    return true;
	  }
	}

	return false;
	
	// The character is not a special one - nor are the escaped
	// characters that didn't get into this switch, they will all
	// be handled together off the bottom of the switch
	// expression.

      default:
	break;
      };
    };
    
    // Simple character in regular expression must match that in
    // matchString.  If the character is a \ however it is assumed to
    // escape the following character - which in turn must match the
    // character in the matchString.

    if(*matchString == new_char) {
      matchString++;

      if(!incrementRegExpPtr) {
	regExpPtr_ = tail;
	incrementRegExpPtr = true;
      }
    } else {

      return false;
    };
  };
}

/**.......................................................................
 * Given an input string, return either the first character or if that
 * first character is a \ return the corresponding escaped character.
 * If there is an error return '\0'. The pointer into the input string
 * will be returned via the second argument, incremented to the point
 * just after the last character used. The flag, 'wasEscaped' is returned
 * as 1 if the returned character was an escape sequence, and 0 otherwise.
 */
char RegExpParser::getNextChar(char *str, char **tail, bool& wasEscaped)
{
  char ch;   // Will hold the character that is to be returned 

  
  // Check for escape sequences.

  switch (*str) {
  case '\\':
    str++;
    wasEscaped=1;
    switch (*str) {
      
      // Standard escape equivalents for control characters.

    case 'n':
      ch = '\n';
      break;
    case 'r':
      ch = '\r';
      break;
    case 't':
      ch = '\t';
      break;
    case 'f':
      ch = '\f';
      break;
      
      // Octal escape sequence.

    case '0': case '1': case '2': case '3': case '4': case '5': case '6':
    case '7': case '8': case '9':
      ch = (char) strtol(str, &str, 8);
      break;
      
      // Hex escape sequence.

    case 'x': case 'X':
      if(!isxdigit((int) str[1])) {
	ReportError("Incomplete \\x.. hexadecimal escape sequence");
	*tail=str;
	return '\0';
      };
      ch = (char) strtol(str, &str, 16);
      break;
      
      // No char to escape - whoops - error.

    case '\0':
      ReportError("Incomplete escape sequence at end of str");
      *tail = str;
      return '\0';
      break;
      
      // Any other character gets passed unscathed.

    default:
      ch = *str;
    };
    break;
  default:
    ch = *str;
    wasEscaped=0;
    break;
  };
  
  // Return the result.

  *tail = ++str;
  return ch;
}

/**.......................................................................
 * Try to match a single character ch with a [] regexp character list.
 * Return 1 on success, 0 on failure. Normally *was_error is returned
 * as 0, but if there is a regexp syntax error it is returned as 1.
 * The regexp can be formed of any arrangement of the following:
 *
 * A-Z  : Any capital letter between the two characters provided.
 * a-z  : Any lower case letter between the two characters provided.
 * 1-9  : Any digit between the numbers specified.
 * adbc : Any charcter from the specified list.
 * ^    : When placed before any of the above, the match will succede
 *      : if the character is not one of those specified. Characters
 *      : may be escaped to remove any special meanings or to include
 *      : control characters etc..
 */
bool RegExpParser::matchSingle(char *start_ptr, char *end_ptr, char ch)
{
  int in_range;  /* Flags when half way through processing a regexp range */
  bool wasEscaped;/* Flags characters that were escaped when read */
  char last_char;/* Keeps a record of first char of a potential regexp range */
  char new_char; /* Latest char read from regexp */
  char *regexp;  /* Used to step through regexp */
  char *ptr;     /* Pointer for general usage */
  
  // Strings holding sensible collating sequence for ranges.  It would
  // be much faster to assume ASCII but not very portable.

  static char upper[]="ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  static char lower[]="abcdefghijklmnopqrstuvwxyz";
  static char digit[]="0123456789";

  // Copy start pointer to regexp-stepping pointer.

  regexp=start_ptr;
  
  // Process each char list in regexp in turn until a match is found
  // or the ending ']' is reached.

  in_range=0;
  last_char='\0';
  while(regexp <= end_ptr) {
    
    // Get the next character of the regexp, with due regard for
    // escape sequences.

    new_char=getNextChar(regexp, &regexp, wasEscaped);
    
    // Check for the range-symbol.

    if(!wasEscaped && new_char == '-') {
      if(in_range || last_char == '\0') 
	ThrowError("Incomplete regexp range");

      in_range=1;
      continue;
    };
    
    // Try to match the latest character with ch.

    if(ch==new_char)
      return true;
    
    // Is this the final char of a range designation?

    if(!in_range) {
      last_char=new_char;
    }
    
    // Complete range designation recieved - process it.

    else {
      
      // Check types of the two characters and see if ch is between
      // them.

      if(isdigit((int) last_char) && isdigit((int) new_char)) {
	ptr = strchr(digit, ch);
	if(strchr(digit,last_char) <= ptr && strchr(digit, new_char) >= ptr)
	  return true;
      }
      else if(isupper((int) last_char) && isupper((int) new_char)) {
	ptr = strchr(upper, ch);
	if(strchr(upper,last_char) <= ptr && strchr(upper, new_char) >= ptr)
	  return true;
      }
      else if(islower((int) last_char) && islower((int) new_char)) {
	ptr = strchr(lower, ch);
	if(strchr(lower,last_char) <= ptr && strchr(lower, new_char) >= ptr)
	  return true;
      }
      else {
	ThrowError("Syntax error in regexp character range");
      };
      
      // Range parsed, but with no match - prepare for next part of
      // regexp.

      in_range = 0;
      last_char = '\0';
    };
  };
  
  // No match found.

  return false;
}
