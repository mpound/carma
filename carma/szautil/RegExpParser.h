#ifndef SZA_UTIL_REGEXPPARSER_H
#define SZA_UTIL_REGEXPPARSER_H

/**
 * @file RegExpParser.h
 * 
 * Tagged: Sat Oct  2 16:22:41 UTC 2004
 * 
 * @author 
 */
#include <string>

namespace sza {
  namespace util {
    
    class RegExpParser {
    public:
      
      /**
       * Constructor with RegExp string
       */
      RegExpParser(std::string regexp);
      
      /**
       * Destructor.
       */
      virtual ~RegExpParser();
      
      /**
       * This is the top level matching routine. It returns true if
       * the string in this RegExpParser matches the string, otherwise
       * it returns false.
       */
      bool matches(std::string matchString);
      
    private:
      
      // The reg exp string to match
      
      std::string regExpString_;
      
      // Utility pointer to the above
      
      char* regExpPtr_; 
      
      // Private recursion method for matching a string
      
      bool matches(char* matchString, bool incrementRegExpPtr=true);
      
      /**
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
      bool matchSingle(char *start_ptr, char *end_ptr, char ch);
      
      /**
       * Given an input string, return either the first character or
       * if that first character is a \ return the corresponding
       * escaped character.  If there is an error return '\0'. The
       * pointer into the input string will be returned via the
       * second argument, incremented to the point just after the
       * last character used. The flag, 'wasEscaped' is returned as
       * 1 if the returned character was an escape sequence, and 0
       * otherwise.
       */
      char getNextChar(char *string, char **tail, bool& wasEscaped);
      
    }; // End class RegExpParser
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REGEXPPARSER_H
