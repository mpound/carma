#ifndef SZA_UTIL_STRING_H
#define SZA_UTIL_STRING_H

/**
 * @file String.h
 * 
 * Tagged: Wed May 12 09:30:13 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <string>
#include <vector>

namespace sza {
  namespace util {
    
    class String {
    public:
      
      /**
       * Constructor.
       */
      String();
      String(unsigned int, bool convertNonAscii = false);
      String(const std::string& str, bool convertNonAscii = false);
      String(const std::vector<unsigned char>& ucvec, bool convertNonAscii = false);
      String(const std::vector<char>& ucvec, bool convertNonAscii = false);
      
      /**
       * Destructor.
       */
      virtual ~String();
      
      static void strip(std::string& targetStr, const std::string& stripStr);
      static void strip(std::string& targetStr, char stripChar);

      void strip(const std::string& stripStr);
      void strip(char stripChar);
      
      // Wrap strings to the specified length.  Strings longer than
      // this will have newlines inserted into them as appropriate.
      // However, existing newlines will be preserved

      void wrapTo(unsigned nChar, unsigned nIndent);

      bool contains(char c);
      bool contains(std::string s);
      bool remainderContains(std::string s);
      String remainder();

      void replace(char stripChar, char replaceChar);
      static void replace(std::string& targetStr, char stripChar, char replaceChar);

      void advanceToNextNonWhitespaceChar();

      void operator=(const std::string& str);
      void operator=(const String str);
      
      bool operator<(String& str); 
      bool operator==(String str);
      bool operator==(std::string str);
      bool operator!=(String str);

      char& operator[](unsigned int index);

      inline std::string& str()
	{
	  return str_;
	}
      
      // Allows cout << String

      friend std::ostream& operator<<(std::ostream& os, String str);
      
      String findFirstInstanceOf(std::string start, bool useStart, 
				 std::string stop, bool useStop);

      String findFirstInstanceOf(std::string start, std::string stop);

      String findFirstInstanceOf(std::string stop);

      String findNextInstanceOf(std::string start, bool useStart, 
				std::string stop, bool useStop, bool consumeStop=false);

      String findNextInstanceOf(std::string start, std::string stop);

      String findNextInstanceOf(std::string stop);

      String findNextString();

      String toLower();
      String toUpper();
      String firstToLower();
      String firstToUpper();
      String capitalized();

      static const std::string emptyString_;
      static const std::string whiteSpace_;

      static std::string toLower(std::string str);
      static std::string toUpper(std::string str);
      static std::string firstToLower(std::string str);
      static std::string firstToUpper(std::string str);
      static std::string capitalized(std::string str);

      String findNextStringSeparatedByChars(std::string separators, bool matchEndOfString=true);
      bool atEnd();
      void resetToBeginning();

      void initialize();
      
      static int toInt(std::string s);

      int toInt();
      float toFloat();
      double toDouble();

      bool isEmpty();
      bool matches(unsigned char c, std::string matchSet);

      unsigned size();

      // Convert from string representation of non-ascii bytes to
      // bytes
      
      static std::vector<unsigned char> 
	stringToBytes(std::string s, bool convertNonAscii=false);

      static void parseNonAscii(std::string& s, 
				unsigned int& iChar, unsigned int nChar,
				std::vector<unsigned char>& ucvec);

      static std::string bytesToString(std::vector<unsigned char> ucvec, bool convertNonAscii = false);
      static std::string bytesToString(std::vector<char> cvec, bool convertNonAscii = false); 

      std::vector<unsigned char> getData();

    private:
      
      std::string::size_type iStart_;
      std::string str_;
      std::vector<unsigned char> ucvec_;

      // If true, the 'string' may contain non-ascii bytes.  In this
      // case, we represent non-printable characters by their decimal
      // integer equivalents, and any string to be compared to our
      // string is similary converted before the comparison is
      // performed

      bool convertNonAscii_;

      // Convert an input char or string to the appropriate
      // representation for comparison with this String.  For example,
      // if we can contain non-Ascii bytes, then any non-ascii bytes
      // in the input will have been converted to their decimal
      // integer equivalents

      std::string convert(char& c);
      std::string convert(std::string& s);
      
    }; // End class String
    
  } // End namespace util
} // End namespace sza




#endif // End #ifndef SZA_UTIL_STRING_H
