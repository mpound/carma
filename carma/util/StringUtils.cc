/**
 * @file
 * Implementation for some common string functions.
 *
 * @author Original: Dave Mehringer
 *
 * $Id: StringUtils.cc,v 1.44 2009/03/20 18:59:38 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */

#include "carma/util/StringUtils.h"

#include <algorithm>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <cmath>
#include <list>

#include <openssl/evp.h>

#include "carma/util/IllegalArgumentException.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma::util;


namespace {


string
myEscapeChar( const char c )
{
    if ( c == '\\' )
        return "\\\\";
    
    if ( c == '\'' )
        return "\\'";
    
    if ( c == '\0' )
        return "\\0";
    
    if ( c == '\n' )
        return "\\n";
    
    if ( c == '\r' )
        return "\\r";
    
    if ( c == '\t' )
        return "\\t";
    
    string result;

    if ( (c >= 0x20) && (c <= 0x7E) ) {
        result += c;
    } else {
        ostringstream oss;
        
        oss << "\\x" << hex << static_cast< unsigned int >( c );
        
        result += oss.str();
    }
    
    return result;
}


string
myEscapeString( const string & s )
{
    string result;
    result.reserve( 3 * s.size() );
    
    string::const_iterator i = s.begin();
    const string::const_iterator iEnd = s.end();
    
    for ( ; i != iEnd; ++i ) {
        const char c = *i;
        
        if ( c == '\\' )
            result += "\\\\";
        else if ( c == '"' )
            result += "\\\"";
        else if ( c == '\0' )
            result += "\\0";
        else if ( c == '\n' )
            result += "\\n";
        else if ( c == '\r' )
            result += "\\r";
        else if ( c == '\t' )
            result += "\\t";
        else if ( (c >= 0x20) && (c <= 0x7E) )
            result += c;
        else {
            ostringstream oss;
            
            oss << "\\x" << hex << static_cast< unsigned int >( c );
            
            result += oss.str();
        }
    }
    
    return result;
}


}  // namespace < anonymous >


// Default constructor does nothing
StringUtils::StringUtils() { }

// Destructor does nothing
StringUtils::~StringUtils() { }


string
StringUtils::lowASCIIAlphaNumericToUpper( const string & str ) {
    string newstr = str;

    string::iterator i = newstr.begin();
    const string::iterator iEnd = newstr.end();

    for ( ; i != iEnd; ++i ) {
        const char c = *i;
    
        if ( c == '\0' )
            break;
        
        // 'a' == 0x61, 'z' = 0x7A
        if ( (c >= 'a') && (c <= 'z') ) {
            *i = static_cast< char >( c - 32 );
        } else if ( (c < 0x20) || (c > 0x7E) ) {
            if ( (c == '\t') || (c == '\n') || (c == '\r') ) {
                // Silently strip them
            } else {
                ostringstream emsg;
    
                emsg << "character '" << myEscapeChar( c ) << "' (hex 0x"
                     << hex << static_cast< unsigned int >( c )
                     << ") is out of range in"
                     << " StringUtils::lowASCIIAlphaNumericToUpper(\""
                     << myEscapeString( str ) << "\")";
                     
                // Noisely strip them
                programLogErrorIfPossible( emsg.str() );
                // throw out_of_range( emsg.str() );
            }
        }
    }
    
    return newstr;
}


string
StringUtils::lowASCIIAlphaNumericToLower( const string & str ) {
    string newstr = str;

    string::iterator i = newstr.begin( );
    const string::iterator iEnd = newstr.end( );

    for ( ; i != iEnd; ++i ) {
        const char c = *i;
    
        if ( c == '\0' )
            break;
        
        // 'A' == 0x41, 'Z' = 0x5A
        if ( (c >= 'A') && (c <= 'Z') ) {
            *i = static_cast< char >( c + 32 );
        } else if ( (c < 0x20) || (c > 0x7E) ) {
            if ( (c == '\t') || (c == '\n') || (c == '\r') ) {
                // Silently strip them
            } else {
                ostringstream emsg;
    
                emsg << "character '" << myEscapeChar( c )  << "' (hex 0x"
                     << hex << static_cast< unsigned int >( c )
                     << ") is out of range in"
                     << " StringUtils::lowASCIIAlphaNumericToLower(\""
                     << myEscapeString( str ) << "\")";
                     
                // Noisely strip them
                programLogErrorIfPossible( emsg.str() );
                // throw out_of_range( emsg.str() );
            }
        }
    }
    
    return newstr;
}


string StringUtils::erase(const string& str, const vector<char>& chars) {
    string newstr = str;
    string::iterator i = newstr.begin();
    string::const_iterator iEnd = newstr.end();
    vector<char>::const_iterator c;
    vector<char>::const_iterator cEnd = chars.end();
    bool found;
    while(i != iEnd) {
        found = false;
        c = chars.begin();
        while(c != cEnd) {
            if(*i == *c) {
                found = true;
                break;
            }
            c++;
        }
        if(found) {
            newstr.erase(i);
            iEnd = newstr.end();
        } else {
            i++;
        }
    }
    return newstr;
}


void
StringUtils::trimInplace( string &       str,
                          const string & chars )
{
    if ( str.empty() || chars.empty() ) return;

    // remove characters at the beginning of the string.
    const string::size_type firstNonCharPos = str.find_first_not_of( chars );
    if ( firstNonCharPos == string::npos ) { 
        str.clear();
        return;
    } else {
        str.erase( 0, firstNonCharPos );
    }

    // remove characters at the end of the string
    const string::size_type lastNonCharPos = str.find_last_not_of( chars );
    const string::size_type nextCharPos = lastNonCharPos + 1;
    if ( nextCharPos < str.size( ) ) { 
        str.erase( nextCharPos );
    }
}


string
StringUtils::trim( const string & str,
                   const string & chars )
{
    string newstr = str;

    trimInplace( newstr, chars );

    return newstr;
}


void
StringUtils::trimWhiteSpaceInplace( string & str )
{
    const string whiteSpace( " \n\r\t" );

    trimInplace( str, whiteSpace );
}


string
StringUtils::trimWhiteSpace( const string & str )
{
    string newstr = str;

    trimWhiteSpaceInplace( newstr );

    return newstr;
}


string StringUtils::collapse(const string& str, const vector<char>& chars) {
    string newstr = str;
    vector<char>::const_iterator c;
    vector<char>::const_iterator cEnd = chars.end();
    bool found = false;
    string::iterator i = newstr.begin();
    string::const_iterator iEnd = newstr.end();
    string::const_iterator target;
    while(i != iEnd) {
        if(found && *target == *i) {
            newstr.erase(i);
            iEnd = newstr.end();
        } else {
            found = false;
            c = chars.begin();
            while(c != cEnd) {
                if(*i == *c) {
                    found = true;
                    target = i;
                    break;
                }
                c++;
            }
            i++;
        }
    }
    return newstr;
}

string StringUtils::collapse(const string& str, const char c) {
    vector<char> vc;
    vc.push_back(c);
    return collapse(str, vc);
}


string
StringUtils::replace( const string & str,
                      const string & origStr,
                      const string & replStr ) {
    string newstr = str;
    
    bool found = true;
    string::size_type pos = newstr.size( );
    const string::size_type olen = origStr.size( );
    
    while ( found ) {
        pos = newstr.rfind( origStr, (pos - 1) );
        
        if ( pos == string::npos )
            found = false;
        else
            newstr.replace( pos, olen, replStr );
    }
    
    return newstr;
}


void
StringUtils::tokenizeNInplace( vector< string > & tokens,
                               const string &     str,
                               const size_t       maxTokens,
                               const string &     delimiterSet,
                               const bool         skipNull )
{
    // code copied and slightly modified that found at
    // http://oopweb.com/CPP/Documents/CPPHOWTO/Volume/C++Programming-HOWTO-7.html

    if ( tokens.empty() == false )
        tokens.clear();

    if ( maxTokens < 1 )
        return;
    
    const string::size_type strSize = str.size();

    // Skip delimiters at beginning.
    string::size_type lastPos = str.find_first_not_of( delimiterSet, 0 );
    
    // Find real first delimiter.
    string::size_type pos = str.find_first_of( delimiterSet, lastPos );
    
    while ( (pos != string::npos) || (lastPos != string::npos) ) {
        // Found a token, add it to the vector.
        if ( pos == string::npos )
            tokens.push_back( str.substr( lastPos ) );
        else
            tokens.push_back( str.substr( lastPos, pos - lastPos ) );

        if ( tokens.size() >= maxTokens )
            break;

        // Find next token start position
        if ( skipNull ) {
            // Skip null tokens
            
            lastPos = str.find_first_not_of( delimiterSet, pos );
        } else {
            // Accept null tokens
            
            if ( pos == string::npos )
                lastPos = string::npos;
            else if ( (pos + 1) == strSize )
                lastPos = string::npos;
            else
                lastPos = pos + 1;
        }
        
        // Find next token end delimiter
        pos = str.find_first_of( delimiterSet, lastPos );
    }
}


vector< string >
StringUtils::tokenizeN( const string & str,
                        const size_t   maxTokens,
                        const string & delimiterSet,
                        const bool     skipNull )
{
    vector< string > tokens;
    
    tokenizeNInplace( tokens,
                      str,
                      maxTokens,
                      delimiterSet,
                      skipNull );

    return tokens;
}


void
StringUtils::tokenizeInplace( vector< string > & tokens,
                              const string &     str,
                              const string &     delimiterSet,
                              const bool         skipNull )
{
    tokenizeNInplace( tokens,
                      str,
                      (~(size_t( 0 ))),
                      delimiterSet,
                      skipNull );
}


vector< string >
StringUtils::tokenize( const string & str,
                       const string & delimiterSet,
                       const bool     skipNull )
{
    vector< string > tokens;
    
    tokenizeNInplace( tokens,
                      str,
                      (~(size_t( 0 ))),
                      delimiterSet,
                      skipNull );

    return tokens;
}


bool StringUtils::miniGlob(const char *pattern, const char *checkString) {
  const char *originalPattern = pattern;

  while (*checkString) {
    switch(*pattern) {
    case '?':
      // if pattern has ?, and it doesn't correspond to a '.' in the
      // string, continue with the search
      if (*checkString != '.') {
	checkString++;
	pattern++;
      } else {
	return false;
      }
      break;
    case '*':
      // first check to see if they, for some reason, have multiple '*'s
      while (*pattern == '*') pattern++;
      // need find the next place after '*' where pattern matches checkString
      while (*pattern != *checkString) {
	// below check is to avoid a true return for "Ant*" with "Ant1.24V"
	//	if (*checkString == '.') return false;
	// this is to avoid matches between "*.*" and "Ant1"
	if (*checkString == 0) return false;
	checkString++;
      }
      break;
    case '[':
      // if it's here, assume [x-y] format (where x,y are single digits
      pattern++; // advance 1 char past '['
      if ((*checkString < *pattern) || (*checkString > *(pattern+2))) 
	return false;
      // if it's here, then it checks out, so advance pointers
      pattern += 4;
      checkString++;
      break;
    default:
      if (*pattern != *checkString) {
	// this is necessary because if the checkString has repeating
	// characters (Ant1.AntennaControl.Drive) we need to be able
	// write a pattern (*Ante*) and have it pass ... so we need to
	// be able to go back to the last good "*" in case of
	// premature failure
	while (*pattern != '*') {
	  if(*pattern == *originalPattern) return false;
	  pattern--;
	}
      } else {
	pattern++;
	checkString++;
      }
      break;
    } // end switch
  } // end while
  
  // if it gets here, and the pattern is incomplete, then still false
  if(*pattern) return false;

  // if it's here, then all tests have passed
  return true;
}

int StringUtils::stringToInt(const string& str) {
    istringstream myStream(str);
    int i;
    if (myStream>>i) {
        return i;
    }
    else {
        string emsg = str;
        emsg += " does not represent an integer";
        throw CARMA_EXCEPTION(IllegalArgumentException, emsg);
    }
}

string  StringUtils::hms(const double angle, const int precision) { 
    return sexa(angle, false, true,  precision); 
}
string  StringUtils::dms(const double angle, const int precision) { 
    return sexa(angle, false, false, precision); 
}
string StringUtils::shms(const double angle, const int precision) { 
    return sexa(angle, true,  true,  precision); 
}
string StringUtils::sdms(const double angle, const int precision) { 
    return sexa(angle, true,  false, precision); 
}

// There is only a leading zero for the tens place in degrees (not for
// the hundreds place). This is done to conform to standard display of 
// declination. If we want both ways we need another flag.
string StringUtils::sexa(const double angleRadians, const bool sign, 
                         const bool hms, const int precizion) {
  // Normalize to hours or degrees                      
  double normalized = (hms ? angleRadians*12.0/M_PI : angleRadians*180.0/M_PI);
  bool negative = angleRadians < 0;
  normalized = abs(normalized);
  // Range check the precision
  int precision = precizion;
  if (precision > 15) precision = 15;
  if (precision <  0) precision =  0;

  // ------------------- Old code ------------------
  // Delete after 01 Dec 2008 - Steve Scott
  double a1 = normalized;
  int dd,mm, ndig;
  double ss;
  char fmt[32], number[32];  // pathetic
  dd = static_cast< int >( a1 );  // or use nearest integer below ?
  a1 = (a1-dd)*60;
  mm = static_cast< int >( a1 );  // or use nearest integer below ?
  ss = (a1-mm)*60;
  if (negative) dd = -dd;
  // @todo sign is currently ignored
  // @todo precision does not work at all
  // @todo make sure we don't print stuff like  13:59:60 instead of 14:00:00.00 
  ostringstream os;
  ndig=3;
  if (hms) ndig--;
  if (sign) sprintf(fmt,"%%+0%dd",ndig+1);
  else sprintf(fmt,"%%0%dd",ndig);
  sprintf(number,fmt,dd);
  os << number << ":";
  sprintf(number,"%02d",mm);
  os << number << ":";
  if (precision > 0) {
    sprintf(fmt,"%%0%d.%df",precision+3,precision);
    sprintf(number,fmt,ss);
  } else {
    sprintf( number, "%02d", static_cast< int >( ss ) );
  }
  os << number;
  //return os.str();
  // ---------------------- End old code ------------------------
  
  // The trick here is to get the rounding to flow up higher in the string,
  // and recognizing that the ostream insertion will round on its own.
  const int SECONDS_PER_X = 3600; // Per hour or per degree...
  // The total quantity in seconds
  double realTotalSecs = SECONDS_PER_X * normalized;
  // Must round half of final digit in seconds...
  double rounding = 0.5;
  for (int i=0; i < precision; i++) rounding *= 0.1;
  // Add on the rounding factor
  double realTotalRoundedSecs = realTotalSecs + rounding;
  int intTotalRoundedSecs = static_cast<int> (realTotalRoundedSecs);
  // We'll call the most significant part 'h', understanding that it can be
  // either hours or degrees.
  int h = intTotalRoundedSecs/SECONDS_PER_X;
  int m = (intTotalRoundedSecs - h*SECONDS_PER_X)/60;
  // Not rounded as output stream insertion operator rounds!
  double s = realTotalSecs - 60*(h*60 + m);
  // If the rounding has flowed into the minutes then secs will be negative
  if (s < 0) s = 0;
  int secWidth = precision + 3;
  if (precision == 0) secWidth = 2; // No decimal point
  string signstr = "";
  if (sign)     signstr = "+"; // + only inserted if sign is requested
  if (negative) signstr = "-"; // - inserted for all negative numbers
  ostringstream o;
  o.setf(ios::fixed);          // Required for precision to work
  o.fill('0');                 // For leading zeros
  o << signstr << setw(2) << h << ":" << setw(2) << m << ":"
    << setw(secWidth) << setprecision(precision) << s;  
  return o.str();
  
}


string
StringUtils::computeMessageDigest( const string     & str,
                                   const DigestType   digestType ) {
    OpenSSL_add_all_digests();

    string digest;
    
    switch ( digestType ) {
        case MD5:
            digest = "md5";
            break;
            
        case SHA1:
            digest = "sha1";
            break;
            
        default:
            {
                ostringstream emsg;

                emsg << "Unhandled DigestType " << digestType;

                throw CARMA_ERROR( emsg.str( ) );
            }
    }
    
    const EVP_MD * const md = EVP_get_digestbyname( digest.c_str( ) );

    if ( md == 0 ) {
        ostringstream emsg;

        emsg << "Unknown message digest " << digest;

        throw CARMA_ERROR( emsg.str( ) );
    }
    
    EVP_MD_CTX mdctx;

    EVP_MD_CTX_init( &mdctx );
    EVP_DigestInit_ex( &mdctx, md, 0 );
    
    const string::size_type maxChunkSize = 8192;
    const string::size_type strSize = str.size( );
    string::size_type chunkStart = 0;
    
    while ( chunkStart < strSize ) {
        const string::size_type bytesLeft = strSize - chunkStart;
        const string::size_type chunkBytes = min( bytesLeft, maxChunkSize );
    
        const string chunk( str, chunkStart, chunkBytes );

        EVP_DigestUpdate( &mdctx, chunk.c_str( ), chunk.size( ) );
        
        chunkStart += chunkBytes;
    }
    
    uint md_len;
    unsigned char md_value[ EVP_MAX_MD_SIZE ];
    
    EVP_DigestFinal_ex( &mdctx, md_value, &md_len );
    
    EVP_MD_CTX_cleanup( &mdctx );
    
    ostringstream messageDigest;
    
    for ( uint i = 0; i < md_len; ++i ) {
        messageDigest << hex
                      << setw( 2 )
                      << setfill( '0' )
                      << static_cast< int >( md_value[ i ] );
    }
    
    return messageDigest.str( );
}


bool
StringUtils::equalsIgnoreCase( const string & lhs,
                               const string & rhs )
{
    if ( lhs.size() != rhs.size() )
        return false;
        
    string::const_iterator i = lhs.begin();
    const string::const_iterator iEnd = lhs.end();
    
    string::const_iterator j = rhs.begin();
    
    for ( ; i != iEnd; ++i, ++j ) {
        if ( *i != *j ) {
            char ci = *i;
            char cj = *j;
            
            if ( (ci >= 'a') && (ci <= 'z') )
                ci -= 32;

            if ( (cj >= 'a') && (cj <= 'z') )
                cj -= 32;
                
            if ( ci != cj )
                return false;
        }
    }
    
    return true;
}


bool
StringUtils::containsOnly( const string & str,
                           const string & allowedChars )
{
    const string upperCasedStr = lowASCIIAlphaNumericToUpper( str );
    
    const string upperCasedAllowedChars =
        lowASCIIAlphaNumericToUpper( allowedChars );
    
    const string::size_type firstBadCharPos =
        upperCasedStr.find_first_not_of( upperCasedAllowedChars );
        
    return (firstBadCharPos == string::npos);
}

string
StringUtils::sort ( const string & str, const sortType direction )
{

        vector< char > vc( str.begin(), str.end() );
        stable_sort( vc.begin(), vc.end() );
	switch ( direction ) 
	{
	    default:
	    case StringUtils::ASCENDING_SORT:
		return string( vc.begin(), vc.end() );
		break;
	    case StringUtils::DESCENDING_SORT:
		return string( vc.rbegin(), vc.rend() );
		break;
	}

	return string();// shut the compiler up
}

/**
 * remove all <i>adjacent</i> identical chars from str
 * Example: input = abcdcefffghgi, output = abcdcefghgi
 */
string
StringUtils::uniq ( const string & str )
{
    list<char> lc( str.begin(), str.end() );
    lc.unique();
    return string( lc.begin(), lc.end() );

}

/**
 * remove <i>any</i> duplicate chars from str
 * Example: input = abcdcefffghgi, output = abcdefghi
 */
string
StringUtils::reallyUniq ( const string & str )
{
    list<char> lc( str.begin(), str.end() );
    lc.unique();
    string uStr( lc.begin(), lc.end() );
    string uStrCopy( uStr );
    string::const_iterator si = uStr.begin();
    while ( si != uStr.end() ) {
	string::size_type first = uStr.find_first_of( *si );
	string::size_type last  = uStr.find_last_of( *si );
        while ( first != last ) {
	    uStr.erase( last, 1 );
	    last  = uStr.find_last_of( *si );
	}
	++si;
    }
    return uStr;

}


bool
StringUtils::isEmptyOrContainsWhiteSpace( const string & s )
{
    if ( s.empty() || s.find(" ") != string::npos )
	return true;
    else
	return false;
}

void
StringUtils::toUpper( vector<string> & v ) 
{
    const unsigned int vsize = v.size();
    for(unsigned int i = 0; i < vsize; ++i)
    {
      v[i] = lowASCIIAlphaNumericToUpper( v[i] );
    }
}

void
StringUtils::toLower( vector<string> & v ) 
{
    const unsigned int vsize = v.size();
    for(unsigned int i = 0; i < vsize; ++i)
    {
	v[i] = lowASCIIAlphaNumericToLower( v[i] );
    }
}
