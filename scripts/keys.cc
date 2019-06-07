//
// keys: Parses C++ code comments looking for special tags of the form @xxx
//       which define the command line interface for a CARMA program.
//
// @author Peter Teuben
//
// $Id: keys.cc,v 1.73 2010/06/08 19:56:56 iws Exp $
// $CarmaCopyright$
//


#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <fstream>
#include <sstream>

#include <unistd.h>


using namespace std;


namespace {


typedef enum {
    BOOL_PARAM_TYPE,
    DOUBLE_PARAM_TYPE,
    INT_PARAM_TYPE,
    STRING_PARAM_TYPE,
} ParamType;


const char kMyVersionId[ ] =
    "$Id: keys.cc,v 1.73 2010/06/08 19:56:56 iws Exp $";


const string kVersionTag = "@version";
const string kUsageTag = "@usage";
const string kDescriptionTag = "@description";
const string kKeyTag = "@key";
const string kNoKeysTag = "@noKeys";
const string kLoggerTag = "@logger";


const string kSystemKeyNames[ ] = {
    "debug",
    "daemonize",
    "syslog",
    "logfile",
    "logname",
    "imr",
    "imrpoa",
    "nice",
    "traceLevel",
    "traceFile",
    "traceVerbose",
    "useDBMS",
    "umask",
    "useGMT"
};


const ::size_t kNumSystemKeyNames =
    (sizeof( kSystemKeyNames ) / sizeof( kSystemKeyNames[ 0 ] ));


struct CaseInsensitiveStringLess {
    bool operator()( const string & s1, const string & s2 ) const;
};


const set< string, CaseInsensitiveStringLess >
    kSystemKeyNameGroup( kSystemKeyNames,
                         kSystemKeyNames + kNumSystemKeyNames );


bool
CaseInsensitiveStringLess::operator()( const string & s1,
                                       const string & s2 ) const {
    string ls1( s1 );
    string ls2( s2 );
    
    for ( string::iterator i = ls1.begin(); i != ls1.end(); ++i )
        *i = tolower( *i );

    for ( string::iterator i = ls2.begin(); i != ls2.end(); ++i )
        *i = tolower( *i );
        
    return (ls1 < ls2);
}


typedef map< string, string > FacilityFileMapType;
typedef FacilityFileMapType::value_type MapEntry;

// If user has set up /etc/syslog.conf according to CARMA standard, facilities
// will be mapped to these output files.
const MapEntry kMapEntries[ ] = {
    MapEntry( "DEFAULT_FACILITY",        "/var/carma/log/carma.log" ),
    MapEntry( "MONITOR_FACILITY",        "/var/carma/log/monitor.log" ),
    MapEntry( "CONTROL_FACILITY",        "/var/carma/log/control.log" ),
    MapEntry( "INTERFEROMETRY_FACILITY", "/var/carma/log/interferometry.log" ),
    MapEntry( "ENVIRONMENT_FACILITY",    "/var/carma/log/environment.log" ),
    MapEntry( "RX_FACILITY",             "/var/carma/log/receivers.log" )
};

const ::size_t kNumMapEntries =
    (sizeof( kMapEntries ) / sizeof( kMapEntries[ 0 ] ));

const FacilityFileMapType
    kFacilityFileMap( kMapEntries, kMapEntries + kNumMapEntries );

const string kTestFacilityFakeName = "TEST_FACILITY";
const string kTestFacilityRealName = "DEFAULT_FACILITY";
const string kTestFacilityLognamPrefix = "carma.test.";

bool gDebug = false;

bool
initialMatch( const string & a, const string & b ) {
    return (a.substr( 0, b.size() ) == b);
}


bool
initialWordMatch( const string & a, const string & b ) {
    const string::size_type aSize = a.size();
    const string::size_type bSize = b.size();

    if ( aSize < bSize )
        return false;  // a is too short to match b at all

    if ( a.substr( 0, bSize ) != b )
        return false;  // a does not match b at all

    if ( aSize == bSize )
        return true;  // a matches b exactly

    if ( isspace( a[ bSize ] ) == false )
        return false;  // a matches b but not at a word boundary

    return true;
}


bool
getline( FILE *    inFile,
         bool &    outLineCouldBeKeysComment,
         string &  outLine ) {
    // Quasi-global variables
    static const ::size_t kRawLineBufferCount = 1024;
    static ::size_t gLineNumber = 0;
    static char gRawLineBuffer[ kRawLineBufferCount ];
    static bool gInCStyleCommentBlock = false;

    // Assume the worst
    outLineCouldBeKeysComment = false;
    outLine = "";

    // Fetch the raw line from the file
    string line;
    {
        if ( fgets( gRawLineBuffer, kRawLineBufferCount, inFile ) == 0 ) {
            if ( gDebug ) {
                cout << "End of file reached."
                     << endl;
            }

            return false;
        }

        gRawLineBuffer[ kRawLineBufferCount - 1 ] = '\0';

        ++gLineNumber;

        line = gRawLineBuffer;
    }

    // Trim trailing whitespace and control characters.
    while ( true ) {
        if ( line.empty() == true ) {
            // empty line

            if ( gDebug ) {
                cout << "Line " << gLineNumber << " "
                     << "trimmed to empty."
                     << endl;
            }

            return true;
        }

        const string::size_type lastIndex = line.size() - 1;

        if ( isspace( line[ lastIndex ] ) )
            line.erase( lastIndex, 1 );
        else if ( iscntrl( line[ lastIndex ] ) )
            line.erase( lastIndex, 1 );
        else
            break;
    }

    // Trim leading whitespace
    {
        string::const_iterator s = line.begin();
        const string::const_iterator sEnd = line.end();

        while ( (s != sEnd) && isspace( *s ) ) { ++s; }

        line = string( s, sEnd );
    }

    // Look for comment start marker and trim it if it's there
    // This state machine is NOT robust at properly finding comment blocks
    if ( initialMatch( line, "//" ) ) {
        line = line.substr( 2 );
    } else if ( initialMatch( line, "/*" ) ) {
        gInCStyleCommentBlock = true;

        line = line.substr( 2 );
    } else if ( initialMatch( line, "*/" ) ) {
        gInCStyleCommentBlock = false;

        line = line.substr( 2 );
    } else if ( gInCStyleCommentBlock && initialMatch( line, "*" ) ) {
        line = line.substr( 1 );
    } else {
        // We don't appear to have a comment start marker
        if ( gDebug ) {
            cout << "Line " << gLineNumber << " "
                 << "does not look like a keys comment."
                 << endl;
        }

        return true;
    }

    // Trim whitespace after comment start marker
    {
        string::const_iterator s = line.begin();
        const string::const_iterator sEnd = line.end();

        while ( (s != sEnd) && isspace( *s ) ) { ++s; }

        line = string( s, sEnd );
    }

    if ( gDebug ) {
        cout << "Line " << gLineNumber << " "
             << "possible keys comment "
             << "\"" << line << "\"."
             << endl;
    }

    outLineCouldBeKeysComment = true;
    outLine = line;

    return true;
}


void
show_version( ostream & os ) {
    os << "// \n";
    os << "// " << kVersionTag << "  <some version ID, probably CVS based>\n";
    os << "// \n";
}


void
show_usage( ostream & os ) {
    os << "// \n";
    os << "// " << kUsageTag << "  -a-one-line-usage-for-this-program\n";
    os << "// \n";
}


void
show_desc( ostream & os ) {
    os << "// \n";
    os << "// " << kDescriptionTag << "\n";
    os << "//   a multi line descriptions of your program\n";
    os << "//   can go here\n";
    os << "// \n";
}


void
show_key( ostream & os ) {
    os << "// \n";
    os << "// " << kKeyTag << " key1 def1   type1   one line help for key1\n";
    os << "// " << kKeyTag << " key2 def2   type2   one line help for key2\n";
    os << "// " << kKeyTag << " key3 def3   type3   one line help for key3\n";
    os << "// \n";
    os << "//     ** allowed type's: \n";
    os << "//        \"string\", \"s\"\n";
    os << "//        \"int\", \"integer\", \"i\"\n";
    os << "//        \"double\", \"d\",\n";
    os << "//        \"bool\", \"b\"\n";
}


void
show_example( ostream & os ) {
    show_version( os );
    show_usage( os );
    show_desc( os );
    show_key( os );
}


class FileProcessor {
    public:

        FileProcessor( int  warningVerbosity,
                       int  infoVerbosity,
                       bool requireLoggerTag,
                       bool requireKeysOrNoKeys );

        void processInputFile( FILE *         inFile,
                               const string & inputFileDisplayName );

        void validate( const string & inputFileDisplayName ) const;

        void emitOutputFile( const string & inputFileDisplayName,
                             ostream &      os ) const;

    private:

        void processVersionTagLine( const string & lineAfterVersionTag,
                                    const string & line,
                                    const string & inputFileDisplayName );

        void processVersionLine( const string & line,
                                 const string & inputFileDisplayName );

        void processVersionExtraLine( const string & line,
                                      const string & inputFileDisplayName );

        void processUsageTagLine( const string & lineAfterUsageTag,
                                  const string & line,
                                  const string & inputFileDisplayName );

        void processUsageLine( const string & line,
                               const string & inputFileDisplayName );

        void processUsageExtraLine( const string & line,
                                    const string & inputFileDisplayName );

        void processDescriptionTagLine( const string & lineAfterVersionTag,
                                        const string & line,
                                        const string & inputFileDisplayName );

        void processDescriptionLine( const string & line,
                                     const string & inputFileDisplayName );

        void processKeyTagLine( const string & lineAfterKeyTag,
                                const string & line,
                                const string & inputFileDisplayName );

        void processKeyHelpLine( const string & line,
                                 const string & inputFileDisplayName );

        void processKeyHelpExtraLine( const string & line,
                                      const string & inputFileDisplayName );

        void processNoKeysTagLine( const string & lineAfterNoKeysTag,
                                   const string & line,
                                   const string & inputFileDisplayName );

        void processLoggerTagLine( const string & lineAfterLoggerTag,
                                   const string & line,
                                   const string & inputFileDisplayName );

        void validateKeywords( const string & inputFileDisplayName ) const;

        void validateLoggerInfo( const string & inputFileDisplayName ) const;

        void emitVersion( const string & inputFileDisplayName,
                          ostream &      os ) const;

        void emitUsage( const string & inputFileDisplayName,
                        ostream &      os ) const;

        void emitDescription( const string & inputFileDisplayName,
                              ostream &      os ) const;

        void emitKeywords( const string & inputFileDisplayName,
                           ostream &      os ) const;

        void emitLoggerInfo( const string & inputFileDisplayName,
                             ostream &      os ) const;

        bool checkTagNearMiss( const string & inputFileDisplayName,
                               const string & line,
                               const string & tag ) const;

        void checkKeyName( const string & inputFileDisplayName,
                           const string & line,
                           const string & name ) const;

        void checkKeyDefaultValue( const string &  inputFileDisplayName,
                                   const string &  line,
                                   const string &  defaultValue,
                                   const bool      defaultValueWasQuoted,
                                   const ParamType paramType ) const;
                                   
        ParamType convertParameterType( const string & inputFileDisplayName,
                                        const string & line,
                                        const string & typeName ) const;
                                        
        string errorPrefix( const string & inputFileDisplayName ) const;

        string warningPrefix( const string & inputFileDisplayName ) const;

        string infoPrefix( const string & inputFileDisplayName ) const;

        typedef struct {
            string name;
            string defaultValue;
            string typeName;
            string usageValue;
            string help;
        } KeyData;

        typedef enum {
            NONE_PARSE_STATE,

            VERSION_PARSE_STATE,
            VERSION_EXTRA_PARSE_STATE,

            USAGE_PARSE_STATE,
            USAGE_EXTRA_PARSE_STATE,

            DESCRIPTION_PARSE_STATE,

            KEY_HELP_PARSE_STATE,
            KEY_HELP_EXTRA_PARSE_STATE
        } ParseState;


        const int  warningVerbosity_;
        const int  infoVerbosity_;
        const bool requireLoggerTag_;
        const bool requireKeysOrNoKeys_;
        
        ParseState parseState_;

        bool haveVersion_;
        string versionString_;

        bool haveUsage_;
        string usageString_;

        bool haveDescription_;
        vector< string > descriptionLines_;

        bool noKeysTagSeen_;
        set< string > keyNamesSeen_;
        vector< KeyData > keyDatas_;

        bool haveLoggerInfo_;
        string facilityNameString_;
        string lognameString_;
};


FileProcessor::FileProcessor( const int  warningVerbosity,
                              const int  infoVerbosity,
                              const bool requireLoggerTag,
                              const bool requireKeysOrNoKeys ) :
warningVerbosity_( warningVerbosity ),
infoVerbosity_( infoVerbosity ),
requireLoggerTag_( requireLoggerTag ),
requireKeysOrNoKeys_( requireKeysOrNoKeys ),

parseState_( NONE_PARSE_STATE ),

haveVersion_( false ),
versionString_(),

haveUsage_( false ),
usageString_(),

haveDescription_( false ),
descriptionLines_(),

noKeysTagSeen_( false ),
keyNamesSeen_(),
keyDatas_(),

haveLoggerInfo_( false ),
facilityNameString_(),
lognameString_()
{
}


void
FileProcessor::processVersionTagLine( const string & lineAfterVersionTag,
                                      const string & line,
                                      const string & inputFileDisplayName ) {
    if ( haveVersion_ ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Multiple " << kVersionTag << " tags found."
             << endl;

        exit( 1 );
    }

    haveVersion_ = true;

    string::const_iterator s = lineAfterVersionTag.begin();
    const string::const_iterator sEnd = lineAfterVersionTag.end();

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    versionString_ = string( s, sEnd );

    parseState_ = VERSION_PARSE_STATE;
}


void
FileProcessor::processVersionLine( const string & line,
                                   const string & inputFileDisplayName ) {
    if ( (parseState_ != VERSION_PARSE_STATE) ||
         (haveVersion_ == false) ) {
        cerr << "Internal logic error."
             << endl;

        exit( 1 );
    }

    if ( line.empty() )
        parseState_ = VERSION_EXTRA_PARSE_STATE;
    else {
        if ( infoVerbosity_ >= 2 ) {
            cout << infoPrefix( inputFileDisplayName )
                 << "Appending version line. "
                 << "line: \"" << line << "\"."
                 << endl;
        }

        if ( versionString_.empty() == false )
            versionString_ += " ";

        versionString_ += line;
    }
}


void
FileProcessor::processVersionExtraLine( const string & line,
                                        const string & inputFileDisplayName ) {
    if ( (parseState_ != VERSION_EXTRA_PARSE_STATE) ||
         (haveVersion_ == false) ) {
        cerr << "Internal logic error."
             << endl;

        exit( 1 );
    }

    if ( line.empty() == false ) {
        if ( infoVerbosity_ >= 2 ) {
            cout << infoPrefix( inputFileDisplayName )
                 << "Possible extra " << kVersionTag << " text. "
                 << "line: " << line << "."
                 << endl;
        }

        parseState_ = NONE_PARSE_STATE;
    }
}


void
FileProcessor::processUsageTagLine( const string & lineAfterUsageTag,
                                    const string & line,
                                    const string & inputFileDisplayName ) {
    if ( haveUsage_ ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Multiple " << kUsageTag << " tags found."
             << endl;

        exit( 1 );
    }

    haveUsage_ = true;

    string::const_iterator s = lineAfterUsageTag.begin();
    const string::const_iterator sEnd = lineAfterUsageTag.end();

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    usageString_ = string( s, sEnd );

    parseState_ = USAGE_PARSE_STATE;
}


void
FileProcessor::processUsageLine( const string & line,
                                 const string & inputFileDisplayName ) {
    if ( (parseState_ != USAGE_PARSE_STATE) ||
         (haveUsage_ == false) ) {
        cerr << "Internal logic error."
             << endl;

        exit( 1 );
    }

    if ( line.empty() )
        parseState_ = USAGE_EXTRA_PARSE_STATE;
    else {
        if ( infoVerbosity_ >= 2 ) {
            cout << infoPrefix( inputFileDisplayName )
                 << "Appending usage line. "
                 << "line: \"" << line << "\"."
                 << endl;
        }

        if ( usageString_.empty() == false )
            usageString_ += " ";

        usageString_ += line;
    }
}


void
FileProcessor::processUsageExtraLine( const string & line,
                                      const string & inputFileDisplayName ) {
    if ( (parseState_ != USAGE_EXTRA_PARSE_STATE) ||
         (haveUsage_ == false) ) {
        cerr << "Internal logic error."
             << endl;

        exit( 1 );
    }

    if ( line.empty() )
        parseState_ = NONE_PARSE_STATE;
    else {
        if ( infoVerbosity_ >= 2 ) {
            cout << infoPrefix( inputFileDisplayName )
                 << "Possible extra " << kUsageTag << " text. "
                 << "line: " << line << "."
                 << endl;
        }

        parseState_ = NONE_PARSE_STATE;
    }
}


void
FileProcessor::processDescriptionTagLine( const string & lineAfterDescriptionTag,
                                          const string & line,
                                          const string & inputFileDisplayName ) {
    if ( haveDescription_ ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Multiple " << kDescriptionTag << " tags found."
             << endl;

        exit( 1 );
    }

    haveDescription_ = true;

    string::const_iterator s = lineAfterDescriptionTag.begin();
    const string::const_iterator sEnd = lineAfterDescriptionTag.end();

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    if ( s != sEnd )
        descriptionLines_.push_back( string( s, sEnd ) );

    parseState_ = DESCRIPTION_PARSE_STATE;
}


void
FileProcessor::processDescriptionLine( const string & line,
                                       const string & inputFileDisplayName ) {
    if ( (parseState_ != DESCRIPTION_PARSE_STATE) ||
         (haveDescription_ == false) ) {
        cerr << "Internal logic error."
             << endl;

        exit( 1 );
    }

    if ( line.empty() )
        descriptionLines_.push_back( "" );
    else {
        if ( (infoVerbosity_ >= 2) &&
             (descriptionLines_.empty() == false) &&
             (descriptionLines_.back().empty() == true) ) {
            cout << infoPrefix( inputFileDisplayName )
                 << "Appending description paragraph. "
                 << "line: \"" << line << "\"."
                 << endl;
        }

        descriptionLines_.push_back( line );
    }
}


void
FileProcessor::checkKeyName( const string & inputFileDisplayName,
                             const string & line,
                             const string & name ) const {
    for ( string::size_type i = 0; i < name.size(); ++i ) {
        const char c = name[ i ];

        if ( c != '\\' && c != '"' )
            continue;

        cerr << errorPrefix( inputFileDisplayName )
             << "Invalid key name character '" << c << "'. "
             << "line: " << line << "."
             << endl;

        exit( 1 );
    }
}


void
FileProcessor::checkKeyDefaultValue( const string &  inputFileDisplayName,
                                     const string &  line,
                                     const string &  defaultValue,
                                     const bool      defaultValueWasQuoted,
                                     const ParamType paramType ) const {
    if ( defaultValue == "???" ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "keyword default value \"???\" is no longer the magic value "
             << "for mandatory parameters. Use @mandatory for the keyword "
             << "default value instead. "
             << "line: " << line << "."
             << endl;

        exit( 1 );
    }

    bool defaultValueIsMagic = false;

    if ( (defaultValue.empty() == false) && (defaultValue[ 0 ] == '@')) {
        const string afterAtSign = defaultValue.substr( 1 );

        if ( (afterAtSign != "mandatory") &&
             (afterAtSign != "noDefault") ) {
            cerr << errorPrefix( inputFileDisplayName )
                 << "Unknown magic keyword default value "
                 << "\"" << defaultValue << "\". "
                 << "line: " << line << "."
                 << endl;

            exit( 1 );
        }

        if ( defaultValueWasQuoted ) {
            cerr << errorPrefix( inputFileDisplayName )
                 << "Do not quote magic keyword default value "
                 << "\"" << defaultValue << "\". "
                 << "line: " << line << "."
                 << endl;

            exit( 1 );
        }

        defaultValueIsMagic = true;
    }

    if ( defaultValueWasQuoted && (paramType != STRING_PARAM_TYPE) ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Non-string type keyword default value was quoted. "
             << "line: " << line << "."
             << endl;

        exit( 1 );
    }

    if ( defaultValueIsMagic == false ) {
        if ( paramType == BOOL_PARAM_TYPE ) {
            if ( (defaultValue != "t") &&
                 (defaultValue != "true") &&
                 (defaultValue != "f") &&
                 (defaultValue != "false") ) {
                cerr << errorPrefix( inputFileDisplayName )
                     << "Invalid bool type keyword default value "
                     << "\"" << defaultValue << "\". "
                     << "Please use an unquoted keyword default value of "
                     << "\"true\" or \"false\". "
                     << "line: " << line << "."
                     << endl;
    
                exit( 1 );
            }
        } else if ( paramType == DOUBLE_PARAM_TYPE ) {
            const char * const pvsBegin = defaultValue.c_str();
            const char * const pvsEnd = pvsBegin + defaultValue.size();
            char * endPtr = 0;
        
            errno = 0;
            strtod( pvsBegin, &endPtr );
            const int errnoVal = errno;
            
            if ( (errnoVal != 0) || (endPtr != pvsEnd) ) {
                cerr << errorPrefix( inputFileDisplayName )
                     << "Invalid double type keyword default value "
                     << "\"" << defaultValue << "\". "
                     << line << "."
                     << endl;
    
                exit( 1 );
            }
        } else if ( paramType == INT_PARAM_TYPE ) {
            const char * const pvsBegin = defaultValue.c_str();
            const char * const pvsEnd = pvsBegin + defaultValue.size();
        
            char * endPtr = 0;
            long numericalValue = 0;
            int errnoVal = -1;
            
            if ( (pvsBegin != pvsEnd) &&
                 (*pvsBegin == '0') &&
                 ((pvsBegin + 1) != pvsEnd) &&
                 ((*(pvsBegin + 1) == 'x') || (*(pvsBegin + 1) == 'X')) ) {
                // hex notation
                errno = 0;
                numericalValue = strtol( pvsBegin, &endPtr, 16 );
                errnoVal = errno;
            } else {
                // decimal notation
                errno = 0;
                numericalValue = strtol( pvsBegin, &endPtr, 10 );
                errnoVal = errno;
            }
            
            if ( (errnoVal != 0) || (endPtr != pvsEnd) ) {
                cerr << errorPrefix( inputFileDisplayName )
                     << "Invalid integer type keyword default value "
                     << "\"" << defaultValue << "\". "
                     << line << "."
                     << endl;
    
                exit( 1 );
            }
        }
    }

    for ( string::size_type i = 0; i < defaultValue.size(); ++i ) {
        const char c = defaultValue[ i ];

        if ( c != '\\' && c != '"' )
            continue;

        cerr << errorPrefix( inputFileDisplayName )
             << "Invalid keyword value character "
             << "'" << c << "'. "
             << "line: " << line << "."
             << endl;

        exit( 1 );
    }
}


ParamType
FileProcessor::convertParameterType( const string & inputFileDisplayName,
                                     const string & line,
                                     const string & typeName ) const {
    // first, look for our "preferred" type names

    if ( typeName == "bool" )
        return BOOL_PARAM_TYPE;

    if ( typeName == "double" )
        return DOUBLE_PARAM_TYPE;

    if ( (typeName == "int") || (typeName == "integer") )
        return INT_PARAM_TYPE;

    if ( typeName == "string" )
        return STRING_PARAM_TYPE;


    // second, look for our "terse" type names

    if ( typeName == "b" )
        return BOOL_PARAM_TYPE;

    if ( typeName == "d" )
        return DOUBLE_PARAM_TYPE;

    if ( typeName == "i" )
        return INT_PARAM_TYPE;

    if ( typeName == "s" )
        return STRING_PARAM_TYPE;

    // Houston, we have a problem

    cerr << errorPrefix( inputFileDisplayName )
         << "Invalid keyword type name "
         << "\"" << typeName << "\". "
         << "line: " << line << "."
         << endl;

    exit( 1 );
}


void
FileProcessor::processKeyTagLine( const string & lineAfterKeyTag,
                                  const string & line,
                                  const string & inputFileDisplayName ) {
    if ( noKeysTagSeen_ ) {
        cerr << errorPrefix( inputFileDisplayName )
             << kKeyTag << " tag found after " << kNoKeysTag << " tag."
             << endl;

        exit( 1 );
    }

    string::const_iterator s = lineAfterKeyTag.begin();
    const string::const_iterator sEnd = lineAfterKeyTag.end();

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    // parse the key name
    string name;
    {
        if ( s == sEnd ) {
            cerr << errorPrefix( inputFileDisplayName )
                 << "Line ended before key name. "
                 << "line: " << line << "."
                 << endl;

            exit( 1 );
        }

        const string::const_iterator nameBegin = s;  // set start

        // scan to the end of word
        while ( (s != sEnd) && (isspace( *s ) == false) ) { ++s; }

        name = string( nameBegin, s );

        // check that the key name is valid
        checkKeyName( inputFileDisplayName, line, name );

        // check for conflicts with system keywords
        {
            const set< string, CaseInsensitiveStringLess >::const_iterator
                iSystemKeyName = kSystemKeyNameGroup.find( name );
                
            if ( iSystemKeyName != kSystemKeyNameGroup.end() ) {
                cerr << errorPrefix( inputFileDisplayName )
                     << "Program key \"" << name << "\" conflicts with "
                     << "system key \"" << *iSystemKeyName << "\". "
                     << "line: " << line << "."
                     << endl;

                exit( 1 );
            }
        }

        // check for duplicates
        if ( keyNamesSeen_.find( name ) != keyNamesSeen_.end() ) {
            cerr << errorPrefix( inputFileDisplayName )
                 << "Multiple keys with name \"" << name << "\". "
                 << "line: " << line << "."
                 << endl;

            exit( 1 );
        }

        keyNamesSeen_.insert( name );
    }

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    // parse the default value
    bool defaultValueWasQuoted = false;
    string defaultValue;
    {
        if ( s == sEnd ) {
            cerr << errorPrefix( inputFileDisplayName )
                 << "Line ended before key default value. "
                 << "line: " << line << "."
                 << endl;

            exit( 1 );
        }

        if ( *s == '"' ) {
            // quoted value

            defaultValueWasQuoted = true;

            ++s;  // skip over the opening quote

            const string::const_iterator valBegin = s;

            // scan to closing quote
            while ( (s != sEnd) && ((*s) != '"') ) { ++s; }

            if ( s == sEnd ) {
                cerr << errorPrefix( inputFileDisplayName )
                     << "Line ended before default value closing quote. "
                     << "line: " << line << "."
                     << endl;

                exit( 1 );
            }

            defaultValue = string( valBegin, s );

            ++s;  // skip over the closing quote

            if ( (s == sEnd) || !isspace( *s ) ) {
                cerr << errorPrefix( inputFileDisplayName )
                     << "No whitespace after quoted keyword value. "
                     << "line: " << line << "."
                     << endl;

                exit( 1 );
            }
        } else {
            // single word value

            defaultValueWasQuoted = false;

            const string::const_iterator valBegin = s;

            // scan to the end of word
            while ( (s != sEnd) && (isspace( *s ) == false) ) { ++s; }

            defaultValue = string( valBegin, s );
        }
    }

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    // parse the type
    string typeName;
    ParamType paramType;
    {
        if ( s == sEnd ) {
            cerr << errorPrefix( inputFileDisplayName )
                 << "Line ended before key type. "
                 << "line: " << line << "."
                 << endl;

            exit( 1 );
        }

        const string::const_iterator typeNameBegin = s;

        // scan to the end of word
        while ( (s != sEnd) && (isspace( *s ) == false) ) { ++s; }

        typeName = string( typeNameBegin, s );

        // check that the keyword type is valid and convert to the enum value
        paramType = convertParameterType( inputFileDisplayName,
                                          line,
                                          typeName );
    }


    // check that the keyword value is valid
    checkKeyDefaultValue( inputFileDisplayName,
                          line,
                          defaultValue,
                          defaultValueWasQuoted,
                          paramType );

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    // parse "optional" usage value
    string usageValue = "@autogen";

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    // parse any remaining text as the start of the help
    const string help( s, sEnd );

    KeyData keyData;

    keyData.name = name;
    keyData.defaultValue = defaultValue;
    keyData.typeName = typeName;
    keyData.usageValue = usageValue;
    keyData.help = help;

    keyDatas_.push_back( keyData );

    parseState_ = KEY_HELP_PARSE_STATE;
}

/*
 * This is static and only has one call site. With these conditions met
 * gcc will always inline the function.
 *
 * This is a fairly smart string escaper. It replaces any unescaped double
 * quotes and escapes them. This is used so that help text can contain
 * unescaped quotes and the keys program will still output valid C++.
 */
std::string escape_quotes(const std::string &src)
{
    std::string s(src);
    size_t pos = 0;

    while (true) {
        /* find the next double quote */
        pos = s.find_first_of('\"', pos);

        /* no more quotes, we're done */
        if (pos == std::string::npos)
            break;

        /* we have an un-escaped quote */
        if (pos > 1 && s.at(pos - 1) != '\\') {
            s.insert(pos, 1, '\\');
            pos += 1;
        }

        /* quote at the beginning of the string */
        if (pos == 0) {
            s.insert(pos, 1, '\\');
            pos += 1;
        }

        pos += 1;
    }

    return s;
}


void
FileProcessor::processKeyHelpLine( const string & line,
                                   const string & inputFileDisplayName ) {
    if ( (parseState_ != KEY_HELP_PARSE_STATE) || keyDatas_.empty() ) {
        cerr << "Internal logic error."
             << endl;

        exit( 1 );
    }

    if ( line.empty() )
        parseState_ = KEY_HELP_EXTRA_PARSE_STATE;
    else {
        if ( infoVerbosity_ >= 2 ) {
            cout << infoPrefix( inputFileDisplayName )
                 << "Appending key help line. "
                 << "line: \"" << line << "\"."
                 << endl;
        }

        KeyData & back = keyDatas_.back();

        if ( back.help.empty() == false )
            back.help += " ";

        back.help += escape_quotes(line);
    }
}


void
FileProcessor::processKeyHelpExtraLine( const string & line,
                                        const string & inputFileDisplayName ) {
    if ( (parseState_ != KEY_HELP_EXTRA_PARSE_STATE) || keyDatas_.empty() ) {
        cerr << "Internal logic error."
             << endl;

        exit( 1 );
    }

    if ( line.empty() == false ) {
        if ( infoVerbosity_ >= 2 ) {
            cout << infoPrefix( inputFileDisplayName )
                 << "Possible extra " << kKeyTag << " help text. "
                 << "line: " << line << "."
                 << endl;
        }

        parseState_ = NONE_PARSE_STATE;
    }
}


void
FileProcessor::processNoKeysTagLine( const string & lineAfterNoKeysTag,
                                     const string & line,
                                     const string & inputFileDisplayName ) {
    if ( noKeysTagSeen_ ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Multiple " << kNoKeysTag << " tags found."
             << endl;

        exit( 1 );
    }
    
    if ( keyNamesSeen_.empty() == false ) {
        cerr << errorPrefix( inputFileDisplayName )
             << kNoKeysTag << " tag found after " << kKeyTag << " tag(s)."
             << endl;

        exit( 1 );
    }

    noKeysTagSeen_ = true;

    string::const_iterator s = lineAfterNoKeysTag.begin();
    const string::const_iterator sEnd = lineAfterNoKeysTag.end();

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    if ( s != sEnd ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Extra text found after " << kNoKeysTag << " tag."
             << endl;

        exit( 1 );
    }

    parseState_ = NONE_PARSE_STATE;
}


void
FileProcessor::processLoggerTagLine( const string & lineAfterLoggerTag,
                                     const string & line,
                                     const string & inputFileDisplayName ) {
    if ( haveLoggerInfo_ ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Multiple " << kLoggerTag << " tags found."
             << endl;

        exit( 1 );
    }

    haveLoggerInfo_ = true;

    string::const_iterator s = lineAfterLoggerTag.begin();
    const string::const_iterator sEnd = lineAfterLoggerTag.end();

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    // parse the facility name
    string facilityName;
    bool facilityNameWasTestFakeName = false;
    {
        if ( s == sEnd ) {
            cerr << errorPrefix( inputFileDisplayName )
                 << "Line ended before facility. "
                 << "line: " << line << "."
                 << endl;

            exit( 1 );
        }

        const string::const_iterator facilityNameBegin = s;  // set start

        // scan to the end of word
        while ( (s != sEnd) && (isspace( *s ) == false) ) { ++s; }

        facilityName = string( facilityNameBegin, s );

        if ( facilityName == kTestFacilityFakeName ) {
            facilityName = kTestFacilityRealName;
            facilityNameWasTestFakeName = true;
        }
    }

    // check validity of facility name
    if ( kFacilityFileMap.find( facilityName ) == kFacilityFileMap.end() ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Invalid facility \"" << facilityName << "\". "
             << "line: " << line << "."
             << endl;

        exit( 1 );
    }

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }

    // parse the logname
    string logname;
    {
        if ( s == sEnd ) {
            cerr << errorPrefix( inputFileDisplayName )
                 << "Line ended before logname. "
                 << "line: " << line << "."
                 << endl;

            exit( 1 );
        }

        const string::const_iterator lognameBegin = s;  // set start

        // scan to the end of word
        while ( (s != sEnd) && (isspace( *s ) == false) ) { ++s; }

        logname = string( lognameBegin, s );
        
        if ( facilityNameWasTestFakeName ) {
            if ( initialMatch( logname, kTestFacilityLognamPrefix ) == false ) {
                cerr << errorPrefix( inputFileDisplayName )
                     << "Facility was " << kTestFacilityFakeName
                     << " but logname did not start with \""
                     << kTestFacilityLognamPrefix << "\". "
                     << "line: " << line << "."
                     << endl;
    
                exit( 1 );
            }
        }
    }

    // skip whitespace
    while ( (s != sEnd) && isspace( *s ) ) { ++s; }
    if ( s != sEnd ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Extra text found after logname."
             << endl;

        exit( 1 );
    }

    facilityNameString_ = facilityName;
    lognameString_ = logname;

    parseState_ = NONE_PARSE_STATE;
}


bool
isNearMiss( const string & a,
            const string & b ) {
    const string::size_type aSize = a.size();
    const string::size_type bSize = b.size();

    if ( (aSize == 0) || (bSize == 0) )
        return false;

    string::size_type i = 0;

    while ( true ) {
        if ( (i == aSize) || (i == bSize) )
            return true;

        if ( tolower( a[ i ] ) != tolower( b[ i ] ) )
            return false;

        ++i;
    }
}


string
FileProcessor::errorPrefix( const string & inputFileDisplayName ) const {
    return inputFileDisplayName + ": keys error: ";
}


string
FileProcessor::warningPrefix( const string & inputFileDisplayName ) const {
    return inputFileDisplayName + ": keys warning: ";
}


string
FileProcessor::infoPrefix( const string & inputFileDisplayName ) const {
    return inputFileDisplayName + ": keys info: ";
}


bool
FileProcessor::checkTagNearMiss( const string & inputFileDisplayName,
                                 const string & line,
                                 const string & tag ) const {
    const bool result = isNearMiss( line, tag );

    if ( result && (warningVerbosity_ >= 1) ) {
        cout << warningPrefix( inputFileDisplayName )
             << "Possible typo intended to be " << tag << ". "
             << "line: " << line << "."
             << endl;
    }

    return result;
}


void
FileProcessor::processInputFile( FILE *         inFile,
                                 const string & inputFileDisplayName ) {
    parseState_ = NONE_PARSE_STATE;

    bool done = false;

    while ( done == false ) {
        bool lineCouldBeKeysComment = false;
        string line;

        if ( getline( inFile, lineCouldBeKeysComment, line ) == false ) {
            done = true;

            parseState_ = NONE_PARSE_STATE;
        } else if ( lineCouldBeKeysComment == false ) {
            parseState_ = NONE_PARSE_STATE;
        } else if ( initialWordMatch( line, kVersionTag ) ) {
            processVersionTagLine( line.substr( kVersionTag.size() ),
                                   line,
                                   inputFileDisplayName );
        } else if ( initialWordMatch( line, kUsageTag ) ) {
            processUsageTagLine( line.substr( kUsageTag.size() ),
                                 line,
                                 inputFileDisplayName );
        } else if ( initialWordMatch( line, kDescriptionTag ) ) {
            processDescriptionTagLine( line.substr( kDescriptionTag.size() ),
                                       line,
                                       inputFileDisplayName );
        } else if ( initialWordMatch( line, kKeyTag ) ) {
            processKeyTagLine( line.substr( kKeyTag.size() ),
                               line,
                               inputFileDisplayName );
        } else if ( initialWordMatch( line, kNoKeysTag ) ) {
            processNoKeysTagLine( line.substr( kNoKeysTag.size() ),
                                  line,
                                  inputFileDisplayName );
        } else if ( initialWordMatch( line, kLoggerTag ) ) {
            processLoggerTagLine( line.substr( kLoggerTag.size() ),
                                  line,
                                  inputFileDisplayName );
        } else if ( initialMatch( line, "@" ) ) {
            if ( (line.size() <= 1) || (isspace( line[ 1 ] )) ) {
                if ( warningVerbosity_ >= 1 ) {
                    cout << warningPrefix( inputFileDisplayName )
                         << "line starts with a lonely @ character. "
                         << "line: " << line << "."
                         << endl;
                }
            } else {
                checkTagNearMiss( inputFileDisplayName, line, kVersionTag );
                checkTagNearMiss( inputFileDisplayName, line, kUsageTag );
                checkTagNearMiss( inputFileDisplayName, line, kDescriptionTag );
                checkTagNearMiss( inputFileDisplayName, line, kKeyTag );
                checkTagNearMiss( inputFileDisplayName, line, kNoKeysTag );
                checkTagNearMiss( inputFileDisplayName, line, kLoggerTag );

                if ( infoVerbosity_ >= 2 ) {
                    cout << infoPrefix( inputFileDisplayName )
                         << "Ignoring unknown tag. "
                         << "line: " << line << "."
                         << endl;
                }
            }

            parseState_ = NONE_PARSE_STATE;
        } else if ( parseState_ == VERSION_PARSE_STATE ) {
            processVersionLine( line, inputFileDisplayName );
        } else if ( parseState_ == VERSION_EXTRA_PARSE_STATE ) {
            processVersionExtraLine( line, inputFileDisplayName );
        } else if ( parseState_ == USAGE_PARSE_STATE ) {
            processUsageLine( line, inputFileDisplayName );
        } else if ( parseState_ == USAGE_EXTRA_PARSE_STATE ) {
            processUsageExtraLine( line, inputFileDisplayName );
        } else if ( parseState_ == DESCRIPTION_PARSE_STATE ) {
            processDescriptionLine( line, inputFileDisplayName );
        } else if ( parseState_ == KEY_HELP_PARSE_STATE ) {
            processKeyHelpLine( line, inputFileDisplayName );
        } else if ( parseState_ == KEY_HELP_EXTRA_PARSE_STATE ) {
            processKeyHelpExtraLine( line, inputFileDisplayName );
        } else if ( parseState_ != NONE_PARSE_STATE ) {
            cerr << "Internal logic error."
                 << endl;

            exit( 1 );
        }
    }
}


void
FileProcessor::validateKeywords( const string & inputFileDisplayName ) const
{
    if ( keyNamesSeen_.empty() != keyDatas_.empty() ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Internal logic error. "
             << "keyNamesSeen_ empty state mismatch with keyDatas_."
             << endl;

        exit( 1 );
    }
    
    if ( keyNamesSeen_.empty() && (noKeysTagSeen_ == false) ) {
        if ( requireKeysOrNoKeys_ ) {
            cerr << errorPrefix( inputFileDisplayName )
                 << "Never found any " << kKeyTag << " tags nor a "
                 << kNoKeysTag << " tag."
                 << endl;
                 
            exit( 1 );
        } else if ( warningVerbosity_ >= 1 ) {
            cout << warningPrefix( inputFileDisplayName )
                 << "Never found any " << kKeyTag << " tags nor a "
                 << kNoKeysTag << " tag."
                 << endl;
        }
    }

    // Validate the helps that we built up using line appending
    {
        vector< KeyData >::const_iterator i = keyDatas_.begin();
        const vector< KeyData >::const_iterator iEnd = keyDatas_.end();

        while ( i != iEnd ) {
            if ( i->help.empty() ) {
                cerr << errorPrefix( inputFileDisplayName )
                     << "Program key "
                     << "\"" << i->name << "\" "
                     << "has no help string."
                     << endl;

                exit( 1 );
            }

            if ( i->help[ 0 ] == '@' ) {
                cerr << errorPrefix( inputFileDisplayName )
                     << "Program key "
                     << "\"" << i->name << "\" "
                     << "help string started with an '@'. ";

                exit( 1 );
            }

            if ( i->help[ 0 ] == '"' ) {
                cerr << errorPrefix( inputFileDisplayName )
                     << "Program key "
                     << "\"" << i->name << "\" "
                     << "help string started with an '\"'. ";

                exit( 1 );
            }

            ++i;
        }
    }
}


void
FileProcessor::validateLoggerInfo( const string & inputFileDisplayName ) const
{
    if ( haveLoggerInfo_ ) {
        if ( facilityNameString_.empty() ) {
            if ( warningVerbosity_ >= 1 ) {
                cout << warningPrefix( inputFileDisplayName )
                     << kLoggerTag << " tag should have a facility value."
                     << endl;
            }
        }

        if ( lognameString_.empty() ) {
            if ( warningVerbosity_ >= 1 ) {
                cout << warningPrefix( inputFileDisplayName )
                     << kLoggerTag << " tag should have a name value."
                     << endl;
            }
        }
    } else if ( requireLoggerTag_ ) {
        cerr << errorPrefix( inputFileDisplayName )
             << "Missing " << kLoggerTag << " tag."
             << endl;

        exit( 1 );
    } else if ( warningVerbosity_ >= 1 ) {
        cout << warningPrefix( inputFileDisplayName )
             << "Missing " << kLoggerTag << " tag."
             << endl;
    }
}


void
FileProcessor::validate( const string & inputFileDisplayName ) const
{
    validateKeywords( inputFileDisplayName );
    validateLoggerInfo( inputFileDisplayName );
}


void
FileProcessor::emitVersion( const string & inputFileDisplayName,
                            ostream &      os ) const
{
    os << "\n";
    os << "const char * const carma::util::ProgramBase::kVersion_ =\n";

    if ( haveVersion_ ) {
        if ( (warningVerbosity_ >= 1) && versionString_.empty() ) {
            cout << warningPrefix( inputFileDisplayName )
                 << kVersionTag << " tag should have a value."
                 << endl;
        }

        os << "  \"" << versionString_ << "\";\n";
    } else {
        if ( warningVerbosity_ >= 2 ) {
            cout << warningPrefix( inputFileDisplayName )
                 << "Missing " << kVersionTag << " tag; "
                 << "use the -e flag to see an example."
                 << endl;
        }

        os << "  \"No version ID available\";\n";
    }

    os << "\n";
}


void
FileProcessor::emitUsage( const string & inputFileDisplayName,
                          ostream &      os ) const
{
    os << "\n";
    os << "const char * const carma::util::ProgramBase::kUsage_ =\n";

    if ( haveUsage_ ) {
        if ( (warningVerbosity_ >= 1) && usageString_.empty() ) {
            cout << warningPrefix( inputFileDisplayName )
                 << kUsageTag << " tag should have a value."
                 << endl;
        }

        os << "  \""<< usageString_ << "\";\n";
    } else {
        if ( warningVerbosity_ >= 2 ) {
            cout << warningPrefix( inputFileDisplayName )
                 << "Missing " << kUsageTag << " tag; "
                 << "use the -e flag to see an example."
                 << endl;
        }

        os << "  \"No usage description available\";\n";
    }

    os << "\n";
}


void
FileProcessor::emitDescription( const string & inputFileDisplayName,
                                ostream &      os ) const
{
    os << "\n";

    os << "const char * const carma::util::ProgramBase::kDescription_ =\n";

    if ( haveDescription_ ) {
        const vector< string >::const_iterator iBegin =
            descriptionLines_.begin();

        vector< string >::const_iterator iNonBlankEnd = iBegin;

        // Find the end of the non-blank lines
        {
            vector< string >::const_iterator j = iBegin;

            const vector< string >::const_iterator jEnd =
                descriptionLines_.end();

            while ( j != jEnd ) {
                if ( j->empty() == false ) {
                    iNonBlankEnd = j;
                    ++iNonBlankEnd;
                }

                ++j;
            }
        }

        // Emit all lines up to the end of the non-blank lines
        if ( iBegin == iNonBlankEnd ) {
            if ( warningVerbosity_ >= 1 ) {
                cout << warningPrefix( inputFileDisplayName )
                     << kDescriptionTag << " tag should have a value."
                     << endl;
            }
        } else {
            vector< string >::const_iterator i = iBegin;

            while ( i != iNonBlankEnd ) {
                if ( warningVerbosity_ >= 1 ) {
                    string::size_type slashPos = i->find_first_of( '\\' );

                    while ( slashPos != string::npos ) {
                        if ( slashPos == (i->size() - 1) ) {
                            if ( warningVerbosity_ >= 1 ) {
                                cout << warningPrefix( inputFileDisplayName )
                                     << kDescriptionTag << " tag value "
                                     << "line contains a trailing '\\' "
                                     << "character."
                                     << endl;
                            }

                            slashPos = string::npos;
                        } else {
                            if ( warningVerbosity_ >= 2 ) {
                                cout << warningPrefix( inputFileDisplayName )
                                     << kDescriptionTag << " tag value "
                                     << "line contains a \"\\"
                                     << ((*i)[ slashPos + 1 ])
                                     << "\" sequence."
                                     << endl;
                            }

                            slashPos =
                                i->find_first_of( '\\', (slashPos + 2) );
                        }
                    }
                }

                os << "  \"" << *i << "\\n\"\n";

                ++i;
            }
            
            os << "  \"\\n\"\n";
        }
    } else {
        if ( warningVerbosity_ >= 2 ) {
            cout << warningPrefix( inputFileDisplayName )
                 << "Missing " << kDescriptionTag << " tag; "
                 << "use the -e flag to see an example."
                 << endl;
        }

        os << "  \"No description available\\n\"\n";
        os << "  \"\\n\"\n";
    }

    if ( haveLoggerInfo_ ) {
        os << "  \"The default logger for this program has\\n\"\n";

        if ( lognameString_.empty() )
            os << "  \"  logname  = UNDEFINED\\n\"\n";
        else
            os << "  \"  logname  = " << lognameString_ << "\\n\"\n";
        
        if ( facilityNameString_.empty() ) {
            os << "  \"  facility = UNDEFINED\\n\"\n";
        } else {
            os << "  \"  facility = " << facilityNameString_ << "\\n\"\n";

            FacilityFileMapType::const_iterator i =
                kFacilityFileMap.find( facilityNameString_ );
            
            if ( i != kFacilityFileMap.end() ) {
                os << "  \"\\n\"\n";
                os << "  \"If /etc/syslog.conf is the CARMA acc configuration,\\n\"\n";
                os << "  \"log messages are in " << i->second << "\\n\"\n";
                os << "  \"\\n\"\n";
                os << "  \"If /etc/syslog.conf is not the CARMA acc configuration,\\n\"\n";
                os << "  \"log messages may be in /var/log/messages or may be forwarded\\n\"\n";
                os << "  \"to another machine. Consult /etc/syslog.conf to find out.\\n\"\n";
            }
        }
    } else
        os << "  \"This program has no default logger.\\n\"\n";

    os << "  \"\\n\"\n";
    os << "  \"For more information on facilities and logging see:\\n\"\n";
    os << "  \"  http://www.mmarray.org/project/system/API/loghowto.html\\n\";\n";
}


void
FileProcessor::emitKeywords( const string & inputFileDisplayName,
                             ostream &      os ) const
{
    os << "\n";

    os << "const carma::util::KeyTabEntry "
       << "carma::util::ProgramBase::kKeywords_[ ] = {\n";

    {
        vector< KeyData >::const_iterator i = keyDatas_.begin();
        const vector< KeyData >::const_iterator iEnd = keyDatas_.end();

        while ( i != iEnd ) {
            os << "  {\n";

            os << "    \"" << i->name  << "\", "
               << "\"" << i->defaultValue  << "\", "
               << "\"" << i->typeName << "\", "
               << "\"" << i->usageValue << "\",\n";

            os << "    \"" << i->help << "\"\n";

            os << "  },\n";

            ++i;
        }
    }

    if ( keyDatas_.empty() == false )
        os << "\n";

    os << "  { 0, 0, 0, 0, 0 }\n";

    os << "};\n";

    os << "\n";
}


void
FileProcessor::emitLoggerInfo( const string & inputFileDisplayName,
                               ostream &      os ) const
{
    os << "\n";

    if ( haveLoggerInfo_ ) {
        os << "const bool carma::util::ProgramBase::kHaveInitialLoggerInfo_ = true;\n";
        os << "const char * const carma::util::ProgramBase::kInitialFacilityName_ =\n";
        os << "   \"" << facilityNameString_ << "\";\n";
        os << "const char * const carma::util::ProgramBase::kInitialLogname_ =\n";
        os << "   \"" << lognameString_ << "\";\n";
    } else {
        os << "const bool carma::util::ProgramBase::kHaveInitialLoggerInfo_ = false;\n";
        os << "const char * const carma::util::ProgramBase::kInitialFacilityName_ = \"\";\n";
        os << "const char * const carma::util::ProgramBase::kInitialLogname_ = \"\";\n";
    }

    os << "\n";
}


void
FileProcessor::emitOutputFile( const string & inputFileDisplayName,
                               ostream &      os ) const
{
    if ( gDebug )
        os << "// DEBUG is on\n";

    os << "// do not edit this file, automatically generated by keys\n";
    os << "// with " << kMyVersionId << "\n";

    os << "\n";
    os << "#include \"carma/util/Program.h\"\n";
    os << "\n";

    emitVersion( inputFileDisplayName, os );
    emitUsage( inputFileDisplayName, os );
    emitDescription( inputFileDisplayName, os );
    emitKeywords( inputFileDisplayName, os );
    emitLoggerInfo( inputFileDisplayName, os );
}


void
emitMyUsage( ostream & os ) {
    os << "keys: Version " << kMyVersionId << "\n";
    os << "Usage: \n";
    os << "  keys [--help] [-hdw?] [-o <out-file>] <in-file>\n";
    os << "    --help, -h, -?  help (this list)\n";
    os << "    -d              turn some debugging on\n";
    os << "    -w              print out warnings\n";
    os << "    -W              print out even more warnings\n";
    os << "    -i              print out informational messages\n";
    os << "    -I              print out even more informational messages\n";
    os << "    -e              show an example source code\n";
    os << "    -o <out-file>   write results to <out-file> instead of stdout\n";
}


}  // namespace < anonymous >


int
main( const int argc, const char * argv[ ] ) {
    int result = -1;

    try {
        // Handle the command line.

        vector< string > inFileNames;
        vector< string > outFileNames;
        int warningVerbosity = 0;
        int infoVerbosity = 0;
        
        for ( int i = 1; i < argc; ++i ) {
            string argI = argv[ i ];

            if ( argI.empty() ) {
                // skip empty args
            } else if ( argI == "--help" ) {
                emitMyUsage( cerr );
                exit( 0 );
            } else if ( argI == "-o" ) {
                ++i;

                bool gotIt = false;

                if ( i < argc ) {
                    string outFileName = argv[ i ];

                    if ( (outFileName.empty() == false) &&
                         (outFileName[ 0 ] != '-' ) ) {
                        gotIt = true;
                        outFileNames.push_back( outFileName );
                    }
                }

                if ( gotIt == false ) {
                    cerr << "keys error:  "
                         << "-o option must be by itself and followed "
                         << "by a file name."
                         << endl;

                    exit( 1 );
                }
            } else if ( argI[ 0 ] == '-' ) {
                for ( unsigned int j = 1; j < argI.size(); ++j ) {
                    switch ( argI[ j ] ) {
                        case 'd':
                            gDebug = true;
                            break;

                        case 'w':
                            warningVerbosity = max( warningVerbosity, 1 );
                            break;

                        case 'W':
                            warningVerbosity = max( warningVerbosity, 2 );
                            break;

                        case 'i':
                            infoVerbosity = max( infoVerbosity, 1 );
                            break;

                        case 'I':
                            infoVerbosity = max( infoVerbosity, 2 );
                            break;

                        case 'e':
                            show_example( cerr );
                            exit( 0 );
                            break;

                        case 'h':
                        case '?':
                            emitMyUsage( cerr );
                            exit( 0 );

                        case 'o':
                            cerr << "keys error:  "
                                 << "-o option must be by itself and followed "
                                 << "by a file name."
                                 << endl;

                            exit( 1 );

                        default:
                            cerr << "keys error:  "
                                 << "unrecognized command line flag "
                                 << argI[ j ]
                                 << endl;
                                 
                            exit( 1 );
                    }
                }
            } else
                inFileNames.push_back( argI );
        }

        // Exactly one inFileName must be present, else it's an error
        // Exactly zero or one outFileName must be present, else it's an error
        if ( (inFileNames.size() != 1) ||
             (outFileNames.size() > 1) ) {
            emitMyUsage( cerr );

            exit( 0 );
        }

        const string inputFileName = inFileNames[ 0 ];

        FILE * inFile = fopen( inputFileName.c_str(), "r" );
        if ( inFile == 0 ) {
            cerr << "keys error:  "
                 << "failed to open input file "
                 << "\"" << inputFileName << "\"."
                 << endl;

            exit( 2 );
        }

        const bool requireLoggerTag = true;
        const bool requireKeysOrNoKeys = true;

        string inputFileDisplayName = inputFileName;
        
        if ( (inputFileDisplayName.size() >= 3) &&
             (inputFileDisplayName[ 0 ] = '.') &&
             (inputFileDisplayName[ 1 ] = '/') )
            inputFileDisplayName = inputFileName.substr( 2 );

        // Process the input file and emit the output file
        {
            FileProcessor fileProcessor( warningVerbosity,
                                         infoVerbosity,
                                         requireLoggerTag,
                                         requireKeysOrNoKeys );

            fileProcessor.processInputFile( inFile, inputFileDisplayName );

            fclose( inFile );

            fileProcessor.validate( inputFileDisplayName );

            if ( outFileNames.empty() )
                fileProcessor.emitOutputFile( inputFileDisplayName, cout );
            else {
                ostringstream oss;
                
                fileProcessor.emitOutputFile( inputFileDisplayName, oss );

                const string outputFileName = outFileNames[ 0 ];

                ofstream outFileStream( outputFileName.c_str() );

                if ( outFileStream.is_open() == false ) {
                    cerr << "keys error:  "
                         << "failed to open output file "
                         << "\"" << outputFileName << "\"."
                         << endl;

                    exit( 2 );
                }

                outFileStream << oss.str();
            }
        }

        result = 0;
    } catch ( ... ) {
        cerr << "keys error: exception caught trying to escape main."
             << endl;

        result = -1;
    }

    return result;
}
