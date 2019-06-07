/**
 * @file
 * Implementation of common functions needed for key-value config files
 *
 * @author Original: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include <sstream>

#include "carma/util/NotFoundException.h"
#include "carma/util/KeyValueConfigFile.h"
#include "carma/util/StringUtils.h"
#include <fstream>
#include <libgen.h>

using namespace std;
using namespace carma::util;


// Default constructor does nothing
KeyValueConfigFile::KeyValueConfigFile() {}

// Destructor does nothing
KeyValueConfigFile::~KeyValueConfigFile() {}

map<string,string> KeyValueConfigFile::load(const string& filename,
                                            const string& delimiter,
                                            const string& commentStr,
                                            const string& macroStartStr,
                                            const string& macroEndStr,
                                            const bool& useEnvironment) {
    
    ifstream fin(filename.c_str());
    if(!fin) {
        throw CARMA_EXCEPTION(NotFoundException,"Unable to open \"" + filename
                              + "\" for reading");
    }
    map<string,string> pairs;
    string line;
    const string space( " \t" );
    vector<string> tokens;
    tokens.reserve(3);
    string::size_type pos = 0;
    string::size_type mBeginPos, mEndPos;
    string keysub,includeFilename,directoryName;
    map<string,string> includedPairs;
    map<string,string>::iterator iter;
    char *fc = 0;
    while(!fin.eof()) {
        getline(fin,line);
        StringUtils::trimInplace( line, space );
        char *envVar;
        if(line.find(commentStr) > 0) {
            StringUtils::tokenizeInplace( tokens, line, delimiter );
            if(tokens.size() == 2) {
                string & key = tokens[0];
                StringUtils::trimInplace( key, space );

                string & value = tokens[1];
                StringUtils::trimInplace( value, space );

                // find "macros" 
                // FIXME this should be done using a regexp library...
                pos = 0;
                mBeginPos = value.find(macroStartStr);
                if ( mBeginPos == string::npos )
                    pairs[key].swap( value );
                else {
                    mEndPos = value.find(macroEndStr);
                    string tmpvalue = value;
                    while(mBeginPos != string::npos && mEndPos != string::npos 
                       && mBeginPos < mEndPos) {
                        keysub = value.substr(mBeginPos+macroStartStr.size(),
                                     (mEndPos-mBeginPos-macroStartStr.size()));
                        if(pairs.count(keysub) > 0) {
                            tmpvalue = StringUtils::replace(tmpvalue,
                                              macroStartStr + keysub + macroEndStr,
                                              pairs[keysub]);
                        } else if(useEnvironment 
                                  && ((envVar = getenv(keysub.c_str())) != 0) ) {
                            tmpvalue = StringUtils::replace(tmpvalue,
                                            macroStartStr + keysub + macroEndStr,
                                            string(envVar));
                        } else {
                            // no matching macro, replace with the empty string
                            tmpvalue = StringUtils::replace(tmpvalue,
                                            macroStartStr + keysub + macroEndStr,
                                            "");
                        }
                        pos = mEndPos + macroEndStr.size();
                        mBeginPos = value.find("${",pos);
                        mEndPos = value.find("}",pos);
                    }
                    //value = tmpvalue;
                    pairs[key].swap( tmpvalue );
                }
            } else if(line.find("include") == 0) {
                // process included file and merge values into the pairs map
                line = StringUtils::collapse(line,' ');
                StringUtils::tokenizeInplace( tokens, line, " " );
                if(tokens.size() > 1) {
                    includeFilename = tokens[1];
                    fc = new char[filename.size() + 1];
                    strcpy(fc,filename.c_str());
                    directoryName = string(dirname(fc));
                    delete [] fc;
                    includeFilename = directoryName + "/" + includeFilename;
                    try {
                        includedPairs = load(includeFilename, delimiter, 
                                             commentStr, macroStartStr, 
                                             macroEndStr, useEnvironment);
                    } catch ( const NotFoundException & ) {
                        ostringstream emsg;
                        emsg << "Unable to open file \"" << includeFilename 
                             << "\" included by file \"" << filename << "\". Check "
                             << "that the path of the included file is "
                             << "relative to the path of the file including "
                             << "it";
                        throw CARMA_EXCEPTION(NotFoundException,emsg.str());
                    }

                    for(iter = includedPairs.begin(); 
                        iter != includedPairs.end(); iter++) {
                        pairs[iter->first] = iter->second;
                    }
                }
            }
        }
    }
    fin.close();
    return pairs;
}

