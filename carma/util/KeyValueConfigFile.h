/**
 *
 * @file   
 * class of static methods to manipulate configuration files with entries of
 * the form 
 * key1 = value1
 * key2=value2
 * ...
 *
 * @author Original: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#ifndef CARMA_UTIL_KEYVALUECONFIGFILE_H
#define CARMA_UTIL_KEYVALUECONFIGFILE_H

#include <string>
#include <map>

namespace carma {
    namespace util {
/**
 * Common functions for manipulating key-value config files
 * This class contains no state.
 *
 */
class KeyValueConfigFile {
public:
    /**
    ** Default constructor
    */
    KeyValueConfigFile();

    /**
    ** Destructor
    */
    virtual ~KeyValueConfigFile();

    /**
     * read a config file and return its values as a key-value map
     * Lines which begin with commentStr are considered comments and are
     * not processed. Lines with more than one delimiter are not processed.
     * Tabs and blank spaces are removed from the beginning and end (but not
     * the middle) of both keys and values. Macro expansion (a'la Makefiles) 
     * is used using <code>macroStartStr</code> and <code>macroEndStr</code>.
     * For example, in a file with the following lines
     * <pre>
     * foo = bar
     * baz = ${foo}.raz
     * </pre>
     * This method will expand <code>baz</code> to <code>bar.raz</code> 
     * (assuming the default
     * values of <code>macroStartStr</code> and <code>macroEndStr</code> are
     * used).
     * If a macro has not been defined previously in the config file, but
     * is an environment variable, the value of the environment variable
     * will be substituted unless the <code>useEnvironment</code> parameter
     * is <code>false</code>
     * config files may be included in config files by using the
     * <pre>
     * include &lt;filename&gt;
     * </pre> 
     * directive.  The path of the included file must be relative to the
     * file which includes it.  <code>include</code> directives may occur
     * at any point in a config file; the result is just if the code in
     * the included file had been inserted in place of the 
     * <code>include</code> directive (e.g., any variables defined prior to
     * the <code>include</code> directive will be overwritten if they are
     * defined again in the include file, any variables defined in the include
     * file will be overwritten if they are redefined in the original file
     * after the <code>include</code> directive
     * @param filename the file to read
     * @param delimiter the key-value delimiter to use
     * @param commentStr lines starting with this string (after stripping 
     *        leading spaces will be treated as comments
     * @param macroStartStr string indicating the start of a macro
     * @param macroEndStr string indicating the end of a macro
     * @param useEnvironment if true, search for an environment variable
     *        matching the macro and use its value if it exists
     * @return the key-value map
     * @throws NotFoundException if file cannot be read 
     */
    static std::map<std::string,std::string> load(const std::string& filename,
                                       const std::string& delimiter="=",
                                       const std::string& commentStr="#",
                                       const std::string& macroStartStr="${",
                                       const std::string& macroEndStr="}",
                                       const bool& useEnvironment = true);
};



}}
#endif  
