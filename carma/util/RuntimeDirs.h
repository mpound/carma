#ifndef CARMA_UTIL_RUNTIMEDIRS_H
#define CARMA_UTIL_RUNTIMEDIRS_H


/*
 * @file
 *
 * Class to get all of the runtime directories for a carma Program.
 * Contains only static methods.
 *
 * @author Original:Steve Scott 
 * $id: $
 *
 * $CarmaCopyright$
 *
 */




namespace carma {
    namespace util { 


/**
  * This class is a collection of static methods to return specific
  * directories for a standard carma tree.
  * The standard carma tree is assumed to be of the form 
  * <pre>
                carmaRoot
                    |
       --------------------------- 
      /        |         |        \ 
    bin       lib      carma      conf 
                         |
                    -------------
                  /       |       \
                Xpgk    Ypkg     Zpkg 
                  |       |        |
                Test    Test     Test   
  </pre> 
  * These routines start by getting the full path of the executable
  * using the "which"
  * shell command for the program name (usually passed in as arg[0]
  * to the program) . We assume that the these are carma programs 
  * and are run from one of two locations:
  * <ul>
  * <li> carmaRoot/bin
  * <li> carmaRoot/carma/... <br>
  * which is usually a Test sub-directory
  * </ul>
  * If the program has been run from a "bin" directory, then its parent is
  * selected as carmaRoot, otherwise the tree is ascended until "carma" is 
  * found and then its parent is selected as carmaRoot. 
  * If neither of these directories is found then an exception is thrown.
  * All filenames are canonicalized, removing symbolic links and 
  * redundant slashes.  
  */
class RuntimeDirs {
public:
     
    /**
     * Get the absolute path of a configuration file. 
     * If the filename begins with a slash then it is assumed to
     * be an absolute path and is left untouched.
     * If the filename begins with a dash then it is assumed to be
     * a path local to the current working directory and the dash
     * is removed with no further processing.
     * All other filenames are appended to the conf directory 
     * and the conf directory canonicalized if requested.
     * @param programName as invoked (e.g. arg0)
     * @param confFilename 
     * @see getConfDir
     * @see getRootDir
     * @throws ErrorException if getRootDir() or canonicalize() has problems
     */
    static std::string getConfFile(const std::string& programName,
            const std::string& confFilename);
            
    /**
     * Get the configuration directory - assumes standard carma tree.
     * The conf directory is assumed to be /carmaRoot/conf/.
     * This string is an absolute path that ends with a slash character (/).
     * @param programName as invoked (e.g. arg0)
     * @see getConfFile
     * @see getRootDir
     * @throws ErrorException if getRootDir() or canonicalize() has problems
     */
    static std::string getConfDir(const std::string& programName);
      
    /**
     * Get the root build or install directory - assumes standard carma tree.
     * This string is an absolute path that ends with a slash character (/).
     * @param programName as invoked (e.g. arg0)
     * @see getConfDir
     * @see getConfFile
     * @throws ErrorException if the directory from which the program was
     * run has no parent, or is not part of a standard carma tree, or if 
     * cannot get filename of executable
     */
    static std::string getRootDir(const std::string& programName);

    /**
     * Get the directory where this program executable lives.
     * This string is an absolute path that ends with a slash character (/).
     * @param programName as invoked (e.g. arg0)
     * @throws ErrorException if get Excecutable or canonicalize() has problems    
     */
    static std::string getExecutableDir(const std::string& programName);
      
    /**
     * Get absolute full path of executable, including program executable.
     * The bash builtin "which" command is used to obtain this path.
     * @param programName as invoked (e.g. arg0)
     * @throws ErrorException under numerous conditions associated with 
     * fork/exec of the bash which command
     */
    static std::string getExecutable(const std::string& programName);
    
private:
    // Disable construction.
    RuntimeDirs();
    /*
     * Get the full path to the executable, using bash "which" command
     * @param programName as invoked (e.g. arg0)
     * @return canonicalized full path binary filename
     */
    static std::string findExecutable(const std::string& programName); 
    /*
     * @throws ErrorException if realpath() fails
     */
    static std::string canonicalize(const std::string& filename);
    // Like basename() and dirname() except for strings rather than char*'s
    static std::string basenameString(const std::string& filename);
    static std::string dirnameString(const std::string& filename);
    // Lop off any trailing slashes
    static std::string trim(const std::string& filename);

};

}}  // End namespace carma::util

#endif  // CARMA_UTIL_RUNTIMEDIRS_H 
