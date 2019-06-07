#ifndef CARMA_UTIL_PROGRAM_H
#define CARMA_UTIL_PROGRAM_H

/*!
 * @file Program.h
 * @brief This is the include file for the CARMA generic Program class.
 *
 * @author N. S. Amarnath, Peter Teuben
 *
 * File containing declarations for the CARMA base Program class.
 * The carma::util::Program class is meant to be used as a means of ensuring
 * uniformity across all CARMA programs for command line interfaces,
 * signal handling policies, managing CARMA configurations, and default
 * exception handling. Program provides methods for accessing command line
 * parameters by name, and by type. It also provides convenience methods
 * for accessing common resources like the system logger.
 *
 * @see http://www.mmarray.org/project/system/API/Program.pdf
 */

#include <cstdlib>
#include <cstdio>
#include <string>
#include <map>
#include <memory>

#include <sys/types.h>

#include "carma/util/FacilityType.h"

// Declaration of carma_main.  See comments in Program.cc for more info.
int carma_main( int argc, char ** argv );

namespace log4cpp {

class Category;

}  // namespace log4cpp


namespace carma {

namespace corba {

    class Server;
    class Client;

} // namespace carma::corba

namespace util {

class Orb;
class ProcessMonitorClient;
class Trace;


//! @brief Convenience structure to hold the key=value/type/usage/help-string
//!        information.
//!
//! @note This structure also is implicitly used by scripts/keys which
//!       compiles @@key comment lines into C++ code. See scripts/keys.cc
typedef struct {
    //! @brief keyword (name used on commad line)
    const char * const key;

    //! @brief default value for keyword
    //!
    //! Magic values "@mandatory" and "@noDefault" have special meaning here.
    const char * const val;

    //! @brief Name of the type of the value
    //!
    //! Should be "bool", "double", "int", or "string".
    //! "b", "d", "i" or "s" are also accepted for those who hate typing;)
    const char * const type;

    //! @brief Usage value shown on the usage line
    //!
    //! Magic value "@autogen" has special meaning here.
    const char * const usageValue;

    //! @brief Help string
    //!
    //! Used to provide detailed information on options
    const char * const help;
} KeyTabEntry;


class Program;

//! @brief Base class for managing resources and interfaces common across all
//!        carma programs.
class ProgramBase {
    friend int ::carma_main( int argc, char ** argv );

    public:

      //! @brief Query whether the value of the parameter was specified on the
      //!        command line
      //! @return true if and only if the value of the parameter was specified
      //!         on the command line.
      bool parameterWasSpecified( const ::std::string & key );

      //! @brief get value of a bool parameter
      //! @return value of a bool parameter
      bool getBoolParameter( const ::std::string & key );

      //! @brief get value of a double parameter
      //! @return value of a double parameter
      double getDoubleParameter( const ::std::string & key );

      //! @brief get value of an int parameter
      //! @return value of an int parameter
      int getIntParameter( const ::std::string & key );

      //! @brief get value of a string parameter
      //! @return value of a string parameter
      ::std::string getStringParameter( const ::std::string & key );

      //! @brief get the raw value string of a parameter
      //! @return raw value string of a parameter
      ::std::string getParameterRawValueString( const ::std::string & key );


      //! @brief debug level of the program environment
      //! @return debug level
      int getDebugLevel( ) const;

      //! @brief has a debug level exceeded the set value?
      //! @return debug level high enough?
      bool DebugLevel( int level = 0 ) const;


      //! @brief set the logger logname for this instance of a Program
      //!
      //! This method is useful if you will have multiple instances of the 
      //! same Program which differ only by keyword values given on the command
      //! line. For example, maybe you have six instances of a program "pod"
      //! that take different command line arguments of podNo=1, podNo=2, etc.
      //! The @@logger tag for the program might have
      //! "@logger DEFAULT_FACILITY carma.pod.foo" and using this method each
      //! instance could, in it's Program::main implementation refine this
      //! logname by calling
      //! setInstanceLogname( "carma.pod" + podNoString + ".foo" )
      //!
      //! @param logname
      //!        The hierarchical namespace for this Category,
      //!        e.g. "carma.monitor".  The root logger will be "carma", all
      //!        other loggers should be "carma.something[.somethingelse]" to
      //!        make use of:
      //!          - log4cpp's automatic heirarchical logging.
      //!          - for sorting in e.g. database queries.
      //!
      //! @see Logger::getSyslogger
      void setInstanceLogname( const ::std::string & logname );

      //! @brief get the log name of for the logger of this Progam
      //!
      //! @see Logger::getSyslogger
      ::std::string getLogname(  ) const;

      //! @brief Gets hostname of machine that stores the log file.
      //!
      //! Class method that returns the hostname of the machine
      //! hosting the log file. Default value is "localhost", that is,
      //! the machine on which the process is executing.
      //!
      //! @return string - name of the log host machine, default is
      //!            "localhost".
      //!
      //! @todo Consider deprecation of this routine and removal of the
      //! logHost command line option.
      //!
      //! @see carma/util/Logger.h, log4cpp
      ::std::string getLogHostname( ) const;


      //! @brief Was imr hostname specified on the commmand line?
      bool haveImrHostname( ) const;

      //! @brief Retrieve the imr hostname.
      //!
      //! Make sure the imr hostname was specified prior to calling this.
      //! @see haveImrHostname
      ::std::string getImrHostname( ) const;


      /**
       * @brief  Set ORB parameters
       *
       * This function should be used _instead_ of the
       * CorbaUtils::setOrb commands.  With no arguments, it
       * simplifies what the programmer needs to know.
       *
       * @return bool value if everything is set okay.
       */
      bool orbInit(Orb* orb);

      carma::corba::Server & getCorbaServer();
      carma::corba::Client & getCorbaClient();


      /**
       * @brief Check for an IMR instructed termination request. 
       *
       * Indicates if this application has been instructed to shutdown by 
       * the IMR.  This should be called as part of a main event loop.
       */
      bool imrTerminationRequested( );

      // class static methods

      /*!
       * @brief Returns a reference to the process-wide default logger.
       *
       * Class global method that returns a reference to the process-wide
       * default logger. The logger will not produce any output
       * if it has been configured incorrectly. For example, if the carma
       * default logger is the syslog (using a syslogd), then syslogd
       * must be configured to log carma messages using syslog.conf.
       * No output will be produced by the logger if its configured
       * incorrectly.
       *
       * @return log4cpp::Category& - reference to the carma default logger -
       *               usually syslogd.
       *
       * @see carma/util/Logger.h, log4cpp
       */
      static log4cpp::Category & getLogger( );

      static log4cpp::Category * getLoggerIfAvailable( );

      static carma::util::Trace * getTraceObject( );

      static carma::util::Trace * getTraceObjectIfAvailable( );


      /**
       * Returns value of system keyword "useDBMS". If useDBMS is true, then
       * monitor points are assigned tagID's from the database, or from a
       * proxy conf file if the database isn't reachable. If useDBMS is false, 
       * tagID's are assigned on the fly. 
       *
       * @return bool value of boolean system keyword "useDBMS"
       */
      static bool getUseDBMS( ) ;


      //! @brief Get the first argument passed to main()
      //!
      //! This is usually the program name.
      static ::std::string getArg0( );


      //! @brief Obtain the length of the array of command line arguments with
      //!        all CARMA arguments removed
      //!
      //! This can only legitimately be called after the command line has been
      //! parsed which is certainly true by the time the Program::main
      //! is entered.
      //!
      //! @return length of the array of command line arguments with
      //!         all CARMA arguments removed.
      //!
      static int getExtraArgc( );

      //! @brief Obtain the array of command line arguments with all CARMA
      //!        arguments removed
      //!
      //! This can only legitimately be called after the command line has been
      //! parsed which is certainly true by the time the Program::main
      //! is entered.
      //!
      //! @return array of command line arguments with all CARMA arguments
      //!         removed.
      //!
      static char ** getExtraArgv( );


      /**
       * Get the configuration directory - assumes standard carma tree.
       * The conf directory is assumed to be /carmaRoot/conf/.
       * See the documentation for the RuntimeDirs class for the definition
       * of the carma tree and how it is used.
       * The returned string is an absolute path that ends with a slash
       * character (/).
       * @see getConfFile
       * @see getRootDir
       * @see RuntimeDirs
       * @throws ErrorException if not a standard carma tree
       */
      static ::std::string getConfDir( );

      /**
       * Get the absolute path of a configuration file.
       * If the filename begins with a slash then it is assumed to
       * be an absolute path and is left untouched.
       * If the filename begins with a dash then it is assumed to be
       * a path local to the current working directory and the dash
       * is removed.
       * All other filenames are appended to the conf directory.
       * See the documentation for the RuntimeDirs class for the definition
       * of the carma tree and how it is used.
       * @param filename
       * @see getConfDir
       * @see getRootDir
       * @see RuntimeDirs
       * @throws ErrorException if not a standard carma tree
       */
      static ::std::string getConfFile( const ::std::string & filename );


      /**
       * Get the root build or install directory - assumes standard carma tree.
       * See the documentation for the RuntimeDirs class for the definition
       * of the carma tree and how it is used.
       * The returned string is an absolute path that ends with a slash
       * character (/).
       * @see getConfDir
       * @see getConfFile
       * @see RuntimeDirs
       * @throws ErrorException if not a standard carma tree
       */
      static ::std::string getRootDir( );

      /**
       * Get the directory where this program binary lives.
       * This string is an absolute path that ends with a slash character (/).
       */
      static ::std::string getExecutableDir( );

      /**
       * Get absolute full path of executable.
       */
      static ::std::string getExecutable( );

      /**
       * Get pid of this program.
       *
       */
     static pid_t getPid( );

      /**
       * Get hostname of the machine this program is running on.
       * @param shorten Retrieve short hostname only, otherwise return hostname
       *  as reported by gethostname.
       */
     static ::std::string getHostname( bool shorten );

      /**
       * Get the current working directory. 
       * Unless changed by the application itself, this is generally the
       * directory the program was invoked from.
       */
     static ::std::string getCwd( );

     int getTraceLevel( ) const;

     void adjustTraceLevel( int newTraceLevel );

     /**
      * @return the nice value this program was started with
      */
     int getNiceLevel( ) const;
     
    /**
     * @return the value of useGMT that program was started with
     */
     bool getUseGMT() const; 

     //! @brief Get the usage string for this program
     //! @return Usage string for this program
     ::std::string getUsageString( ) const;

    protected:

        //! @brief Default constructor
        ProgramBase( );

        //! @brief Destructor
        ~ProgramBase( );

    private:

        typedef enum {
            // Please note that these states are in a particular order.

            BEING_CONSTRUCTED_STATE,
            CONSTRUCTED_STATE,

            ADDING_KEY_DEFINITIONS_STATE,
            KEY_DEFINITIONS_ADDED_STATE,

            PARSING_COMMAND_LINE_STATE,
            COMMAND_LINE_PARSED_STATE,

            INITIALISING_SYSTEM_KEYWORD_MEMBERS_STATE,
            SYSTEM_KEYWORD_MEMBERS_INITIALISED_STATE,

            LOGGER_AVAILABLE_STATE,
            TRACE_OBJECT_AVAILABLE_STATE,

            FULLY_INITIALISED_STATE,
            
            // most of our time is spent in here between these two states

            BEING_DESTRUCTED_STATE,
            DESTRUCTED_STATE
        } StateType;

        typedef enum {
            BOOL_PARAM_TYPE,
            DOUBLE_PARAM_TYPE,
            INT_PARAM_TYPE,
            STRING_PARAM_TYPE
        } ParamType;

        typedef enum {
            NORMAL_PARAM_SPECIFY_TYPE,
            MANDATORY_PARAM_SPECIFY_TYPE,
            NO_DEFAULT_PARAM_SPECIFY_TYPE
        } ParamSpecifyType;

        //! @brief Private structure used by ProgramBase to hold all relevant
        //!        information per keyword.
        typedef struct {
            bool              system;              // system keyword?
            ::size_t          declIndex;           // index in decl order

            ::std::string     defaultValueString;  // default value string
            ParamType         type;                // type
            ::std::string     usageValue;          // usage value
            ::std::string     help;                // help
            ParamSpecifyType  specifyType;         // "normal", "mandatory", or
                                                   // "no default"

            ::std::string     valueString;         // value string
            bool              valueSpecified;      // user specified on
                                                   // command line?
            bool              valueSpecifiedWasChecked;
            bool              valueWasRead;        // value was read
        } ParameterInfo;

        typedef ::std::map< ::std::string, ParameterInfo > ParameterInfoMap;

        void verifyStateIsInRange( StateType rangeBegin,
                                   StateType rangeEnd ) const;

        void verifyStateIsInitializingSystemKeywordMembers( ) const;

        void verifyStateIsNormal( ) const;

      /**
       * @brief setup and run the program
       * @return an integer representing the return status from the
       *         user supplied Program::main().
       * starts carma level code - second statment in the "real" or Unix main
       * should never be called by the user, since this is called by the
       * true main(argc,argv)
       */
      static int run( Program & program );

      /**
       * initializeCarma is called by run() to setup the Program environment:
       * initialize/setup, initial check if system-only information requested,
       * parse the command line, ...
       */
      int initializeCarma( );


      /**
       * called by run, this function makes sure Carma programs are
       * properly released from the carma environment
       */
      void terminateCarma( );

      void show( );

      void addKey( const KeyTabEntry & kt,
                   bool                system,
                   ::size_t            declIndex );

      void docKeys( bool system, bool declOrder ) const;

      ::std::string getDescriptionString( ) const;


      ::std::string getParameterValueString( const ::std::string & key,
                                             bool                  system,
                                             ParamType             type,
                                             bool                  checkType );

      bool parameterWasSpecified( const ::std::string & key,
                                  bool                  system );

      bool getBoolParameter( const ::std::string & key,
                             bool                  system );

      double getDoubleParameter( const ::std::string & key,
                                 bool                  system );

      int getIntParameter( const ::std::string & key,
                           bool                  system );

      ::std::string getStringParameter( const ::std::string & key,
                                        bool                  system );

      ::std::string getParameterRawValueString( const ::std::string & key,
                                                bool                  system );

      static void docKey( const ::std::string & name,
                          const ParameterInfo & parameterInfo );


      /*!
       * @brief Sets up logger for Program based on the user-assigned
       * values for the system keywords "syslog" and "logfile".
       *
       * The method sets up a logger that logs to the system logs
       * if "syslog=t". If "logfilename" is assigned on the command line
       * to a non-null string representing a pathname, then the method
       * either creates a new logger with, or adds to the existing logger,
       * a file appender that writes to the specified log file using
       * the file layout specified by Logger::getFilelogger.
       *
       * @return none
       * @exception BaseExceptionObj thrown if no file appender is available.
       * @see carma/util/Logger.h, log4cpp
       */
      void setLogger( );

      /**
       * Sets up the Trace object for Program based on user-assigned values
       * for the system keywords "traceLevel" , "traceFile", and "syslog".
       * @return none
       */
      void setTrace( );

      /**
       * Sets up the Corba environment based on system keywords "imr" as well
       * as additional 'extra' arguments.
       * @return none
       */
      void setCorba( );

      /*!
       * @brief Returns a reference to the carma default logger for
       * the process.
       *
       * Class method that returns a reference to the carma
       * default logger. The logger will not produce any output
       * if its been configured incorrectly. For example, if the carma
       * default logger is the syslog (using a syslogd), then syslogd
       * must be configured to log carma messages using syslog.conf.
       * No output will be produced by the logger if its configured
       * incorrectly.
       *
       * @return log4cpp::Category& - reference to the carma default logger -
       *               usually syslogd.
       *
       * @see carma/util/Logger.h, log4cpp
       */
      log4cpp::Category & getDefaultLogger( ) const;


      static ParamType convertParameterType( const ::std::string & s );

      void advanceState( StateType newState );

      /**
       * Attempt to renice the program to the value of nice_.
       * Warn message is logged if the attempt failed (e.g.
       * process does not have permission to lower its nice value.
       */
      void renice( void );

      class Config;

      // class static data members
      static const KeyTabEntry  kSystemKeywords_[ ];

      static const char * const kUsage_;        // one line usage
      static const KeyTabEntry  kKeywords_[ ];  // {key,val,type,help} tuples
                                                // for program keys
      static const char * const kVersion_;      // one line version
      static const char * const kDescription_;  // multiple line description

      static const bool         kHaveInitialLoggerInfo_;
      static const char * const kInitialFacilityName_;
      static const char * const kInitialLogname_;

      // instance data members
      StateType            state_;

      ::std::string        progname_;       // short program name without path

      ParameterInfoMap     parameterInfoMap_;

      bool                 syslog_;         // true or false
      log4cpp::Category *  logger_;         // logger set up as specified by
                                            // system key values
      ::std::string        logFileName_;    // pathname for file log
      ::std::string        logHostName_;    // logger is hosted on this machine
                                            // default for logHostName is
                                            // gethostname(3)
      int                  corbaRrtt_;     
      bool                 imrHostnameWasSpecified_;// IMR info
      ::std::string        imrHostnameSpecified_;

      ::std::auto_ptr<carma::corba::Server> corbaServer_;
      ::std::auto_ptr<carma::util::ProcessMonitorClient> processMonitorClient_;

      int                  debugLevel_;     // debug level (0 or larger)
      bool                 daemonize_;      // program is a daemon ?

      int                  nice_;           // Nice value for managing 
                                            // resource usage.  Higher
                                            // value is nicer, lower value
                                            // means higher priority 
                                            // (less than zero requires
                                            // root permission).
      bool                 useGMT_;         // Use GMT timezone for program                                      
      int                  traceLevel_;     // trace level
                                            // (>=0 ; 0 means no tracing)
      bool                 traceVerbose_;   // trace verbosity
                                            // (true means maximum, false means
                                            //  log4cpp::SimpleLayout)
      ::std::string        traceFile_;      // file (including stdout/syslog)
                                            // where trace output will go
      carma::util::Trace * traceObject_;    // trace object

      const carma::util::facilityType facility_;  // Default syslog facility

      ::std::string        logname_;        // logname for Logger Category.
      bool                 instanceLognameSet_;
      
      bool                 useDBMS_;
};  // class carma::util::ProgramBase


//! @brief Class for managing resources and interfaces common across all
//!        carma programs.
//!
//! Singleton class - only one instance per process.
class Program : public ProgramBase {
    friend class ProgramBase;
    friend int ::carma_main( int argc, char ** argv );

    public:

        //! @brief Class static method to get the process-wide singleton
        //!        instance of Program.
        //!
        //! Class static method that returns a reference to the process-wide
        //! single instance of class Program. Basically a factory method that
        //! is tailored to return the same instance for every call.
        //!
        //! @return Reference to the singleton instance.
        //!
        //! @see Singleton pattern in "Design Patterns", by
        //!      Gamma, Helm, Johnson, and Vlissides.
        static Program & getProgram( );

    private:

        Program( );

        ~Program( );

        //! @brief user defined main() program, returns the program exit status
        //!
        //! Program::main is the routine written by the user.
        //!
        //! @return exit status requested (normally 0)
        int main( );

};  // class carma::util::Program


}  // namespace carma::util
}  // namespace carma

#endif
