#ifndef CARMA_UTIL_CONFIGCHECKER_H
#define CARMA_UTIL_CONFIGCHECKER_H

#include <iostream>
#include <iomanip>
#include <map>
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pthread.h>

#include "carma/util/Time.h"
#include "carma/util/IoLock.h"
#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadRWLock.h"

#include <log4cpp/Category.hh> // carma tools include
#include <log4cpp/Priority.hh> // carma tools include


#ifdef RH_DEBUG
#define DPRINT(objptr, classname, statement) \
{ if(objptr->isDebug("All") || objptr->isDebug(classname)) {\
    const carma::util::IoLock::ScopedCerrLock cerrLock; \
    if (objptr->printTime()) {\
       std::cerr << carma::util::Time::getTimeString(2) << ": "; \
    }\
    if (objptr->prettyFunction()) {\
       std::cerr <<  "In " \
                 << __PRETTY_FUNCTION__ << ": "; \
    } \
    std::cerr << statement << std::endl; \
}}
#else
#define DPRINT(objptr, classname, statement) {}
#endif

#define LOGCMD(objptr, classname, statement) \
{if(objptr->isLogCmd("All") || objptr->isLogCmd(classname)) {\
    const carma::util::IoLock::ScopedCerrLock cerrLock; \
    log4cpp::Category& log = objptr->getLogger(); \
    std::ostringstream os; \
    if (objptr->prettyFunction()) {\
       os << "In " << __PRETTY_FUNCTION__ << ": "; \
    } \
    os << statement; \
    log << log4cpp::Priority::INFO << os.str(); \
}}

#define LOGEXCEPTION(objptr, classname, statement) \
{ \
    const carma::util::IoLock::ScopedCerrLock cerrLock; \
    log4cpp::Category& log = objptr->getLogger(); \
    std::ostringstream os; \
    os << "In " << __PRETTY_FUNCTION__ << ": "; \
    os << statement; \
    log << log4cpp::Priority::ERROR << os.str(); \
}

#define LOGPROBLEM(objptr, classname, statement) \
{ \
    const carma::util::IoLock::ScopedCerrLock cerrLock; \
    log4cpp::Category& log = objptr->getLogger(); \
    std::ostringstream os; \
    os << "In " << __PRETTY_FUNCTION__ << ": "; \
    os << statement; \
    log << log4cpp::Priority::WARN << os.str(); \
}

#define LOGINFO(objptr, classname, statement) \
{ \
    const carma::util::IoLock::ScopedCerrLock cerrLock; \
    log4cpp::Category& log = objptr->getLogger(); \
    std::ostringstream os; \
    os << "In " << __PRETTY_FUNCTION__ << ": "; \
    os << statement; \
    log << log4cpp::Priority::INFO << os.str(); \
}

/**
 * @file ConfigChecker.h
 * 
 * @author Rick Hobbs
 */
namespace carma {
  namespace util {

      /**
       * Abstract Base class for managing configuration files.
       * This class will spawn a thread and read the input configuration
       * file every N seconds and store its contents internally. Sub-classes
       * will present an API allowing read-only access to config parameters
       * allowing run-time program manipulation(ie. turning on/off debugging,
       * turning on/off listeners or monitoring output etc.). Delay between
       * file reads is controlled using the 'delayBetweenFileReads' 
       * parameter in the configuration file. The default is 5 seconds.
       * The input config file can also be changed by using the
       * configFilename parameter. This allows one to switch to a private
       * file with minimal changes to the default system file. By using
       * a filename of 'default', the code will read the original filename
       * passed in the constructor.
       * Class is thread safe.
       */
      class ConfigChecker {
      public:
        /**
         * Constructor.
         */
        explicit ConfigChecker( const std::string & filename );

        /**
         * Destructor.
         */
        virtual ~ConfigChecker();

        /**
         *  start reading the configuration file in a separate thread
         */
        void start();

        /**
         *  return the value associated with key. If key does not
         *  exist, then an empty string ("") is returned.
         */
        std::string getValue( const ::std::string & key ) const;

        bool valueIsEmpty( const ::std::string & key ) const;

        bool valueIsOneString( const ::std::string & key ) const;

        bool valueIsZeroString( const ::std::string & key ) const;

        /**
         *  Get a logger
         */
        log4cpp::Category & getLogger( ) const;

      protected:
        bool getConfigFileNotFound( ) const;

      private:
        static void updateThreadEntrypoint( ConfigChecker & configChecker );

        /*
         * Return the time in seconds between file reads.
         * Controlled with 'delayBetweenFileReads' variable in configuration
         * file.
         */
        int getDelay( ) const;

        /*
         * Return the configuration filename to use. Default will be the
         * name used in the constructor.
         */
        ::std::string getFilename( ) const;

      protected:
        const ::std::string                        kOneString_;
        const ::std::string                        kZeroString_;
        
      private:
        const int                                  delayBetweenFileReads_;
        const ::std::string                        fileName_;

        mutable PthreadRWLock                      guard_;
        bool                                       configFileNotFound_;
        ::std::map< ::std::string, ::std::string > pairs_;
        bool                                       haveFirstReadCtime_;
        time_t                                     firstReadCtime_;
        log4cpp::Category &                        log_;
        
        mutable PthreadMutex updateThreadGuard_;
        bool                 updateThreadStarted_;
        ::pthread_t          updateThread_;
      }; // End class ConfigChecker

  } // End namespace util
} // End namespace carma



#endif // End #ifndef CARMA_UTIL_CONFIGCHECKER_H
