#ifndef CARMA_UTIL_SYSTEM_EXCEPTION_H
#define CARMA_UTIL_SYSTEM_EXCEPTION_H

/*!
 * @file SystemException.h
 * This is the include file for Carma exception handling utilities.
 * @author N. S. Amarnath
 *
 * File containing declarations for the CARMA base exception class.
 * Uses some tricks to ensure that file name and line number of location
 * where exception is created is captured by the exception handler. Actual
 * constructor uses three parameters, but most users will really use something 
 * that has only the first parameter. However, the macro definition at the
 * end of the file  will ensure that the file name and line number are 
 * inserted at the appropriate place. <br>
 * <B>WARNING</B> The macro has to be disabled if for some reason the
 * original definition of the constructor with three parameters 
 * is required. Macros may be disabled using the compiler
 * option '-Umacro', or by using '#ifdef'/'#undef' combinations in your
 * source file.
 *
 * @see define BaseException(x)
 * @see carma::util::BaseException
 */

#include <string>

// show be including <iosfwd> but the API passes std::ostringstream's by value
#include <sstream>

#include <cerrno>

#include "carma/util/BaseException.h"


namespace carma  {
namespace util  {


/*! @class SystemException BaseException.h "carma/util/BaseException.h"
 *  @brief Exception class for managing system errors.
 *  
 *  Class that automatically system error messages and provides facilities for
 *  logging error messages to a log4cpp logger.
 */
class SystemException : public BaseException  {
    public:
        SystemException( const SystemException & rhs );

        /*!
         * @brief  Constructor - creates an instance of class SystemException. 
         * 
         * Users should use the macro definition (see the #define
         * towards the end of this file).
         *
         * @param mesg     The message to be logged for this exception. 
         *           If the exception is to be caught outside the
         *           scope of the function where the exception was 
         *           constructed, then 'mesg' should not be a stack 
         *           variable. Stack variables are lost as the exception 
         *           unwinds stacks on its way to the catcher.
         * @param sysMesg  The system error message to be logged for this 
         *            exception. If the exception is to be caught 
         *            outside the scope of the function where the 
         *            exception was constructed, then 'sysMesg' should 
         *            not be a stack variable. Stack variables are 
         *            lost as the exception unwinds stacks on its way 
         *            to the catcher.
         * @param fileName The source file containing the code throwing the
         *                 exception - can be set using the cpp 
         *                 macro '__FILE__'.  
         * @param lineNum  The line number in the source file where the 
         *                 exception is created - should use the cpp
         *                 macro '__LINE__'.
         * @return none.
         */
        SystemException( const char * mesg,
                         const char * sysMesg = strerror( errno ),
                         const char * fileName = __FILE__, 
                         const int    lineNum = __LINE__ );

        /*!
         * @brief Constructor - creates an instance of class SystemException. 
         *
         * @param message   An object of class ostringstream containing
         *                 the message to be logged. If the exception is 
         *                 to be caught outside the scope of the function 
         *                 where the exception was constructed, then 
         *                 'message' should not be a stack variable. 
         *                 Stack variables are lost as the exception 
         *           unwinds stacks on its way to the catcher.
         * @param sysMessage  The system error message to be logged for this 
         *            exception. If the exception is to be caught 
         *            outside the scope of the function where the 
         *            exception was constructed, then 'sysMesg' should 
         *            not be a stack variable. Stack variables are 
         *            lost as the exception unwinds stacks on its way 
         *            to the catcher.
         * @param fileName The source file containing the code throwing the
         *                 exception - can be set using the cpp 
         *                 macro '__FILE__'.  
         * @param lineNum  The line number in the source file where the 
         *                 exception is created - should use the cpp
         *                 macro '__LINE__'.
         * @return none.
         */
        SystemException( std::ostringstream & message,
                         const char *         sysMessage = strerror( errno ),
                         const char *         fileName = __FILE__,
                         const int            lineNum = __LINE__ );

        /*! 
         * @brief Constructor - creates an instance of class SystemException. 
         *
         * @param message   An object of class std::ostringstream containing
         *                 the message to be logged. If the exception is 
         *                 to be caught outside the scope of the function 
         *                 where the exception was constructed, then 
         *                 'message' should not be a stack variable. 
         *                 Stack variables are lost as the exception 
         *           unwinds stacks on its way to the catcher.
         * @param sysMessage   An object of class ostringstream containing
         *                 the system error message to be logged. If the 
         *                 exception is to be caught outside the scope of 
         *                 the function where the exception was constructed, 
         *                 then 'sysMessage' should not be a stack variable. 
         *                 Stack variables are lost as the exception 
         *           unwinds stacks on its way to the catcher.
         * @param fileName The source file containing the code throwing the
         *                 exception - can be set using the cpp 
         *                 macro '__FILE__'.  
         * @param lineNum  The line number in the source file where the 
         *                 exception is created - should use the cpp
         *                 macro '__LINE__'.
         * @return none.
         */
        SystemException( std::ostringstream &     message,
                         const std::ostringstream sysMessage = std::ostringstream( strerror( errno ) ),
                         const char *             fileName = __FILE__,
                         const int                lineNum = __LINE__ );

        /*!
         * @brief Constructor - creates an instance of class SystemException. 
         *
         * @param istring  An object of class string containing
         *                 the message to be logged. If the exception is 
         *                 to be caught outside the scope of the function 
         *                 where the exception was constructed, then 
         *                 'istring' should not be a stack variable. 
         *                 Stack variables are lost as the exception 
         *           unwinds stacks on its way to the catcher.
         * @param sysMessage   An object of class ostringstream containing
         *                 the system error message to be logged. If the 
         *                 exception is to be caught outside the scope of 
         *                 the function where the exception was constructed, 
         *                 then 'sysMessage' should not be a stack variable. 
         *                 Stack variables are lost as the exception 
         *           unwinds stacks on its way to the catcher.
         * @param fileName The source file containing the code throwing the
         *                 exception - can be set using the cpp 
         *                 macro '__FILE__'.  
         * @param lineNum  The line number in the source file where the 
         *                 exception is created - should use the cpp
         *                 macro '__LINE__'.
         * @return none.
         */
        SystemException( std::string &     istring,
                         const std::string sysMessage = strerror( errno ),
                         const char *      fileName = __FILE__,
                         const int         lineNum = __LINE__ );

        /*!
         * @brief Destructor for SystemException.
         */
        virtual ~SystemException( ) throw( );

        SystemException & operator=( const SystemException & rhs );

        /*!
         * @brief gets error message set within this instance.
         * 
         * getSysMessage - gets error message set within this instance of 
         * class SystemException. This message may be logged when the 
         * exception is caught. sysMesg__ is set using strerror( ) in 
         * the constructor.
         *
         * @return error message as a constant char*.
         */
        virtual const char * getSysMessage( ) const;

        virtual ::std::string getLogString( ) const;
        
    protected:
        explicit SystemException( );

    private:
        void clearSysMesg( );
        
        void setSysMesg( const ::std::string & sysMesg );

        const char * sysMesg_;
        bool         weAllocatedSysMesg_;
};  // class SystemException


}  // namespace carma::util
}  // namespace carma


//! @brief Trick to get the file name and line number passed to the exception
//!        handler.
//!
//! Constructs an instance of carma::util::SystemException using argument
//! @a msg as the message parameter to the constructor and automagically
//! generating the system message, file and line number parameters to the
//! constructor.
//!
//! @hideinitializer
#define CARMA_SYSTEM_EXCEPTION(msg) carma::util::SystemException((msg), strerror(errno), __FILE__, __LINE__)


#endif    // CARMA_UTIL_SYSTEM_EXCEPTION_H
