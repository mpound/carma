#ifndef CARMA_UTIL_ERROR_EXCEPTION_H
#define CARMA_UTIL_ERROR_EXCEPTION_H

/**
 * @file
 * Exception class for errors
 *
 * @author: Steve Scott
 *          Original: 08 Oct, 2002
 *          Based on Error.h by Brian Glendenning
 *          and BaseException.h by N. S. Amarnath
 * 
 * 
 * This class allocates memory to form the final message and is therefore
 * subject to the hazard of running out of resources. If this happens,
 * with a new exception being thrown in the constructor of an exception,
 * then the result is an uncaught exception. 
 * You are advised to use carma::util::BaseException if a
 * safer exception is desired. 
 * <br>
 *                 
 * We use some tricks to ensure that the file name and line number of the
 * location where the exception is created is captured by the exception 
 * handler. 
 * The actual constructor uses three parameters, but most common use will
 * be via the CARMA_ERROR macro, defined at the end of this file. 
 * This macro uses only a single parameter 
 * thus allowing it to
 * get the filename and line number at compile time. 
 * <br>
 * The full error message that is created always has the file and line number 
 * preceeding the user supplied message.
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include <iosfwd>

#include <log4cpp/Priority.hh>

#include "carma/util/BaseException.h"


namespace carma {
    namespace util {

/**
 * Exception class for errors
 * The exception comes with a text string that can be printed or logged.
 */
class ErrorException: public BaseException {
public:
    
    /**
     * Constructor
     * @param msg      The message for this exception. 
     * @param filename The source file containing the code throwing the
     *                 exception. 
     * @param lineNo   The line number in the source file where the 
     *                 exception is created.
     */
    ErrorException(const std::string& msg, const char* filename, int lineNo);


    /**
     * Constructor
     * @param msg      The message for this exception.  Make sure that the
     *                 stream has been terminated with "<< ends";
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the 
     *                 cpp macro '__FILE__'.  
     * @param lineNo   The line number in the source file where the 
     *                 exception is created. Can be set using the 
     *                 cpp macro '__LINE__'.
     */
    ErrorException(const std::ostringstream& msg, 
            const char* filename, int lineNo);


    /**
     * Copy constructor
     * Very important for an exception, as it is actually a copy of the
     * exception that is transported up to the catch();
     */
    ErrorException(const ErrorException& errorException);

    /**
     * Destructor
     */
    virtual ~ErrorException() throw();


    ErrorException & operator=( const ErrorException & rhs );

    /**
     * Get the error message; 
     * overrides BaseException & std::exception.what()
     */
    virtual const char* what() const throw();


    virtual ::std::string getLogString( ) const;


    /**
     * Report error to standard err.
     * Reports error to standard error by printing the
     * error message, filename and line number.
     */
    void report() const;

    /**
     * Log the exception.
     * @param priority log4cpp priority of error.
     */
    void log(log4cpp::Priority::PriorityLevel priority) const;

    /**
     * Get the full error message, including line number and file name
     * @return full error message
     */
    std::string getErrorMessage() const ;

protected:
    /**
     *  Default constructor
     */
    explicit ErrorException( );

private:
    void swap( ErrorException & rhs );

    void buildErrorMsg( const char * filename );

    const char * errorMsg_;  // Full message w/line# and file name
};

} }  // End namespace carma::util  


/**
 * @relatesalso std::exception
 * @relatesalso carma::util::BaseException
 * @brief Insert (i.e. output) the error message from any ::std::exception into
 *        an output stream.
 *
 * Typical usage is like so:
 * @code
 * ::std::cout << myStdException << ::std::endl;
 * @endcode
 *
 * @param os
 *        The output stream to insert the message into.
 *
 * @param error
 *        The exception to get the message from.
 *
 * @return The @p os output stream parameter so that stream insertions
 *         can be chained in the usual C++ way (as shown in the example).
 */
::std::ostream & operator<<( ::std::ostream &         os,
                             const ::std::exception & error );


//! @brief Trick to get the file name and line number passed to the exception
//!        handler.
//!
//! @hideinitializer
#define CARMA_ERROR(y)	CARMA_EXCEPTION(carma::util::ErrorException,(y))

// Macros added by EML to allow error messages to be contructed inline

#define ThrowCarmaError(text) \
{\
  std::ostringstream _macroOs; \
  _macroOs << text;\
  throw CARMA_ERROR(_macroOs.str());\
}

#define ThrowCarmaUserException(text) \
{\
  std::ostringstream _macroOs; \
  _macroOs << text;\
  throw CARMA_EXCEPTION( carma::util::UserException, _macroOs.str().c_str() );\
}

#define CARMA_ASSERT( assertion )                                           \
    do {                                                                    \
        if ( !(assertion) )                                                 \
            throw CARMA_ERROR( #assertion );                                \
    } while ( false )

// ADB: Throw an arbitrary exception ala Erik's methods above.
// Note that I 'swallow the semicolon' with the do while loop
// to protect against a common macroism.  Google it for details.
#define ThrowCarmaException(exception, text)                 \
    do {                                                     \
        std::ostringstream _macroOs;                         \
        _macroOs << text;                                    \
        throw CARMA_EXCEPTION( exception, _macroOs.str() );  \
    } while ( false )      

// ADB: Create a typed derivative error exception 
// sans boilerplate.  Note the derived exception will 
// live in the namespace this macro is called in.
#define MAKE_DERIVED_ERROR_EXCEPTION(T)             \
                                                    \
class T : public carma::util::ErrorException {      \
public:                                             \
                                                    \
    T( const std::string & os,                      \
       const char * filename,                       \
       const int lineNumber) :                      \
        carma::util::ErrorException(                \
            ( std::string )( #T ) + ": " + os,      \
            filename,                               \
            lineNumber)                             \
    { };                                            \
                                                    \
    T( const std::ostringstream & os,               \
       const char * filename,                       \
       const int lineNumber) :                      \
        carma::util::ErrorException(                \
            ( std::string )( #T ) + ": " + os.str(),\
            filename,                               \
            lineNumber )                            \
    { };                                            \
                                                    \
    virtual ~T() throw ()                           \
    { };                                            \
                                                    \
};

#endif // CARMA_UTIL_ERROR_EXCEPTION_H
