#ifndef CARMA_ANTENNA_BIMA_SHAREDMEMORY_EXCEPTION_H
#define CARMA_ANTENNA_BIMA_SHAREDMEMORY_EXCEPTION_H

/**
 * @file
 * Exception class for Shared Memory errors specific to BIMA
 * Adapted from ErrorException
 *
 * @author: Colby Gutierrez-Kraybill
 *          Original: 08 Oct, 2002
 *          Based on ErrorException.h by Steve Scott
 *          which is based on Error.h by Brian Glendenning
 *          and BaseException.h by N. S. Amarnath
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
 * @todo The setMessage() methods inherited from BaseException will not
 * work properly with this class (if stack data is used it will disappear
 * when the exception is thrown). 
 *                
 * $CarmaCopyright$
 *
 */

#include <string>
#include <iosfwd>

#include "carma/util/BaseException.h"


namespace carma
{
 namespace antenna
 {
  namespace bima
  {

/**
 * Exception class for errors
 * The exception comes with a text string that can be printed or logged.
 */
class SharedMemoryException: public carma::util::BaseException {
public:

    /**
     * Constructor
     * @param msg      The message for this exception. 
     * @param filename The source file containing the code throwing the
     *                 exception. 
     * @param lineNo   The line number in the source file where the 
     *                 exception is created.
     */
    SharedMemoryException(const std::string& msg, const char* filename, int lineNo);


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
    SharedMemoryException(const std::ostringstream& msg, 
            const char* filename, int lineNo);


    /**
     * Constructor
     * @param name     The shared memory value name..
     */
    SharedMemoryException( const char *name );

    /**
     * Copy constructor
     * Very important for an exception, as it is actually a copy of the
     * exception that is transported up to the catch();
     */
    SharedMemoryException(const SharedMemoryException& shmException);

    /**
     * Destructor
     */
    virtual ~SharedMemoryException() throw();


    /**
     * Get the error message; 
     * overrides BaseException & std::exception.what()
     */
    virtual const char* what() const throw();


    /**
     * Report error to standard err.
     * Reports error to standard error by printing the
     * error message, filename and line number.
     */
    void report() const;

    /**
     * Get the full error message, including line number and file name
     * @return full error message
     */
    std::string getErrorMessage() const ;

    void setName( const char *name );
    const char *getName();

    std::string *_name;

protected:
    /**
     *  Default constructor
     */
    SharedMemoryException();

private:
    char* errorMsg_;  // Full message w/line# and file name
    void makeCharStrings(const char* filename);
    // Make a new copy of a character string
    char* makeCopy(const char* string) const;

    


};

} } } // End namespace carma::antenna::bima  


#endif // CARMA_ANTENNA_BIMA_SHAREDMEMORY_EXCEPTION_H
