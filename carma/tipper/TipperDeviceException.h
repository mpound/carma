#ifndef CARMA_TIPPER_DEVICE_EXCEPTION_H
#define CARMA_TIPPER_DEVICE_EXCEPTION_H

/**
 * @file
 * Exception class for Phase Monitor Device errors
 * Adapted from ErrorException
 *                
 * $CarmaCopyright$
 *
 */

#include <iostream>
#include <string>
#include <iosfwd>

#include "carma/util/ErrorException.h"


namespace carma
{
  namespace tipper
  {
    class TipperDeviceException : public carma::util::ErrorException
    {
      public:

	/**
	 * create a TipperDeviceException 
	 * Suggested usage CARMA_EXCEPTION(TipperDeviceException, string msg);
	 * @param msgstr   an output string containing the message.
	 * @param filename The source file containing the code throwing the
	 *                 exception. Can be set using the
	 *                 cpp macro '__FILE__'.
	 * @param lineNo   The line number in the source file where the
	 *                 exception is created. Can be set using the
	 *                 cpp macro '__LINE__'.
	 */
	TipperDeviceException(
	    const std::string & msgstr, 
	    const char *        filename,
	    int                 lineNo );

	/**
	 * create a TipperDeviceException 
	 * Suggested usage CARMA_EXCEPTION(TipperDeviceException, ostringstream msg);
	 * @param msgstr   an output string stream contining the message
	 * @param filename The source file containing the code throwing the
	 *                 exception. Can be set using the
	 *                 cpp macro '__FILE__'.
	 * @param lineNo   The line number in the source file where the
	 *                 exception is created. Can be set using the
	 *                 cpp macro '__LINE__'.
	 */
	TipperDeviceException(
	    const std::ostringstream & msgstr, 
	    const char *               filename,
	    int                        lineNo );

	/**
	 * copy a TipperDeviceException
	 * @param ex   the exception to copy
	 */
	TipperDeviceException(const TipperDeviceException & ex);

    };


  }
} // End namespace carma::tipper


#endif // CARMA_TIPPER_DEVICE_EXCEPTION_H
