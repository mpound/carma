/** @file
 * Declarations of carma::canbus exceptions.   
 *  
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2013/01/30 19:54:06 $
 * $Id: exceptions.h,v 1.2 2013/01/30 19:54:06 abeard Exp $
 */

#ifndef CARMA_CANBUS_EXCEPTIONS_H
#define CARMA_CANBUS_EXCEPTIONS_H

// C++ Standard Library includes 
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

// Carma includes
#include "carma/util/ErrorException.h"

// Alien includes
#include "log4cpp/Priority.hh"

#ifdef SystemException
#undef SystemException
#endif

namespace carma {
  namespace canbus {

      /** 
       * @class SystemException 
       * @brief System Exception class
       *
       * This error signals failure of a system dependent interface.
       * It is usually thrown during failure to open or write to a 
       * system device such as /dev/null.
       */
      MAKE_DERIVED_ERROR_EXCEPTION(SystemException);
      
      /**
       * @class TxBufferFullException
       * @brief TxBuferFullException class
       * 
       * Thrown when a Janz onboard tx message queue has filled up.
       * This is generally a recoverable error and almost always signals
       * that a bus is disconnected or does not have a module attached to it.
       * It differs from carm::canbus::BufferOverflowException in that it is
       * specific to the Janz VMOD-ICAN onboard message buffer rather than
       * other software defined buffers.
       */
      MAKE_DERIVED_ERROR_EXCEPTION(TxBufferFullException);
      
      /**
       * @class BadDataSizeException
       * @brief Exception class for invalid data vector size
       *
       * The carma::canbus library contains many routines for packing and 
       * unpacking raw data from a CAN message.  CAN messages have a maximum
       * of 8 bytes.  This exception will be thrown, if for some reason the
       * data from a CAN message is larger than expected (exceeds 8 bytes),
       * is not large enough to perform the desired task (e.g. you are 
       * trying to convert one byte of data to a double), or the desired task
       * will result in a data size > 8 bytes (e.g. you are trying to add a 
       * double to a vector already containing 8 bytes).  This exception
       * generally signals a programming error.
       */
      MAKE_DERIVED_ERROR_EXCEPTION(BadDataSizeException);
      
      /**
       * @class BadParameterException
       * @brief Exception class to signal an invalid parameter
       *
       * This exception is a generic exception to signal a bad parameter
       * of some kind was passed to a function or applicatoin.  It generally 
       * indicates a programming error.
       */
      MAKE_DERIVED_ERROR_EXCEPTION(BadParameterException);
      
      /**
       * @class BufferOverflowException
       * @brief Exception class to indicate write overflows
       *
       * This exception is used to signal that overflows have occured on 
       * a shared memory queue. It is utilized mostly for DirectCan 
       * usage, where it signals that a user is posting messages to the 
       * DirectCan shared memory queues too quickly, or is not retrieving
       * messages from it quickly enough.  See canbus::DirectCan::postMessage
       * and canbus::DirectCan::getMessage.
       */
      MAKE_DERIVED_ERROR_EXCEPTION(BufferOverflowException);
      
      /**
       * @class JanzFailException
       * @brief Exception class to indicate janz interface failure
       *
       * This exception is a generic exception to indicate that some
       * janz dependent interface has failed.  Janz provides two 
       * underlying c interfaces upon which carma::canbus is built. Since
       * these are c interfaces, carma::canbus contains many checks to make 
       * sure that a call was successful.  This exception is thrown upon 
       * failure of one of those calls.  It often will signal a misconfigured
       * system or in rare cases a programming error.
       */
      MAKE_DERIVED_ERROR_EXCEPTION(JanzFailException);

      /**
       * @class PthreadFailException
       * @brief Exception class to indicate pthread interface failure.
       *
       * carma::canbus relies heavily on the c pthread library.  If
       * any pthread interfaces fail, this exception will be thrown.
       */
      MAKE_DERIVED_ERROR_EXCEPTION(PthreadFailException);
          
    } // namespace canbus
} // namesapce carma

#endif
