// $Id: ErrHandler.h,v 1.1 2010/12/13 21:06:30 eml Exp $

#ifndef SZA_UTIL_ERRHANDLER_H
#define SZA_UTIL_ERRHANDLER_H

/**
 * @file ErrHandler.h
 * 
 * Tagged: Mon Apr 10 13:32:05 PDT 2006
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:30 $
 * 
 * @author Erik Leitch
 */
#include <sstream>

#define ERR_HANDLER_FN(fn) \
  void (fn)(std::ostringstream& os, const char* fileName, const int lineNumber, const char* functionName, bool isErr, bool isSimple, bool isSysErr)

namespace sza {
  namespace util {

    class ErrHandler {
    public:

      /**
       * Constructor.
       */
      ErrHandler();

      /**
       * Destructor.
       */
      virtual ~ErrHandler();

      static ERR_HANDLER_FN(defaultThrowFn);
      static ERR_HANDLER_FN(defaultReportFn);
      static ERR_HANDLER_FN(defaultLogFn);

      // Install user-defined error handling functions

      static void installThrowFn(ERR_HANDLER_FN(*throwFn));
      static void installReportFn(ERR_HANDLER_FN(*reportFn));
      static void installLogFn(ERR_HANDLER_FN(*logFn));

      static ERR_HANDLER_FN(throwError);
      static ERR_HANDLER_FN(report);
      static ERR_HANDLER_FN(log);

    private:

      static ERR_HANDLER_FN(*throwFn_);
      static ERR_HANDLER_FN(*reportFn_);
      static ERR_HANDLER_FN(*logFn_);

    }; // End class ErrHandler

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ERRHANDLER_H
