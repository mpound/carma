// $Id: Connection.h,v 1.1 2010/12/13 21:06:29 eml Exp $

#ifndef SZA_UTIL_CONNECTION_H
#define SZA_UTIL_CONNECTION_H

/**
 * @file Connection.h
 * 
 * Tagged: Fri Dec  4 13:28:34 PST 2009
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:29 $
 * 
 * @author tcsh: username: Command not found.
 */
#include <string>

namespace sza {
  namespace util {

    class Connection {
    public:

      /**
       * Constructor.
       */
      Connection();

      /**
       * Destructor.
       */
      virtual ~Connection();

      bool isReachable(std::string host);

    private:

    }; // End class Connection

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CONNECTION_H
