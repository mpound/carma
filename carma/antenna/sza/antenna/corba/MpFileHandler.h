// $Id: MpFileHandler.h,v 1.1 2010/10/19 21:09:24 eml Exp $

#ifndef SZA_ANTENNA_CORBA_MPFILEHANDLER_H
#define SZA_ANTENNA_CORBA_MPFILEHANDLER_H

/**
 * @file MpFileHandler.h
 * 
 * Tagged: Tue May  4 12:00:50 PDT 2010
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/10/19 21:09:24 $
 * 
 * @author username: Command not found.
 */
#include "carma/antenna/sza/antenna/corba/CarmaFileHandler.h"

namespace sza {
  namespace antenna {
    namespace corba {

      class MpFileHandler : public CarmaFileHandler {
      public:

	/**
	 * Constructor.
	 */
	MpFileHandler(std::string dir);

	/**
	 * Destructor.
	 */
	virtual ~MpFileHandler();

      private:
      }; // End class MpFileHandler

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_MPFILEHANDLER_H
