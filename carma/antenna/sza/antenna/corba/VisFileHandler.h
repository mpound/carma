// $Id: VisFileHandler.h,v 1.1 2010/10/19 21:09:25 eml Exp $

#ifndef SZA_ANTENNA_CORBA_VISFILEHANDLER_H
#define SZA_ANTENNA_CORBA_VISFILEHANDLER_H

/**
 * @file VisFileHandler.h
 * 
 * Tagged: Wed Sep 16 16:14:34 PDT 2009
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/10/19 21:09:25 $
 * 
 * @author username: Command not found.
 */
#include "carma/antenna/sza/antenna/corba/CarmaFileHandler.h"

namespace sza {
  namespace antenna {
    namespace corba {

      class VisFileHandler : public CarmaFileHandler {
      public:

	/**
	 * Constructor.
	 */
	VisFileHandler(std::string dir, bool wideband=false);

	/**
	 * Destructor.
	 */
	virtual ~VisFileHandler();

	void setWideband(bool wideband);

	bool isFileType(sza::util::String& str);

      private:

	bool wideband_;

      }; // End class VisFileHandler

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_VISFILEHANDLER_H
