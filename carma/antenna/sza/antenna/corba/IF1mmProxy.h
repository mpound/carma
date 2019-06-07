// $Id: IF1mmProxy.h,v 1.3 2013/07/08 21:53:14 eml Exp $

#ifndef SZA_ANTENNA_CORBA_IF1MMPROXY_H
#define SZA_ANTENNA_CORBA_IF1MMPROXY_H

/**
 * @file IF1mmProxy.h
 *
 * Tagged: Fri Jul 24 14:11:43 PDT 2009
 *
 * @version: $Revision: 1.3 $, $Date: 2013/07/08 21:53:14 $
 *
 * @author username: Command not found.
 */
#include "carma/antenna/sza/antenna/corba/IFProxy.h"

namespace sza {
  namespace antenna {
    namespace corba {

      class IF1mmProxy : public IFProxy {
      public:

	/**
	 * Constructor.
	 */
	IF1mmProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	virtual ~IF1mmProxy();

	// Select this receiver as the IF input

	virtual void selectRx();

      }; // End class IF1mmProxy

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_IF1MMPROXY_H
