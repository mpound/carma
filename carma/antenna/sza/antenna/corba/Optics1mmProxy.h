// $Id: Optics1mmProxy.h,v 1.2 2011/05/11 18:14:03 iws Exp $

#ifndef SZA_ANTENNA_CORBA_OPTICS1MMPROXY_H
#define SZA_ANTENNA_CORBA_OPTICS1MMPROXY_H

/**
 * @file Optics1mmProxy.h
 *
 * Tagged: Fri Jul 24 14:22:38 PDT 2009
 *
 * @version: $Revision: 1.2 $, $Date: 2011/05/11 18:14:03 $
 *
 * @author username: Command not found.
 */
#include "carma/antenna/sza/antenna/corba/OpticsProxy.h"

namespace sza {
  namespace antenna {
    namespace corba {

      class Optics1mmProxy : public OpticsProxy {
      public:

	/**
	 * Constructor.
	 */
	Optics1mmProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	virtual ~Optics1mmProxy();

	void selectRx();

      }; // End class Optics1mmProxy

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_OPTICS1MMPROXY_H
