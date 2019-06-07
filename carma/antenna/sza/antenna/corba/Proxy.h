// $Id: Proxy.h,v 1.6 2012/11/27 22:58:02 abeard Exp $

#ifndef SZA_ANTENNA_CORBA_PROXY_H
#define SZA_ANTENNA_CORBA_PROXY_H

/**
 * @file Proxy.h
 * 
 * Tagged: Fri Jul 24 11:34:30 PDT 2009
 * 
 * @version: $Revision: 1.6 $, $Date: 2012/11/27 22:58:02 $
 * 
 * @author username: Command not found.
 */
#ifdef SystemException
#undef SystemException
#endif

#include "carma/antenna/sza/antenna/corba/Corba.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Mutex.h"
#include "carma/szautil/PointingMode.h"

#include "carma/util/ErrorException.h"

namespace sza {
  namespace antenna {
    namespace control {
      class AntennaMaster;
   }
  }
}

namespace sza {
  namespace antenna {
    namespace corba {

      class SzaShareCorba;

      class Proxy {
      public:

	/**
	 * Constructor.
	 */
	Proxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	virtual ~Proxy();

      protected:

	sza::antenna::control::AntennaMaster* parent_;

	SzaShareCorba* share_;

	unsigned long seq_;

	sza::util::Mutex seqGuard_;

	unsigned long sequenceNumber();

	void setSkyOffsets(unsigned long seq=0);
	virtual void setFlexure(sza::util::PointingMode::Type model, sza::util::Angle& sFlex, sza::util::Angle& cFlex);
	virtual void setFlexure();

      }; // End class Proxy

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_PROXY_H
