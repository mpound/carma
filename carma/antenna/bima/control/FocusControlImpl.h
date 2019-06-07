/**
 * @file
 * BIMA FocusControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.10 $
 * $Id: FocusControlImpl.h,v 1.10 2013/02/20 18:07:20 friedel Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_FOCUSCONTROLIMPL_H
#define CARMA_ANTENNA_BIMA_FOCUSCONTROLIMPL_H

#include "carma/antenna/bima/RxClient.h"
#include "carma/corba/corba.h"
#include "carma/antenna/bima/SharedMemory.h"

namespace log4cpp {
    class Category;
}

namespace carma
{
  namespace antenna
  {
    namespace bima
    {

      class Configuration;

      /**
       * CORBA FocusControl implementation.
       */
      class FocusControlImpl:
	public RxClient
      {
	public:

	  /**
	   * Constructor
	   */
	  FocusControlImpl
	    (
	     Configuration &config
	    );

	  /**
	   * Destructor
	   */
	  ~FocusControlImpl();

	  void setX(::CORBA::Float position);

	  void setX(::CORBA::Float position, ::CORBA::ULong seqNo);

	  // These calls are ignored by bima antennas
	  void setY(::CORBA::Float position);
	  void setY(::CORBA::Float position, ::CORBA::ULong seqNo);

	  void setZ(::CORBA::Float position);
	  void setZ(::CORBA::Float position, ::CORBA::ULong seqNo);
	  void setZoffset(::CORBA::Float offset, ::CORBA::ULong seqNo);

	  void doZTracking(::CORBA::Boolean tracking);
	  void doZTracking(::CORBA::Boolean position, ::CORBA::ULong seqNo);

	private:

	  log4cpp::Category &log_;
	  Configuration &_config;
	  float _tmmFocusPosition;
	  SharedMemory *bimaShm;
      };
    } // End namespace bima
  } // End namespace antenna
} // End namespace carma

#endif // CARMA_ANTENNA_BIMA_FOCUSCONTROLIMPL_H

