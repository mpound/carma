/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.9 $
 * $Id: LOControlImpl.h,v 1.9 2012/02/21 21:06:58 abeard Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_LOCONTROLIMPL_H
#define CARMA_ANTENNA_BIMA_LOCONTROLIMPL_H

#include "carma/antenna/bima/RxClient.h"
#include "carma/antenna/bima/control/IDLutils.h"

namespace log4cpp {
  class Category;
}

namespace carma
{
  namespace antenna
  {
    namespace bima
    {

      /**
       * LOControlImpl Corba control class.
       */
      class LOControlImpl :
	public RxClient
      {
	public:

	  /**
	   * Constructor.
	   */
	  LOControlImpl( Configuration &config );

	  /**
	   * Destructor.
	   */
	  virtual ~LOControlImpl();

	  void setFrequency(double yigFreq, double LOfreq);

	  void toggleSweep( bool on );

	  void toggleYigSweep( bool on );

      void setYigFrequency( CORBA::Double yigFreq );

	  void setLoFrequency(double Frequency);

	  void setLoTerminatorAttenuation(::CORBA::UShort atten);

  private:

	  log4cpp::Category &log_;
	  Configuration &_config;

      }; // End class LOControlImpl

    } // bima
  } // antenna
} // End namespace carma

#endif // CARMA_ANTENNA_BIMA_LOCONTROLIMPL_H
