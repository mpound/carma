/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.5 $
 * $Date: 2012/02/21 21:06:58 $
 * $Id: BimaDewarRegulationImpl.h,v 1.5 2012/02/21 21:06:58 abeard Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_BIMADEWARREGULATION_H
#define CARMA_ANTENNA_BIMA_BIMADEWARREGULATION_H

#include <stdio.h>

#include <vector>
#include <string>

// System includes
#include <sys/time.h>
#include <unistd.h>

// CARMA includes
#include "carma/corba/corba.h"
#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/services/Table.h"
#include "carma/antenna/bima/DewarRegulation.h"

#include "carma/util/ExceptionUtils.h"




namespace carma
{

  namespace corba {
    class Server;
  }

  namespace antenna
  {
    namespace bima
    {
      class BimaDewarRegulationImpl 
      {

	public:
	  BimaDewarRegulationImpl(
	      DewarRegulation &dereg,
	      bool emulate );

	  ~BimaDewarRegulationImpl();

	  static void thread( BimaDewarRegulationImpl &This );
	  void run();
	  bool isOk();

	  // CORBA Service routines
	  void on();
	  void off();
	  void setPoint( CORBA::Float point );
	  void defrost();
	  void cancelDefrost();

	private:
	  DewarRegulation &_dereg;
	  log4cpp::Category &_log;
	  double _setPoint;
	  bool _emulate;

      carma::corba::Server & _server;

      }; // class BimaDewarRegulationImpl
    } // namespace bima
  } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_BIMADEWARREGULATION_H
