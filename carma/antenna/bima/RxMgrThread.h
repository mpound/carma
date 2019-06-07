/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.6 $
 * $Date: 2007/05/29 21:54:01 $
 * $Id: RxMgrThread.h,v 1.6 2007/05/29 21:54:01 colby Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_RXMGRTHREAD_H
#define CARMA_ANTENNA_BIMA_RXMGRTHREAD_H

#include <vector>

// System includes
#include <string>
#include <unistd.h>

// CARMA includes
#include "carma/util/Program.h"
#include "log4cpp/Category.hh"
#include "carma/util/IPQreader.h"
#include "carma/services/Table.h"
#include "carma/antenna/bima/Rx.h"
#include "carma/antenna/bima/LO.h"
#include "carma/antenna/bima/CalWheel.h"
#include "carma/antenna/bima/Secondary.h"
#include "carma/antenna/bima/RxCommand.h"
#include "carma/antenna/bima/Configuration.h"

#define RRGET(V) RCGET( _rxReader, V )

namespace carma
{
  namespace antenna
  {
    namespace bima
    {
      class RxMgrThread 
      {

	public:
	  RxMgrThread
	    (
	     Configuration &config,
	     Rx &rx,
	     LO &lo,
	     Secondary &secondary,
	     CalWheel &calwheel
	    );

	  static void thread( RxMgrThread &This );
	  void run();

	  bool isOk();
	  int rampDown( int oldV, double rampDownModifier );

	private:
	  std::string _name;
	  Configuration &_config;
	  Rx &_rx;
	  LO &_lo;
	  Secondary &_secondary;
	  CalWheel &_calwheel;
	  static bool _ok;
	  int _relockTimer, _relockRetries;

	  RxCommand::CommandType _command;

	  log4cpp::Category &_logger;

          carma::util::IPQreader<RxCommand> *_rxReader;

      }; // class RxMgrThread
    } // namespace bima
  } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_RXMGRTHREAD_H
