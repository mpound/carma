/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Kim Drongesen, ported by Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.5 $
 * $Date: 2011/09/01 18:19:13 $
 * $Id: TipperControlImpl.h,v 1.5 2011/09/01 18:19:13 abeard Exp $
 */



#ifndef CARMA_TIPPER_TIPPERCONTROLIMPL_H
#define CARMA_TIPPER_TIPPERCONTROLIMPL_H

// UNIX system related
#include <termios.h>

// CORBA related
#include "carma/corba/corba.h"

// CARMA includes
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/Logger.h"
#include "carma/util/Time.h"

#include "carma/tipper/TipperControlThread.h"

namespace carma
{
  namespace tipper
  {
    class TipperControlImpl
    {

      public:
        TipperControlImpl( TipperControlThread &tipperControlThread );
        virtual ~TipperControlImpl();

        virtual void doTip();

      private:
        log4cpp::Category &_logger;
        TipperControlThread &_tipperControlThread;
        ::std::string _portDevName;

    }; // class TipperControlImpl
  } // namespace tipper
} // namespace carma

::std::ostream& operator<<( ::std::ostream& os,
    ::carma::tipper::TipperControlImpl& tControl );

#endif // CARMA_TIPPER_TIPPERCONTROLIMPL_H

// vim: set expandtab sw=2 ts=2 cindent :
