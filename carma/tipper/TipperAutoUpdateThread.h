/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.1 $
 * $Date: 2008/12/05 11:15:37 $
 * $Id: TipperAutoUpdateThread.h,v 1.1 2008/12/05 11:15:37 colby Exp $
 */



#ifndef CARMA_TIPPER_TIPPERAUTOUPDATER_H
#define CARMA_TIPPER_TIPPERAUTOUPDATER_H

// UNIX system related
#include <termios.h>

#include <string>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "log4cpp/Category.hh"

#include "carma/tipper/TipperControlThread.h"



namespace carma
{
  namespace tipper
  {
    class TipperAutoUpdateThread
    {

      public:
        TipperAutoUpdateThread( TipperControlThread &tipperControl,
            int update = 300 ); // every 5 minutes
        static void thread( TipperAutoUpdateThread &This );
        void run();


      private:
        log4cpp::Category &_log;
        TipperControlThread &_tipperControl;
        int _update;


    }; // class TipperAutoUpdateThread
  } // namespace antenna
} // namespace carma

::std::ostream& operator<<( ::std::ostream& os,
    ::carma::tipper::TipperAutoUpdateThread& tAutoUpdate );

#endif // CARMA_TIPPER_TIPPERAUTOUPDATER_H

// vim: set expandtab sw=2 ts=2 cindent :
