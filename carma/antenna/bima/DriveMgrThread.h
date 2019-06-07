/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.7 $
 * $Date: 2006/04/27 18:06:46 $
 * $Id: DriveMgrThread.h,v 1.7 2006/04/27 18:06:46 colby Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_DRIVEMGRTHREAD_H
#define CARMA_ANTENNA_BIMA_DRIVEMGRTHREAD_H

#include <vector>

// System includes
#include <string>
#include <unistd.h>

// CARMA includes
#include "carma/util/Program.h"
#include "log4cpp/Category.hh"
#include "carma/util/IPQreader.h"
#include "carma/services/Table.h"
#include "carma/antenna/bima/Drives.h"
#include "carma/antenna/bima/DriveCommand.h"
#include "carma/antenna/bima/Configuration.h"

namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class DriveMgrThread 
	    {

	    public:
	      DriveMgrThread( Configuration &config, Drives &drives );

              static void thread( DriveMgrThread &This );
              void run();
              double getAzOffsetRadians();
              double getElOffsetRadians();

              bool isOk();

	    private:
              std::string _name;
              Drives &_drives;
              Configuration &_config;
              static bool _ok;
             
              int _azNewV, _azOldV;
              int _elNewV, _elOldV;
              double _azRampDownMod, _elRampDownMod;
              double _azTarget, _elTarget;
              double _azOffset, _elOffset;
              double _azMountOffset, _elMountOffset;
              double _azApertureOffset, _elApertureOffset;
	      double _lastAzOffsetRads, _lastElOffsetRads;
	      double _lastObsFreq;
              std::string _lastCUEEwhat;
	      time_t _cueeThrottle;
              DriveCommand::CommandType _command;

              log4cpp::Category &_logger;
        
              carma::util::IPQreader<DriveCommand> *_drvReader;
              
	    }; // class DriveMgrThread
	} // namespace bima
    } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_DRIVEMGRTHREAD_H
