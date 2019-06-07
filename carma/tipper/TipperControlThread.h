/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Kim Drongesen, ported by Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.7 $
 * $Date: 2012/01/13 01:08:28 $
 * $Id: TipperControlThread.h,v 1.7 2012/01/13 01:08:28 iws Exp $
 */



#ifndef CARMA_TIPPER_TIPPERCONTROL_H
#define CARMA_TIPPER_TIPPERCONTROL_H

// UNIX system related
#include <termios.h>

#include <string>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/Logger.h"
#include "carma/util/Time.h"
#include "carma/util/ScopedPthreadMutexLock.h"

#include "carma/tipper/TipData.h"
#include "carma/monitor/OpacityMonitorSubsystem.h"

namespace carma
{
  namespace tipper
  {
    class TipperControlThread
    {

      public:
        TipperControlThread( ::std::string device, ::std::string dataDir, 
            double autoWriterDelayInS, bool emulate );

        typedef enum
        {
          STEP,
          TIP_STARTED,
          PORT_ERROR,
          DIRECT,
          GO_ZENITH,
          IDLE,
          OPEN_PORT,
          INTEGRATING
        } TipperStatus;

        typedef enum
        {
          CLOCKWISE,
          COUNTER_CLOCKWISE
        } TipperDirection;

        struct TipperAngles
        {
          int steps;
          float angle;
        };

        static const int _numAirmasses = 12;
        static const TipperAngles _tipAngles[_numAirmasses];

        void doTip();
        void openPort( ::std::string device );
        void setDirection( TipperDirection dir );
        void goToZenith();
        void doStep( int numsteps );
        void readAllChannels( TipData &tipData );
        void convertAndScaleChannels( TipData &tipData );

      protected:
        void sendTipperCommand();
        void sendTipperPrefix();
        void doMeasurement( TipData *tipData );
        void getCurrentFileName( ::std::string &theName );
        void writeToFile( TipData *tipData );
        void readOneChannel( char channel, float &chanData );
        void doAvgSignalRef( TipData &tipData );

        TipperStatus getStatus() { return _status; };
        void setStatus( TipperStatus status );

        void openPort();
        void setPortTimeout( int seconds );
        void clearReadBuffer();
        void closePort();

        enum Parity {
          EVEN,
          ODD
        };

        void setParity( enum Parity );
        void write( const char * buff, size_t count );

      private:

        ::std::string _portDevName;
        ::std::string _dataDir;
        int _numTips;
        double _autoWriterDelayInS;
        ::std::auto_ptr< ::carma::monitor::OpacityMonitorSubsystem > _monitor;
        bool _emulate;
        log4cpp::Category &_logger;

        char _loDataByte, _command[4];
        int _portFD;
        struct termios _tios;
        struct termios _oldtios;

        TipperStatus _status;

        carma::util::PthreadMutex _tipMeasurementLock;

    }; // class TipperControlThread
  } // namespace antenna
} // namespace carma

::std::ostream& operator<<( ::std::ostream& os,
    ::carma::tipper::TipperControlThread& tControl );

#endif // CARMA_TIPPER_TIPPERCONTROL_H

// vim: set expandtab sw=2 ts=2 cindent :
