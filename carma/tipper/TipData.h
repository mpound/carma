/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Kim Drongesen, ported by Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.3 $
 * $Date: 2011/12/22 20:08:39 $
 * $Id: TipData.h,v 1.3 2011/12/22 20:08:39 mpound Exp $
 */



#ifndef CARMA_TIPPER_TIPDATA_H
#define CARMA_TIPPER_TIPDATA_H

#include <cstring>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Logger.h"
#include "carma/util/Time.h"

namespace carma
{
  namespace tipper
  {
    class TipData
    {

      public:
        TipData();

        double _mjdOfReading;
        int _airMassNumber;
        float _tipAngle;

        static const int _numChannels = 16;
        static const int _numReadings = 10;

        int _tipNumber;
        double _avgSigRef, _scaledAvgSigRef;
        double _rmsSigRef, _scaledRmsSigRef;
        float _channelData[_numChannels];
        float _scaledChannelData[_numChannels];

        void convertAndScaleChannels()
        {
          // Readings are raw, need to be converted to mVolts and scaled


          // mVolt Conversion
          for ( int i = 0; i < _numChannels; i++ )
          { 
            _channelData[i] = (float)(5.0 * (_channelData[i] / 16.0 ));
          }

          // convert average and rms of channel 1
          _avgSigRef = 5.0 * (_avgSigRef/16.0);
          _rmsSigRef = 5.0 * (_rmsSigRef/16.0);

          // scale all values
          _scaledAvgSigRef = _avgSigRef/20.0;
          _scaledRmsSigRef = _rmsSigRef/20.0;
          _scaledChannelData[0] = (float)(_channelData[0]/20.0);     // signal ref
          _scaledChannelData[1] = (float)(_channelData[1]/200.0);    // hot ref
          _scaledChannelData[2] = (float)(_channelData[2]/2.0)+5000; // tot power ref
          _scaledChannelData[3] = (float)(_channelData[3]/100.0);    // ref temp
          _scaledChannelData[4] = (float)(_channelData[4]/100.0);    // hot temp
          _scaledChannelData[5] = (float)(_channelData[5]/100.0);    // amb temp

          // temp probe tends to malfunction, this is a safeguard
          if ((  _scaledChannelData[5] < -40.0 ) || ( _scaledChannelData[5] > 50.0 ))
          {
            _scaledChannelData[5] = (float) 0.0;
          }

          _scaledChannelData[6] = (float)(_channelData[6]/100.0);    // chassis temp
          _scaledChannelData[7] = (float)(_channelData[7]/-1000.0);  // mixer curr
          _scaledChannelData[8] = (float)(_channelData[8]/-1000.0);  // tripler curr
          _scaledChannelData[9] = (float)(_channelData[9]/10000.0);  // gunn curr
          _scaledChannelData[10] = (float)(_channelData[10]/500.0);  // btty volt
          // not passed to monitor system because it is has no physical 
          // connection.  See bug #777
          //_scaledChannelData[11] = (float)(_channelData[11]/50.0);   // zenith angle
          _scaledChannelData[12] = (float)(_channelData[12]/1000.0); // supply curr
          _scaledChannelData[13] = (float)(_channelData[13]/13.89);  // wind dir
          _scaledChannelData[14] = (float)(_channelData[14]/25.0);   // wind speed
          _scaledChannelData[15] = (float)(_channelData[15]/500.0);  // station supply volt
        }

        ::std::string toString()
        {
          ::std::string outStr;
          char rawbuf[80];

          ::snprintf( rawbuf, 79, "%4d %2d %4.1f ", _tipNumber, _airMassNumber, _tipAngle );
          outStr = ::std::string(rawbuf);
          // getDateTimeString still has formmating bug, so, using two calls
          outStr += ::carma::util::Time::getNonBuggyByDesignDateTimeString( (double)(_mjdOfReading),
              "%d%b%y/%H:%M:%S " );
          ::snprintf( rawbuf, 79, "%0.5f %6.1f %4.2f ", _mjdOfReading, _scaledAvgSigRef,
              _scaledRmsSigRef );
          outStr += ::std::string(rawbuf);

          ::snprintf( rawbuf, 79, "%6.1f ", _scaledChannelData[0] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%5.2f ", _scaledChannelData[1] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%4.0f ", _scaledChannelData[2] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%4.1f ", _scaledChannelData[3] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%4.1f ", _scaledChannelData[4] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%5.1f ", _scaledChannelData[5] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%4.1f ", _scaledChannelData[6] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%5.2f ", _scaledChannelData[7] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%5.2f ", _scaledChannelData[8] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%5.2f ", _scaledChannelData[9] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%4.1f ", _scaledChannelData[10] );
          outStr += ::std::string(rawbuf);
          ::snprintf( rawbuf, 79, "%4.2f", _scaledChannelData[12] );
          outStr += ::std::string(rawbuf);

          return outStr;
        }

      protected:

      private:



    }; // class TipData
  } // namespace antenna
} // namespace carma

::std::ostream& operator<<( ::std::ostream& os,
    ::carma::tipper::TipData &tReading );

#endif // CARMA_TIPPER_TIPDATA_H

// vim: set expandtab sw=2 ts=2 cindent :
