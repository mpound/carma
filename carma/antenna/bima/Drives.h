/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.60 $
 * $Date: 2007/11/06 01:42:45 $
 * $Id: Drives.h,v 1.60 2007/11/06 01:42:45 colby Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_DRIVES_H
#define CARMA_ANTENNA_BIMA_DRIVES_H

#include <vector>
#include <cmath>
#include <string>
#include <algorithm>
#include <cctype>


// System includes
#include <unistd.h>
#include <time.h>
#include <sys/time.h>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/services/Table.h"
#include "carma/util/QuadraticInterpolatorNormal.h"
#include "carma/util/Time.h"
#include "carma/antenna/bima/TelemetryClient.h"
#include "carma/antenna/bima/DriveCommand.h"
#include "carma/antenna/bima/Configuration.h"
#include "carma/util/Logger.h"

// This is included so that if we get near the zenith, we can move
// the ambient flap into place to avoid frying the receiver
// with the radar signal from Cloudsat which points straight
// down at the earth and tx's at 94GHz to look at, you guessed
// it, clouds!
#include "carma/antenna/bima/Rx.h"


// This needs to be defined in some globals .h, see Telemetry.h TILTSAMPLES
#define TILTSAMPLES	100

namespace carma
{
  namespace antenna
    {
      namespace bima
        {
          class Drives : public TelemetryClient
            {

            public:
              Drives( Configuration& config, bool checkTelemHost = true );

              typedef enum { UP, DOWN } ElDir;
              typedef enum { CLOCKWISE, COUNTERCLOCKWISE } AzDir;
              typedef enum
              {
                TRACK, CLOSE, SLEW, STOW, SNOW, STOP, DISABLE, HWLIMIT, SWLIMIT, ERROR
              } StateType;
              typedef enum { AZEL, EQUAT, STOWMODE, SNOWMODE, STOPMODE } ModeType;
              typedef enum { ZERO, ADD, SUB } WrapLogic;
              typedef enum { OPTICAL, RADIO } RefractModel;
              typedef enum { KEYENABLE, KEYDISABLE } KeySwitch;
              typedef enum { COMPUTER, MANUAL } ControlSwitch;
            
              bool areFaultBits();
              bool isKeyOn();

              void pegAz( bool peggit );
              void pegEl( bool peggit );

              bool isAzPegged();
              bool isElPegged();

              void pegVelocities( int limit );
              void markAzVel( int vel );
              void markElVel( int vel );
              int getAzVel();
              int getElVel();

              WrapLogic getWrapLogic();
              void setWrapLogic( WrapLogic logic);
              void setOverTheTop( bool overTop );
              bool getOverTheTop();

              StateType getState();
              void setState( StateType state);
              StateType getActiveState();
              void setActiveState( StateType state);

              ModeType getMode();
              void setMode( ModeType mode );

              RefractModel getModel();
              void setModel( RefractModel model );
              float getRefract();
              void setRefract( float refract);

              double getConstantChange();

              double getOneSecRateAz();
              double getOneSecRateEl();
              void markOneSecAzRate( double rate );
              void markOneSecElRate( double rate );

              bool isAzStalled();
              bool isElStalled();
              void markAzStallMJD();
              void markAzStalls( int stalls );
              int getAzStalls();
              void markElStalls( int stalls );
              int getElStalls();
              void markElStallMJD();
              void zeroStallMJD();
              double getAzStallMJD();
              double getElStallMJD();
              void setStallRetries( int cnt );
              int getStallRetries();

              bool onTarget();
              bool onTargetAz();
              bool onTargetEl();

              void slew();
              void stop();
              void stow();
              void setStowPosition( DriveCommand::PositionType );
              DriveCommand::PositionType getStowPosition();
              void snow();
              void servo();
              void toggle();

              void setSoftLimit();
              void setHardLimit();

              double getTargetAz( double mjd = 0);
              double getRawTargetAz();
              double getTargetEl( double mjd = 0);
              double getRawTargetEl();

              void setTargetAz( int whence, double mjd, double az, bool discontinuity );
              void setRawTargetAz( double az );
              void setTargetEl( double mjd, double el, bool discontinuity );
              void setRawTargetEl( double el );

              int getInstantAzResolver();
              int getSmoothedAzResolver();
              double getAzFine();
              int getAzDigVeFilter();
              int getAzDigVeFilterWriteOut();
              int getInstantAzCosEnc();
              int getSmoothAzCosEnc();
              int getInstantAzSinEnc();
              int getSmoothAzSinEnc();
              void setAzCheckDrivePower( bool status );
              bool getAzCheckDrivePower();

              int getInstantElResolver();
              int getSmoothedElResolver();
              double getElFine();
              int getElDigVeFilter();
              int getElDigVeFilterWriteOut();
              int getInstantElCosEnc();
              int getSmoothElCosEnc();
              int getInstantElSinEnc();
              int getSmoothElSinEnc();
              void setElCheckDrivePower( bool status );
              bool getElCheckDrivePower();

              double getAzRequested();
              double getElRequested();
              void setAzRequested( double az );
              void setElRequested( double el );

              double getAz();
              double getEl();

              double getAzErr();
              double getElErr();

              double getAzErrSky();

              double getAzRate();
              double getElRate();

              std::string* getSource();
              void setSource( std::string *name );

              double getRA();
              void setRA( double angle );
              double getDec();
              void setDec( double angle );

              // All offsets  are actually handled by DriveMgrThread
              // and it's internal use of the ephemeris object
              // the following are purely for monitor info reporting
              double getOffsetAz();
              void setOffsetAz( double angle ); // arcmin
              double getOffsetEl();
              void setOffsetEl( double angle ); // arcmin

              double getMountOffsetAz();
              void setMountOffsetAz( double angle );
              double getMountOffsetEl();
              void setMountOffsetEl( double angle );

              void setAperture(  DriveCommand::ApertureType ap );
              DriveCommand::ApertureType getAperture();
              double getApertureOffsetAz( DriveCommand::ApertureType ap );
              void setApertureOffsetAz(  DriveCommand::ApertureType ap, double angle );
              double getApertureOffsetEl( DriveCommand::ApertureType ap );
              void setApertureOffsetEl( DriveCommand::ApertureType ap, double angle );
              double getApertureOffsetSag( DriveCommand::ApertureType ap );
              void setApertureOffsetSag( DriveCommand::ApertureType ap, double angle );

              float getAzRateAdjust();
              float getElRateAdjust();
              void setAzRateAdjust();
              void setElRateAdjust();
              
              float getMaxAzRate();
              void setMaxAzRate( float rate );
              float getMaxElRate();
              void setMaxElRate( float rate );

              float getWindDir();
              void setWindDir( float dir );

              double getTiltMag();
              double getTiltDir();

              double getTilt1Arcmin();
              double getTilt2Arcmin();

              void setTiltOutliers( const char *name, int outliers );
              void setTilt1Outliers( int outliers );
              int getTilt1Outliers();
              void setTilt2Outliers( int outliers );
              int getTilt2Outliers();

              int getTilt1Counts();
              int getTilt2Counts();
              int getTiltCounts( const char *name, int &outliers, int &rms1 );
              void getAveRms( int *samples, int size, int &ave, int &rms );

              int computeAzCoarseOffset( int degrees );
              int computeElCoarseOffset( int degrees );

              void setPointingModelCoefs(
                  double * dazCoefs, unsigned int dazCoefCount,
                  double * delCoefs, unsigned int delCoefCount);

              void setTolerance( double beamWidth );
              double getTolerance();

              void setNextSequenceNo( unsigned long seq );
              unsigned long getNextSequenceNo();
              void setCurSequenceNo( unsigned long seq );
              unsigned long getCurSequenceNo();

              void   setCoefChange( double mjd );
              double getCoefChange();

              int limitAxisAccel( int &stalls, int curcounts, int lastcounts, int &accelLim, int origLim,
                  int &maxSlew, int origMax, float slewMod, struct timeval &tv1 );

              bool settled();
              void updateSettle();
              void resetSettle();

              double getAzSansPointing();
              double getElSansPointing();

              // ew.
              double getObsFreq();

              void setLatitude( double lat );
              double getLatitude();
              void setLongitude( double longi );
              double getLongitude();
              void setAltitude( double alt );
              double getAltitude();

              // Used primarily for diagnostics
              int getAzMaxSlew() { return _azMaxSlew; }
              int getElMaxSlew() { return _elMaxSlew; }
              double getAzRampDown() { return _azRampDown; }
              double getElRampDown() { return _elRampDown; }
              int getAzRampStartSpeed() { return _azRampStartSpeed; }
              int getElRampStartSpeed() { return _elRampStartSpeed; }
              int getAzCoarseOffset() { return _azCoarseOffset; }
              int getElCoarseOffset() { return _elCoarseOffset; }
              double getswAzHiLim() { return _swAzHiLim; }
              double getswElHiLim() { return _swElHiLim; }
              double getswAzLoLim() { return _swAzLoLim; }
              double getswElLoLim() { return _swElLoLim; }
              bool getswSlewLim() { return _limitAccel; }
              int getAzAccelLim() { return _azAccelLim; }
              int getElAccelLim() { return _elAccelLim; }
              float getAzSlewMod() { return _azSlewMod; }
              float getElSlewMod() { return _elSlewMod; }
              float getGain() { return _gain; }
              unsigned int getWaitInUSec() { return _waitInUsec; }

              void setAmbWeatherTemp( float temp );
              float getAmbWeatherTemp();

              void setSafeCalled( int safe )
              {
                putData( "SAFECALLED", &safe );
              }

              bool safeCalled()
              {
                int safeCalled = 0;
                getData( "SAFECALLED", &safeCalled );
                return ( safeCalled != 0 );
              }

              void setSafeRange( float azLow, float azHigh,
                  float elLow, float elHigh )
              {
                setSafeCalled(1);

                _azSafeLow = azLow; _azSafeHigh = azHigh;
                _elSafeLow = elLow; _elSafeHigh = elHigh;

                putData( "AZSAFELO", &_azSafeLow );
                putData( "AZSAFEHI", &_azSafeHigh );
                putData( "ELSAFELO", &_elSafeLow );
                putData( "ELSAFEHI", &_elSafeHigh );
              };

              void getSafeRange( float &azLow, float &azHigh,
                  float &elLow, float &elHigh )
              {
                getData( "AZSAFELO", &azLow );
                getData( "AZSAFEHI", &azHigh );
                getData( "ELSAFELO", &elLow );
                getData( "ELSAFEHI", &elHigh );
              };

              void setSoftLimits( float &azlo, float &azhi, float &ello, float &elhi )
              {
                float sl[4];

                sl[0] = azlo;
                sl[1] = azhi;
                sl[2] = ello;
                sl[3] = elhi;

                putData( "SOFTLIM", sl, 4 );
              }

              void getSoftLimits( float &azlo, float &azhi, float &ello, float &elhi )
              {
                float sl[4];

                getData( "SOFTLIM", sl, 4 );

                azlo = sl[0];
                azhi = sl[1];
                ello = sl[2];
                elhi = sl[3];
              }

              void setHardLimits( float azlo, float azhi, float ello, float elhi )
              {
                float hl[4];

                hl[0] = azlo;
                hl[1] = azhi;
                hl[2] = ello;
                hl[3] = elhi;

                putData( "HARDLIM", hl, 4 );
              }

              void getHardLimits( float &azlo, float &azhi, float &ello, float &elhi )
              {
                float hl[4];

                getData( "HARDLIM", hl, 4 );

                azlo = hl[0];
                azhi = hl[1];
                ello = hl[2];
                elhi = hl[3];
              }

              void getSafeTarget( float &az, float &el )
              {
                // This does not check if setSafeRange has been called
                // as the upper layers will have checked that that method
                // has been called.  In the miraculous happenstance that
                // we arrive here without setSafeRange called, this method
                // defaults to az 0, el 85

                if ( safeCalled() )
                {
                  az = _azSafeLow + ((_azSafeHigh - _azSafeLow) / 2.0);
                  el = _elSafeLow + ((_elSafeHigh - _elSafeLow) / 2.0);
                }
                else
                {
                  // If this has happened, we're sticking to the current
                  // pointing, because it's the safest thing to do
                  az = getAz();
                  el = getEl();
                }

              }


            protected:
              double getFastPacketRxMJD();


            private:

              void loadDrivesConfig();

              std::string _name;
              const char *_dir;
              // TODO Remove these two
              //double _targetAz;
              //double _targetEl;
              double _distAz;
              double _distEl;
              int _azMaxSlew, _azOrigMaxSlew, _elMaxSlew, _elOrigMaxSlew;
              int _lastiAzVel, _lastiElVel;
              int _azDigWOMismatch, _elDigWOMismatch;
              float _gain, _mjdGainLookAhead, _gainConst;
              unsigned int _waitInUsec;
              float _azSlewMod, _elSlewMod;
              float _azSafeLow, _azSafeHigh, _elSafeLow, _elSafeHigh;
              float _azHardLow, _azHardHigh, _elHardLow, _elHardHigh;
              double _azMaxRate, _elMazRate;
              double _azRate, _elRate, _lastAzRate, _lastElRate;
              double _azRampDown, _elRampDown;
              int _azRampStartSpeed, _elRampStartSpeed;
              int _azCoarseOffset, _elCoarseOffset;
              int _azVel, _elVel, _forceVelLimit;
              int _azAccelLim, _azOrigAccelLim;
              int _elAccelLim, _elOrigAccelLim;
              struct timeval _lastLimChkAz;
              struct timeval _lastLimChkEl;
              bool _limitAccel;
              int _toggle, _lastToggle;
              time_t _lastToggleUpdate;
              time_t _lastManWarn;

              int *_rawTilts;

              bool _peggedAz;
              bool _peggedEl;

              double _hardLoLimitAz, _hardHiLimitAz;
              double _hardLoLimitEl, _hardHiLimitEl;

              float _swAzLoLim, _swAzHiLim;
              float _swElLoLim, _swElHiLim;

              double _tolerance;

              Rx *_rx;
              CalWheel *_calwheel;

              void dataInvert( int &value )
              {
                if ( ( value & 0x8000 ) != 0 )
                  value = ~( value & 0x7fff ) + 1;
              };
            
              static const double DR = 0.01745329251994; // M_PI/180.0
              static const double RD = 57.2957795130823; // 180.0/M_PI

              int _azStalls, _azStallCnt, _azRetries;
              int _elStalls, _elStallCnt, _elRetries;
              double _azLastStallChk, _elLastStallChk;

              time_t _settle;
              time_t _stallsStarted;

              Configuration &_config;
              log4cpp::Category &_logger;

            }; // class Drives
        } // namespace bima
    } // namespace antenna
} // namespace carma

::std::ostream& operator<<( ::std::ostream& os,
                                ::carma::antenna::bima::Drives& drives );

#endif // CARMA_ANTENNA_BIMA_DRIVES_H

// vim: set expandtab sw=2 ts=2 cindent :
