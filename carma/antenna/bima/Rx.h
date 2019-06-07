/**@file
 * Class definition for Stepper Motors for BIMA systems.
 * This class is derived from existing BIMA code to
 * generally describe motor parameters.
 * The original structure was defined in inc/nrcvr.h
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.45 $
 * $Date: 2013/02/22 20:40:59 $
 * $Id: Rx.h,v 1.45 2013/02/22 20:40:59 friedel Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_RX_H
#define CARMA_ANTENNA_BIMA_RX_H

// C++
#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <cmath>

// System includes
#include <unistd.h>
#include <ctype.h>

// CARMA includes
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/services/Table.h"
#include "carma/antenna/bima/TelemetryClient.h"
#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/antenna/bima/Dewar.h"
#include "carma/antenna/bima/CalWheel.h"
#include "carma/antenna/bima/AntennaIFClient.h"
#include "carma/antenna/bima/Motor.h"
#include "carma/antenna/bima/Polarizer.h"
#include "carma/antenna/common/SisReceiver.h"
#include "carma/antenna/common/FocusControl.h"
#include "carma/antenna/common/FocusControl_skel.h"
#include "carma/corba/Client.h"

namespace carma
{
  namespace antenna
    {
      namespace bima
	{
	  class Rx : public TelemetryClient
	    {

	    public:
	      Rx( Configuration& config );

              typedef char Band;
              typedef char SISSetMode;
              typedef enum { NOTOK, OK } OKOrNOTState;
              typedef enum { OFF, ON } OnOrOffState;

            
              void turnMMOscOn();
              void turnMMOscOff();
              void turnMMSweepOn();
              void turnMMSweepOff();

              bool getOscOn();
              OnOrOffState getMMOscState();
              OnOrOffState getSweepState();
       
              void setObsFreq( double freq );
              double getObsFreq();

              void setOscFreq( double freq );
              double getOscFreq();

              void setLO1Freq( double freq );
              double getLO1Freq();

              float getLO10MHzOptPwr();
              bool getLO10MHzStatus();
              float getLO50MHzOptPwr();

              void setVop( double vop );
              void setVopCommanded( double vop );
              double getVopCommanded();
              double getVop();
              double getVopB();
              double getVopAD();

	      double getDrainCurrent(unsigned short deviceID);
	      double getGateVoltage(unsigned short deviceID);
	      double getIFCurrent();

              int bandCheck();
	      int getBimaTuneState() ;   // sorry - no enums here, just 0-3

              void setPhaseLockBand();
              void setPhaseLockBand( int band );

              int getPhaseLockBand();

              int getDewarBand();
              void setDewarBand();
              void setDewarBand( int band );

              void setIFSwitch();
              void setIFSwitch( int band );

              bool checkPhaseLockRef();
              bool alreadyTuned();
	      void saveSisTuneParameters(); 

              OKOrNOTState getPhaseLockRefState();

              void getTuningParameters( double &lo_ghz, unsigned short &mmosctarg,
                                        unsigned short &mmbcktarg, double &voptarg,
                                        double &lgaintarg );

              void lockmm( double freq );
	      void relockmm();
	      void forceRelock();
              void tune( double freq, bool leaveAbsorber, bool optimizeReceiver );

	      void cmTune();

	      void setFocus( int band );
	      float loadFocusTable( std::string name );

              bool mmLockStatus();
              int getMMLockInfo();
              void setMMLockInfo( int value );

              void lockmmosc( int osctarget, double maxerrv );

              void setBand( Rx::Band band );
              void selectMotorsAndMod( int iband );
              char getBand();
              void selectMirror( int iband );
              void setDewarWindow( int iband );
              int getDewarWindow();

              void setModulator( double mA );

//            float getSISMxrTemp();
              Rx::SISSetMode getSISMode();

	      void setWBAGateV( double setpoint );
	      void setWBAGateBV( double setpoint );
	      void setWBAGateDV( double setpoint );
	      double getWBAGateBV();
	      double getWBAGateDV();

              void setSIS( double setpoint, Rx::SISSetMode mode );

              void setLOpwr( double bias );
//              void setSISBiasOut( double bias );  // what's being sent down the telem...
              double getSISBiasOut();  // what's being sent down the telem...

              float getIFdB( void );
              double getIFTotPower( void );
              double getIF1TotPower( void );
              double getIF2TotPower( void );
              double getIF1FastPower( void );
              double getIF2FastPower( void );
	      void startIF1FastSample();
	      void stopIF1FastSample();
	      void startIF2FastSample();
	      void stopIF2FastSample();

              double getIMxrA();
              void setLoopGain( double lgain );
              double getLoopGain();
              double mmErrorVolts();
              double getModBmA();
              double getModADmA();
              unsigned short modmAtoCounts( double mA );
              void optimizeLoopGain( double defaultgain );
              double getPhaseNoise();
              void getSISVIBias( double& vbias, double& ibias );
              double getInstantSISVBias();
              double getInstantSISIBias();
              std::string getSISMixerName();
              double getSISMixerTemp();
              void interpSISBiases( double& lo, double& temp,
                                    double& vbias, double& ibias,
                                    double& dvMinus, double& dvPlus,
                                    double& dvgap, double& igap, double& ialtbias );
//              void scanSIS( int istart, int stepsize, int nsteps,
//                            float *vmxr, float *imxr, float *ifpwr );

	      static const int MAX_SCAN_STEPS = 300;  // must match NMAX in AntennaIF.h
	      typedef struct{
		float v1;               	// start voltage
		float v2;                       // stop voltage
		float dv;                       // voltage step
		float Vj[MAX_SCAN_STEPS];       // SIS junction voltage, mV
		float Ij[MAX_SCAN_STEPS];       // SIS junction current, uA
		float pow[MAX_SCAN_STEPS];      // IF total power, mW
		int npts;                       // number of valid points in arrays Vj,Ij,pow
              } Scan;

	      void scanSIS( Rx::Scan &Lscan, Rx::Scan &Rscan) ;


              Dewar& getDewar()   { return *_dewar;   };
              Motor& getMMOscAD() { return *_mmoscAD; };
              Motor& getMMBckAD() { return *_mmbckAD; };
              Motor& getMMOscB()  { return *_mmoscB;  };
              Motor& getMMBckB()  { return *_mmbckB;  };
              Motor& getAttenAD() { return *_attnD;   };
              Motor& getAttenB()  { return *_attnB;   };
              double getMMIFLevel( void );
             
              void ifPowerAtten( unsigned short, float value );
              void ifPowerAtten( unsigned short, short value );
              void setLOTermRFout( float volts );
              void setLOTermAtten( int dB );
              void turnLOTerm( bool onoff );
              int getLOTermAtten();
              float getLOTermPower();
              float getLOTermRFin();
              float getLOTermRFout();
              float getLOTermTemp();
              void setHtr2( float volts );
              void setVGate( float volts );
	      void setCMVGate( unsigned int hStage, float volts );
	      void setCMIDrain( unsigned int hStage, float mAmps );

	      double getVGate();
	      double getVGateCmd();
	      double getVDrain();
	      double getIDrain();
	      double getCMIDrain( unsigned int hStage );
	      double getCMVGate( unsigned int hStage );
	      double getCMVGateCmd( unsigned int hStage );
	      double getCMIDrainCmd( unsigned int hStage );

	      void setNextTuneSequenceNo( int seqno );
	      unsigned long getNextTuneSequenceNo();
	      void setCurTuneSequenceNo( int seqno );
	      unsigned long getCurTuneSequenceNo();

	      void setNextOpticsSequenceNo( int seqno );
	      unsigned long getNextOpticsSequenceNo();
	      void setCurOpticsSequenceNo( int seqno );
	      unsigned long getCurOpticsSequenceNo();

	      // This is mainly here to enable configuration
	      // checking.
	      carma::services::Table *loadSISTable( std::string name );

	      bool isRelockEnabled();
	      void enableRelock();
	      void disableRelock();
	      void incRelock();
	      void zeroRelock();
	      int numTriesRelock();

	    private:
	      Configuration &_config;
	      const char *_name;
	      std::string _dir;
	      std::ostringstream _errMsg;

	      carma::services::Table *loadOscTable( std::string name );

	      unsigned short _attensafe;
	      double _oscfreq;
	      double _voptarg;
	      unsigned short _mmosctarg;
	      unsigned short _mmbcktarg;
	      Dewar *_dewar;
	      CalWheel *_calwheel;
	      AntennaIFClient *_if;
	      Motor *_mmosc;
	      Motor *_mmbck;
	      Motor *_attn;
	      Motor *_mmoscAD;
	      Motor *_mmbckAD;
	      Motor *_attnD;
	      Motor *_mmoscB;
	      Motor *_mmbckB;
	      Motor *_attnB;
	      Polarizer *_pol;
	      carma::services::Table *_oscADtab;
	      carma::services::Table *_oscBtab;
	      float _cmFocusOffset;
	      bool _modulator;
	      Rx::Band _band;
              int _iband;
	      bool _cmOpticsInstalled;
	      float _tmmFocusPosition;

	      // SIS Bias handling
	      static const int MAX_HIST = 25;

	      static const SISSetMode SISv = 'v';
	      static const SISSetMode SISi = 'i';
	      static const SISSetMode SISo = 'o';

	      // static const int MAX_SCAN_STEPS = 40;

	    }; // class Rx
	} // namespace bima
    } // namespace antenna
} // namespace carma

::std::ostream& operator<<( ::std::ostream& os,
                                ::carma::antenna::bima::Rx& rx );

#endif // CARMA_ANTENNA_BIMA_RX_H
