/* $Id: bimaSis.cc,v 1.14 2012/05/23 18:59:30 plambeck Exp $ */

/*
 * @usage bimaSis [antenna=bima1]
 * @key antenna @noDefault s Antenna to assume for configuration lookup
 *                           Defaults to gethostname().
 * @key v 1. d target SIS voltage (mV)
 * @key i 100. d target SIS current (uA)
 * @key mode "v" s Bias mode, v=voltage, i=current, o=open circuit 
 * @key getmode false b Return info on the current bias mode.
 * @key LObias @noDefault d target bias current (uA) from LO
 * @key band B s Select dewar/mixer band in dewar control box (band=A|B|C|D)
 * @key ivcurve false b Create an SIS IV Curve (ivcurve=true|false)
 * @key stepsize 2 d Step size for IV-curve (mV for mode v or o, uA for mode i)
 * @key maxmV 18. d Max mV for IV-curve if mode=v (limited to < 22.0)
 * @key maxuI 490. d Max uA for IV-curve if mode=i (limited to < 550.0)
 * @key setgate 1.0 d Gate voltage for in-dewar IF amplifier
 * @key ampbias true b Read back and print current amplifier bias values
 * @key scan1 6.0 d Start voltage of SIS scan, mV
 * @key scan2 11.0 d Stop voltage of SIS scan, mV
 * @key rx_setvd 1.2 d Set drain voltage on RCP CAN node
 * @key rx_setvg1 0.2 d Set gate1 voltage on RCP CAN node
 * @key rx_setvg2 0.2 d Set gate2 voltage on RCP CAN node
 * @key rx_setvj 9 d Set Vj on RCP CAN node
 * @key rx_tune 230. d Set Vj for freqGHz
 * @key rx_getvgap false b Measure Vgap
 * @key rx_ivcurve false b Measure Vgap
 * @key rx_setloopmode 0 i VJ_CLOSED=0, VJ_OPEN=1, VJ_FINITE=2
 * @logger RX_FACILITY carma.bima.bimaSis
 */

// * @key tunestate false b endlessly dump RxStateMonitorPointEnum

// C includes
#include <stdio.h>

// C++ Includes...
#include <iostream>
#include <sstream>

// CARMA Includes...
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"

#include "carma/antenna/bima/Rx.h"
#include "carma/services/Table.h"
#include "carma/antenna/bima/AntennaIFClient.h"
#include "carma/antenna/bima/CalWheel.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/MonitorUpdater.h"
#include "carma/antenna/common/IVCurve.h"
#include "carma/antenna/common/SisReceiver.h"

#define MAX_SCAN_STEPS 300

using namespace std;
using namespace carma::util;
using namespace carma::antenna::bima;

int Program::main()
{
  double vg, id;
  // double vd;

  try
  {
    AntennaNameResolver *anr;
    string antName;

    if ( parameterWasSpecified( "antenna" ) ) {
      anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
      antName = anr->getAntennaName();
    } 
    else { 
      anr = new AntennaNameResolver();
      antName = anr->getAntennaName() ;
      // new code below converts from new to old antenna names, e.g., "c7" -> "bima1"
      const char* antInput = antName.c_str();
      char antType;
      int antNumber;
      sscanf( antInput, "%c%d", &antType, &antNumber) ;
      if (antType == 'c') {
        ostringstream output;
        output  << "bima" << (antNumber - 6) ;
        antName = output.str() ;
      }
      cout << "# " << anr->getAntennaName() << " -> " << antName << endl;
    }
        
    Configuration config( antName, getConfDir() );
    Rx rx( config );
    AntennaIFClient aifc( config );
    SharedMemory shmBima( config.getAntenna().c_str() );
   
    string modestr = getStringParameter( "mode" );
    double stepsize = getDoubleParameter( "stepsize" );
    double maxmV = getDoubleParameter( "maxmV" );
    double maxuI = getDoubleParameter( "maxuI" );
    Rx::SISSetMode mode = modestr.c_str()[0];

    if ( maxmV > 22. )
      maxmV = 22.;
    if ( maxuI > 550. )
      maxuI = 550.;

    if ( parameterWasSpecified( "band" ) )
    {
      char band = getStringParameter( "band" ).c_str()[0];
      if ( (int)band > 64 && (int)band < 70 )
      {
	rx.setDewarBand( band - 65 );
      }
      else
	throw CARMA_ERROR( "Dewar band must be A,B,C,D, or E" );
    }

    if ( getBoolParameter( "getmode" ) == true )
    {
      int bits;
      rx.getData( "BITSTO50", &bits );
      cout << " Telemetry value: 0x" << (hex) << bits
	<< " translation to mode: '" << (char)rx.getSISMode()
	<< "'" << endl;
    }

    if ( parameterWasSpecified( "v" ) )
    {
      double setpoint = getDoubleParameter( "v" );
      if ( setpoint > maxmV ) setpoint = maxmV;
      rx.setSIS( setpoint, 'v' );
    }

    if ( parameterWasSpecified( "i" ) )
    {
      double setpoint = getDoubleParameter( "i" );
      if ( setpoint > maxuI ) setpoint = maxuI;
      rx.setSIS( setpoint, 'i' );
    }

    if ( parameterWasSpecified( "LObias" ) )
    {
      printf("setting LO bias\n");
      double setpoint = getDoubleParameter( "LObias" );
      rx.setLOpwr ( setpoint );
    }

    if ( parameterWasSpecified( "ivcurve" ) &&
	getBoolParameter( "ivcurve" ) == true )
    {

      FILE *out;

      {
	ostringstream filename;
	filename << "IV." << rx.getSISMixerName() << "." << config.getAntenna()
	  << "." << rx.getBand() << "." << Time::getDateString();

	cout << "File: " << filename.str() << endl;
	out = fopen( filename.str().c_str(), "w" );
	if ( out == 0 )
	{
	  perror( filename.str().c_str() );
	  exit(-1);
	}
      }

      fprintf( out, "# %s, band %c, mixer %s, tmixer %7.2f, mmosc on %d, mode %c\n",
	  config.getAntenna().c_str(), rx.getBand(), rx.getSISMixerName().c_str(),
	  rx.getSISMixerTemp(), rx.getOscOn(), mode );
      fprintf( out, "# %s\n", Time::getDateString( "%d-%b-%y %H:%M" ).c_str() );

      fflush(out);

      double setpoint = 0.;
      rx.setSIS( setpoint, mode );

      double monV = rx.getInstantSISVBias();
      double monI = rx.getInstantSISIBias();

      fprintf( out, "%7.3f %7.3f %7.2f\n", setpoint, monV, monI );

      while ( (monV < maxmV) && (monI < maxuI) )
      {
	setpoint += stepsize;
	rx.setSIS( setpoint, mode );
	monV = rx.getInstantSISVBias();
	monI = rx.getInstantSISIBias();
	fprintf( out, "%7.3f %7.3f %7.2f\n", setpoint, monV, monI );
      }

      while ( setpoint > 0.001 )
      {
	setpoint -= stepsize;
	rx.setSIS( setpoint, mode );
	monV = rx.getInstantSISVBias();
	monI = rx.getInstantSISIBias();
	fprintf( out, "%7.3f %7.3f %7.2f\n", setpoint, monV, monI );
      }

      fclose(out);
      rx.setSIS( 0, 'v' );
    } // iv curve

    if ( parameterWasSpecified( "setgate" ) ) 
    {
      double setpoint = getDoubleParameter( "setgate" );
      if (setpoint<0.0)
	setpoint=0.0;
      if (setpoint>3.5)
	setpoint=3.5;
      printf("Setting IF amplifier gate voltage to %0.2f V\n",setpoint);
      rx.setVGate(setpoint);
      usleep(500000);
    } // setgate

    if ( parameterWasSpecified( "rx_setvd" ) ) {
      double setpoint = getDoubleParameter( "rx_setvd" );
      if (setpoint<0.0)
	setpoint=0.0;
      if (setpoint>2.)
	setpoint=2;
      printf("Setting RCP Vdrain to %0.2f V\n",setpoint);
      aifc.command( AntennaIFClient::SISRX_SET_VDRAIN, (float)setpoint );
    } 

    if ( parameterWasSpecified( "rx_setvg1" ) ) {
      double setpoint = getDoubleParameter( "rx_setvg1" );
      if (setpoint<0.0)
	setpoint=0.0;
      if (setpoint>3.)
	setpoint=3.;
      printf("Setting RCP Vgate1 to %0.2f V\n",setpoint);
      aifc.command( AntennaIFClient::SISRX_SET_VGATE1, (float)setpoint );
    } 

    if ( parameterWasSpecified( "rx_setvg2" ) ) {
      double setpoint = getDoubleParameter( "rx_setvg2" );
      if (setpoint<0.0)
	setpoint=0.0;
      if (setpoint>3.)
	setpoint=3.;
      printf("Setting RCP Vgate2 to %0.2f V\n",setpoint);
      aifc.command( AntennaIFClient::SISRX_SET_VGATE2, (float)setpoint );
    } 

    if ( parameterWasSpecified( "rx_setvj" ) ) {
      double setpoint = getDoubleParameter( "rx_setvj" );
      if (setpoint<0.0)
	setpoint=0.0;
      if (setpoint>20.)
	setpoint=20.;
      aifc.command( AntennaIFClient::SISRX_SET_VJ, (float)setpoint );
      sleep(2);
      // carma::monitor::BimaSubsystem  bimaMon( config.getAntennaNo() );
      // bimaMon.readNewest(); 
      // float actualVj = bimaMon.rx1mm().sisReceiver().actualVj().getValue();
      float actualVj;
      rx.getData( "RCP_VJACT", &actualVj );
      printf("Setting RCP Vj to %0.3f mV; readback %0.3f mV\n",setpoint,actualVj);
    } 

    if ( parameterWasSpecified( "rx_tune" ) ) {
      double freqGHz = getDoubleParameter( "rx_tune" );
      printf("tuning for LO = %0.3f GHz\n",freqGHz);
      aifc.command( AntennaIFClient::SISRX_TUNE, (float)freqGHz );
      float setVj,actualVj;
      int tuneState;
      for (int i=0; i<20; i++) {
	usleep(500000);
	rx.getData( "RTUNESTATE", &tuneState );
	if (tuneState == 6) break;
      }
      rx.getData( "RCP_VJSET", &setVj );
      rx.getData( "RCP_VJACT", &actualVj );
      printf("setVj = %.3f, actualVj = %.3f, tuneState = %d\n", setVj,actualVj,tuneState);
      
      // carma::monitor::BimaSubsystem  bimaMon( config.getAntennaNo() );
      // bimaMon.readNewest(); 
      // float vj = bimaMon.rx1mm().sisReceiver().actualVj().getValue();
      // printf("Vj = %.3f\n", vj);
    } 

    if ( parameterWasSpecified( "rx_getvgap" ) ) {
      printf("measuring Vgap\n");
      aifc.command( AntennaIFClient::SISRX_GETVGAP, (float)0. );
      sleep(2);
      // carma::monitor::BimaSubsystem  bimaMon( config.getAntennaNo() );
      // bimaMon.readNewest(); 
      // float vgap = bimaMon.rx1mm().sisReceiver().gapVoltage().getValue();
      float vgap;
      int tuneState;
      for (int i=0; i<20; i++) {
	usleep(500000);
	rx.getData( "RTUNESTATE", &tuneState );
	if (tuneState == 0) break;
      } 
      rx.getData( "RCP_VGAP", &vgap );
      printf("measured Vgap = %.3f mV\n", vgap) ;
    } 

    if ( parameterWasSpecified( "rx_ivcurve" ) ) {
      Rx::Scan IVpol1, IVpol2;
      IVpol1.dv = 0.;     // dv=0 indicates no total power scan
      IVpol2.v1 = 0.;
      IVpol2.v2 = 22.;
      IVpol2.dv = 0.1;
      rx.scanSIS(IVpol1, IVpol2);
      for (int i=0; i<IVpol2.npts; i++){
	printf("%3d  %8.3f %8.2f %8.4f\n", i, IVpol2.Vj[i], IVpol2.Ij[i], IVpol2.pow[i] );
      }
    } // rx_ivcurve 

    if ( parameterWasSpecified( "rx_setloopmode" ) ) {
      short mode = (short)getIntParameter( "rx_setloopmode" );
      aifc.command( AntennaIFClient::SISRX_SET_LOOP_MODE, mode );
    } 

    if ( parameterWasSpecified( "setgate" ) || parameterWasSpecified( "ampbias" ) ) {
      printf("IF amplifier bias readback:\n");
      vg = rx.getVGate();
      id = rx.getIDrain();
      printf("Vgate = %0.3f V, Idrain = %0.3f mA\n",vg,id);
    } // ampbias

    if ( parameterWasSpecified( "scan1" ) ) 
    {
      double v1 = getDoubleParameter( "scan1" );
      double v2 = getDoubleParameter( "scan2" );
      CalWheel cw( config );

      // print out initial bias, LO freq, temp
      double v0 = rx.getInstantSISVBias();
      double i0 = rx.getInstantSISIBias();
      double loGHz = rx.getLO1Freq();
      double mxrtemp = rx.getSISMixerTemp();
      printf("# freq: %.3f GHz  mxrtemp: %.2f K   initial bias: %.3f mV  %.1f uA\n", loGHz,mxrtemp,v0,i0);

      // make steps an integer multiple for LCP d/a
      float vstep = 5. * 20./2048.;
      while ( (v2-v1)/vstep > MAX_SCAN_STEPS)
        vstep = 2.*vstep;

      // set up the Lscan and Rscan structures
      Rx::Scan LscanAmb,RscanAmb,LscanSky,RscanSky;
      RscanAmb.v1 = RscanSky.v1 = LscanAmb.v1 = LscanSky.v1 = (float)v1;
      RscanAmb.v2 = RscanSky.v2 = LscanAmb.v2 = LscanSky.v2 = (float)v2;
      RscanAmb.dv = RscanSky.dv = LscanAmb.dv = LscanSky.dv = vstep;
      if (loGHz < 150.) {
	RscanAmb.dv = RscanSky.dv = 0.;		// don't bother with RCP if 3mm
      }

      cw.setPosition( CalWheel::AMB, rx.getDewarWindow() );
      rx.scanSIS( LscanAmb, RscanAmb );
      cw.setPosition( CalWheel::SKY, rx.getDewarWindow() );
      rx.scanSIS( LscanSky, RscanSky );

      rx.setSIS( v0, 'v' );

      // dump out scan table
      printf("#  step   L-Vj   L-Ij    L-amb    L-sky     L-Y"); 
      if (RscanAmb.dv > 0.) {
        printf("       R-Vj   R-Ij   R-amb   R-sky   R-Y\n");
      } else {
	printf("\n");
      }

      for ( int i = 0; i < LscanAmb.npts; i++)
      {
        float y = (LscanSky.pow[i] > 0.) ? LscanAmb.pow[i]/LscanSky.pow[i] : 1.0 ;	// protects from division by zero
        printf("   %4d  %6.3f  %6.2f  %7.4f  %7.4f  %7.4f", 
            i, LscanSky.Vj[i], LscanSky.Ij[i], LscanSky.pow[i], LscanAmb.pow[i], y) ;
	if (RscanAmb.dv > 0.) {
          y = (RscanSky.pow[i] > 0.) ? RscanAmb.pow[i]/RscanSky.pow[i] : 1.0 ;	// protects from division by zero
          printf("    %6.3f  %6.2f  %7.4f  %7.4f  %7.4f\n", 
            RscanSky.Vj[i], RscanSky.Ij[i], RscanSky.pow[i], RscanAmb.pow[i], y) ;
	} else {
	  printf("\n");
        }
      }
    }

    /* ------------------------------------------------------------------------------------------------------
    if ( parameterWasSpecified( "tunestate" ) ) {
      typedef carma::monitor::AntennaCommon::RxStateMonitorPointEnum bimaTuneState ;
      carma::monitor::BimaSubsystem  bimaMon( config.getAntennaNo() );
      while (true) {
        bimaMon.readNewest(); 
        carma::monitor::AntennaCommon::RxStateMonitorPointEnum::RXSTATE bimaTuneState 
            = bimaMon.antennaCommon().receivers().rxState().getValue();
        cout << carma::monitor::AntennaCommon::RxStateMonitorPointEnum::valueToString( bimaTuneState ) << endl ;
        usleep(500000);
	
      }
    }
    --------------------------------------------------------------------------------------------------------*/

  }

  catch ( const carma::util::ErrorException & ee )
  {
    cerr << ee.what() << endl;
    sleep(1);
  }

  return 1;
}
