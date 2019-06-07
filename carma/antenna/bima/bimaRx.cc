/* $Id: bimaRx.cc,v 1.34 2013/07/23 04:39:45 plambeck Exp $ */

/*
 * @usage bimaRx
 * @key dumpConfig false b Show the results of reading in configuration information.
 * @key monsisbias @noDefault b Monitor sis bias current and voltage
 * @key lotermAtten @noDefault i Set lo term atten in dB
 * @key lotermRFout @noDefault d Set lo term RFout level in V
 * @key heater3 @noDefault d Set heater3.
 * @key watchifpow @noDefault i Print out total if power
 * @key band @noDefault s Force band selection
 * @key ifband @noDefault i Select IF Band output, 1-4 (corresponds to 1CM, 1MM, 3MM, ANY)
 * @key iffast @noDefault i Test out fast sampling on the specified pol (1 or 2)
 * @key ifpow @noDefault d Set IF1 power in mW.
 * @key if1pow @noDefault d Set IF1 power in mW.
 * @key if2pow @noDefault d Set IF2 power in mW.
 * @key ifatten @noDefault d Set IF1 atten dB.
 * @key if1atten @noDefault d Set IF1 atten dB.
 * @key if2atten @noDefault d Set IF2 atten dB.
 * @key watchtemps 1 i Watch all dewar temps, printed watchtemps*1 seconds.
 * @key loopgain @noDefault d Set loop gain.
 * @key serial @noDefault b Set serial echo on telemetry to true or false.
 * @key lockstats @noDefault d Dump lock status every d seconds (d=0 for once)
 * @key watchlocks false b Dump lock status and motor position every 0.01 secs
 * @key motor "" s Select which motor to operate (mmoscAD, mmoscB, mmbckAD, mmbckAD, attnD, attnB, calwheel, polarizer).
 * @key monpos @noDefault d Monitor motor position every monpos seconds.
 * @key moveToTarget	@noDefault	i	Move motor to a target
 * @key steponce @noDefault b Set up one sequence of steps on motor.
 * @key stepdelay @noDefault i Delay between pulses to motor during sequence.
 * @key modma @noDefault d Set mA on modulator.
 * @key xfreq @noDefault d Lock to X Frequency
 * @key antenna @noDefault s Default antenna.  Uses gethostname() if not specified.
 * @key dewarwindow "A" s set dewar window.
 * @key mmphlckband "A" s Default mm phaselock band.
 * @key dewarband "A" s Default dewar band.
 * @key selectMirror "mm" s Select mm/cm for flip mirror position
 * @key mmosc "on" s Turn mmosc on/off.
 * @key mmsweep "on" s Turn mmsweep on/off.
 * @key optloopgain "" s Optimize mm phaselock loop gain.
 * @key freq 90 d Lock mmosc to freq (GHz); now sets synth
 * @key vop 2.3 d Set mmVop.
 * @key cal "" s Set cal wheel to position, 'sky', 'amb', 'hot'
 * @key optflap "" s Open or close optical flap 
 * @key focus @noDefault d Set focus (in mm, [-25.0,25.0]
 * @key tune "" s Optimize SIS bias for current oscfreq
 * @key relock @noDefault b Control Rx relocking by the RxMgr
 * @key loref 0 d LO Reference Frequecy (MHz)
 * @key lorep 10.0 d LO Reference Power (dBm)
 * @key synth_gpib 7 i Synthesizer GPIB address (6 for 4425, 7 for 8662)
 * @key synth_8662 t b Flag for 8662 synths (f for 4425, tfor 8662)
 * @key setsynth f b Set the synthesizer?  Only works in Berkeley
 * @key relockmm t b attempt to relock mm osc
 * @key getpwr t b Read IF pwr from pwr meter on sky and amb, compute tsys
 * @key getpwr1 t b Read IF pwr from pwr meter
 * @key setYIGfilt 2. d Set YIG filter (Berkeley only)
 * @key scope "" s Monitor t3,t4,55 rapidly for 10 secs 
 * @key harm 1 i 0 for even, 1 for odd
 * @key hcheck 9 i Harmonic check and override harmonic
 * @key network t b test network analyzer
 * @key sptrace t b dump trace from spectrum analyzer
 * @key passband t b measure passband using spectrum analyzer
 * @key diodecurve t b use modB output and HP34401 to measure phslck diode IV curve
 * @key read34401 t b dump voltage from agilent 34401
 * @key forceRelock t b force mmosc to tune even if already locked
 * @logger RX_FACILITY carma.bima.bimaRx
 */

// * @key tune "A" s Lock MM Osc to band.

// UNIX Includes...
#include <unistd.h>
#include <stdio.h>
#include <math.h>

#include <sys/time.h>

// C++ Includes...
#include <iostream>
#include <ostream>

// CARMA Includes...
#include "carma/antenna/bima/TelemetryClient.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/AntennaIFClient.h"
#include "carma/antenna/bima/Rx.h"
#include "carma/antenna/bima/LO.h"
#include "carma/antenna/bima/Dewar.h"
#include "carma/antenna/bima/Motor.h"
#include "carma/antenna/bima/CalWheel.h"
#include "carma/antenna/bima/Secondary.h"
#include "carma/antenna/bima/OpticalFlap.h"
#include "carma/util/Program.h"

#include "carma/corba/corba.h"
#include "carma/loref/LOReferenceControlImpl.h"
#include <gpib/ib.h>

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;
using namespace carma::loref;

char * getStatus(int synth_gpib, bool synth_8662) {
  // 25 should be long enough
  const unsigned int cmdSize = 25;
  char cmd[cmdSize];
  // 80 should be long enough
  const unsigned int statusSize = 80;
  char status[statusSize];

  if(synth_8662) {
    strncpy(cmd,"MS?", cmdSize);
  } else {
    strncpy(cmd,"SYST:ERR?", cmdSize);
  }

  char * response = LOReferenceControlImpl::gpib(0,synth_gpib,cmd);

  if(synth_8662) {
    if(!strncmp(response,"00",2)
    || !strncmp(response,"13,00",5)) {
      strncpy(status, "OK ", statusSize);
    } else {
      strncpy(status, "ERROR ", statusSize);
    }
  } else {
    if(!strncmp(response,"+0,",3)) {
      strncpy(status, "OK ", statusSize);
    } else {
      strncpy(status, "ERROR ", statusSize);
    }
  }

  strncat(status, response, 80-strlen(status));
  status[statusSize - 1]='\0';
  //free(response);
  return strdup(status);
}

void setSynthFreq(int synth_gpib, double freqMHz, bool synth_8662) {
  const unsigned int cmdSize = 25;
  char cmd[cmdSize]; // 25 should be long enough
  if(synth_8662) {
    printf("sending command to 8662\n");
    snprintf(cmd, cmdSize, "FR%.1fHZ",freqMHz*1000000);
  } else {
    snprintf(cmd, cmdSize, "FREQ %lf",freqMHz*1000000);
  }
  cout << "Setting synth frequency to " << freqMHz << " MHz: "
       << LOReferenceControlImpl::gpib(0,synth_gpib,cmd)
       << endl;
  char * status = getStatus(synth_gpib, synth_8662);
  cout << status << endl;
  free(status);
}

void setSynthPower(int synth_gpib, double power, bool synth_8662) {
  const unsigned int cmdSize = 25;
  char cmd[cmdSize]; // 25 should be long enough
  if(synth_8662) {
    snprintf(cmd, cmdSize, "AP%.1fDM",power);
  } else {
    snprintf(cmd, cmdSize, "POW %lf",power);
  }
  cout << "Setting synth power to " << power << " dBm: ";
  cout << LOReferenceControlImpl::gpib(0,synth_gpib,cmd);
  cout << endl;
  char * status = getStatus(synth_gpib, synth_8662);
  cout << status << endl;
  free(status);

  if (! synth_8662) {
    snprintf(cmd, cmdSize, "OUTP:STAT ON");	// turn on RF
    cout << "Setting RF ON" << endl;
    cout << LOReferenceControlImpl::gpib(0,synth_gpib,cmd);
    char * status = getStatus(synth_gpib, synth_8662);
    cout << status << endl;

    snprintf(cmd, cmdSize, "OUTP:MOD OFF");	// turn off modulation
    cout << "Turning modulation OFF" << endl;
    cout << LOReferenceControlImpl::gpib(0,synth_gpib,cmd);
    status = getStatus(synth_gpib, synth_8662);
    cout << status << endl;
    free(status);
  }
}

/* TODO: Make these into command line parameters someday */
#define POWER_METER_GPIB 13
#define SPECTRUM_ANALYZER_GPIB 18
/* General purpose ASCII(!) interface to gpib */
char * gpib(int address, char * cmd)
{
  return LOReferenceControlImpl::gpib(0,address,cmd);
}

double readPwrMeter() 
{
  double pwr;
  const unsigned int cmdSize = 25;
  char cmd[cmdSize]; // 25 should be long enough
  snprintf(cmd, cmdSize, "UNIT:POW DBM");
//  printf("switching to dBm\n");
  char * response = LOReferenceControlImpl::gpib(0,13,cmd);
//  printf("%s\n",response);
  snprintf(cmd, cmdSize, "MEAS?");
  response = LOReferenceControlImpl::gpib(0,13,cmd);
  printf("# %s\n",response);
  sscanf(response, "%lf", &pwr);
  if (pwr > 900.) {
    snprintf(cmd, cmdSize, "MEAS?");
    response = LOReferenceControlImpl::gpib(0,13,cmd);
//    printf("%s",response);
    sscanf(response, "%lf", &pwr);
  }
  return pwr;
}
    
int dumpSpectrumAnalyzer(float * buf, int nbuf,
    float * start_freq, float * stop_freq, int * num_points)
{
#define RAWBUF_LEN 64000
  static char rawbuf[RAWBUF_LEN];
  char *cmd; 

  int gpib_address = SPECTRUM_ANALYZER_GPIB;
  int ud = ibdev(0, gpib_address, 0, T1s, 1, 0);
  if (ud < 0)
  {
    fprintf(stderr, "ibdev failed\n");
    return -1;
  }

  // Set byte order to "swapped" (i.e. little-endian)
  cmd = (char *)":format:border swapped\n";
  if(ibwrt(ud, cmd, strlen(cmd)) & ERR)
  {
    fprintf(stderr, "ibwrt(%s) failed\n", cmd);
    // Close device
    ibonl(ud, 0);
    return -1;
  }

  // Set format to real32
  cmd = (char *)":format:data real,32\n";
  if(ibwrt(ud, cmd, strlen(cmd)) & ERR)
  {
    fprintf(stderr, "ibwrt(%s) failed\n", cmd);
    // Close device
    ibonl(ud, 0);
    return -1;
  }

  // Get trace data
  cmd = (char *)":trace:data? trace1";
  if(ibwrt(ud, cmd, strlen(cmd)) & ERR)
  {
    fprintf(stderr, "ibwrt(%s) failed\n", cmd);
    // Close device
    ibonl(ud, 0);
    return -1;
  }
  if(ibrd(ud, rawbuf, RAWBUF_LEN) & ERR)
  {
    fprintf(stderr, "ibrd failed\n");
    // Close device
    ibonl(ud, 0);
    return -1;
  }

  // Read trace data
  char digitbuf[11];
  int num_digits = rawbuf[1] - '0';
  strncpy(digitbuf, rawbuf+2, num_digits);
  digitbuf[num_digits] = '\0';
  int num_bytes = atoi(digitbuf);
  if(num_bytes > RAWBUF_LEN - 2 - 5) {
    num_bytes = RAWBUF_LEN - 2 - 5;
  }
  int num_floats = num_bytes / sizeof(float);
  if( num_floats > nbuf) {
    num_floats = nbuf;
  }
  memcpy(buf, rawbuf+2+num_digits, num_floats*sizeof(float));

  // Get start freq, stop freq, num points
  if(start_freq) {
    cmd = (char *)":sense:freq:start?\n";
    if(ibwrt(ud, cmd, strlen(cmd)) & ERR)
    {
      fprintf(stderr, "ibwrt(%s) failed\n", cmd);
      // Close device
      ibonl(ud, 0);
      return -1;
    }
    if(ibrd(ud, rawbuf, RAWBUF_LEN) & ERR)
    {
      fprintf(stderr, "ibrd failed\n");
      // Close device
      ibonl(ud, 0);
      return -1;
    }
    *start_freq = strtof(rawbuf, NULL);
  }

  if(stop_freq) {
    cmd = (char *)":sense:freq:stop?\n";
    if(ibwrt(ud, cmd, strlen(cmd)) & ERR)
    {
      fprintf(stderr, "ibwrt(%s) failed\n", cmd);
      // Close device
      ibonl(ud, 0);
      return -1;
    }
    if(ibrd(ud, rawbuf, RAWBUF_LEN) & ERR)
    {
      fprintf(stderr, "ibrd failed\n");
      // Close device
      ibonl(ud, 0);
      return -1;
    }
    *stop_freq = strtof(rawbuf, NULL);
  }

  if(num_points) {
    cmd = (char *)":sense:sweep:points?\n";
    if(ibwrt(ud, cmd, strlen(cmd)) & ERR)
    {
      fprintf(stderr, "ibwrt(%s) failed\n", cmd);
      // Close device
      ibonl(ud, 0);
      return -1;
    }
    if(ibrd(ud, rawbuf, RAWBUF_LEN) & ERR)
    {
      fprintf(stderr, "ibrd failed\n");
      // Close device
      ibonl(ud, 0);
      return -1;
    }
    *num_points = atoi(rawbuf);
  }

  // Close device
  ibonl(ud, 0);

  return num_floats;
}

int dumpNetworkAnalyzer( )
{
  const unsigned int cmdSize = 25;
  char cmd[cmdSize];

  snprintf(cmd, cmdSize, "CHAN1" );
  char * response = LOReferenceControlImpl::gpib(0,15,cmd);
  printf("CHAN1: %s\n", response);

  snprintf( cmd, cmdSize, "FORM4" );
  response = LOReferenceControlImpl::gpib(0,15,cmd);
  printf("FORM4: %s\n", response);

  snprintf( cmd, cmdSize, "POIN201" );
  response = LOReferenceControlImpl::gpib(0,15,cmd);
  printf("POIN201: %s\n", response);
 
  sleep(3);

  snprintf( cmd, cmdSize, "SING;" );
  response = LOReferenceControlImpl::gpib(0,15,cmd);
  printf("SING: %s\n", response);

 
  snprintf( cmd, cmdSize, "OUTPIDEN?" );
  response = LOReferenceControlImpl::gpib(0,15,cmd);
  printf("OUT: %s\n", response);
 
  return 1;
}


// #define SET_SYNTH(f,p) { / 

int Program::main()
{

#ifdef TEST_DUMP_SA
  float buf[10];
  float start_freq, stop_freq;
  int num_points;
  int rc = dumpSpectrumAnalyzer(buf, 10, &start_freq, &stop_freq, &num_points);
  printf("%d\n", rc);
  printf("start_freq is %f\n", start_freq);
  printf("stop_freq is %f\n", stop_freq);
  printf("num_points is %d\n", num_points);
  for(int i=0; i<10; i++) {
    printf("%f\n", buf[i]);
  }
  return 0;
#endif

  try
  {
    Category &logger = Program::getLogger();

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

//    if ( parameterWasSpecified( "antenna" ) )
//      anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
//    else
//      anr = new AntennaNameResolver();

//    Configuration config( anr->getAntennaName(), Program::getConfDir() );

    CPTRACE( Trace::TRACE1, "Using " << config );

    setInstanceLogname( string( "carma." )
                        + config.getAntenna() + string( ".bimaRx" ) );

    CPTRACE( Trace::TRACE6, "using tm conf file: " << config.getTelemConfFile() );
    CPTRACE( Trace::TRACE6, "using antenna name: " <<  config.getAntenna() );

    SharedMemory sm( config.getAntenna().c_str() );

    string motorName("");
    Motor *aMotor = 0; // get rid of uninit compiler warning

    if ( getBoolParameter( "dumpConfig" ) )
    {
      cout << config;
    }

    if ( parameterWasSpecified( "dewarwindow" ) )
    {
      const char *sband = getStringParameter( "dewarwindow" ).c_str();
      unsigned char band = sband[0];
      Rx rx( config );
      int iband = band - 65 ;
      cout << "Setting Dewar Window to '" << band << "'" << endl;
      rx.setDewarWindow ( iband );
    }

    if ( parameterWasSpecified( "selectMirror" ) )
    {
      Rx rx( config );
      string mmOrCm = getStringParameter("selectMirror");
      if ( mmOrCm.compare("mm") == 0) {
        rx.selectMirror( 0 );	// pretend this is band 0 = 'A'
      } else if ( mmOrCm.compare("cm") == 0) {
        rx.selectMirror( 4 );   // pretend this is band 4 = 'E' = cm
      } else {
	cout << "unrecognized option for mirrorSelect - use mm or cm" << endl;
      }
    }

    if ( parameterWasSpecified( "relock") )
    {
      Rx rx( config );
      if ( getBoolParameter( "relock" ) == true )
      {
	rx.enableRelock();
	cout << "Enabling relock." << endl;
      }
      else
      {
	rx.disableRelock();
	cout << "Disabling relock." << endl;
      }
    }

    if ( parameterWasSpecified( "forceRelock") ) {
       Rx rx( config );
       if ( getBoolParameter( "forceRelock" ) == true ) {
         rx.forceRelock();
         cout << "Forcing relock." << endl;
       }
    }

    if ( parameterWasSpecified( "loref") )
    {
      int synth_gpib = getIntParameter("synth_gpib");
      double loref = getDoubleParameter("loref");
      double lorep = getDoubleParameter("lorep");
      bool synth_8662 = getBoolParameter("synth_8662");

      setSynthFreq( synth_gpib, loref, synth_8662); \
      setSynthPower( synth_gpib, lorep, synth_8662);
    }

    if ( parameterWasSpecified( "heater3" ) )
    {
      Dewar d( config );
      double v = getDoubleParameter( "heater3" );
      cout << "HEATER3 now: " << d.getHeater3mW() << " mW" << endl;
      d.setHeater3V( v );
      usleep( 300000 );
      cout << "Setting to: " << v << " V" << endl;
      cout << "HEATER3 now: " << d.getHeater3mW() << " mW" << endl; 
    }

    if ( parameterWasSpecified( "focus" ) )
    {
      Secondary s( config );
      double mm = getDoubleParameter( "focus" );

      s.setFocus( mm );
      cout << "Focus now: " << s.getFocus() << "mm Raw pos: " << s.getRawFocus() << endl;
    }

    if ( parameterWasSpecified( "optflap" ) )
    {
      OpticalFlap of( config );
      string status = getStringParameter( "optflap" );

      if ( status.compare( "open" ) == 0 )
        of.open();
      else
        of.close();
    }

    if ( parameterWasSpecified( "lotermRFout" ) )
    {
      Rx rx( config );
      float setting = (float)getDoubleParameter( "lotermRFout" );

      rx.setLOTermRFout( setting );
    }

    if ( parameterWasSpecified( "lotermAtten" ) )
    {
      Rx rx( config );
      int setting = getIntParameter( "lotermAtten" );

      rx.setLOTermAtten( setting );
    }

    if ( parameterWasSpecified( "monsisbias" ) )
    {
      Rx rx( config );
      cout.precision(5);
      double vb, ib;
      rx.getSISVIBias( vb, ib );
      cout <<(char)13 << "sis V bias: ";
      cout.width(8); cout << vb; cout.width(0);
      cout << " mV ib: ";
      cout.width(8); cout << ib; cout.width(0);
      cout << " uA\n ";
      cout.flush();
    }

    if ( parameterWasSpecified( "scope" ) )
    {
      struct timeval tm;
      struct timezone tz;
      float t3[6000],t4[6000],t5[6000];
      double secs[6000],t0;
      int np;
      Dewar d( config );

      gettimeofday( &tm, &tz );
      t0 = (double)(tm.tv_sec) + (double)(tm.tv_usec)/1000000.;

      printf("# taking data for 60 seconds...\n");
      for (np=0; np<6000; np++){
          gettimeofday( &tm, &tz );
          secs[np] = (double)(tm.tv_sec) + (double)(tm.tv_usec)/1000000. - t0  ;
	  t3[np] = d.stage3temp();
	  t4[np] = d.stage4temp();
	  t5[np] = d.stage5temp();
	  usleep(8000);
      }
      for (np=0; np<6000; np++){
	printf("%7.4lf  %7.4f  %7.4f  %7.4f\n",secs[np],t3[np],t4[np],t5[np]);
      }
    }


    if ( parameterWasSpecified( "watchtemps" ) )
    {
      time_t t0;
      double t1,t2,t3,t4,t5,htrmW;
      int secs = getIntParameter( "watchtemps" );
      int npts = 10*secs; 
      int np;

      Dewar d( config );

      cout << "# Secs  Stage1 Stage2 Stage3 Stage4 Stage5  Htr3mW" << endl;
      t0 = time(NULL);

      while ( true ) 
      {
        t1 = 0.;
	t2 = 0.;
	t3 = 0.;
	t4 = 0.;
	t5 = 0.;
	htrmW = 0.;
	for (np=0; np<npts; np++){
	  t1 += d.stage1temp();
	  t2 += d.stage2temp();
	  t3 += d.stage3temp();
	  t4 += d.stage4temp();
	  t5 += d.stage5temp();
	  htrmW += d.getHeater3mW();
	  usleep(100000);	// wait 0.1 secs
        }
	printf("%6d  %6.2f %6.2f %7.3f %7.3f %7.3f  %6.2f\n",
	    (int)(time(NULL)-t0), t1/npts, t2/npts, t3/npts, t4/npts, t5/npts, htrmW/npts) ;
        cout.flush();
      }
    }


    if ( parameterWasSpecified( "watchifpow" ) )
    {
      int secs = getIntParameter( "watchifpow" );

      Rx rx( config );

      while ( true )
      {
        cout << "IF Power: " << rx.getIFTotPower() << endl;
        sleep(secs);
      }
    }
 
    if ( parameterWasSpecified( "iffast" ) )
    {
      int pol = getIntParameter( "iffast" );

      if ( pol != 1 && pol != 2 )
      {
	cout << " must specify 1 or 2 for iffast" << endl;
	exit(EXIT_FAILURE);
      }

      Rx rx( config );

      cout << "Testing fast sampling on pol: " << pol << endl;
      cout << "will sample for ~5 seconds..." << endl;

      if ( pol == 1 )
	rx.startIF1FastSample();
      else
	rx.startIF2FastSample();

      time_t s, t;
      t = s = time(NULL);
      cout << fixed << setprecision(5);
      while ( t - s < 5 )
      {
	cout << (char)13;

	if ( pol == 1 )
	  cout << rx.getIF1FastPower();
	else
	  cout << rx.getIF2FastPower();

	cout.flush();

	usleep(8000);
	t = time(NULL);
      }
	  
      if ( pol == 1 )
	rx.stopIF1FastSample();
      else
	rx.stopIF2FastSample();

      cout << endl << " Done." << endl;
    }


    if ( parameterWasSpecified( "ifpow" ) || parameterWasSpecified( "if1pow" ) )
    {
      double power;

      if ( parameterWasSpecified( "ifpow" ) )
        power = getDoubleParameter( "ifpow" );
      else
        power = getDoubleParameter( "if1pow" );

      AntennaIFClient aifc( config );
      aifc.command( AntennaIFClient::SET_IF1_LEVEL, (float)power );
    }

    if ( parameterWasSpecified( "if2pow" ) )
    {
      double power = getDoubleParameter( "if2pow" );
      AntennaIFClient aifc( config );
      aifc.command( AntennaIFClient::SET_IF2_LEVEL, (float)power );
    }


    if ( parameterWasSpecified( "ifatten" ) || parameterWasSpecified( "if1atten" ) )
    {
      double atten;

      if ( parameterWasSpecified( "ifatten" ) )
        atten = getDoubleParameter( "ifatten" );
      else
        atten = getDoubleParameter( "if1atten" );

      AntennaIFClient aifc( config );
      aifc.command( AntennaIFClient::SET_IF1_ATTEN, (float)atten );
    }

    if ( parameterWasSpecified( "if2atten" ) )
    {
      double atten = getDoubleParameter( "if2atten" );
      AntennaIFClient aifc( config );
      aifc.command( AntennaIFClient::SET_IF2_ATTEN, (float)atten );
    }

    if ( parameterWasSpecified( "ifband" ) )
    {
      short band = (short)getIntParameter( "ifband" );
      AntennaIFClient aifc( config );
      printf(" setting IF band to %d\n", band);
      aifc.command( AntennaIFClient::SELECT_IF_BAND, band );
    }

    if ( parameterWasSpecified( "serial" ) )
    {
      TelemetryClient tmc( config );
      bool serial = getBoolParameter( "serial" );

      if ( serial )
	tmc.serialEnable();
      else
	tmc.serialDisable();
    }

    /* -------------------------------------------------------------------
    * @key xfreqmon @noDefault b Dump xband freq readback every 0.1 sec.
    if ( parameterWasSpecified( "xfreqmon" ) )
    {
      LO lo( config );

      while ( true )
      {
	cout << (char)13 << "xfreq: " << lo.xBandFreq() << " ";
	cout.flush();
	usleep(10000);
      }
    }
    ---------------------------------------------------------------------*/

    if ( parameterWasSpecified( "motor" ) )
    {
      motorName = getStringParameter( "motor" );
      aMotor = new Motor( motorName, config );
    }

    if ( parameterWasSpecified( "cal" ) )
    {
      string calpos = getStringParameter( "cal" );
      CalWheel cw( config );
      Rx rx( config );

      cw.setPosition( calpos, rx.getDewarWindow() );
    }

    if ( parameterWasSpecified( "monpos" ) )
    {
      double wait = getDoubleParameter( "monpos" );
      if (wait < 0.1) wait = 0.1;

      while ( true )
      {
	cout << (char)13 << motorName << ": " << aMotor->position() << " ";
	cout.flush();
	usleep((int)nearbyint(1000000*wait));
      }
    }

    if ( parameterWasSpecified( "loopgain" ) )
    {
      Rx rx( config );
      double gain = getDoubleParameter( "loopgain" );
      rx.setLoopGain( gain );
      cout << "gain set to: " << gain << endl;
    }

    if ( parameterWasSpecified( "optloopgain" ) )
    {
      Rx rx( config );
      rx.optimizeLoopGain( 5.5 );
    }

    if ( parameterWasSpecified( "lockstats" ) )
    {
      Rx rx( config );
      LO lo( config );
      const unsigned int ssSize = 140;
      char ss[ssSize];

      if ( parameterWasSpecified( "motor" ) == false )
      {
	throw CARMA_EXCEPTION( ErrorException, "lockstats requires motor name on command-line" );
      }

      unsigned short mmoscpos = aMotor->position();
      snprintf(ss, ssSize, "# Freq: %7.3f  MMosc: %5d  MMlock:%2d  MMerrV: %5.2f  MMlgain: %4.2f  PhNoise: %4.2f   Xfreq: %6.3f  Xlock:%2d\n",
	  rx.getOscFreq(), mmoscpos, rx.mmLockStatus(), rx.mmErrorVolts(), rx.getLoopGain(), rx.getPhaseNoise(),
	  lo.getCommanded(), lo.xLockStatus() );
      printf("%s", ss);
    }

    if ( parameterWasSpecified( "watchlocks" ) )
    {
      if ( parameterWasSpecified( "motor" ) == false )
      {
	throw CARMA_EXCEPTION( ErrorException, "watchlocks requires motor name on command-line" );
      }

      cout << boolalpha;
      cout.precision(3);

      Rx rx( config );
      LO lo( config );

      while ( true )
      {
	//        cout << (char)13 << "10MHz: " << rx.getLO10MHzStatus() << "  ";
	//        cout << " xLock: " << lo.xLockStatus() << " errv: " << lo.xBandErrorVolts() << "   ";
	//        cout << " mmLock: " << rx.mmLockStatus() << " errv: " << rx.mmErrorVolts() << "   ";
	//	cout << " motor(" << aMotor->getName() << "): " << aMotor->position() << endl ;

	printf(" xLock:%2d errv: %5.2f   mmLock:%2d  errv: %5.2f   %5d\n",
	    lo.xLockStatus(), lo.xBandErrorVolts(), rx.mmLockStatus(), rx.mmErrorVolts(),
	    aMotor->position() );

	cout.flush();
	usleep(10000);
      }
    }

    if ( parameterWasSpecified( "moveToTarget" ) )
    {
      unsigned short target = (unsigned short)getIntParameter( "moveToTarget" );

      /* -------------------------------------------------------------------------
      if ( parameterWasSpecified( "band" ) )
      {
	Rx rx( config );
	string band = getStringParameter( "band" );
	const char *cb = band.c_str();
	char b = toupper(cb[0]);
	rx.setBand( b );
      }
         ------------------------------------------------------------------------- */

      /*  ------------------------- this didn't work ------------------------------
	  if (parameterWasSpecified( "from" ) ) 
	  {
	  Motor::StepFrom from = Motor::EITHER ;
	  string direction = getStringParameter( "motor" );
	  if (direction == "above")
	  from = Motor::ABOVE ;
	  else if (direction == "below")
	  from = Motor::BELOW ;
	  }
	  -------------------------------------------------------------------------- */

      if ( aMotor != (Motor *)0 )
      {
	cout << motorName << " motor position now: " << aMotor->position() << endl;
	cout << motorName << " motor moving to target: " << target << endl;
	aMotor->moveToTarget( (unsigned short)target );
	cout << motorName << " motor position now: " << aMotor->position() << endl;
      }
      else
	throw CARMA_EXCEPTION( ErrorException, "aMotor is NULL!!! (shouldn't happen)" );
    }

    if ( parameterWasSpecified( "xfreq" ) && parameterWasSpecified( "freq" ) == false )
    {
      double xGHz = getDoubleParameter( "xfreq" );
      LO lo( config );

      bool setsynth = getBoolParameter( "setsynth" );
      if (setsynth)
      {
	int nharm = (int)((xGHz + .01) / 1.105);
	double refMHz = 1000. * (xGHz + .01) / nharm;
	cout << "xGHz = " << xGHz << " refMHz = " << refMHz << endl;
	int synth_gpib = getIntParameter("synth_gpib");
	double lorep = getDoubleParameter("lorep");
	bool synth_8662 = getBoolParameter("synth_8662");
	cout << "setting synthesizer to " << refMHz;
	setSynthFreq( synth_gpib, refMHz, synth_8662);    // set the synth
	setSynthPower( synth_gpib, lorep, synth_8662);
      }
      BIMASTATUSINFO( lo, "Locking X-Band: " << xGHz );
      lo.lockX( xGHz );

      BIMASTATUSINFO( lo, "Finished X-Band lock call: " << xGHz );
    }

    if ( parameterWasSpecified( "vop" ) )
    {
      double vop = getDoubleParameter( "vop" );
      Rx rx( config );
      rx.setVop( vop );
    }

    if ( parameterWasSpecified( "modma" ) )
    {
      double modma = getDoubleParameter( "modma" );
      Rx rx( config );
      rx.setModulator( modma );
    }


    if ( parameterWasSpecified( "mmosc" ) )
    {
      Rx rx( config );
      if ( getStringParameter( "mmosc" ).compare( "off" ) == 0 )
	rx.turnMMOscOff();
      else
	rx.turnMMOscOn();
    }

    if ( parameterWasSpecified( "mmsweep" ) )
    {
      Rx rx( config );
      if ( getStringParameter( "mmsweep" ).compare( "off" ) == 0 )
	rx.turnMMSweepOff();
      else
	rx.turnMMSweepOn();
    }

    if ( parameterWasSpecified( "mmphlckband" ) )
    {
      const char *sband = getStringParameter( "mmphlckband" ).c_str();
      unsigned char band = sband[0];
      Rx rx( config );
      rx.turnMMOscOn();
      rx.turnMMSweepOn();
      cout << "Setting PhaseLockBand to '" << band << "'" << endl;
      rx.setPhaseLockBand ( band - 65 );
    }

    if ( parameterWasSpecified( "dewarband" ) )
    {
      const char *sband = getStringParameter( "dewarband" ).c_str();
      unsigned char band = sband[0];
      Rx rx( config );
      cout << "Setting Dewar Band to '" << band << "'" << endl;
      rx.setDewarBand ( band - 65 );
    }

    if ( parameterWasSpecified( "steponce" ) )
    {
      int stepdelay = getIntParameter( "stepdelay" );
      int n=20;
      aMotor->enableLock();
      aMotor->setDirection( Motor::DOWN );
      aMotor->step( stepdelay );
      while (1) {
	cout << motorName << " motor position now: " << aMotor->position() << endl;
	usleep( 10000 );
	if (n-- < 0) {
	  aMotor->step( stepdelay );
	  n = 20;
	}
      }

      //      aMotor->disableLock();
      //      aMotor->disableToggleBit();
    }

    if ( parameterWasSpecified( "setYIGfilt" ) )   // use in Berkeley only!
    {
      //      Rx rx( config );
      //      double fGHz = getDoubleParameter( "setYIGfilt" ) ;
      //      float volts = (fGHz - 1.0) /9.;	// 0-1 volt <--> 1-10 GHz, apparently
      //      rx.setHtr2( volts );
      //      cout << "set heater2 (= YIG filter) to " << volts << " volts (= " << fGHz << " GHz)" << endl ;
      cout << "THIS COMMAND HAS BEEN COMMENTED OUT, SEE SOURCE CODE..." << endl;
    }

    if ( parameterWasSpecified( "relockmm" ) )
    {
      Rx rx( config );		// creating Rx object
      rx.relockmm();
    }


    if ( parameterWasSpecified( "freq" ) )
    {
      //      const char *sband = getStringParameter( "tune" ).c_str();
      //      unsigned char band = sband[0];

      unsigned char band;
      double xGHz;

      LO lo( config );		// creating LO object
      Rx rx( config );		// creating Rx object

      double lo1 = getDoubleParameter( "freq" );
      band = (lo1 > 150.) ? 'D' : 'B' ;
      if (lo1 < 50.) band = 'E';
      double oscfreq = (lo1 > 120.) ? lo1/3. : lo1 ;
      cout << "lo1: " << lo1 << "  oscfreq: " << oscfreq << endl;

      BIMASTATUSINFO( rx, "Attempting to set frequency, lo1: " << lo1 << " oscfreq: " << oscfreq );
      rx.setOscFreq( oscfreq );
      rx.setLO1Freq( lo1 );
      cout << "Setting Rx band to '" << band << "'" << endl;
      BIMASTATUSINFO( rx, "Set Band: " << band );
      rx.setBand ( band );

      if ( parameterWasSpecified( "dewarband" ) )
      {
	const char *sband = getStringParameter( "dewarband" ).c_str();
	unsigned char band = sband[0];
	Rx rx( config );
	cout << "Setting Dewar Band to '" << band << "'" << endl;
	rx.setDewarBand ( band - 65 );
      }

      /* --- in Berkeley, use synth keyword to automatically set synth --- */
      // in Berkeley, phaselock IF is at 40 MHz, not 50 MHz
      bool setsynth = getBoolParameter( "setsynth" );
      if (setsynth)
      {
	int m = (int)((oscfreq - .04) / 10.8 + 1);	// choose harmonic which puts xband close to 10.5 GHz
	if ( parameterWasSpecified( "harm" ) ) {
	  int evenodd = getIntParameter( "harm" );
	  if (evenodd == 0) {
	    if ( m%2 == 1) m--;			// choose an even harmonic
	  } else {
	    if ( m%2 == 0) m--;			// choose an odd harmonic (default)
	  }
	}
	xGHz = (oscfreq - .04) / (double) m;
	int nharm = (int)((xGHz + .01) / 1.105);
	double refMHz = 1000. * (xGHz + .01) / nharm;
	cout << "mharm = " << m << "  xGHz = " << xGHz << " refMHz = " << refMHz << endl;
	int synth_gpib = getIntParameter("synth_gpib");
	double lorep = getDoubleParameter("lorep");
	bool synth_8662 = getBoolParameter("synth_8662");
	BIMASTATUSINFO( rx, "BERKELEY LAB - Set Synth, mharm: " << m << " xGHz: " << xGHz << " refMHz: " << refMHz );
	setSynthFreq( synth_gpib, refMHz, synth_8662);    // set the synth
	setSynthPower( synth_gpib, lorep, synth_8662);
      }
      else	// synth set by somebody else, xfreq specified explicitly
      {
	if ( parameterWasSpecified( "xfreq" ) )
	  xGHz = getDoubleParameter( "xfreq" );
	else {
	  cout << "Unless setsynth=true, keyword 'xfreq' must also be specified" << endl;
	  exit(EXIT_FAILURE);
	}
      }          

      float RFoutLevel;
      if ( parameterWasSpecified( "lotermRFout" ) ) {
        RFoutLevel = (float)getDoubleParameter( "lotermRFout" );
      } else {
	RFoutLevel = 0.90;
      }
      cout << "Setting RFout to " << RFoutLevel << " mV" << endl;
      rx.setLOTermRFout( RFoutLevel );

      BIMASTATUSINFO( rx, "Lock X-Band: " << xGHz );
      lo.lockX( xGHz ); 

      if (band != 'E' ) {
        cout << "Tuning Rx  " << oscfreq << endl;
        BIMASTATUSINFO( rx, "Lock MM osc: " << oscfreq );
        rx.lockmm( oscfreq );
      }

      BIMASTATUSINFO( rx, "Finished setfreq attempt, lo1: " << lo1 << " xfreq: " << xGHz << " oscfreq: " << oscfreq );
    }

    if ( parameterWasSpecified( "tune" ) )
    {

      Rx rx( config );
      if (rx.getBand() != 'E' ) {
	double freq;
	freq = rx.getLO1Freq();
	
	cout << "optimizing SIS bias for " << freq << " GHz" << endl;
	rx.tune( freq, true, true );
	cout << "done!" << endl;
      }
    }

    // note: keep this at the end!!!
    if ( parameterWasSpecified( "getpwr") )
    {
      logger << Priority::INFO << "getpwr specified";

      CalWheel cw( config );
      Dewar d( config );
      Rx rx( config );
      int j;
      double vb, ib;

      double freq = rx.getLO1Freq();
      string motorName = (freq > 150.) ? "attnD" : "attnB" ;
      aMotor = new Motor( motorName, config );
      unsigned short attenpos = aMotor->position();
      //      printf("atten = %d\n",attenpos);

      cw.setPosition( CalWheel::AMB, rx.getDewarWindow() );
      double pAmbAgilent = 0.;
      double pAmbAtoD = 0.;
      double tmxr = 0.;
      double vAmb = 0.;
      double iAmb = 0.;
      for (j=0; j<6; j++) {
	pAmbAgilent += 1000.*pow(10., 0.1*readPwrMeter() );	       // convert from dBm to uW
	pAmbAtoD += rx.getIFTotPower() ;
	tmxr += d.stage4temp() ;
	rx.getSISVIBias( vb, ib );
	vAmb += vb ;
	iAmb += ib ;
	//	usleep(30000) ;
      }

      cw.setPosition( CalWheel::SKY, rx.getDewarWindow() );
      double pSkyAgilent = 0.;
      double pSkyAtoD = 0.;
      double vSky = 0.;
      double iSky = 0.;
      for (j=0; j<6; j++) {
	pSkyAgilent += 1000.*pow(10., 0.1*readPwrMeter() ) ;	// convert from dBm to uW
	pSkyAtoD += rx.getIFTotPower() ;
	tmxr += d.stage4temp() ;
	rx.getSISVIBias( vb, ib );
	vSky += vb ;
	iSky += ib ;
	//	usleep(30000) ;
      }

      double trcvrAgilent = (215. * pSkyAgilent / (pAmbAgilent-pSkyAgilent)) - 77.;
      double trcvrAtoD = (215. * pSkyAtoD / (pAmbAtoD-pSkyAtoD)) - 77.;
      printf("%8.3f %6.3f %5d  %6.3f %6.3f  %6.2f %6.2f  %7.4f %7.4f %8.2f   %7.4f %7.4f %8.2f\n", 
	  freq, tmxr/12., attenpos, vAmb/6., vSky/6., iAmb/6., iSky/6.,
	  pAmbAtoD/6., pSkyAtoD/6., trcvrAtoD,
	  pAmbAgilent/6., pSkyAgilent/6., trcvrAgilent); 
    }

    if ( parameterWasSpecified( "diodecurve") )
    {
      char *cmd;
      int gpib_address = 12;
      Rx rx( config );

      static char rawbuf[100];
      int ud = ibdev(0, gpib_address, 0, T1s, 1, 0);
      if (ud < 0)
      {
        fprintf(stderr, "ibdev failed\n");
	printf("ibdev failed\n");
        return -1;
      }
      
      cmd = (char *)"CLS*\n";
      printf("%s\n", cmd);
//      ibwrt(ud, cmd, strlen(cmd));
//      cmd = (char *)":CONF:VOLT:DC";
//      ibwrt(ud, cmd, strlen(cmd));
//      cmd = (char *)":TRIG:SOUR IMM";
//      ibwrt(ud, cmd, strlen(cmd));

 
      double mA = -2.0532 - .3422;
      double offset = 2.7376;	// setModulator(offset) -> 0 mA out

      while (mA <= 10.2) {
        mA = mA + 0.3422;
        rx.setModulator( mA + offset );  	
        sleep(1);
        cmd = (char *)":MEAS:VOLT:DC?\n";
        ibwrt(ud, cmd, strlen(cmd));
        if(ibrd(ud, rawbuf, RAWBUF_LEN) & ERR)
        {
          fprintf(stderr, "ibrd failed\n");
          // Close device
          ibonl(ud, 0);
          return -1;
        }
        double volts = strtof(rawbuf, NULL);
        printf("%9.4f  %9.4f\n",volts,mA);
      }
      rx.setModulator( offset );  
    }
                
    if ( parameterWasSpecified( "read34401") )
    {
      char *cmd;
      int gpib_address = 12;
      static char rawbuf[100];
      int ud = ibdev(0, gpib_address, 0, T1s, 1, 0);
      if (ud < 0)
      {
        printf("ibdev failed\n");
	return 0;
      }
      cmd = (char *)":MEAS:VOLT:DC?\n";
      ibwrt(ud, cmd, strlen(cmd));
      if(ibrd(ud, rawbuf, RAWBUF_LEN) & ERR)
      {
        printf("ibrd failed\n");
        ibonl(ud, 0);  // close device
	return 0;
      }
      double volts = strtof(rawbuf, NULL);
      printf("%11.6f\n",volts);
    }

      
// temporarily set up manually; avg=64, video bw = 10 kHz; avgtype=power
    if ( parameterWasSpecified( "passband") )
    {
      char *cmd; 
// #define RAWBUF_LEN 64000
//      static char rawbuf[RAWBUF_LEN];
       
      int gpib_address = SPECTRUM_ANALYZER_GPIB;
      CalWheel cw( config );
      Dewar d( config );
      Rx rx( config );
      float amb[401],cold[401];
      float start_freq, stop_freq, freq_step, ymin ;
      float y, fGHz, trcvr ;
      double ambav, coldav;
      float tamb = 297.;
      float tcold = 77.;
      int num_points;  // number of points actually returned by spectrum analyzer
      int npts = 401;  // number of points in passband computation
      int istep = 3;  // number of points for boxcar avg

      int ud = ibdev(0, gpib_address, 0, T1s, 1, 0);
      if (ud < 0)
      {
        fprintf(stderr, "ibdev failed\n");
	printf("ibdev failed\n");
        return -1;
      }

      cw.setPosition( CalWheel::AMB, rx.getDewarWindow() );
      cmd = (char *)":sense:average:state on";
      ibwrt(ud, cmd, strlen(cmd));
//      cmd = (char *)":sense:average:clear";
//      ibwrt(ud, cmd, strlen(cmd));
      cmd = (char *)":init:imm";         // start single sweep
      ibwrt(ud, cmd, strlen(cmd));
/* ---------------------------------------------------------
      cmd = (char *)"*OPC?";
      if(ibwrt(ud, cmd, strlen(cmd)) & ERR)
      {
         fprintf(stderr, "ibwrt(%s) failed\n", cmd);
         // Close device
         ibonl(ud, 0);
         return -1;
      }
---------------------------------------------------------*/
      sleep(20);
//      cmd = (char *)":trace:math:smooth:points 25";
//      ibwrt(ud, cmd, strlen(cmd));
//      cmd = (char *)":trace:math:smooth";
//      ibwrt(ud, cmd, strlen(cmd));
      int rc = dumpSpectrumAnalyzer(amb, npts, &start_freq, &stop_freq, &num_points);
      printf("# retrieved %d points\n", rc);
      printf("# start_freq is %f\n", start_freq);
      printf("# stop_freq is %f\n", stop_freq);
      printf("# num_points is %d\n", num_points);

      cw.setPosition( CalWheel::SKY, rx.getDewarWindow() );
//      cmd = (char *)":sense:average:clear";
//      ibwrt(ud, cmd, strlen(cmd));
      cmd = (char *)":init:imm\n";         // start single sweep, wait for completion
      ibwrt(ud, cmd, strlen(cmd));
      sleep(20);
//      cmd = (char *)":trace:math:smooth";
//      ibwrt(ud, cmd, strlen(cmd));
      rc = dumpSpectrumAnalyzer(cold, npts, &start_freq, &stop_freq, &num_points);
      printf("# retrieved %d points\n", rc);
      printf("# start_freq is %f\n", start_freq);
      printf("# stop_freq is %f\n", stop_freq);
      printf("# num_points is %d\n", num_points);

      freq_step = (stop_freq - start_freq)/float(num_points);
      ymin = (9999. + tamb)/(9999. + tcold);	// max trcvr to calculate is 9999.
      for(int i=0; i<(npts-istep); i=i+istep) {
          fGHz = 0.;
          ambav = 0.;
          coldav = 0.;
          for (int j=0; j<istep; j++) {
            fGHz = fGHz + (start_freq + (i+j) * freq_step)/1.e9;	// convert freq to GHz
//	    printf(" j = %d, fGHz = %.9f\n", j, fGHz);
            ambav = ambav + pow(10.,0.1*amb[i+j]);
            coldav = coldav + pow(10.,0.1*cold[i+j]);
          } 
          y = ambav/coldav;
          trcvr = 9999. ;
          if (y > ymin) {
            trcvr = (tamb - y*tcold)/(y - 1.);
          }
	  ambav = 10.*log10(ambav);
	  coldav = 10.*log10(coldav);
          printf("%13.9f  %9.4f %9.4f  %8.2f\n", fGHz/float(istep), ambav, coldav, trcvr ); 
      }
    }

    if ( parameterWasSpecified( "sptrace") )
    {
      char *cmd; 
       
      int gpib_address = SPECTRUM_ANALYZER_GPIB;
      float buf[401];
      float start_freq, stop_freq, freq_step ;
      float fMHz ;
      int num_points;  // number of points actually returned by spectrum analyzer
      int npts = 401;  // number of points in passband computation

      int ud = ibdev(0, gpib_address, 0, T1s, 1, 0);
      if (ud < 0)
      {
        fprintf(stderr, "ibdev failed\n");
	printf("ibdev failed\n");
        return -1;
      }

      cmd = (char *)":init:imm";         // start single sweep
      ibwrt(ud, cmd, strlen(cmd));
      sleep(5);
      int rc = dumpSpectrumAnalyzer(buf, npts, &start_freq, &stop_freq, &num_points);
      printf("# retrieved %d points\n", rc);
      printf("# start_freq is %f\n", start_freq);
      printf("# stop_freq is %f\n", stop_freq);
      printf("# num_points is %d\n", num_points);

      freq_step = (stop_freq - start_freq)/float(num_points);
      for (int i=0; i<npts; i++) {
          fMHz = (start_freq + i * freq_step)/1.e6;	// convert freq to MHz
          printf("%15.9f  %9.4f\n", fMHz, buf[i] ); 
      }
    }

    // network analyzer test
    if ( parameterWasSpecified( "network" ) )
    {
      dumpNetworkAnalyzer( );
    }

    // add this to calibrate PAM attenuators
    if ( parameterWasSpecified( "getpwr1") )
    {
      Rx rx( config );

      double Agilent = readPwrMeter();	
      double Pam = rx.getIFTotPower();
      printf( "%9.5f  %8.5f\n", Agilent,Pam);
    }


    // harmonic check: suggest "bimaRx hcheck=1 | grep Freq:" for easy to read listing
    if ( parameterWasSpecified( "hcheck") )
    {
      Rx rx( config );
      LO lo( config );
      //int hfinal = getIntParameter("hcheck");	// if valid, will be used as override harmonic
      double oscfreq = rx.getOscFreq();
      //int mstart = (int) ((oscfreq - 0.05)/lo.getCommanded() + 0.001) ;
      const unsigned int ssSize = 140;
      char ss[ssSize];
      int m;

      for ( m=15; m>5; m--)
      {
	double xGHz = (oscfreq - .04) / (double) m;
	if ( (xGHz > 8.0) && (xGHz < 12.5) )
	{
	  int nharm = (int)((xGHz + .01) / 1.105);
	  double refMHz = 1000. * (xGHz + .01) / nharm;
	  cout << "mharm = " << m << "  xGHz = " << xGHz << " refMHz = " << refMHz << endl;
	  int synth_gpib = getIntParameter("synth_gpib");
	  double lorep = getDoubleParameter("lorep");
	  bool synth_8662 = getBoolParameter("synth_8662");
	  setSynthFreq( synth_gpib, refMHz, synth_8662);    // set the synth
	  setSynthPower( synth_gpib, lorep, synth_8662);
	  lo.lockX( xGHz );
	  sleep(1);
	  snprintf(ss, ssSize, "Freq: %7.3f  mharm:%3d   MMlock:%2d  MMerrV: %5.2f  PhNoise: %4.2f   Xfreq: %6.3f  Xlock:%2d\n",
	      rx.getOscFreq(), m, rx.mmLockStatus(), rx.mmErrorVolts(), rx.getPhaseNoise(),
	      xGHz, lo.xLockStatus() );
	  printf("%s", ss);
	}
      }
    }

  }
  catch ( const carma::util::ErrorException & ee )
  {
    cerr << ee.what() << endl;
    sleep(1);
  }

  return 1;

}

