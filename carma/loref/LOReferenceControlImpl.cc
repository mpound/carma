/**
 *
 * Implementation for LO reference.
 *
 * @author: Steve Scott
 *
 * $Id: LOReferenceControlImpl.cc,v 1.68 2014/06/27 21:40:59 scott Exp $
 * $CarmaCopyright$
 *
 */

#include <iostream>
#include <log4cpp/Category.hh>
#include "carma/corba/corba.h"
#include "carma/loref/LOReferenceControlImpl.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/programLogging.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <gpib/ib.h>

// Convenience macro to convert BCD byte to int
#define bcdbyte2int(b) (10*((b&0xf0)>>4)+(b&0x0f))

using namespace ::std;
using namespace ::carma::loref;
using namespace ::carma::monitor;
using namespace ::carma::util;
using namespace log4cpp;
using namespace std;

// Convenience macros for setting/getting monitor point values
#define SET_SYNTH_MP(i,m,v)   monitor_->loRefSynthesizer(i).m().setValue(v)
#define GET_SYNTH_MP(i,m)     monitor_->loRefSynthesizer(i).m().getValue()
#define SET_DISTBOX_MP(i,m,v) monitor_->loRefDistributionBox(i).m().setValue(v)
#define GET_DISTBOX_MP(i,m)   monitor_->loRefDistributionBox(i).m().setValue()
typedef  LoRefSubsystem::LoRefSynthesizer     CM_SYNTH;
typedef  LoRefSubsystem::LoRefDistributionBox CM_BOX;

namespace {
  // Lock object to serialize gpib access
  ::pthread_mutex_t gpibLock = PTHREAD_MUTEX_INITIALIZER;
} // anonymous namespace

LOReferenceControlImpl::LOReferenceControlImpl(
    int counter_gpib,
    int switch_gpib,
    int synth_gpib[],
    int synth_8662[],
    int synth_mux[],
    bool emulate,
    const double autoWriteDelayInS ) :
  monitor_(new ::carma::monitor::LoRefSubsystem()),
  log_(::carma::util::Program::getLogger()),
  counter_gpib_(counter_gpib),
  switch_gpib_(switch_gpib),
  emulate_(emulate)
{
  for(int i = 0; i < 3; i++) {
    synth_gpib_[i] = synth_gpib[i];
    synth_8662_[i] = synth_8662[i];
    synth_mux_[i] = synth_mux[i];
  }

  monitor_->readNewest();

  monitor_->startAutoWriter( autoWriteDelayInS );
  synthCount_ = monitor_->loRefSynthesizerCount();
  boxCount_   = monitor_->loRefDistributionBoxCount();

  // Only support up to three synths
  if(synthCount_ > 3) {
    synthCount_ = 3;
  }

  // Emulated synthesizers are perfect
  if(emulate_) {
    for( unsigned int i = 0; i < synthCount_; ++i) {
      SET_SYNTH_MP(i, synthModel, "Emulatron Frequency Synthesizer - Model T");
      SET_SYNTH_MP(i, synthStatus, "Emulated");
    }
    for( unsigned int i = 0; i < boxCount_; ++i) {
      SET_DISTBOX_MP(i, synthFreqErr, 0);
      SET_DISTBOX_MP(i, loRefOffsetFreqErr, 0);
      SET_DISTBOX_MP(i, masterOffsetFreqErr, 0);
    }
  } 
  else {
    for( unsigned int i = 0; i < synthCount_; ++i) {
      const char* model = "(no synthesizer configured)";
      if(0 <= synth_gpib_[i]) {
        if(synth_8662_[i]) {
          // Init synth to known state
          gpib(0, synth_gpib_[i], "BLX6 M0 W1");
          model = "HP 8662 (or compatible)";
        } else {
          // TODO Init synth to known state
          model = gpib(0, synth_gpib_[i], "*IDN?");
        }
        // At the start of this process, turn on RF power
        // and set output to 16 dBm
        setRFOutput(i+1, true);
        setPower(i+1, 16.0);

        // Set "commanded" MPs to be the same as "reported"
        commandedFreq[i]    = getFrequency(i+1);
        commandedFreqValid[i] = true;
        SET_SYNTH_MP(i, synthFreqCmd, commandedFreq[i]);
        SET_SYNTH_MP(i, synthAmpCmd, getPower(i+1));
      }
      SET_SYNTH_MP(i, synthModel, model);
    }
    monitor_->write();
  }

  // Start pthread
  log_.info("Starting monitor thread");
  carma::util::StartPthreadWithRef(
      LOReferenceControlImpl::monitorGPIB, *this);
}

LOReferenceControlImpl::~LOReferenceControlImpl()
{
}

// This is the entry point method for the GPIB monitoring thread.
// It is static, but takes a LOReferenceControlImpl& (named "self")
// so things like self.memberFunction() will work as if this were
// a non-static member function. It run an infinite loop and is run in
// its own thread.
void
LOReferenceControlImpl::monitorGPIB(LOReferenceControlImpl& self)
    try {
  CARMA_CPTRACE( ::carma::util::Trace::TRACE6, __FUNCTION__ << " starting");
  self.log_.info("Monitor thread started");
  bool debug = false;

  // Timer parameters
  const long offsetNanos = 5000;   // 5 usec
  const long frames = 2;           // 1 sec

  // There is no particular reason that this needs to be frame aligned
  // The sleep is only to keep the process from chewing all the CPU if
  // for some reason the counting is skipped or drops through. Normally,
  // the loop is dominated by counting 3 synthesizers in 1+ seconds.
  carma::util::FrameAlignedTimer framer(offsetNanos, frames);

  // Loop forever
  while(true) {
    //framer.ResetNextFireTimeAndWait(frames);
    CARMA_CPTRACE( ::carma::util::Trace::TRACE7, "Fire!");
    // No emulation code yet...
    if(self.emulate_) return;

    // Loop through synths, read frequency and power
    for(unsigned int i = 0; i < self.synthCount_; i++) {
      #define HWBAD MonitorPoint::INVALID_HW_BAD
      CM_SYNTH& synth = self.monitor_->loRefSynthesizer(i);
      CM_BOX& box     = self.monitor_->loRefDistributionBox(i);
      if(self.synth_gpib_[i] >= 0) {
        try {
          synth.synthFreqRpt().setValue(self.getFrequency(i+1));
        } catch ( ... ) {
          ostringstream o;
          o << "Error counting synthesizer#" << (i+1) << " frequency";
          self.log_.error(o.str());
          synth.synthFreqRpt().setValidity(HWBAD);
        }
        try {
          synth.synthAmpRpt().setValue(self.getPower(i+1));
        } catch ( ... ) {
          ostringstream o;
          o << "Error getting synthesizer#" << (i+1) << " power level";
          self.log_.error(o.str());
          synth.synthAmpRpt().setValidity(HWBAD);
        }
        try {
          synth.synthStatus().setValue(self.getStatus(i+1));
        } catch ( ... ) {
          ostringstream o;
          o << "Error getting synthesizer#" << (i+1) << " status";
          self.log_.error(o.str());
          synth.synthStatus().setValidity(HWBAD);
        }
      }

      // Counting a single synthesizer takes about 350 msecs
      if(self.synth_mux_[i] > 0) {
        self.closeMux(self.synth_mux_[i]);
        if (self.commandedFreqValid[i]) {
          double freqCmd = self.commandedFreq[i];
          double countedFreq = 0.0;
          try {
            double t0 = Time::MJD()*Time::SECONDS_PER_DAY; // Time in seconds
            countedFreq = self.getCounterFrequency(freqCmd);
            double t1 = Time::MJD()*Time::SECONDS_PER_DAY; // Time in seconds
            double dt = t1 - t0;
            double freqErr = countedFreq-freqCmd;
            // This assumes 1:1 mapping between synths and dist boxes!
            box.synthFreqErr().setValue(freqErr);
            box.synthFreqErr().setValidity(MonitorPoint::VALID);
            if (debug) {
              // ****** DEBUGGING *******
              ostringstream o;
              o << "SYNTH#" << i+1 << " COUNTING COMPLETE: "
                << fixed << setprecision(9) << countedFreq*1e-9 << " GHz"
                << ",  took " << setprecision(0) << dt*1000 << "msec";
              programLogInfoIfPossible(o.str());
            }
          } catch ( ... ) {
            ostringstream o;
            o << "Error counting frequency for synthesizer#" << (i+1) ;
            self.log_.error(o.str());
            box.synthFreqErr().setValidity(HWBAD);
          }
        }
        else {
          ostringstream o;
          o << "Setting synthFreqErr MP bad for synthesizer#" << (i+1)
            << " because synthFreqCmd MP is not valid";
          self.log_.error(o.str());
          box.synthFreqErr().setValidity(HWBAD);
        }
      }
      else {
        ostringstream o;
        o << "Setting synthFreqErr MP bad for synthesizer#" << (i+1)
          << " because synthmux < 0";
        self.log_.error(o.str());
        box.synthFreqErr().setValidity(HWBAD);
      }
      // We need a little bit of time between counting each of the synthesizers
      // so that the autowriter has a chance to write to the FSP.
      // This also provides resonable behavior if all of the counting fail
      // immediately, resulting in a possible infinite loop.
      usleep(80000); // 80 msec      
    } // Loop over synthesizers
  }   // While forever
} catch(std::exception& e) {
      ostringstream o;
      o << "std::exception caught in monitoring thread:" << e.what();
      self.log_.crit(o.str());
} catch(...) {
      self.log_.crit("Unkown exception caught in monitoring thread");
}

void
LOReferenceControlImpl::setFrequencyPower(
    ::CORBA::ULong synthesizerIndex,
    ::CORBA::Double frequency,
    ::CORBA::Double power)
{
  CARMA_CPTRACE( ::carma::util::Trace::TRACE6, __FUNCTION__ << "("
      << synthesizerIndex
      << "," << frequency
      << "," << power << ")" );
  /*
  ostringstream info;
  info << "setFrequencyPower(synthIndex=" << synthesizerIndex
         << fixed << setprecision(9)
         << ", freq=" << frequency*1e-9 << " GHz, amp="
         << setprecision(1) << power << " dBm)";
  programLogInfoIfPossible( info.str() );
  */

  // Set the power and then frequency
  // That was the order done at Hat Creek/BIMA (at least for 8662s)
  setPower(synthesizerIndex,power);
  bool log = false;
  setFrequency(synthesizerIndex, frequency, log);
}

void
LOReferenceControlImpl::setFrequency(
    ::CORBA::ULong synthesizerIndex,
    ::CORBA::Double frequency)
{
  bool log = true;
  return setFrequency(synthesizerIndex, frequency, log);
}

void
LOReferenceControlImpl::setFrequency(
    ::CORBA::ULong synthesizerIndex,
    ::CORBA::Double frequency, bool log)
{
  bool debug = false;
  ostringstream info;
  info << "setFrequency(synthIndex=" << synthesizerIndex
       << fixed << setprecision(9)
       << ", freq=" << frequency*1e-9 << " GHz)";
  if (log) programLogInfoIfPossible( info.str() );

  CARMA_CPTRACE( ::carma::util::Trace::TRACE6, info.str() );

  // Check parameters
  if((synthesizerIndex < 1) || (synthesizerIndex > synthCount_)) {
    ostringstream oss;
    oss << "Synthesizer index " << synthesizerIndex
        << " is out of range [1," << synthCount_ << "]";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  }
  synthesizerIndex--; // Convert to zero-based!!!
  if(frequency <= 0) {
    ostringstream oss;
    oss << "Frequency " << frequency
        << " is out of range (0,Inf?)";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  }

  // Set the corresponding monitor point
  SET_SYNTH_MP(synthesizerIndex, synthFreqCmd, frequency);

  // Emulated synthesizers are perfect
  if(emulate_) {
    SET_SYNTH_MP(synthesizerIndex, synthFreqRpt, frequency);
  } else if(synth_gpib_[synthesizerIndex] < 0) {
    ostringstream oss;
    oss << "Address of synthesizer #" << (synthesizerIndex+1)
      << " is unknown.  Add 'synth" << (synthesizerIndex+1)
      << "_gpib=ADDR' to command line.";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  } else {

    char cmd[25]; // 25 should be long enough
    if(synth_8662_[synthesizerIndex]) {
      sprintf(cmd,"FR%.1fHZ",frequency);
    } else {
      sprintf(cmd,"FREQ %lf",frequency);
    }

    if (debug) {
      info.str("");
      info << "SENDING ON GPIB: setFrequency(synthIndex="
           << synthesizerIndex+1
           << fixed << setprecision(9)
           << ", freq=" << frequency*1e-9 << " GHz)";
      programLogInfoIfPossible( info.str() );
    }
    // TODO If we ever use more than 1 GPIB card, we'll have to keep
    // track of a per-device minor number.
    try {
      gpib(0,synth_gpib_[synthesizerIndex], cmd);
    } catch(...) {
      commandedFreqValid[synthesizerIndex] = false;
      throw;
    }
    commandedFreq[synthesizerIndex]      = frequency;
    commandedFreqValid[synthesizerIndex] = true;
    if (debug) {
      info.str("");
      info << "DONE SENDING ON GPIB: setFrequency(synthIndex="
           << synthesizerIndex+1
           << fixed << setprecision(9)
           << ", freq=" << frequency*1e-9 << " GHz)";
      programLogInfoIfPossible( info.str() );
    }
  }
}

void LOReferenceControlImpl::setRFOutput(
    ::CORBA::ULong synthesizerIndex,
    ::CORBA::Boolean onoff)
{
  CARMA_CPTRACE( ::carma::util::Trace::TRACE6, __FUNCTION__ << "("
      << onoff  << ")" );

  if(synthesizerIndex < 1 || synthCount_ < synthesizerIndex)
  {
    ::std::ostringstream oss;
    oss << "Synthesizer index " << synthesizerIndex
      << " is out of range [1," << synthCount_ << "]";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  }

  synthesizerIndex--; // Convert to zero-based

  if(emulate_) {
  }
  else if(synth_gpib_[synthesizerIndex] < 0) {
    ::std::ostringstream oss;
    oss << "Address of synthesizer #" << (synthesizerIndex+1)
        << " is unknown.  Add 'synth" << (synthesizerIndex+1)
        << "_gpib=ADDR' to command line.";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  }
  else {

    char cmd[25]; // 25 should be long enough
    if(synth_8662_[synthesizerIndex]) {
      // TODO, Find out the GPIB command to turn off/on RF power
      ::std::ostringstream oss;
      oss << "Cannot modify RF output power on 8662!";
      logError(oss.str());
    }
    else {
      sprintf(cmd,"OUTP %s",(onoff == true ? "ON" : "OFF"));
    }

    // TODO If we ever use more than 1 GPIB card, we'll have to keep
    // track of a per-device minor number.
    gpib(0,synth_gpib_[synthesizerIndex],cmd);
  }

}


void
LOReferenceControlImpl::setPower(
    ::CORBA::ULong synthesizerIndex,
    ::CORBA::Double power)
{
  CARMA_CPTRACE( ::carma::util::Trace::TRACE6, __FUNCTION__ << "("
      << synthesizerIndex
      << "," << power << ")" );
  const float powerLimit=16.0;

  // Check parameters
  if ( power > powerLimit) {
    ::std::ostringstream oss;
    oss << "Requested power of " << fixed << setprecision(1)
      << power << " dBm is too high; should be <= " << powerLimit << " dBm";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  }

  if(synthesizerIndex < 1 || synthCount_ < synthesizerIndex) {
    ::std::ostringstream oss;
    oss << "Synthesizer index " << synthesizerIndex
      << " is out of range [1," << synthCount_ << "]";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  }
  synthesizerIndex--; // Convert to zero-based

  // Set the corresponding monitor point
  SET_SYNTH_MP(synthesizerIndex, synthAmpCmd, power);

  // Emulated synthesizers are perfect
  if(emulate_) {
    SET_SYNTH_MP(synthesizerIndex, synthAmpRpt, power);
  } else if(synth_gpib_[synthesizerIndex] < 0) {
    ::std::ostringstream oss;
    oss << "Address of synthesizer #" << (synthesizerIndex+1)
        << " is unknown.  Add 'synth" << (synthesizerIndex+1)
        << "_gpib=ADDR' to command line.";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  } else {

    char cmd[25]; // 25 should be long enough
    if(synth_8662_[synthesizerIndex]) {
      sprintf(cmd,"AP%.1fDM",power);
    } else {
      sprintf(cmd,"POW %lf",power);
    }

    // TODO If we ever use more than 1 GPIB card, we'll have to keep
    // track of a per-device minor number.
    gpib(0,synth_gpib_[synthesizerIndex],cmd);
  }
}

// Get freq that the synthesizer thinks it is at.
::CORBA::Double
LOReferenceControlImpl::getFrequency(::CORBA::ULong synthesizerIndex)
{
  double frequency = -1e10;

  CARMA_CPTRACE( ::carma::util::Trace::TRACE6, __FUNCTION__ << "("
      << synthesizerIndex << ")" );

  // Check parameters
  if(synthesizerIndex < 1 || synthCount_ < synthesizerIndex) {
    ::std::ostringstream oss;
    oss << "Synthesizer index " << synthesizerIndex
        << " is out of range [1," << synthCount_ << "]";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  }
  synthesizerIndex--; // Convert to zero-based

  // Emulated synthesizers are perfect
  if(emulate_) {
    return GET_SYNTH_MP(synthesizerIndex, synthFreqCmd);
  } else if(synth_gpib_[synthesizerIndex] < 0) {
    ::std::ostringstream oss;
    oss << "Address of synthesizer #" << (synthesizerIndex+1)
        << " is unknown.  Add 'synth" << (synthesizerIndex+1) 
        << "_gpib=ADDR' to command line.";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  }
  else {
    // 25 should be long enough
    char cmd[25];
    if(synth_8662_[synthesizerIndex]) {
      strcpy(cmd,"L1?");
    } else {
      strcpy(cmd,"FREQ?");
    }

    // TODO If we ever use more than 1 GPIB card, we'll have to keep
    // track of a per-device minor number.
    char* response = gpib(0, synth_gpib_[synthesizerIndex], cmd);

    // Same format for 8662 and SCPI commands
    const char* fmt = "%lf";

    if (strncmp("ERROR", response, 5) == 0) {
      throw CARMA_EXCEPTION(carma::util::UserException, response);
    }

    if(sscanf(response, fmt, &frequency) == 0) {
      ostringstream oss;
      oss << "Error parsing frequency response \'" << response
        << "\' using format \'" << fmt << "\'";
      logError(oss.str());
      throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
    }
  }
  return frequency;
}

::CORBA::Double
LOReferenceControlImpl::getPower(::CORBA::ULong synthesizerIndex)
{
  double power = -1e10;

  CARMA_CPTRACE( ::carma::util::Trace::TRACE6, __FUNCTION__ << "("
      << synthesizerIndex << ")" );

  // Check parameters
  if(synthesizerIndex < 1 || synthCount_ < synthesizerIndex) {
    ::std::ostringstream oss;
    oss << "Synthesizer index " << synthesizerIndex
      << " is out of range [1," << synthCount_ << "]";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  }
  synthesizerIndex--; // Convert to zero-based

  // Emulated synthesizers are perfect
  if(emulate_) {
    return GET_SYNTH_MP(synthesizerIndex, synthAmpCmd);
  } else if(synth_gpib_[synthesizerIndex] < 0) {
    ::std::ostringstream oss;
    oss << "Address of synthesizer #" << (synthesizerIndex+1)
      << " is unknown.  Add 'synth" << (synthesizerIndex+1)
      << "_gpib=ADDR' to command line.";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  } else {
    // 25 should be long enough
    char cmd[25];
    if(synth_8662_[synthesizerIndex]) {
      strcpy(cmd,"L1?");
    } else {
      strcpy(cmd,"POW?");
    }

    // TODO If we ever use more than 1 GPIB card, we'll have to keep
    // track of a per-device minor number.
    char* response = gpib(0,synth_gpib_[synthesizerIndex],cmd);

    std::string fmtstr = "%lf";
    if(synth_8662_[synthesizerIndex]) {
      fmtstr = "%*lf,%lf";
    }
    const char* fmt = fmtstr.c_str();

    if(sscanf(response, fmt, &power) == 0) {
      ::std::ostringstream oss;
      oss << "Error parsing response to " << cmd << ": \'" << response
        << "\' using format \'" << fmt << "\'";
      logError(oss.str());
      throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
    }
  }
  return power;
}

std::string LOReferenceControlImpl::getStatus(::CORBA::ULong synthesizerIndex)
{
  std::string status;

  CARMA_CPTRACE( ::carma::util::Trace::TRACE6, __FUNCTION__ << "("
      << synthesizerIndex << ")" );

  // Check parameters
  if(synthesizerIndex < 1 || synthCount_ < synthesizerIndex) {
    ::std::ostringstream oss;
    oss << "Synthesizer index " << synthesizerIndex
      << " is out of range [1," << synthCount_ << "]";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  }
  synthesizerIndex--; // Convert to zero-based

  // Emulated synthesizers are perfect
  if(emulate_) {
    return ::CORBA::string_dup("Emulated");
  } else if(synth_gpib_[synthesizerIndex] < 0) {
    ::std::ostringstream oss;
    oss << "Address of synthesizer #" << (synthesizerIndex+1)
      << " is unknown.  Add 'synth" << (synthesizerIndex+1)
      << "_gpib=ADDR' to command line.";
    logError(oss.str());
    throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str());
  } else {
    // 25 should be long enough
    char cmd[25];
    if(synth_8662_[synthesizerIndex]) {
      strcpy(cmd,"MS?");
    } else {
      strcpy(cmd,"SYST:ERR?");
    }

    // TODO If we ever use more than 1 GPIB card, we'll have to keep
    // track of a per-device minor number.
    char* response = gpib(0,synth_gpib_[synthesizerIndex],cmd);

    if(synth_8662_[synthesizerIndex]) {
      if(!strncmp(response,"00",2) || !strncmp(response,"13,00",5)) {
        status += "OK ";
      } else {
        status += "ERROR ";
      }
    } else {
      if(!strncmp(response,"+0,",3)) {
        status += "OK ";
      } else {
        status += "ERROR ";
      }
    }

    status += response;
  }

  // truncate to 80 characters max
  return std::string(status, 0, 80);
}

void
LOReferenceControlImpl::closeMux(int mux )
{
  // Bail out if GPIB of switch is not known
  if(switch_gpib_ < 0) {
    // TODO Throw exception
    return;
  }

  char cmd[30];
  sprintf(cmd, "ROUTE:CLOSE (@%d)", mux);
  gpib(0, switch_gpib_, cmd);

  if(120 <= mux) {
    gpib(0, switch_gpib_, "ROUTE:CLOSE (@111)");
  }
}

double LOReferenceControlImpl::getCounterFrequency(double expectedValue)
{
  // Bail out if GPIB of counter is not known
  if(counter_gpib_ < 0) {
    // TODO Throw exception
    return 0.0;
  }

  double frequency = 0.0;
  char cmd[80];
  const char * fmt = "%lf";
  // TODO Be more sophisticated in how we measure
  sprintf(cmd, "MEASURE:FREQUENCY? %lf,1,(@2)", expectedValue);
  const char * response = gpib(0, counter_gpib_, cmd);

  if(sscanf(response, fmt, &frequency) == 0) {
    ostringstream oss;
    oss << "Error parsing response to Counter cmd: " << cmd << " \'"
      << response
      << "\' using format \'" << fmt << "\'";
    logError(oss.str());
    throw CARMA_EXCEPTION(
        carma::util::UserException, oss.str().c_str());
  }

  return frequency;
}

//
// IDL:carma/loref/LOReferenceControl/Gpib:1.0
//
char* LOReferenceControlImpl::gpib(
    ::CORBA::ULong minor,
    ::CORBA::ULong address,
    const char* command)
{
  int sad = 0;
  int timeout = T1s;
  int send_eoi = 1;
  int eos_mode = 0;
  static char buffer[4096];
  int bytes_read = 0;
  ::std::ostringstream oss;

  CARMA_CPTRACE( ::carma::util::Trace::TRACE6, __FUNCTION__ << "("
      << minor
      << ","  << address
      << ",'" << command << "')" );

  // Serialize access to gpib
  carma::util::ScopedLock< ::pthread_mutex_t > lock ( gpibLock );

  int ud = ibdev( minor, address, sad, timeout, send_eoi, eos_mode );

  if ( ud < 0 ) {
    // TODO throw exception
    oss << "Error opening gpib with gpib(minor=" << minor
        << ", address=" << address << "); GPIB status=" << ibsta;
    logError(::carma::util::Program::getLogger(), oss.str());
    return ::CORBA::string_dup("ERROR OPENING GPIB");
  }

#if 0
  // Set I/O timeout
  if( ibtmo( ud, T3s )) {
    int ibsta_save = ibsta;
    // Close device
    ibonl(ud, 0);
    // TODO throw exception?
    oss.seekp(::std::ostringstream::beg);
    oss << "ERROR setting GPIB timeout to 3 seconds (" << ibsta_save << ")";

    logError(::carma::util::Program::getLogger(), oss.str());
    return ::CORBA::string_dup(oss.str().c_str());
  }
#endif

  buffer[0] = '\0';

  oss << command << '\n';

  // Special case command to clear device
  if ( !strncasecmp(command, "#CLR", 4) ) {
    // Clear device
    if(ibclr(ud) & ERR) {
      // Close device
      ibonl(ud, 0);
      // TODO throw exception
      oss.seekp(::std::ostringstream::beg);
      oss << "ERROR clearing device.";
      logError(::carma::util::Program::getLogger(), oss.str());
      return ::CORBA::string_dup(oss.str().c_str());
    }
  }
  else if(ibwrt(ud, oss.str().c_str(), min(oss.str().length(),size_t(4096))) & ERR) {
    // Close device
    ibonl(ud, 0);
    // TODO throw exception
    oss.seekp(::std::ostringstream::beg);
    oss << "ERROR sending command:" << command;
    logError(::carma::util::Program::getLogger(), oss.str());
    return ::CORBA::string_dup(oss.str().c_str());
  }
  else {
    if ( strchr( command, '?' ) != NULL ) {
      if ( ibrd( ud, buffer, 4096 - 1 ) & ERR ) {
        // Close device
        ibonl(ud, 0);
        // TODO throw exception?
        oss.seekp(::std::ostringstream::beg);
        oss << "Error status returned after sending command:"
            << " command=" << command << ",   status=" << ibsta ;
        logError(Program::getLogger(), oss.str());
        return ::CORBA::string_dup("ERROR SENDING COMMAND");
      } else {
        bytes_read = ibcnt;
        /* null terminate */
        buffer[bytes_read] = '\0';
        /* chomp \n and \r */
        while(bytes_read > 0 &&
            (buffer[bytes_read-1] == '\n' || buffer[bytes_read-1] == '\r')) {
          buffer[bytes_read-1] = '\0';
          bytes_read--;
        }

        // Special cases for certain 8662 responses
        // See pages 3-44 and 3-45 in the 8662A Operating Manual
        // (HP Part Number 08662-90031.  Printed: August 1979)

        // For L1 command, return "Hz,dBm" string
        if(!strncasecmp(command, "L1?", 3)) {
          // Frequency (deciHertz) in buffer[10..5] in BCD
          unsigned long long dHz = bcdbyte2int(buffer[10] & 0x0f);
          for(int i = 9; i > 4; --i) {
            dHz = 100 * dHz + bcdbyte2int(buffer[i]);
          }
          // High frequencies have only 0.2 Hz resolution
          if(dHz > 6400000000ULL) {
            dHz &= ~1ULL;
          }
          // Power (centi-dBm) in buffer[34..32] in BCD
          int cdBm =
            10000 * bcdbyte2int(buffer[34] & 0x0f)
            + 100 * bcdbyte2int(buffer[33]       )
            +       bcdbyte2int(buffer[32] & 0xf0);
          if( (buffer[34] & 0xf0) == 0x80 ) {
            cdBm = -cdBm;
          }
          sprintf(buffer,"%10.1lf,%+3.1lf", dHz/10.0, cdBm/100.0);
        }
      }
    }
  }

  // Close device
  ibonl(ud, 0);

  // Trace return value
  CARMA_CPTRACE( ::carma::util::Trace::TRACE6, __FUNCTION__
      << " returning '" << buffer << "'");

  return ::CORBA::string_dup(buffer);
}

void
LOReferenceControlImpl::logError(const std::string& msg) const
{
  logError(log_, msg);
}

void LOReferenceControlImpl::logError(Category& log, const std::string& msg)
{
  log.error(msg);
}

// vim: set expandtab sw=2 ts=2 :
