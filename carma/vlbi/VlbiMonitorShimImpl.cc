#include <carma/util/Trace.h>
#include <carma/util/FrameAlignedTimer.h>
#include <carma/util/ScopedPthreadMutexLock.h>
#include <carma/util/StartPthread.h>
#include <carma/util/Time.h>
using namespace carma::util;

#include <carma/correlator/obsRecord2/CorDataBase.h>
#include <carma/vlbi/VlbiMonitorShimImpl.h>
using namespace carma::vlbi;

#include <string>
using namespace std;

using namespace carma::monitor;

VlbiMonitorShimImpl::VlbiMonitorShimImpl(double autoWriteDelayInS)
  : _mon( *(new VlbiSubsystem))
  , _autoWriteDelayInS(autoWriteDelayInS)
{
  // Init _bandata
  for(int i=1; i<=num_bands; i++) {
    VlbiBandData & vbd = bandData_[i];
    vbd.delays.valid_frames = 0;
    vbd.bandwidth_ok   = false;
    vbd.fpgaMode_ok    = false;
    vbd.seqNo_ok       = false;
    vbd.astroBandNo_ok = false;
    vbd.noiseOn_ok     = false;
    vbd.centerMHz_ok   = false;
    vbd.dconMHz_ok     = false;
    vbd.sb_ok          = false;
    vbd.bdcEnabled_ok  = false;
    vbd.lastUpdated_ok = false;
    vbd.online_ok      = false;
  }

  // Start monitor thread
  StartPthreadWithRef( VlbiMonitorShimImpl::thread, *this );
}

void
VlbiMonitorShimImpl::setOnlineState (
  ::CORBA::Long band,
  ::CORBA::Boolean isOnline)
{
    ScopedPthreadMutexLock lock(mutex_);

    VlbiBandData & vbd = bandData_[band];

    vbd.online = isOnline;

    vbd.online_ok = true;
}

void
VlbiMonitorShimImpl::setBandwidth (
  ::CORBA::Long band,
  ::carma::vlbi::BandWidthType bandwidth,
  ::carma::vlbi::FpgaModeType fpgaMode,
  ::CORBA::Long seqNo,
  ::CORBA::Long astroBandNo)
{
    ScopedPthreadMutexLock lock(mutex_);

    VlbiBandData & vbd = bandData_[band];

    vbd.bandwidth   = bandwidth;
    vbd.fpgaMode    = fpgaMode;
    vbd.seqNo       = seqNo;
    vbd.astroBandNo = astroBandNo;
    vbd.lastUpdated = Time::MJD();

    vbd.bandwidth_ok   = true;
    vbd.fpgaMode_ok    = true;
    vbd.seqNo_ok       = true;
    vbd.astroBandNo_ok = true;
    vbd.lastUpdated_ok = true;
}

void
VlbiMonitorShimImpl::setNoiseSourceState (
  ::CORBA::Long band,
  ::CORBA::Boolean isOn)
{
    ScopedPthreadMutexLock lock(mutex_);

    VlbiBandData & vbd = bandData_[band];

    vbd.noiseOn = isOn;
    vbd.lastUpdated = Time::MJD();

    vbd.noiseOn_ok = true;
    vbd.lastUpdated_ok = true;
}

void
VlbiMonitorShimImpl::setDelays (
  ::CORBA::Long band,
  const ::carma::vlbi::DelayTripletSeq & delays)
{
    ScopedPthreadMutexLock lock(mutex_);

    VlbiBandData & vbd = bandData_[band];

    vbd.delays.seq = delays;
    vbd.delays.valid_frames = 25*2; // 20 (+5 extra) seconds @ 2 Hz
    vbd.lastUpdated = Time::MJD();
    vbd.lastUpdated_ok = true;
}

void
VlbiMonitorShimImpl::setDownconverter (
  ::CORBA::Long band,
  ::CORBA::Double centerMHz,
  ::CORBA::Double dconMHz,
  ::carma::vlbi::SidebandType sb,
  ::CORBA::Boolean bdcEnabled)
{
    ScopedPthreadMutexLock lock(mutex_);

    VlbiBandData & vbd = bandData_[band];

    vbd.centerMHz  = centerMHz;
    vbd.dconMHz    = dconMHz;
    vbd.sb         = sb;
    vbd.bdcEnabled = bdcEnabled;
    vbd.lastUpdated = Time::MJD();

    vbd.centerMHz_ok  = true;;
    vbd.dconMHz_ok    = true;;
    vbd.sb_ok         = true;
    vbd.bdcEnabled_ok = true;
    vbd.lastUpdated_ok = true;
}

// Thread run method
void VlbiMonitorShimImpl::run()
{
  CPTRACE( Trace::TRACE1, " Launching Monitor Thread" );

  FrameAlignedTimer framer;

  _mon.startAutoWriter( _autoWriteDelayInS );
  framer.ResetNextFireTime();

  while ( true )
  {
    framer.WaitForNextFireTime();

    ScopedPthreadMutexLock lock(mutex_);

    for(int band_idx=1; band_idx <= num_bands; band_idx++) {

      VlbiBandData & vbd = bandData_[band_idx];
      // Monitor system band index is 0-based
      VlbiSubsystem::Band & band = _mon.band(band_idx-1);

      // Set control monitor points
      VlbiSubsystem::Control & control = band.control();

      if(vbd.online_ok) {
        control.online().setValue(vbd.online);
        if(vbd.lastUpdated_ok) {
          control.lastUpdated().setValue(vbd.lastUpdated);
        }
        if(vbd.astroBandNo_ok) {
          control.bandNumber().setValue(vbd.astroBandNo);
        }
        if(vbd.noiseOn_ok) {
          control.sourceName().setValue(vbd.noiseOn ? "noise" : "rf");
        }
        if(vbd.centerMHz_ok) {
          control.centerFrequency().setValue(vbd.centerMHz);
        }
        if(vbd.dconMHz_ok) {
          control.dconFrequency().setValue(vbd.dconMHz);
        }
        if(vbd.sb_ok) {
          VlbiSubsystem::DconSidebandMonitorPointEnum::DCONSIDEBAND mpval = VlbiSubsystem::DconSidebandMonitorPointEnum::LSB;
          // Map IDL sideband enum to MonitorPoint sideband enum
          switch(vbd.sb) {
            case carma::correlator::obsRecord2::LOWER_SB:
              mpval = VlbiSubsystem::DconSidebandMonitorPointEnum::LSB;
              break;
            case carma::correlator::obsRecord2::UPPER_SB:
              mpval = VlbiSubsystem::DconSidebandMonitorPointEnum::USB;
              break;
          }
          control.dconSideband().setValue(mpval);
        }
        if(vbd.bdcEnabled_ok) {
          control.bdcEnabled().setValue(vbd.bdcEnabled);
        }
        if(vbd.fpgaMode_ok) {
          VlbiSubsystem::AstroBandModeMonitorPointEnum::ASTROBANDMODE mpval = VlbiSubsystem::AstroBandModeMonitorPointEnum::SINGLEPOL;
          // Map IDL fpgaMode enum to MonitorPoint astroBandMode enum
          switch(vbd.fpgaMode) {
            case carma::correlator::obsRecord2::SINGLEPOL:
              mpval = VlbiSubsystem::AstroBandModeMonitorPointEnum::SINGLEPOL;
              break;
            case carma::correlator::obsRecord2::DUALPOL:
              mpval = VlbiSubsystem::AstroBandModeMonitorPointEnum::DUALPOL;
              break;
            case carma::correlator::obsRecord2::FULLPOL:
              mpval = VlbiSubsystem::AstroBandModeMonitorPointEnum::FULLPOL;
              break;
            case carma::correlator::obsRecord2::CARMA23:
              mpval = VlbiSubsystem::AstroBandModeMonitorPointEnum::CARMA23;
              break;
          }
          control.astroBandMode().setValue(mpval);
        }
        if(vbd.seqNo_ok) {
          control.vlbiSeqNo().setValue(vbd.seqNo);
        }
      }

      // Set delays (aka interpolator samples)
      if(vbd.delays.valid_frames > 0) {
        vbd.delays.valid_frames--;
        // Loop through delays sequence
        int ndelays = vbd.delays.seq.length();
        for(int i=0; i<ndelays; i++) {
          // Get input index for this sequence element
          long input_idx = vbd.delays.seq[i].inputNumber - 1;
          // Get DelayTriplet for this input index (for the current band).
          carma::vlbi::DelayTriplet & delayTriplet = vbd.delays.seq[i];
          // Get CorMonitorInterpSamps for this input index (for the current
          // band).  This corresponds to a DelayTriplet.
          carma::monitor::CorMonitorInterpSamps & cmis =
            band.interpolatorSamples(input_idx);
          // Set values
          cmis.inputNumber().setValue(delayTriplet.inputNumber);
          cmis.inputDelays(0).delay().setValue(delayTriplet.delay0);
          cmis.inputDelays(0).timestamp().setValue(delayTriplet.timestamp0);
          cmis.inputDelays(1).delay().setValue(delayTriplet.delay1);
          cmis.inputDelays(1).timestamp().setValue(delayTriplet.timestamp1);
          cmis.inputDelays(2).delay().setValue(delayTriplet.delay2);
          cmis.inputDelays(2).timestamp().setValue(delayTriplet.timestamp2);
        }
      }
    }
    
    _mon.online().setValue(true);
  }
}

// vim: set expandtab sw=2 ts=2 :
