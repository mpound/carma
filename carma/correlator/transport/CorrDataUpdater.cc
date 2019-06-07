#include "carma/correlator/transport/CorrDataUpdater.h"

#include "carma/corba/corba.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/correlator/obsRecord2/CorbaCorrProducer.h"
#include "carma/correlator/obsRecord2/DefaultCorrControl.h"
#include "carma/correlator/obsRecord2/SimInfo.h"
#include "carma/monitor/SlDataflowSubsystem.h"
#include "carma/monitor/WbDataflowSubsystem.h"
#include "carma/monitor/C3gDataflowSubsystem.h"
#include "carma/util/corrUtils.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Sleeper.h"
#include "carma/util/StringUtils.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/complexManip.h"

#include <cerrno>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>
#include <utility>

#include <pthread.h>
#include <unistd.h>
#include <signal.h>

// Network
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

// COBRA data interface
#include <cobra/CorrelatorBandClient.h>
#include <cobra/CorrelatorConfigurationIniImporter.h>
#include <cobra/CorrelatorCDR.h>
#include <cobra/CobraVersion.h>
#include <cobra/Types.h>
#include <cobra/debugUtils.h>

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;
using namespace carma::correlator::transport;
using namespace carma::monitor;
using namespace carma::util;


// This maps bandNumber to index in the monitor subsystem vector
typedef std::map<unsigned short,unsigned short> BandNoToMonSysMap;

namespace {

const unsigned kBandErrorCountWarnThresh = 5 * 60 * 2;


const Trace::TraceLevel TRACE_MISC = Trace::TRACE4;
const Trace::TraceLevel TRACE_RW_TIMING = Trace::TRACE1;

// Convert cobra::CorrelatorBand data into CorrelatorData
template <typename CORRDATAFLOW>
void
convertData( const int                     expectedBandNo,
             const cobra::CorrelatorBand & band,
             CorrelatorData &              cdata,
             unsigned &                    consecutiveBandErrorCount,
             const cobra::hardwareType     hwType,
             CORRDATAFLOW & du
            )
{
  const ScopedLogNdc ndc("CorrDataUpdater::convertData");

  const int bandNo = band.parameters().bandNumber();
  // Don't warn here because if full-stokes or carma23 we actually
  // expect bandNo = (expectedBandNo-1)
  /*
  if ( bandNo != expectedBandNo )  {
      ostringstream os;
      os << "Expected band number " << expectedBandNo
         << ". Got band number " << bandNo;
      programLogWarnIfPossible( os.str() );
  }
  */

  const double mjdNow = Time::MJD();

  // Get the number of antennas.
  vector<int32_t> antennaNumbers;
  band.inputs(antennaNumbers);
  const int numAnts = antennaNumbers.size();
  CARMA_CPTRACE( TRACE_MISC, "There are " << numAnts << " antennas" );

  // Fill in header
  CorrelatorHeader head;
  head.setAssembledMJD(mjdNow);

  // The sequence number is the half-second into the current day
  const double mjd_data = band.timestamp().mjdDays();
  const int seq_num =
    static_cast< int >( round((mjd_data - floor(mjd_data))*86400.0*2) );
  head.setSequenceNumber(seq_num);
  head.setMJD(mjd_data);
  cdata.setHeader(head);

  // CARMA_CPTRACE( TRACE_MISC, "staring to fill band: " << idx );
  CorrelatorBand b;

  b.setMJD(mjd_data);
  b.setBandNumber(bandNo);
  b.setSelfTest(false);
  b.setSequenceNumber(seq_num);
  b.setBandwidth(band.parameters().bandwidth() / 1e6);
  b.setNumberOfInputs(numAnts);

  const bool bandValid = ( band.status() == 0 );
  du.bandNo().setValue( bandNo );
  du.seqNo().setValue( seq_num );
  du.bandValidity().setValue(bandValid);
  du.numInputs().setValue(numAnts);

  if ( bandValid ) {
    b.setValid( true );
  } else {
    b.setValid( false, CorrelatorSideband::CORR_DATA_INVALID );
  }

  if ( !bandValid ) {
    if ( consecutiveBandErrorCount++ % kBandErrorCountWarnThresh == 0 ) {
        ostringstream err;
        err << "bad band caught from corr band " << expectedBandNo
            << " correlator band server (astro band " << bandNo << ").";

        if ( consecutiveBandErrorCount > 1 ) {
            err << " There have been " << consecutiveBandErrorCount
                << " consecutive instances of this error.";
        }
        programLogErrorIfPossible( err.str() );
    }

    du.consecutiveErrors().setValue( consecutiveBandErrorCount );
    du.numValidBaselines().setValue( 0 );
    return; // Take status at face value and assume all containing info bad.

  } else if ( consecutiveBandErrorCount > 0 ) {
    consecutiveBandErrorCount = 0;
  }
  du.consecutiveErrors().setValue( consecutiveBandErrorCount );

  // Copy the correlation data between objects
  const vector< cobra::CorrelatorDataPtr > & bandData = *(band.data());
  const int numCorrelations = bandData.size();
  int numberOfLags;

  vector< CorrelatorBaseline > baselines;
  baselines.resize( numCorrelations );

  int numSkipped = 0;  // some correlations may be skipped
  int baCount = 0;     // contiguous index into baselines[]
                       // (keep empty entries at end)

  for ( int i = 0; i < numCorrelations; ++i ) {
    CorrelatorBaseline & ba = baselines.at( baCount );

    cobra::CorrelatorBoardInfo boardInfo = bandData.at(i)->boardInfo();
    cobra::CorrelatorInputInfo inputInfo = bandData.at(i)->inputInfo();
    ba.setBoardId(boardInfo.boardNumber());
    ba.setBoardSN(boardInfo.slotNumber());

    if ( inputInfo.isAuto( ) ) { // Autospectra

      const int inputNo = inputInfo.input(0);

      ba.setInput1Number(inputNo);
      ba.setInput2Number(inputNo);

      const cobra::AutoSpectra & autoSpectra =
        *(static_cast< const cobra::AutoSpectra * >( bandData.at(i).get() ));

      if ( autoSpectra.status() != 0 ) { // spectra invalid
          ++numSkipped;
          continue;
      }

      const vector< float > & autoData = *(autoSpectra.data());

      // Input info
      cobra::CorrelatorParameters parameters = autoSpectra.parameters();
      // CARMA_CPTRACE( TRACE_MISC, "filling auto" );
      CorrelatorSideband sb( CorrelatorSideband::AUTO_FLAVOR );
      const size_t numChans = autoData.size();
      const int numChansMinusOne = static_cast< int >( numChans ) - 1;
      sb.setRxOutFrequency(parameters.channelZeroFrequency() / 1e9);
      sb.setDeltaFrequency(parameters.bandwidth() / (1e6 * numChansMinusOne));

      // 3/17/2008 Temporary code
      //
      // numberOfLags = 120 for 62.5MHz mode
      // CorrelatorParameters needs numberOfLags() as a new field.
      //
      // THERE ARE THREE INSTANCES OF THIS HACK TOTAL (2 below)
      if ( hwType == cobra::HARDWARE_TYPE_COBRA &&
           ::fabs( parameters.bandwidth() - 62.5e6 ) < 1e6 ) {
          numberOfLags = 120;
      } else {
          numberOfLags = 2 * (numChans - 1);
      }
      sb.setNumberOfLags(numberOfLags);

      {
          vector< complex< float > > complexData;
          complexData.reserve( numChans );

          for ( size_t j = 0; j < numChans; ++j )
              complexData.push_back( complex< float >( autoData[j], 0.0f ) );

          sb.swapData( complexData );
      }

      CorrelatorStats stats;

      // TODO: which integration time is this? one phase switch or the total?
      //          stats.setIntegrationTime(
      //  parameters.phaseSwitchIntegrationTime());
      stats.setIntegrationTime(
        parameters.totalIntegrationTime() * 1000.0);
      stats.setNumberOfSamples(
                parameters.numberOfPhaseSwitchesIntegrated());
      sb.setStats(stats);
      // remember to compute stats after setting them but do not include
      // end channels.
      sb.computeStats(false);
      ba.addSideband(sb);

    } else { // Cross-spectra

      // For debugging cobra it is sometimes useful to have
      // cross-correlation data of an antenna with itself.
      // This differs from autocorrelation data in that there
      // are still two sidebands.  If we detect these data,
      // ignore them.  Do not increase baseline counter baCount
      if ( inputInfo.input(0) == inputInfo.input(1) )  {
          ++numSkipped;
          continue;
      }

      const int inputNo1 = inputInfo.input(0);
      const int inputNo2 = inputInfo.input(1);

      ba.setInput1Number( inputNo1 );
      ba.setInput2Number( inputNo2 );

      const cobra::CrossSpectra & crossSpectra =
        *(static_cast< const cobra::CrossSpectra * >( bandData.at(i).get() ));

      if ( crossSpectra.status() != 0 ) { // spectra invalid
          ++numSkipped;
          continue;
      }

      const cobra::CorrelatorParameters parameters = crossSpectra.parameters();
      const vector< vector< complex< float > > > & crossData =
                  *(crossSpectra.data());

      CorrelatorSideband usb( CorrelatorSideband::UPPER_FLAVOR );
      CorrelatorSideband lsb( CorrelatorSideband::LOWER_FLAVOR );

      // LSB data
      {
          const vector< complex< float > > & lsbData = crossData.at(0);
          const int numLsbChans = lsbData.size();
          /** DEBUG MWP
          if ( inputNo1 == 1 && inputNo2 == 2 ) {
              cout << "LSB" << endl << "CHAN    AMP   PHASE" << endl;
              for(int i=0;i<numLsbChans;++i)
              {
                  cout << i+1 << "  "
                       << amp(lsbData[i]) << "  " <<  phase(lsbData[i])
                       << endl;
              }
          } else {
                  cout << inputNo1 <<  "-"
                       << inputNo2 << endl;
          }*/
          lsb.setRxOutFrequency(parameters.channelZeroFrequency() / 1e9);
          lsb.setDeltaFrequency(parameters.bandwidth() /
                                 (1e6 * (numLsbChans - 1)));
          // 3/17/2008 Temporary code
          //
          // numberOfLags = 120 for 62.5MHz mode
          // CorrelatorParameters needs numberOfLags() as a new field.
          //
          // THERE ARE THREE INSTANCES OF THIS HACK TOTAL (1 above & below)
          if ( hwType == cobra::HARDWARE_TYPE_COBRA &&
               ::fabs(parameters.bandwidth()-62.5e6) < 1e6) {
              numberOfLags = 120;
          } else {
              numberOfLags = 2 * (numLsbChans - 1);
          }
          lsb.setNumberOfLags(numberOfLags);
          lsb.setData(lsbData);
      }

      // USB data
      {
          const vector< complex< float > > & usbData = crossData.at(1);
          const int numUsbChans = usbData.size();
          usb.setRxOutFrequency(parameters.channelZeroFrequency() / 1e9);
          usb.setDeltaFrequency(parameters.bandwidth() /
                                 (1e6 * (numUsbChans - 1)));
          // 3/17/2008 Temporary code
          //
          // numberOfLags = 120 for 62.5MHz mode
          // CorrelatorParameters needs numberOfLags() as a new field.
          //
          // THERE ARE THREE INSTANCES OF THIS HACK TOTAL (2 above)
          if ( hwType == cobra::HARDWARE_TYPE_COBRA &&
               ::fabs(parameters.bandwidth()-62.5e6) < 1e6) {
              numberOfLags = 120;
          } else {
              numberOfLags = 2 * (numUsbChans - 1);
          }
          usb.setNumberOfLags(numberOfLags);
          usb.setData(usbData);

      } // USB data

      CorrelatorStats stats;
      // TODO: which integration time is this? one phase switch or the total?
      //        stats.setIntegrationTime(
      //          parameters.phaseSwitchIntegrationTime());
      // Milliseconds
      stats.setIntegrationTime(
        parameters.totalIntegrationTime()*1000.0);
      stats.setNumberOfSamples(
                parameters.numberOfPhaseSwitchesIntegrated());
      usb.setStats(stats);
      // remember to compute stats after setting them but do not include
      // end channels
      usb.computeStats(false);

      // now for lsb
      lsb.setStats(stats);
      // remember to compute stats after setting them but do not include
      // end channels
      lsb.computeStats(false);
      ba.addSideband(usb);
      ba.addSideband(lsb);
    }
    baCount++;

  } // for i < numCorrelations


  // now get rid of the empty and invalid correlations
  const int nBa = numCorrelations - numSkipped;
  vector<CorrelatorBaseline>::iterator baf = baselines.begin()+nBa;
  baselines.erase( baf, baselines.end() );
  du.numValidBaselines().setValue( nBa );
  du.numInvalidBaselines().setValue( numSkipped );

  b.swapBaselines( baselines );

  cdata.addBandViaSwap( b );
  /* debug
  {
      ostringstream os;
      os << "CorrDataUpdator::convertData - "
          << "skipped " << numSkipped << " spectra ";
      programLogNoticeIfPossible( os.str() );
  }
  */
}
template <typename CORRDATAFLOW>
void
runDataServer(
    vector<CORRDATAFLOW  *> & cdfMonSubsys,
    const string &        doName,
    const vector<string> & ecNames,
    const cobra::hardwareType &hwType,
    const std::string &controlHost,
    const unsigned short controlPort,
    const int            portOffset,
    const int            maxCorrDataAgeFrames,
    const BandNoToMonSysMap & bandNoToMonSysMap,
    std::vector<CorbaCorrProducer *> & corbaCorrProducer
    )
{
    const ScopedLogNdc ndc("CorrDataUpdater::runDataServer");
    CARMA_CPTRACE( TRACE_MISC, "invoked with doName = " << doName
            << " ecNames[0] = " << ecNames[0]
            << " hwType = " << hwType
            << " controlHost = " << controlHost <<":"<<controlPort
            << " portOffset = " << portOffset
            );

    //unsigned numbands = ecNames.size();

    // ------------------------------------------------------------
    // Correlator data reception
    // -------------------------
    //
    // The following code connects to the correlator data server
    // for a band, and converts the received data into the
    // CARMA data pipeline format.
    //
    // ------------------------------------------------------------
    //
    CARMA_CPTRACE( TRACE_MISC, "Instantiating CorrelatorBandClient");
    cobra::CorrelatorBandClient<cobra::CorrelatorBand> bandClient(cobra::dataServerPort);

    const int kFailedConnectStallSecs = 10;

    Sleeper failedConnectSleeper;
    unsigned long consecutiveWrongTimes = 0;
    unsigned long consecutiveDroppedFrames = 0;
    const unsigned long wrongTimeWarnThresh = 600;

    //except for CARMA-3G, size() should be 1.
    const unsigned long cdfsize = cdfMonSubsys.size();
    CARMA_CPTRACE( TRACE_MISC, "starting autowriters. Size="<<cdfsize);
    for( unsigned i=0; i < cdfsize ; ++i) {
        // stagger autowriter delays 0.01,0.02,0.03,...,0.16
        double delaySec = 0.01*i;
        if ( cdfMonSubsys[i] != 0 ) {
            cdfMonSubsys.at(i)->startAutoWriter(delaySec);
            CARMA_CPTRACE( TRACE_MISC, "aw#"<<i << " started");
        } else {
            CARMA_CPTRACE( TRACE_MISC, "aw#"<<i << " was null");
        }
    }

    // Connect to the data server and read data.
    // Read error (server dying) will drop back to this entry point
    CARMA_CPTRACE( TRACE_MISC, "connecting to dataserver");
    while ( true ) {
        ThreadQuitTestSelf();

        if ( bandClient.connected() )
            bandClient.close();

        const int connectStatus =
            bandClient.connect( controlPort, controlHost.c_str(), portOffset );

        if ( connectStatus < 0 ) {
            const int savedErrno = errno;

            ostringstream oss;
            oss << "Connection to band server failed - "
                 << strerror(savedErrno)
                 << ". Will retry in " << kFailedConnectStallSecs
                 << " seconds.";
            programLogWarnIfPossible( oss.str() );

            failedConnectSleeper.waitForWholeSecDuration(
                kFailedConnectStallSecs );

            continue;
        }


        // Read until error
        unsigned consecBandErrCount = 0;
        const double maxCorrDataAgeSecs = maxCorrDataAgeFrames * 0.5;
        int integ_cnt = 0;
        while ( true ) {
            try {
                ThreadQuitTestSelf();
            } catch ( ... ) {
                if ( bandClient.connected() )
                    bandClient.close();

                throw;
            }

            integ_cnt++;
            CARMA_CPTRACE( TRACE_MISC,
                           "-------------------------------------------" );
            CARMA_CPTRACE( TRACE_MISC,
                           "Client: iteration " << integ_cnt );

            // Next integration
            cobra::CorrelatorBand band;

            const double readStartMjd = Time::MJD();
            double readEndMjd = 0;

            const int bandReadStatus = bandClient.read( band );
            readEndMjd = Time::MJD();
            const double readMs =
                (readEndMjd - readStartMjd) * Time::MILLISECONDS_PER_DAY;
            CARMA_CPTRACE( TRACE_RW_TIMING, "bandClient.read() "
                << " in " << readMs << " ms." );

            if ( bandReadStatus < 0 ) {
                programLogErrorIfPossible( "band read failed" );
                // Break out of read loop and try to reconnect
                if ( bandClient.connected() )
                    bandClient.close();
                break;
            }

            unsigned index = 0;
            unsigned short astrobandNo = band.parameters().bandNumber();
            BandNoToMonSysMap::const_iterator ip = bandNoToMonSysMap.find( astrobandNo );
            if ( ip != bandNoToMonSysMap.end() ) {
                index = ip->second;
            }

            // the online monitor point is always true

            cdfMonSubsys.at(index)->receivedTime().setValue( readEndMjd );
            cdfMonSubsys.at(index)->online().setValue(true);
            cdfMonSubsys.at(index)->online().setValidity(MonitorPoint::VALID);

            cdfMonSubsys.at(index)->controlPort().setValue( controlPort );
            cdfMonSubsys.at(index)->controlHost().setValue( controlHost );
            // DO name is e.g. carma.correlator.slcBand1.  This is too long
            // to fit in a reasonably-sized RTD table cell,
            // so remove "carma.correlator."
            string shortenedDoName = doName.substr(doName.rfind(".")+1);
            cdfMonSubsys.at(index)->controlObject().setValue( shortenedDoName );
            // name is e.g. carma.correlator.slcData1.  Also too long.
            string shortenedEcName = ecNames[index].substr(ecNames[index].rfind(".")+1);
            cdfMonSubsys.at(index)->eventChannel().setValue( shortenedEcName );

            try {
                ThreadQuitTestSelf();
            } catch ( ... ) {
                if ( bandClient.connected() )
                    bandClient.close();

                throw;
            }

            {
                const double nowMjdSecs = Time::MJD() * 86400.0;
                const double tsMjdSecs = band.timestamp().mjdSeconds();
                // half second subtracted here because data timestamp
                // is expected to be the beginning of the frame
                const double diffMjdSecs = (nowMjdSecs - 0.5 - tsMjdSecs);
                const double errMjdSecs = fabs( diffMjdSecs );

                if ( errMjdSecs > 0.5 ) {

                    // Print an error message on first instance and every
                    // wrongTimeWarnThresh consecutive instances following.
                    if ( consecutiveWrongTimes++ % wrongTimeWarnThresh == 0 ) {
                        ostringstream oss;

                        oss << "timestamp appears to be wrong:"
                            << fixed << setprecision(6)
                            << " current time = " << nowMjdSecs
                            << ", data timestamp = " << tsMjdSecs
                            << ", diff (current minus data minus 0.5) = "
                            << fixed << setprecision(2) << diffMjdSecs
                            << " seconds";

                        if ( consecutiveWrongTimes > 1 ) {
                            oss << "There have been " << consecutiveWrongTimes
                                << " consecutive instances of this error.";
                        }
                        programLogWarnIfPossible( oss.str() );
                    }
                } else if ( errMjdSecs > 0.3 ) {
                    consecutiveWrongTimes = 0;
                    CARMA_CPTRACE( TRACE_RW_TIMING,
                                   "WARNING: " <<
                                   "timestamp appears to be wrong:" <<
                                   fixed << setprecision(6) <<
                                   " current time = " << nowMjdSecs <<
                                   ", data timestamp = " << tsMjdSecs <<
                                   ", diff = " << diffMjdSecs );
                } else {
                    consecutiveWrongTimes = 0;
                    CARMA_CPTRACE( TRACE_RW_TIMING,
                                   fixed << setprecision(6) <<
                                   "current time = " << nowMjdSecs <<
                                   ", data timestamp = " << tsMjdSecs <<
                                   ", diff = " << diffMjdSecs );
                }

                if ( errMjdSecs > maxCorrDataAgeSecs ) {
                    ++consecutiveDroppedFrames;
                    if ( consecutiveDroppedFrames  == 1 ) {
                        ostringstream err;
                        err << "CorrDataUpdater - dropped 1 frame which was "
                            << fixed << setprecision(2) << errMjdSecs
                            << " seconds late.";
                        programLogErrorIfPossible( err.str() );
                    }
                    else if ( consecutiveDroppedFrames % 120 == 0 ) {
                        ostringstream err;
                        err << "CorrDataUpdater - dropped "
                            << consecutiveDroppedFrames << " consecutive frames"
                            << " that were later than "
                            << fixed << setprecision(1) << maxCorrDataAgeSecs
                            << " seconds";
                        programLogErrorIfPossible( err.str() );
                    }
                    continue;
                } else {
                    consecutiveDroppedFrames = 0;
                }

            }
            cdfMonSubsys.at(index)->consecutiveDroppedFrames().setValue( consecutiveDroppedFrames );

            // Translate CorrelatorBand into CorrelatorData
            CorrelatorData cdata;
            /* debug
            {
                ostringstream os ;
                os << "converting data for band " << bandNo;
                CARMA_CPTRACE( TRACE_MISC, os.str() );
                programLogNoticeIfPossible( os.str() );
            }*/
            convertData( astrobandNo, band, cdata, consecBandErrCount, hwType,
                   *(cdfMonSubsys.at(index)) );

            // transport
            CARMA_CPTRACE( TRACE_MISC,
                           "Sending data at " << Time::getTimeString(3) );

            try {
                ThreadQuitTestSelf();
            } catch ( ... ) {
                if ( bandClient.connected() )
                    bandClient.close();

                throw;
            }

            const double writeStartMjd = Time::MJD();
            cdata.getHeader().setTransmissionMJD( writeStartMjd );

            //unsigned index = (numbands == 1 ? 0 : bandNo - 1);
            corbaCorrProducer.at( index )->sendCorData( cdata, 0, 0 );

            const double writeEndMjd = Time::MJD();
            const double writeMs =
                (writeEndMjd - writeStartMjd) * Time::MILLISECONDS_PER_DAY;
            CARMA_CPTRACE( TRACE_RW_TIMING, "sendCorData() in "
                           << writeMs << " ms." );

            const double totalMs = ( writeEndMjd - readEndMjd ) * Time::MILLISECONDS_PER_DAY;
            CARMA_CPTRACE( TRACE_RW_TIMING, "total proc time from read end to "
                           << "write end is " << totalMs << " ms." );

            cdfMonSubsys.at(index)->publishedTime().setValue( writeEndMjd );

        } // Read integration
    } // Retry connection
}

}  // namespace < anonymous >


void
CorrDataUpdater::runUpdateLoop(
    const string &        doName,
    const string &        ecNameBase,
    const unsigned short  controlPort,
    const string &        controlHost,
    const int             portOffset,
    const int             bandNo,
    void                (*shutdownCallback)( void * ),
    void * const          shutdownCallbackArg,
    const string &        hwStr ,
    const bool            spectralLineMode,
    const int             maxCorrDataAgeFrames )
{
    const ScopedLogNdc ndc("CorrDataUpdater::runUpdateLoop");
    CARMA_CPTRACE( TRACE_MISC,
                   "CorrelatorServer starting:" <<
                   " data channel =" << ecNameBase << ", control object =" << doName );

    cobra::hardwareType hwType = cobra::findHardwareType(hwStr.c_str());

    unsigned numbands = 1;
    unsigned firstCarma3gBandNo = 0;

    if (   StringUtils::equalsIgnoreCase(hwStr,"c3gmax8") ) {
        firstCarma3gBandNo = 33;
        numbands = 8; // used in vectorization of corbaCorrProducer
        // The following line can be removed when cobra 2.99 is installed.
        hwType = cobra::HARDWARE_TYPE_CARMA3G; 
    }

    if (   StringUtils::equalsIgnoreCase(hwStr,"c3gmax23") )  {
        firstCarma3gBandNo = 25;
        numbands = 8; // used in vectorization of corbaCorrProducer
        // The following line can be removed when cobra 2.99 is installed.
        hwType = cobra::HARDWARE_TYPE_CARMA3G; 
    }
    CARMA_CPTRACE(TRACE_MISC," DATAUPDATER hardware type " << cobra::getNameForHardwareTypeEnum(hwType));

    int producerControlPort = controlPort;
    CARMA_CPTRACE( TRACE_MISC,
                       "Set the control port number to " <<
                       producerControlPort );

    std::vector<CorbaCorrProducer *> corbaCorrProducer;

    std::vector<string> ecNames;
    for (unsigned i=0; i < numbands; ++i ) {
        ostringstream os;
        // bandNo is ZERO for carma3g.

        switch ( hwType ) {
           // for COBRA hardware the channel name uses bandNo-8. ugh.
            case cobra::HARDWARE_TYPE_COBRA:
                os << ecNameBase << (bandNo+i-8);
                break;
            case cobra::HARDWARE_TYPE_CARMA:
                os << ecNameBase << (bandNo+i);
                break;
            case cobra::HARDWARE_TYPE_CARMA3G:
                os << ecNameBase << (bandNo+i+1);
                break;
            default:
            case cobra::HARDWARE_TYPE_UNKNOWN:
                os << "Unrecognized hardware type : " << hwStr
                   << ".  Valid values are COBRA, CARMA, C3GMAX23, or C3GMAX8";
                throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, os.str());
                break;
        }
        const string ec = os.str();
        ecNames.push_back(ec);
        CARMA_CPTRACE( TRACE_MISC, "Create a CorbaCorrProducer on channel " << ec);
        CorbaCorrProducer * producer = new CorbaCorrProducer( ec );
        CARMA_CPTRACE( TRACE_MISC, "Created a CorbaCorrProducer on channel " << ec);
        corbaCorrProducer.push_back( producer );
        CARMA_CPTRACE( TRACE_MISC, "Pushed back CorbaCorrProducer on channel " << ec);
    }


    CARMA_CPTRACE( TRACE_MISC, "DefaultCorrControl init");
    DefaultCorrControl corrControl( producerControlPort );

    CARMA_CPTRACE( TRACE_MISC, "DefaultCorrControl start server ");
    corrControl.startControlServer( Program::getProgram().getCorbaServer(),
                                    doName,
                                    shutdownCallback,
                                    shutdownCallbackArg );

    CARMA_CPTRACE( TRACE_MISC, "Starting read-back loop ... " );

    CARMA_CPTRACE( TRACE_MISC, "Remote name " << controlHost );

    BandNoToMonSysMap bmm;
    switch ( hwType ) {
        default:
        case cobra::HARDWARE_TYPE_UNKNOWN:
        {
            std::ostringstream oss;
            oss << "Unrecognized hardware type : " << hwStr
                << ".  Valid values are COBRA, CARMA, C3GMAX23, or C3GMAX8";
            throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, oss.str());
        }
        break;
        case cobra::HARDWARE_TYPE_CARMA:
        {
            // spectral line boards
            vector<SlDataflowSubsystem *> mon;
            for (unsigned short i=0; i < numbands; ++i ) {
                unsigned short astrobandNo = bandNo+i;
                SlDataflowSubsystem * slDataflow = new SlDataflowSubsystem( astrobandNo );

                // Set the initial online value to true, indicating
                // the monitor system publisher is online
                slDataflow->online().setValue(true);
                slDataflow->online().setValidity(MonitorPoint::VALID);

                mon.push_back(slDataflow);
                bmm.insert(make_pair<unsigned short, unsigned short>(astrobandNo, i));
            }
           runDataServer( 
                    mon, doName, ecNames, hwType, 
                    controlHost, controlPort,portOffset, 
                    maxCorrDataAgeFrames, bmm,
                    corbaCorrProducer
                    );

           // If runDataServer returns, then the monitor system
           // writer is no longer online
           for (unsigned i=0; i < numbands; ++i ) {
               mon.at(i)->online().setValue(false);
               mon.at(i)->online().setValidity(MonitorPoint::VALID);
           }
        }
        break;
        case cobra::HARDWARE_TYPE_CARMA3G:
        {
            // spectral line boards
            vector<C3gDataflowSubsystem *> mon;
            for (unsigned short i=0; i < numbands; ++i ) {
                unsigned short astrobandNo = i+firstCarma3gBandNo;
                // Constructor to C3GDataflowSubsystem takes an integer
                // 1 through 16.
                unsigned short c3gIndex = i+firstCarma3gBandNo-24;
    CARMA_CPTRACE( TRACE_MISC, "Instantiating DataFlow monsys for band " << astrobandNo << " index " << c3gIndex);
                C3gDataflowSubsystem * c3gDataflow = new C3gDataflowSubsystem( c3gIndex );

                // Set the initial online value to true, indicating
                // the monitor system publisher is online
                c3gDataflow->online().setValue(true);
                c3gDataflow->online().setValidity(MonitorPoint::VALID);

    CARMA_CPTRACE( TRACE_MISC, "pushing back C3gDataflow("<< c3gIndex<<")");
                mon.push_back(c3gDataflow);
                bmm.insert(make_pair<unsigned short, unsigned short>(astrobandNo, i));
           }
    CARMA_CPTRACE( TRACE_MISC, "Running dataserver");
           runDataServer(
                    mon, doName, ecNames, hwType,
                    controlHost, controlPort, portOffset,
                    maxCorrDataAgeFrames, bmm,
                    corbaCorrProducer
                    );

           // If runDataServer returns, then the monitor system
           // writer is no longer online
           for (unsigned i=0; i < numbands; ++i ) {
               mon.at(i)->online().setValue(false);
               mon.at(i)->online().setValidity(MonitorPoint::VALID);
           }
        }
        break;
        case cobra::HARDWARE_TYPE_COBRA:
        {
            // cobra boards
            vector<WbDataflowSubsystem*> mon(1);
            WbDataflowSubsystem wbDataflow( bandNo - 8 );
            mon.at(0) = &wbDataflow;

            // Set the initial online value to true, indicating
            // the monitor system publisher is online
            mon[0]->online().setValue(true);
            mon[0]->online().setValidity(MonitorPoint::VALID);
            bmm.insert(make_pair<unsigned short, unsigned short>(bandNo, 0));
            runDataServer(
                    mon, doName, ecNames, hwType,
                    controlHost, controlPort, portOffset,
                    maxCorrDataAgeFrames, bmm,
                    corbaCorrProducer
                    );

            // If runDataServer returns, then the monitor system
            // writer is no longer online
            mon.at(0)->online().setValue(false);
            mon.at(0)->online().setValidity(MonitorPoint::VALID);

            break;
        }
    };

}

