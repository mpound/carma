// $Id: CorrelatorDataTestWb.cc,v 1.8 2011/05/04 18:15:34 abeard Exp $
#include <cmath>

#include "carma/correlator/lib/CorrelatorDataTestWb.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

const std::string CorrelatorDataTestWb::className_("CorrelatorDataTestWb");

/**.......................................................................
 * Constructor.
 */
CorrelatorDataTestWb::CorrelatorDataTestWb() {
  ccc_ = CorrelatorConfigChecker::getInstance();
  // Bands are numbered 1 to N.
  bandNumber_ = 16;
  createTestData();
}

CorrelatorDataTestWb::CorrelatorDataTestWb(int bandNumber) {
  ccc_ = CorrelatorConfigChecker::getInstance();
  bandNumber_ = bandNumber;
  createTestData();
}

void CorrelatorDataTestWb::setBandNumber(int bandNumber) {
  bandNumber_ = bandNumber;
}

/**.......................................................................
 * Destructor.
 */
CorrelatorDataTestWb::~CorrelatorDataTestWb() {}

void CorrelatorDataTestWb::createTestData() {
  // WB
  int numChans = 17;
  int numAnts = 8;
  int numBands = 16;


  // SL
  /*
  int numChans = 65;
  int numAnts = 15;
  int numBands = 1;
  //int numBands = 8;
  */

  CorrelatorHeader head;
  head.setMJD(123.456);
  head.setAssembledMJD(1.2);
  head.setTransmissionMJD(3.4);
  head.setReceivedMJD(5.6);
  head.setSequenceNumber(123456789);
  setHeader(head);
  DPRINT(ccc_, className_,
         " mjd= " << head.getMJD() <<
         " asmmjd= " << head.getAssembledMJD() <<
         " txmjd= " << head.getTransmissionMJD() <<
         " rxmjd= " << head.getReceivedMJD() <<
         " seq= " << head.getSequenceNumber());

  for (int idx = 0; idx < numBands; ++idx) {
    int bandNumber = bandNumber_ + idx;
    DPRINT(ccc_, className_, "staring to fill band: " << bandNumber);
    CorrelatorBand b;
    b.setMJD(123.456);
    b.setBandNumber(bandNumber);
    b.setSelfTest(false);
    b.setSimulation(true);
    b.setSequenceNumber(bandNumber + 100);
    b.setBandwidth(500.0);

    DPRINT(ccc_, className_, "bandwidth= " << b.getBandwidth());

    b.setNumberOfInputs(numAnts);
    b.setValid(true);
    for (int a1Idx = 0; a1Idx < numAnts; ++a1Idx) {
      for (int a2Idx = a1Idx; a2Idx < numAnts; ++a2Idx) {
        DPRINT(ccc_, className_,
               "filling ant pair: " << a1Idx << " : " << a2Idx);
        CorrelatorBaseline ba;
        ba.setInput1Number(a1Idx + 1);
        ba.setInput2Number(a2Idx + 1);
        ba.setBoardId(a1Idx + a2Idx * (a2Idx + 1) / 2);
        ba.setBoardSN(a1Idx + a2Idx * (a2Idx + 1) / 2 + 100);
        if (a1Idx == a2Idx) {
          DPRINT(ccc_, className_, "filling autoSideband");
          //ba.setNumberOfSidebands(1);
          CorrelatorSideband sb( CorrelatorSideband::AUTO_FLAVOR );
          sb.setNumberOfLags(2 * (numChans - 1));
          sb.setRxOutFrequency(100.0 + a1Idx + a2Idx * (a2Idx + 1) / 2);
          sb.setDeltaFrequency(32.0);
          DPRINT(ccc_, className_, "created auto CorrelatorSideband");

          // create some fake data
          vector<complex<float> > data;

          // New test data
          simData(bandNumber, a1Idx+1, a2Idx+1, numChans, 0, data);
          sb.setData(data);
          sb.setValidAll(true);
          CorrelatorStats stats;
          stats.setIntegrationTime(300.0);
          stats.setNumberOfSamples(3000);
          sb.setStats(stats);
          sb.computeStats();
          //stats->setAvg(complex<float>(getId(a1Idx, a2Idx),
          //			   getId(a1Idx, a2Idx) + 1));
          ba.addSideband(sb);
        } else {
          DPRINT(ccc_, className_, "filling CrossSideband");
          //ba.setNumberOfSidebands(2);
          CorrelatorSideband usb( CorrelatorSideband::UPPER_FLAVOR );
          CorrelatorSideband lsb( CorrelatorSideband::LOWER_FLAVOR );
          usb.setNumberOfLags(2 * (numChans - 1));
          usb.setRxOutFrequency(250.0 + a1Idx + a2Idx * (a2Idx + 1) / 2);
          usb.setDeltaFrequency(32.0);
          lsb.setNumberOfLags(2 * (numChans - 1));
          lsb.setRxOutFrequency(200.0 + a1Idx + a2Idx * (a2Idx + 1) / 2);
          usb.setDeltaFrequency(-32.0);

          // create some fake data
          vector<complex<float> > data;        

          // New test data
          simData(bandNumber, a1Idx+1, a2Idx+1, numChans, 0, data);
          usb.setData(data);
          usb.setValidAll(true);


          // New test data
          simData(bandNumber, a1Idx+1, a2Idx+1, numChans, 1, data);
		  
          lsb.setData(data);
          lsb.setValidAll(true);

          CorrelatorStats stats;
          //	  stats->setAvg(complex<float>(getId(a1Idx, a2Idx) + 1000,
          //			   getId(a1Idx, a2Idx) + 1001));
          stats.setIntegrationTime(300.0);
          stats.setNumberOfSamples(3000);
          usb.setStats(stats);
          usb.computeStats();
          // stats->setAvg(complex<float>(getId(a1Idx, a2Idx) - 1000,
          //			   getId(a1Idx, a2Idx) - 1001));
          stats.setIntegrationTime(-300.0);
          stats.setNumberOfSamples(-3000);
          lsb.setStats(stats);
          lsb.computeStats();
          ba.addSideband(usb);
          ba.addSideband(lsb);
        }
        b.addBaseline(ba);
      }
    }
    addBand(b);
  }
}

int CorrelatorDataTestWb::getId(int a1, int a2) {
  return bandNumber_ + a1 + a2 * (a2 + 1) / 2;
}

// Create a uniform magnitude spectrum, with a notch
// at an antenna dependent location. Scale the
// spectrum by the band number.
//
// Assumptions:
// band     = 1, 2, 3, etc
// an1, an2 = 0, 1, 2, 3, 4, 5, 6, 7
// 
void CorrelatorDataTestWb::simData(
                                    int band, int an1, int an2, int numChans, int sideband,
                                    vector<complex<float> > &data)
{
  float mag, pha;
  float pi = M_PIl; /* in math.h included by cmath */
  data.resize(numChans);

  // Check for bad index
  if (band == 0) {
    band = 1;
  }
	
  for (int dIdx = 0; dIdx < numChans; ++dIdx) {
    // Nominal magnitude and phase
    mag = 1.0*band;
    pha = pi/(numChans-1)*dIdx - pi/2;
		
    // Antenna notches
    if (dIdx == 2*an1) {
      mag = 0.9*band;
    }
    if (dIdx == 2*an2) {
      mag = 0.9*band;
    }

    // Last channel amplitude
    if (dIdx == numChans-1) {
      mag = 0.1*band;
    }
		
    // Spectra
    if (an1 == an2) {
      // No phase for autos
      data[dIdx] = complex<float>(mag, 0.0);
    } else {
      if (sideband == 0) {
        data[dIdx] = complex<float>(mag*cos(pha), mag*sin(pha));
      } else {
        // Invert the phase to make them different
        data[dIdx] = complex<float>(mag*cos(-pha), mag*sin(-pha));
      }
    }
  }
}

