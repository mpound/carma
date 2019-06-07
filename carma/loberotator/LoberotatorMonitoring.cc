/**
 * @file
 * Monitoring method and simulation method definitions for Loberotator class 
 *
 * @author Steve Scott
 * $Revision: 1.19 $
 * $Date: 2014/03/18 20:21:21 $
 * $Id: LoberotatorMonitoring.cc,v 1.19 2014/03/18 20:21:21 scott Exp $
 *
 * $CarmaCopyright$
 */

// System includes
#include <iomanip>
#include <math.h>
#include <stdlib.h>

// CARMA includes
#include "carma/loberotator/Loberotator.h"
#include "carma/loberotator/LoberotatorMaster.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/LoberotatorSubsystem.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"
#include "carma/util/SystemException.h"

using namespace log4cpp;
using namespace std;
using namespace carma::canbus;
using namespace carma::util;
using namespace carma::monitor;
using namespace carma::loberotator;


// **************************************************************************
map<carma::canbus::msgType, string> Loberotator::getHalfSecMonitors() const 
{    
  static map<carma::canbus::msgType, string> halfsecs;
  static bool hereAlready = false;
  
  if ( hereAlready == false ) {
      //CARMA_CPTRACE(Trace::TRACE6, "getHalfSecMonitors called." );

      halfsecs[int(BLANK_FRAME_PACKET_1)]  = "BLANK_FRAME_PACKET_1";
      halfsecs[int(BLANK_FRAME_PACKET_2)]  = "BLANK_FRAME_PACKET_2"; 
      halfsecs[int(BLANK_FRAME_PACKET_3)]  = "BLANK_FRAME_PACKET_3";
      halfsecs[int(BLANK_FRAME_PACKET_4)]  = "BLANK_FRAME_PACKET_4";
      halfsecs[int(BLANK_FRAME_PACKET_5)]  = "BLANK_FRAME_PACKET_5";
      halfsecs[int(BLANK_FRAME_PACKET_6)]  = "BLANK_FRAME_PACKET_6";
      halfsecs[int(BLANK_FRAME_PACKET_7)]  = "BLANK_FRAME_PACKET_7";
      halfsecs[int(BLANK_FRAME_PACKET_8)]  = "BLANK_FRAME_PACKET_8";
      halfsecs[int(BLANK_FRAME_PACKET_9)]  = "BLANK_FRAME_PACKET_9";
      halfsecs[int(BLANK_FRAME_PACKET_10)] = "BLANK_FRAME_PACKET_10";
      halfsecs[int(BLANK_FRAME_PACKET_11)] = "BLANK_FRAME_PACKET_11";

      hereAlready = true;
    }
  
    return halfsecs;
}

// **************************************************************************
map<carma::canbus::msgType, string> Loberotator::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}


// **************************************************************************
void Loberotator::processMsg(carma::canbus::msgType mid, 
        vector<byteType> &data, bool sim) 
{
    Xac& xac = mon().board(getBoardIndex()).xac();

    switch (mid) {

    case BLANK_FRAME_PACKET_1:
         processPhasePacket(data, 1);
         break;
    case BLANK_FRAME_PACKET_2:
         processPhasePacket(data, 2);
         break;
    case BLANK_FRAME_PACKET_3:
         processPhasePacket(data, 3);
         break;
    case BLANK_FRAME_PACKET_4:
         processPhasePacket(data, 4);
         break;
    case BLANK_FRAME_PACKET_5:
         processPacket5(data);
         break;
    case BLANK_FRAME_PACKET_6:
         processPacket6(data);
         break;
    case BLANK_FRAME_PACKET_7:
         processPacket7(data);
         break;
    case BLANK_FRAME_PACKET_8:
         processPacket8(data);
         break;
    case BLANK_FRAME_PACKET_9:
         processPacket9(data);
         break;
    case BLANK_FRAME_PACKET_10:
         processPacket10(data);
         break;

    case BLANK_FRAME_PACKET_11:
         processPacket11(data);
         break;

    case FAST_CHAN_1:
         processFastMonitorChan1(data);
         break;
    case FAST_CHAN_2:
         processFastMonitorChan2(data);
         break;
    case FAST_CHAN_1_DATA:
         processFastMonitorChan1Data(data);
         break;
    case FAST_CHAN_2_DATA:
         processFastMonitorChan2Data(data);
         break;

    case XacDevice::SYSTEM_MONITOR_PACKET_1:
         XacDevice::processSystemMonitorPacket1(data, xac);
         break;
    case XacDevice::SYSTEM_MONITOR_PACKET_2:
         XacDevice::processSystemMonitorPacket2(data, xac);
         break;
    case XacDevice::SYSTEM_MONITOR_PACKET_3:
         XacDevice::processSystemMonitorPacket3(data, xac);
         break;
    case XacDevice::SYSTEM_MONITOR_PACKET_4:
         XacDevice::processSystemMonitorPacket4(data, xac);
         break;
    case XacDevice::SYSTEM_MONITOR_PACKET_5:
         XacDevice::processSystemMonitorPacket5(data, xac);
         break;

    default:
         {
            ostringstream os;
            os << "processMsg(): Unknown message ID - " 
               << hex << mid << "." << dec << endl;
            programLogInfoIfPossible(os.str());
         }
      break;
    }
}


// **************************************************************************
carma::canbus::Message Loberotator::simulateMsg(carma::canbus::msgType mid) 
{
    carma::canbus::Message msg;

    switch (mid) {

    case BLANK_FRAME_PACKET_1:
        msg = simPhasePacket(1);
        break;
    case BLANK_FRAME_PACKET_2:
        msg = simPhasePacket(2);
        break;
    case BLANK_FRAME_PACKET_3:
        msg = simPhasePacket(3);
        break;
    case BLANK_FRAME_PACKET_4:
        msg = simPhasePacket(4);
        break;
    case BLANK_FRAME_PACKET_5:
        msg = simPacket5();
        break;
    case BLANK_FRAME_PACKET_6:
        msg = simPacket6();
        break;
    case BLANK_FRAME_PACKET_7:
        msg = simPacket7();      
        break;    
    case XacDevice::SYSTEM_MONITOR_PACKET_1:
        msg = XacDevice::simSystemMonitorPacket1();
        break;
    case XacDevice::SYSTEM_MONITOR_PACKET_2:
        msg = XacDevice::simSystemMonitorPacket2();
        break;
    case XacDevice::SYSTEM_MONITOR_PACKET_3:
        msg = XacDevice::simSystemMonitorPacket3();
        break;
    case XacDevice::SYSTEM_MONITOR_PACKET_4:
        msg = XacDevice::simSystemMonitorPacket4();
        break;
    default:
        {
            ostringstream os;
            os << "Loberotator::simulateMsg() - "
               << "Called with unrecognized message type "
               << hex << mid << endl;
            // Note -- this logs at a high rate with emulate=true
            CARMA_CPTRACE(Trace::TRACE4, os.str().c_str() );
            //programLogInfoIfPossible(os.str());
        }
        break;
    }
  
    // Set the rx time on the message to the last time
    // we received a real message for this device.
    // This should be enforced in Master.
    msg.setRxMjd( getLastRxTime() );
  
    return msg;
}

// Gets phase rate and phase for a channel
// **************************************************************************
void Loberotator::processPhasePacket(vector<carma::canbus::byteType>&data, 
        int chan)
{
    int chanIndex = 4*getBoardIndex() + (chan - 1);
    if ((chan < 1) || (chan > 4)) {
        ostringstream o;
        o << "Channel number (" << chan << ") must be in range [1-4]";
        throw CARMA_ERROR(o); 
    }
    if ((chanIndex < 0) || (chanIndex > 23)) {
        ostringstream o;
        o << "Channel index (" << chanIndex << ") must be in range [0-23]";
        throw CARMA_ERROR(o); 
    }
    
    long          rate  = dataToLong(data);
    unsigned long phase = dataToUlong(data);    
    mon().channel(chanIndex).phase().setValue(0.001*phase);
    mon().channel(chanIndex).phaseRate().setValue(0.001*rate);
}

// **************************************************************************
void Loberotator::processPacket5( vector<byteType> &data ) 
{
    // Canbus node numbers for the boards are 1, 5, 9, 13. 17, and 21
    int node          = static_cast<nodeType>(getNode()); 
    int chanBaseIndex = node - 1;

    typedef LoberotatorSubsystem::PsColumnStateMonitorPointEnum PSC_STATE;
    for (int c=0; c < 4; c++) {
        int i = chanBaseIndex + c; // channel index
        PSC_STATE& mp = mon().channel(i).psColumnState();
        // Phaseswithing table state
        int state = dataToUbyte(data);
        switch(state) {
        case 0:  mp.setValue(PSC_STATE::OK);
                 break;
        case 1:  mp.setValue(PSC_STATE::BUSY);
                 break;
        case 2:  mp.setValue(PSC_STATE::BADCRC);
                 break;
        case 3:  mp.setValue(PSC_STATE::TMO);
                 break;
        case 4:  mp.setValue(PSC_STATE::DEFAULT);
                 break;
        default: mp.setValue(PSC_STATE::INVALID);
                 break;
        }
    }
    
    typedef LoberotatorSubsystem::DdsFringeTrackingMonitorPointEnum FRINGE;
    for (int c=0; c < 4; c++) {
        int i = chanBaseIndex + c; // channel index
        FRINGE& fringe = mon().channel(i).ddsFringeTracking();
        // Operational state
        int state = dataToUbyte(data);
        switch(state) {
        case 0:  fringe.setValue(FRINGE::ON);
                 break;
        case 1:  fringe.setValue(FRINGE::OFF);
                 break;
        default: fringe.setValue(FRINGE::INVALID);
                 break;
        }
    }
}

// **************************************************************************
void Loberotator::processPacket6( vector<byteType> &data ) 
{
    int           node       = static_cast<nodeType>(getNode());
    int           boardIndex = node/4 ;
    float         modtemp;
    unsigned char ppsStat;       // 1pps
    unsigned char heartbeatStat; // 1kpps
    unsigned char dataValid;
    short         timeDiff;
    

    modtemp       = 0.01*dataToShort(data);
    ppsStat       = dataToUbyte(data);
    heartbeatStat = dataToUbyte(data);
    dataValid     = dataToUbyte(data);
    timeDiff      = dataToShort(data);

    mon().board(boardIndex).temperature().setValue(modtemp);
    typedef LoberotatorSubsystem::PpsMonitorPointEnum       PPS;
    typedef LoberotatorSubsystem::HeartbeatMonitorPointEnum HB;
    typedef LoberotatorSubsystem::DataValidMonitorPointEnum DV;
    if (ppsStat) {
        mon().board(boardIndex).pps().setValue(PPS::OK);
    }
    else {
        mon().board(boardIndex).pps().setValue(PPS::MISSING);
    }
    
    if (heartbeatStat) {
        mon().board(boardIndex).heartbeat().setValue(HB::OK);
    }
    else {
        mon().board(boardIndex).heartbeat().setValue(HB::MISSING);
    }
    
    if (dataValid) {
        mon().board(boardIndex).dataValid().setValue(DV::VALID);
    }
    else {
        mon().board(boardIndex).dataValid().setValue(DV::BAD);
    }
    mon().board(boardIndex).timeDiff().setValue(timeDiff);
}

// **************************************************************************
void Loberotator::processPacket7( vector<byteType> &data ) 
{
    int boardIndex =  getBoardIndex();
    float ps[4];

    for (int i=0; i<4; i++) ps[i] = 0.001*dataToShort(data);

    mon().board(boardIndex).analog5v().setValue(ps[0]);
    mon().board(boardIndex).digital5v().setValue(ps[1]);
    mon().board(boardIndex).digitalNeg5v().setValue(ps[2]);
    mon().board(boardIndex).ps24v().setValue(ps[3]);
}

// **************************************************************************
void Loberotator::processPacket8( vector<byteType> &data ) 
{
    int boardIndex =  getBoardIndex();

    mon().board(boardIndex).lostCommandCount().setValue(dataToShort(data));
    mon().board(boardIndex).ppsResetCount().setValue(dataToShort(data));
    mon().board(boardIndex).missedPPScount().setValue(dataToShort(data));
}

// **************************************************************************
void Loberotator::processPacket9( vector<byteType> &data ) 
{
    // Canbus node numbers for the boards are 1, 5, 9, 13. 17, and 21
    int node          = static_cast<nodeType>(getNode()); 
    int chanBaseIndex = node - 1;

    for (int c=0; c < 4; c++) {
        int i = chanBaseIndex + c; // channel index
        float ct = 0.001 * dataToShort(data);
        mon().channel(i).commandTime().setValue(ct);
    }
}

// **************************************************************************
void Loberotator::processPacket10( vector<byteType> &data ) 
{
    // Canbus node numbers for the boards are 1, 5, 9, 13. 17, and 21
    int node          = static_cast<nodeType>(getNode()); 
    int chanBaseIndex = node - 1;

    for (int c=0; c < 4; c++) {
        int i = chanBaseIndex + c; // channel index
        int walshCol = dataToUbyte(data);
        mon().channel(i).walshColumn().setValue(walshCol);
    }
}

// **************************************************************************
void Loberotator::processPacket11( vector<byteType> &data ) 
{
    // Canbus node numbers for the boards are 1, 5, 9, 13. 17, and 21
    int node          = static_cast<nodeType>(getNode()); 
    int chanBaseIndex = node - 1;

    typedef LoberotatorSubsystem::PhaseSwitch90MonitorPointEnum PS90;
    typedef LoberotatorSubsystem::PhaseSwitch180MonitorPointEnum PS180;
    for (int c=0; c < 4; c++) {
        int i = chanBaseIndex + c; // channel index
        PS90& ps = mon().channel(i).phaseSwitch90();
        int state = dataToUbyte(data);
        switch(state) {
            case 0:  ps.setValue(PS90::ON);
                     break;
            case 1:  ps.setValue(PS90::OFF);
                     break;
            default: ps.setValue(PS90::INVALID);
                     break;
        }
    }
    for (int c=0; c < 4; c++) {
        int i = chanBaseIndex + c; // channel index
        PS180& ps = mon().channel(i).phaseSwitch180();
        int state = dataToUbyte(data);
        switch(state) {
            case 0:  ps.setValue(PS180::ON);
                     break;
            case 1:  ps.setValue(PS180::OFF);
                     break;
            default: ps.setValue(PS180::INVALID);
                     break;
        }
    }
}

// **************************************************************************
void Loberotator::processFastMonitorChan1 (vector<byteType> &data) 
{
  unsigned char dataType, nfast, totalnfast;

  dataType   = dataToUbyte(data);
  nfast      = dataToUbyte(data);
  totalnfast = dataToUbyte(data);
}

// **************************************************************************
void Loberotator::processFastMonitorChan2 (vector<byteType> &data) 
{
  unsigned char dataType, nfast, totalnfast;

  dataType   = dataToUbyte(data);
  nfast      = dataToUbyte(data);
  totalnfast = dataToUbyte(data);        
}

// **************************************************************************
void Loberotator::processFastMonitorChan1Data (vector<byteType> &data) 
{
  // TBD
}

// **************************************************************************
void Loberotator::processFastMonitorChan2Data (vector<byteType> &data) 
{        
  // TBD
}


// **************************************************************************
carma::canbus::Message Loberotator::simPhasePacket(int chanNo)
{
    int chanIndex = 4*getBoardIndex() + (chanNo - 1);
    if ((chanNo < 1) || (chanNo > 4)) {
        ostringstream o;
        o << "Channel number (" << chanNo << ") must be in range [1-4]";
        throw CARMA_ERROR(o); 
    }
    if ((chanIndex < 0) || (chanIndex > 23)) {
        ostringstream o;
        o << "Channel index (" << chanIndex << ") must be in range [0-23]";
        throw CARMA_ERROR(o); 
    }
        
    canbus::Message msg = createMsgToHost(BLANK_FRAME_PACKET_1 + chanNo - 1);
    Loberotator& lr = master().loberotator(chanIndex);
    msg << static_cast<long>(1000*lr.rateSim().simData());
    msg << static_cast<unsigned long>(1000*lr.phaseSim().simData());

    return msg;
}


// **************************************************************************
carma::canbus::Message Loberotator::simPacket5()
{
    int chanBaseIndex = 4*getBoardIndex();
    canbus::Message msg = createMsgToHost(BLANK_FRAME_PACKET_5);

    for (int c=0; c < 4; c++) {
        int chanIndex = chanBaseIndex + c; // channel index
        Loberotator& lr = master().loberotator(chanIndex);
        msg << static_cast<unsigned char>(lr.psColumnStateSim().simData());
    }
    for (int c=0; c < 4; c++) {
        int chanIndex = chanBaseIndex + c; // channel index
        Loberotator& lr = master().loberotator(chanIndex);
        msg << static_cast<unsigned char>(lr.controlStateSim().simData());
    }

    return msg;
}

// **************************************************************************
carma::canbus::Message Loberotator::simPacket6()
{
    canbus::Message msg = createMsgToHost(BLANK_FRAME_PACKET_6);
    Loberotator& lr = master().loberotator(4*getBoardIndex());

    msg << static_cast<short>(100*lr.tempSim().simData());
    msg << static_cast<unsigned char>(lr.ppsStateSim().simData());
    msg << static_cast<unsigned char>(lr.hbStateSim().simData());
    msg << static_cast<unsigned char>(lr.psStateSim().simData());
    msg << static_cast<unsigned char>(lr.dataValidSim().simData());
    msg << static_cast<short>(lr.timeOffsetSim().simData());
    return msg;
}

// **************************************************************************
carma::canbus::Message Loberotator::simPacket7()
{
    canbus::Message msg = createMsgToHost(BLANK_FRAME_PACKET_7);
    Loberotator& lr = master().loberotator(4*getBoardIndex());

    msg << static_cast<short>(1000*lr.ps5vaSim().simData());
    msg << static_cast<short>(1000*lr.ps5vdSim().simData());
    msg << static_cast<short>(1000*lr.psNeg5vSim().simData());
    msg << static_cast<short>(1000*lr.ps24vSim().simData());
    return msg;
}




