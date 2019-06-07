/** @file
 * Definition of carma::canbus::Device class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.33 $
 * $Date: 2011/01/27 20:59:57 $
 * $Id: Device.cc,v 1.33 2011/01/27 20:59:57 abeard Exp $ 
 */

// Carma includes
#include "carma/canbus/Device.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/Time.h"

using namespace carma::canbus;
using namespace carma::util;
using namespace std;

// Unfortunately you must still allocate memory for the static const objects
// EVEN though they are declared and initialized within the Device class!
const msgType Device::RESET; 
const msgType Device::STOP_CHANNEL_1_FAST_SAMPLING;
const msgType Device::STOP_CHANNEL_2_FAST_SAMPLING;
const msgType Device::START_CHANNEL_1_FAST_SAMPLING; 
const msgType Device::START_CHANNEL_2_FAST_SAMPLING; 

// -----------------------------------------------------------------------------
Device::Device(apiType api, nodeType node, CanOutput &io) :
api_(api), node_(node), key_( Device::createKey( api, node ) ), io_(io) 
{
    // Set busId to ALL_BUSSES until Master resets it to the actual bus.
    // In the case of node 0 devices, it should remain ALL_BUSSES.
    sharedData_.busId = ALL_BUSSES; 	
    sharedData_.state = OFFLINE;
    sharedData_.lastRxMjd = 0.0;
    sharedData_.nLatePackets = 0;

    // Initialize the mutex...
    if (pthread_mutex_init(&sharedData_.mutex, 0) != 0)
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "Device::Device() - Unable to initialize sharedData_ mutex.");
            
}

// -----------------------------------------------------------------------------
Device::~Device() 
{
    // Destroy the device mutex
    pthread_mutex_destroy(&sharedData_.mutex); 
        
}

// -----------------------------------------------------------------------------
apiType Device::getApi() const 
{
    return api_;
}

// -----------------------------------------------------------------------------
nodeType Device::getNode() const
{
    return node_;
}

// -----------------------------------------------------------------------------
keyType Device::getKey( ) const
{
    return key_;
}

// -----------------------------------------------------------------------------
keyType Device::createKey( const apiType api, const nodeType node )
{
    keyType key;
    key = (api << 9) + node;
    key = key & 0x01ffff;
    return key;
}

// -----------------------------------------------------------------------------
boardType Device::getBoardType() const
{
    boardType temp;
    pthread_mutex_lock(&sharedData_.mutex);
    temp = sharedData_.boardType;
    pthread_mutex_unlock(&sharedData_.mutex);
    return temp;
}

// -----------------------------------------------------------------------------
serialNumberType Device::getSerialNumber() const
{
    serialNumberType temp;
    pthread_mutex_lock(&sharedData_.mutex);
    temp = sharedData_.serialNumber;
    pthread_mutex_unlock(&sharedData_.mutex);
    return temp;
}

// -----------------------------------------------------------------------------
busIdType Device::getBusId() const
{
    busIdType temp;
    pthread_mutex_lock(&sharedData_.mutex);
    temp = sharedData_.busId;
    pthread_mutex_unlock(&sharedData_.mutex);
    return temp;
}

// -----------------------------------------------------------------------------
deviceStateType Device::getState() const
{
    deviceStateType temp;
    pthread_mutex_lock(&sharedData_.mutex);
    temp = sharedData_.state;
    pthread_mutex_unlock(&sharedData_.mutex);
    return temp;
}

// -----------------------------------------------------------------------------
double Device::getLastRxTime() const
{
    double temp;
    pthread_mutex_lock(&sharedData_.mutex);
    temp = sharedData_.lastRxMjd;
    pthread_mutex_unlock(&sharedData_.mutex);
    return temp;
}

// -----------------------------------------------------------------------------
unsigned int Device::getNlatePackets() const
{
    unsigned int temp;
    pthread_mutex_lock(&sharedData_.mutex);
    temp = sharedData_.nLatePackets;
    pthread_mutex_unlock(&sharedData_.mutex);
    return temp;
}

// -----------------------------------------------------------------------------
char Device::getApiVersion() const
{
    char temp;
    pthread_mutex_lock(&sharedData_.mutex);
    temp = sharedData_.apiVer;
    pthread_mutex_unlock(&sharedData_.mutex);
    return temp;
}

// -----------------------------------------------------------------------------
MsgIdInfoMap Device::getControls() const 
{
    static bool init = false;
    static map<msgType, string> cntrls; // Controls

    if (!init) {
        cntrls[RESET]                         = "Software reset";
        cntrls[STOP_CHANNEL_1_FAST_SAMPLING]  = "Stop channel 1 fast sampling";
        cntrls[STOP_CHANNEL_2_FAST_SAMPLING]  = "Stop channel 2 fast sampling";
        cntrls[START_CHANNEL_1_FAST_SAMPLING] = "Start channel 1 fast sampling";
        cntrls[START_CHANNEL_2_FAST_SAMPLING] = "Start channel 2 fast sampling";
        init = true;
    }
    return cntrls;
}

// -----------------------------------------------------------------------------
void Device::setState(deviceStateType state) 
{
    pthread_mutex_lock(&sharedData_.mutex);
    sharedData_.state = state;
    pthread_mutex_unlock(&sharedData_.mutex);
}

// -----------------------------------------------------------------------------
void Device::setBusId(busIdType busId) 
{
    pthread_mutex_lock(&sharedData_.mutex);
    sharedData_.busId = busId;
    pthread_mutex_unlock(&sharedData_.mutex);
}

// -----------------------------------------------------------------------------
void Device::setSerialNumber(serialNumberType sn)
{
    pthread_mutex_lock(&sharedData_.mutex);
    sharedData_.serialNumber = sn;
    pthread_mutex_unlock(&sharedData_.mutex);
}

// -----------------------------------------------------------------------------
void Device::setBoardType(boardType bt)
{
    pthread_mutex_lock(&sharedData_.mutex);
    sharedData_.boardType = bt;
    pthread_mutex_unlock(&sharedData_.mutex);
}

// -----------------------------------------------------------------------------
void Device::setLastRxTime(double rxMjd) 
{
    pthread_mutex_lock(&sharedData_.mutex);
    sharedData_.lastRxMjd = rxMjd;
    pthread_mutex_unlock(&sharedData_.mutex);
}

// -----------------------------------------------------------------------------
void Device::incrementLatePacketCount() 
{
    pthread_mutex_lock(&sharedData_.mutex);
    sharedData_.nLatePackets++;
    pthread_mutex_unlock(&sharedData_.mutex);
}

// -----------------------------------------------------------------------------
void Device::resetLatePacketCount() 
{
    pthread_mutex_lock(&sharedData_.mutex);
    sharedData_.nLatePackets = 0;
    pthread_mutex_unlock(&sharedData_.mutex);
}

// -----------------------------------------------------------------------------
bool Device::isPacketLate(double window)
{
    frameType lastRxFrame = Time::computeFrame(getLastRxTime());
    frameType currentFrame = Time::computeCurrentFrame();
    double ms = getLastRxTime() - Time::MJD(lastRxFrame); // Ms after 1/2 sec
    bool answer = false;
    
    // Convert to ms
    ms = ms * Time::MILLISECONDS_PER_DAY;
    
    if (lastRxFrame == currentFrame) {
        answer = ((ms >= window) && (ms <= 490));
    } else if (lastRxFrame == currentFrame - 1) {
        answer = (ms <= 490);
    } else {
        // Now it's really late. 
        answer = true;
    }
    return answer;
}
    
// -----------------------------------------------------------------------------
void Device::setApiVersion(char api)
{
    pthread_mutex_lock(&sharedData_.mutex);
    sharedData_.apiVer = api;
    pthread_mutex_unlock(&sharedData_.mutex);
}

// -----------------------------------------------------------------------------
void Device::reset() 
{
    idType address = createId(false, api_, node_, RESET);		
    busIdType busId = (node_ == 0 ? ALL_BUSSES : getBusId());;
    vector<byteType> data;
    data.push_back(0xE1);
    data.push_back(0x1E);
    data.push_back(0xA5);
    data.push_back(0x5A);
    data.push_back(0xC3);
    data.push_back(0x3C);
    data.push_back(0x96);
    data.push_back(0x69);
    Message msg(address, data, busId);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void Device::stopChannelOneFastSampling()
{
    idType address = createId(false, 0, 0, STOP_CHANNEL_1_FAST_SAMPLING);
    busIdType busId = ALL_BUSSES;
    vector<byteType> data;
    Message msg(address, data, busId);
    io_.postMessage(msg);
}
    
// -----------------------------------------------------------------------------
void Device::stopChannelTwoFastSampling()
{
    idType address = createId(false, 0, 0, STOP_CHANNEL_2_FAST_SAMPLING);
    busIdType busId = ALL_BUSSES;
    vector<byteType> data;
    Message msg(address, data, busId);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void Device::startChannelOneFastSampling(unsigned short fastItem)
{
    idType address = createId(false, 0, 0, STOP_CHANNEL_1_FAST_SAMPLING);
    busIdType busId = (node_ == 0 ? ALL_BUSSES : getBusId());
    vector<byteType> data;
    uShortToData(data, fastItem);
    Message msg(address, data, busId);
    
    // First send message to stop channel 1 fast sampling.
    stopChannelOneFastSampling();

    // Next send message to start channel 1 fast sampling.
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void Device::startChannelTwoFastSampling(unsigned short fastItem)
{
    idType address = createId(false, 0, 0, STOP_CHANNEL_2_FAST_SAMPLING);
    busIdType busId = (node_ == 0 ? ALL_BUSSES : getBusId());
    vector<byteType> data;
    uShortToData(data, fastItem);
    Message msg(address, data, busId);
    
    // First send message to stop channel 2 fast sampling.
    stopChannelTwoFastSampling();

    // Next send message to start channel 2 fast sampling.
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void Device::updateFrameData()
{
    // Empty on purpose - a derivative should implement this routine.
}

// -----------------------------------------------------------------------------
carma::canbus::Message Device::createMsgToHost(msgType messageId) const
{
    ScopedLock< ::pthread_mutex_t > scopelock(sharedData_.mutex);
    idType id = createId( TO_HOST, api_, node_, messageId );
    carma::canbus::Message msg( id, sharedData_.busId );
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Device::createMsgToNode(msgType messageId) const
{
    ScopedLock< ::pthread_mutex_t > scopelock(sharedData_.mutex);
    idType id = createId( TO_NODES, api_, node_, messageId );
    carma::canbus::Message msg( id, sharedData_.busId );
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Device::createMsgToAllNodes(msgType messageId) const
{
    idType id = createId( TO_NODES, api_, 0, messageId );
    carma::canbus::Message msg( id, ALL_BUSSES );
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Device::createDummyMsg( ) const
{
    idType id = createId( TO_NODES, DUMMY_PKT_API, 0, DUMMY_PKT_MID );
    carma::canbus::Message msg( id, sharedData_.busId );
    return msg;
}
