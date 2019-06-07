

/**
 *
 * Implementation of the PhysicalDevice class.
 *
 * @author: Steve Scott
 *
 * $Id: PhysicalDevice.cc,v 1.22 2009/09/21 15:52:02 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */
 

#include <sstream>

#include "carma/monitor/PhysicalDevice.h"
#include "carma/util/ErrorException.h"


using namespace ::std;
using namespace carma::monitor;
using namespace carma::util;



PhysicalDevice::PhysicalDevice()
{
}

PhysicalDevice::PhysicalDevice( LOCATION locationID,
                                DEVICE   deviceID ) :
deviceID_(deviceID),
locationID_(locationID)
{
}

PhysicalDevice::~PhysicalDevice()
{
}


PhysicalDevice::DEVICE PhysicalDevice::getDevice() const 
{
	return deviceID_;
}

void PhysicalDevice::setDevice(PhysicalDevice::DEVICE deviceID)
{
	deviceID_ = deviceID;
}

PhysicalDevice::LOCATION PhysicalDevice::getLocation() const 
{
	return locationID_;
}

void PhysicalDevice::setLocation(LOCATION locationID)
{
	locationID_ = locationID;
}

PhysicalDevice::LOCATION PhysicalDevice::getBimaAntenna(int antennaNo)
{
    return static_cast<PhysicalDevice::LOCATION>(BIMA1 + antennaNo - 1);
} 

PhysicalDevice::LOCATION PhysicalDevice::getOvroAntenna(int antennaNo)
{
    return static_cast<PhysicalDevice::LOCATION>(OVRO1 + antennaNo - 1);
} 

PhysicalDevice::LOCATION PhysicalDevice::getSzaAntenna(int antennaNo) 
{
    return static_cast<PhysicalDevice::LOCATION>(SZA1 + antennaNo - 1);
} 

PhysicalDevice::DEVICE 
PhysicalDevice::getWbdcModule(int bandIndex, int inputIndex) 
{
    int dev = WB_DOWNCONVERTER1 + bandIndex*8 + inputIndex;
    return static_cast<PhysicalDevice::DEVICE>(dev);
} 

PhysicalDevice::DEVICE 
PhysicalDevice::getSldcModule(int bandIndex, int inputIndex) 
{
    int dev = SL_DOWNCONVERTER1 + bandIndex*15 + inputIndex;
    return static_cast<PhysicalDevice::DEVICE>(dev);
} 

string PhysicalDevice::getLocationString() const 
{
	return strings().getLocationString(getLocation());
}

string PhysicalDevice::getLocationString(LOCATION locationID) const 
{
    return strings().getLocationString(locationID);
}

string PhysicalDevice::getDeviceString() const 
{
	return strings().getDeviceString(getDevice());
}

string PhysicalDevice::getDeviceString(DEVICE device) const 
{
	return strings().getDeviceString(device);
}

string PhysicalDevice::toString() const 
{
	return getLocationString() + "." + getDeviceString();
}

string PhysicalDevice::toStringAll() 
{
	return strings().toString();
}


string PhysicalDevice::Strings::getLocationString(LOCATION locationID) const 
{
    return locationString_[locationID];
}

string PhysicalDevice::Strings::getDeviceString(DEVICE deviceID) const 
{
    return deviceString_[deviceID];
}

string PhysicalDevice::Strings::toString() const 
{
    ostringstream o;
    o << "LOCATIONS" << endl;
    for(int i=0; i<NUM_LOCATIONS; i++)o << i << ":" <<locationString_[i] << endl;
    o << "DEVICES" << endl;
    for(int i=0; i<NUM_DEVICES; i++)o << i << ":" <<deviceString_[i] << endl;
    return o.str();
}

PhysicalDevice::Strings::Strings()
{
    locationString_[NOLOCATION]             = "none";
    locationString_[BIMA1]                  = "bimaAnt#1";
    locationString_[BIMA2]                  = "bimaAnt#2";
    locationString_[BIMA3]                  = "bimaAnt#3";
    locationString_[BIMA4]                  = "bimaAnt#4";
    locationString_[BIMA5]                  = "bimaAnt#5";
    locationString_[BIMA6]                  = "bimaAnt#6";
    locationString_[BIMA7]                  = "bimaAnt#7";
    locationString_[BIMA8]                  = "bimaAnt#8";
    locationString_[BIMA9]                  = "bimaAnt#9";
    locationString_[OVRO1]                  = "ovroAnt#1";
    locationString_[OVRO2]                  = "ovroAnt#2";
    locationString_[OVRO3]                  = "ovroAnt#3";
    locationString_[OVRO4]                  = "ovroAnt#4";
    locationString_[OVRO5]                  = "ovroAnt#5";
    locationString_[OVRO6]                  = "ovroAnt#6";
    locationString_[SZA1]                   = "szaAnt#1";
    locationString_[SZA2]                   = "szaAnt#2";
    locationString_[SZA3]                   = "szaAnt#3";
    locationString_[SZA4]                   = "szaAnt#4";
    locationString_[SZA5]                   = "szaAnt#5";
    locationString_[SZA6]                   = "szaAnt#6";
    locationString_[SZA7]                   = "szaAnt#7";
    locationString_[SZA8]                   = "szaAnt#8";
    locationString_[WBDC_HOST]              = "wbdcHost"; // Wideband downconverter
    locationString_[SL_CORREL_BAND1]        = "slcBand1";  // Wideband correl
    locationString_[SL_CORREL_BAND2]        = "slcBand2";
    locationString_[SL_CORREL_BAND3]        = "slcBand3";
    locationString_[SL_CORREL_BAND4]        = "slcBand4";
    locationString_[SL_CORREL_BAND5]        = "slcBand5";
    locationString_[SL_CORREL_BAND6]        = "slcBand6";
    locationString_[SL_CORREL_BAND7]        = "slcBand7";
    locationString_[SL_CORREL_BAND8]        = "slcBand8";
    locationString_[SLDC_HOST]              = "sldcHost";
    locationString_[LOBEROTATOR_CONTROLLER] = "loberotatorController";
    locationString_[WB_CORREL_BAND1]        = "wbcBand1";  // Wideband correl
    locationString_[WB_CORREL_BAND2]        = "wbcBand2";
    locationString_[WB_CORREL_BAND3]        = "wbcBand3";
    locationString_[WB_CORREL_BAND4]        = "wbcBand4";
    locationString_[WB_CORREL_BAND5]        = "wbcBand5";
    locationString_[WB_CORREL_BAND6]        = "wbcBand6";
    locationString_[WB_CORREL_BAND7]        = "wbcBand7";
    locationString_[WB_CORREL_BAND8]        = "wbcBand8";
    locationString_[WB_CORREL_BAND9]        = "wbcBand9";
    locationString_[WB_CORREL_BAND10]       = "wbcBand10";
    locationString_[WB_CORREL_BAND11]       = "wbcBand11";
    locationString_[WB_CORREL_BAND12]       = "wbcBand12";
    locationString_[WB_CORREL_BAND13]       = "wbcBand13";
    locationString_[WB_CORREL_BAND14]       = "wbcBand14";
    locationString_[WB_CORREL_BAND15]       = "wbcBand15";
    locationString_[WB_CORREL_BAND16]       = "wbcBand16";
    locationString_[LAB]                    = "lab";
    locationString_[PHASEMONITOR]           = "phaseMonitor";


    deviceString_[NODEVICE]                 = "none";
    deviceString_[WB_QUADMOD1]              = "WbQuadMod1";
    deviceString_[WB_QUADMOD2]              = "WbQuadMod2";
    deviceString_[WB_QUADMOD3]              = "WbQuadMod3";
    deviceString_[WB_QUADMOD4]              = "WbQuadMod4";
    deviceString_[WB_QUADMOD5]              = "WbQuadMod5";
    deviceString_[WB_QUADMOD6]              = "WbQuadMod6";
    deviceString_[WB_QUADMOD7]              = "WbQuadMod7";
    deviceString_[WB_QUADMOD8]              = "WbQuadMod8";
    deviceString_[SL_QUADMOD1]              = "SlQuadMod1";
    deviceString_[SL_QUADMOD2]              = "SlQuadMod2";
    deviceString_[SL_QUADMOD3]              = "SlQuadMod3";
    deviceString_[SL_QUADMOD4]              = "SlQuadMod4";
    deviceString_[SL_QUADMOD5]              = "SlQuadMod5";
    deviceString_[SL_QUADMOD6]              = "SlQuadMod6";
    deviceString_[SL_QUADMOD7]              = "SlQuadMod7";
    deviceString_[SL_QUADMOD8]              = "SlQuadMod8";
    deviceString_[SL_QUADMOD9]              = "SlQuadMod9";
    deviceString_[SL_QUADMOD10]             = "SlQuadMod10";
    deviceString_[SL_QUADMOD11]             = "SlQuadMod11";
    deviceString_[SL_QUADMOD12]             = "SlQuadMod12";
    deviceString_[SL_QUADMOD13]             = "SlQuadMod13";
    deviceString_[SL_QUADMOD14]             = "SlQuadMod14";
    deviceString_[SL_QUADMOD15]             = "SlQuadMod15";
    deviceString_[WB_NOISE_SOURCE]          = "WbNoiseSource";
    deviceString_[SL_NOISE_SOURCE]          = "SlNoiseSource";
    deviceString_[WB_LO_MONITOR]            =  "WbLOmon";
    deviceString_[SL_LO_MONITOR]            =  "SlLOmon";
    deviceString_[LR_CHAN1]                 = "loberotChannel1";
    deviceString_[LR_CHAN2]                 = "loberotChannel2";
    deviceString_[LR_CHAN3]                 = "loberotChannel3";
    deviceString_[LR_CHAN4]                 = "loberotChannel4";
    deviceString_[LR_CHAN5]                 = "loberotChannel5";
    deviceString_[LR_CHAN6]                 = "loberotChannel6";
    deviceString_[LR_CHAN7]                 = "loberotChannel7";
    deviceString_[LR_CHAN8]                 = "loberotChannel8";
    deviceString_[LR_CHAN9]                 = "loberotChannel9";
    deviceString_[LR_CHAN10]                = "loberotChannel10";
    deviceString_[LR_CHAN11]                = "loberotChannel11";
    deviceString_[LR_CHAN12]                = "loberotChannel12";
    deviceString_[LR_CHAN13]                = "loberotChannel13";
    deviceString_[LR_CHAN14]                = "loberotChannel14";
    deviceString_[LR_CHAN15]                = "loberotChannel15";
    deviceString_[LR_CHAN16]                = "loberotChannel16";
    deviceString_[LR_CHAN17]                = "loberotChannel17";
    deviceString_[LR_CHAN18]                = "loberotChannel18";
    deviceString_[LR_CHAN19]                = "loberotChannel19";
    deviceString_[LR_CHAN20]                = "loberotChannel20";
    deviceString_[LR_CHAN21]                = "loberotChannel21";
    deviceString_[LR_CHAN22]                = "loberotChannel22";
    deviceString_[LR_CHAN23]                = "loberotChannel23";
    deviceString_[LR_BOARD1]                = "loberotBoard1";
    deviceString_[LR_BOARD2]                = "loberotBoard2";
    deviceString_[LR_BOARD3]                = "loberotBoard3";
    deviceString_[LR_BOARD4]                = "loberotBoard4";
    deviceString_[LR_BOARD5]                = "loberotBoard5";
    deviceString_[LR_BOARD6]                = "loberotBoard6";
    deviceString_[WB_DIGITIZER1]            = "wbDigBoard1";
    deviceString_[WB_DIGITIZER2]            = "wbDigBoard2";
    deviceString_[WB_DIGITIZER3]            = "wbDigBoard3";
    deviceString_[WB_DIGITIZER4]            = "wbDigBoard4";
    deviceString_[WB_CORRELATOR1]           = "wbCorBoard1";
    deviceString_[WB_CORRELATOR2]           = "wbCorBoard2";
    deviceString_[WB_CORRELATOR3]           = "wbCorBoard3";
    deviceString_[YIG]                      = "yigOscillator";
    deviceString_[GUNN1CM]                  = "gunn1cm";
    deviceString_[GUNN3MM]                  = "gunn3mm";
    deviceString_[GUNN1MM]                  = "gunn1mm";
    deviceString_[IFBOX]                    = "IFbox";
    deviceString_[CRYO_COMPRESSOR]          = "compressor";
    deviceString_[CRYO_DEWAR]               = "dewar";
    deviceString_[CALIBRATOR]               = "calibrator";
    deviceString_[DRIVE]                    = "drives";
    deviceString_[MIXER_1MM]                = "1mmMixer";
    deviceString_[MIXER_3MM]                = "3mmMixer";
    deviceString_[WEATHER_STATION]          = "weatherStation";
    deviceString_[RX_TEMPERATURE_CONTROL]   = "rxTemperature";


    // @todo: can somebody give this 128 thing a name?
    for (int i=0; i< 128; i++) {
        ostringstream s;
        
        s <<  "wbdcModuleBand" << i/8 + 1 << "Input" << i%8 + 1;
        deviceString_[WB_DOWNCONVERTER1+i] = s.str();           
    }

    // @todo: can somebody give this 120 thing a name?
    for (int i=0; i < 120; i++) {
        ostringstream s;
        
        s <<  "sldcModuleBand" << i/15 + 1 << "Input" << i%15 + 1;
        deviceString_[SL_DOWNCONVERTER1+i] = s.str();           
    }
    
    deviceString_[WIDGET1]                = "widget1";
    deviceString_[WIDGET2]                = "widget2";
    deviceString_[WIDGET3]                = "widget3";
    deviceString_[WIDGET4]                = "widget4";
    deviceString_[WIDGET5]                = "widget5";
    deviceString_[WIDGET6]                = "widget6";
    deviceString_[WIDGET7]                = "widget7";
    deviceString_[WIDGET8]                = "widget8";
    deviceString_[WIDGET9]                = "widget9";
    deviceString_[WIDGET10]               = "widget10";
    deviceString_[BIG_WIDGET]             = "bigWidget";
    deviceString_[MASTER_CLOCK]           = "masterClock";
    deviceString_[TILTMETER]              = "tiltmeter";
    deviceString_[SECONDARY]              = "secondary";
    deviceString_[OVRO_OPTICS]            = "ovroOptics";
    deviceString_[ENVIRONMENTAL_MONITOR]  = "environmentalMonitor";
    deviceString_[LOREF_SYNTHESIZER1]      = "lorefSynthesizer1";
    deviceString_[LOREF_SYNTHESIZER2]      = "lorefSynthesizer2";
    deviceString_[LOREF_SYNTHESIZER3]      = "lorefSynthesizer3";
    deviceString_[LOREF_DISTRIBUTION_BOX1] = "lorefDistributionBox1";
    deviceString_[LOREF_DISTRIBUTION_BOX2] = "lorefDistributionBox2";
    deviceString_[LOREF_DISTRIBUTION_BOX3] = "lorefDistributionBox3";
    deviceString_[LOREF_MONITOR]           = "lorefMonitor";
    deviceString_[LL_OPTICAL_RXTX_BOX1]    = "llOptRxTxBox1";
    deviceString_[LL_OPTICAL_RXTX_BOX2]    = "llOptRxTxBox2";
    deviceString_[LL_OPTICAL_RXTX_BOX3]    = "llOptRxTxBox3";
    deviceString_[SL_LO_CONTROL]           = "slLoControl";
    
    for (int i=0; i < 15; i++) {
        ostringstream s;
        
        s <<  "blockDownconverter" << (i + 1);
        deviceString_[BLOCK_DOWNCONVERTER1 +i ] = s.str();           
    }
}

const PhysicalDevice::Strings& PhysicalDevice::strings() 
{
    static const Strings s;
    return s;
}
