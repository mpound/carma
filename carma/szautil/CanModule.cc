#include "carma/szautil/CanModule.h"

using namespace sza::util;

const short CanModule::biasApiNo_        =  16;
const short CanModule::caltertApiNo_     =   8;
const short CanModule::dconApiNo_        = 130;
const short CanModule::ifmodApiNo_       = 224;
const short CanModule::intmodApiNo_      = 184;
const short CanModule::loMonitorApiNo_   = 192;

#ifdef CARMA_LR_API_113
const short CanModule::lobeRotatorApiNo_ = 113;
#else
const short CanModule::lobeRotatorApiNo_ = 112;
#endif

const short CanModule::noiseSourceApiNo_ =  97;

#ifdef CARMA_QM_API_66
const short CanModule::quadModApiNo_     =  66;
#else
const short CanModule::quadModApiNo_     =  65;
#endif

const short CanModule::receiverApiNo_    = 176;
const short CanModule::thermalApiNo_     = 104;
const short CanModule::tiltMeterApiNo_   =  40;
const short CanModule::varactorApiNo_    =  48;
const short CanModule::yigApiNo_         =  80;

String CanModule::biasApiStr_        = String(CanModule::biasApiNo_);
String CanModule::caltertApiStr_     = String(CanModule::caltertApiNo_);
String CanModule::ifmodApiStr_       = String(CanModule::ifmodApiNo_);
String CanModule::intmodApiStr_      = String(CanModule::intmodApiNo_);
String CanModule::loMonitorApiStr_   = String(CanModule::loMonitorApiNo_);
String CanModule::lobeRotatorApiStr_ = String(CanModule::lobeRotatorApiNo_);
String CanModule::noiseSourceApiStr_ = String(CanModule::noiseSourceApiNo_);
String CanModule::quadModApiStr_     = String(CanModule::quadModApiNo_);
String CanModule::receiverApiStr_    = String(CanModule::receiverApiNo_);
String CanModule::thermalApiStr_     = String(CanModule::thermalApiNo_);
String CanModule::tiltMeterApiStr_   = String(CanModule::tiltMeterApiNo_);
String CanModule::varactorApiStr_    = String(CanModule::varactorApiNo_);
String CanModule::yigApiStr_         = String(CanModule::yigApiNo_);
